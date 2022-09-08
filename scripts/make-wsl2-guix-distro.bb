;;;; Creates a WSL2 Guix distribution by automating the steps at:
;;;; https://github.com/giuliano108/guix-packages/blob/master/notes/Guix-on-WSL2.md
;;;; please check this guide for an example of how to use this script.
;;;; Requires `babashka` (https://github.com/babashka/babashka#scoop) and GNU tar (`scoop install tar`)


(require '[clojure.java.io :as io]
         '[clojure.string :as s]
         '[babashka.fs :as fs]
         '[babashka.curl :as curl]
         '[babashka.process :refer [process check]])


(defn slurp-relative [filename]
  (let [base-dir (fs/file (fs/parent (io/file *file*)))]
    (slurp (io/file base-dir filename))))


(def distro-name "guixtest")
(def busybox-version "1.31.0")
(def guix-version "1.3.0")
(def etc-passwd (slurp-relative "etc-passwd.txt"))
(def etc-group (slurp-relative "etc-group.txt"))
(def etc-services (slurp-relative "etc-services.txt"))
(def etc-nsswitch-conf (slurp-relative "etc-nsswitch.conf"))
(def guix-initial-bootstrap (slurp-relative "guix-initial-bootstrap.sh"))
(def boot-script (slurp-relative "boot.sh"))
(def wsl-system-config (slurp-relative "..\\systems\\wsl-config.scm"))
;; Windows bundles bsdtar/libarchive, but we need GNU tar for the `--mode` option.
;; We use `--mode` when creating `rootfs.tar` on Windows, to make `busybox` executable in the archive.
(def gnu-tar
  (-> (io/file (System/getenv "USERPROFILE") "scoop\\shims\\tar.exe")
      .toString))


(defn quit [message]
  (println message)
  (java.lang.System/exit 1))


(defn make-base-directory-structure [name]
  (let [f (io/file name)]
    ;; create base directory unless it already exists
    (when (fs/exists? f)
      (quit (str \` name \` " already exists (either the directory, or the WSL distro by the same name), aborting.")))
    (fs/create-dir f)
    ;; create `rootfs` folder under the base directory
    (-> (io/file name "rootfs")
        fs/canonicalize
        fs/create-dir)))


(defn download-busybox []
  (io/copy
   (:body (curl/get (str "https://busybox.net/downloads/binaries/" busybox-version "-i686-uclibc/busybox")
                    {:as :bytes
                     :compressed false}))
   (io/file distro-name "rootfs" "busybox")))


(defn download-guix []
  (io/copy
   (:body (curl/get (str "https://ftp.gnu.org/gnu/guix/guix-binary-" guix-version ".x86_64-linux.tar.xz")
                    {:as :bytes
                     :compressed false}))
   (io/file distro-name "guix.tar.xz")))


(defn make-rootfs-tar []
  (-> (process [gnu-tar "-C" "rootfs" "--mode=a+x" "-cvf" "rootfs.tar" "."] {:dir distro-name})
      check))


(defn wsl-import []
  (-> (process ["wsl" "--import" distro-name "." "rootfs.tar"] {:dir distro-name})
      check))


(defn busybox-exec
  "Execute a command via busybox in the WSL distro"
  [command]
  (let [command (if (string? command)
                  [command]
                  command)
        command (concat ["wsl" "-d" distro-name "--exec" "/busybox"] command)]
    (-> (process command {:dir distro-name :out :inherit})
        check)))


(defn busybox-write-file
  "Use busybox to write a file inside the WSL distro.
     `content` is anything compatible with `clojure.lang.io/copy`"
  [content filename]
  (-> (process ["wsl" "-d" distro-name "--exec" "/busybox" "sh" "-c" (str "/busybox cat > " filename)]
               {:dir distro-name
                :in content})
      check))


(defn terminate-distro []
  (-> (process ["wsl" "--terminate" distro-name] {:dir distro-name})
      check))


(defn main []
  (println "Downloading busybox, creating the rootfs and the base WSL distro...")
  (make-base-directory-structure distro-name)
  (download-busybox)
  (make-rootfs-tar)
  (wsl-import)

  (busybox-exec (s/split "mkdir -p /root /etc /tmp /var/run /run /home" #" "))
  (busybox-exec (s/split "chmod 1777 /tmp" #" "))
  (busybox-write-file etc-passwd "/etc/passwd")
  (busybox-write-file etc-group "/etc/group")
  (busybox-write-file etc-nsswitch-conf "/etc/nsswitch.conf")
  (busybox-exec (s/split "cp /busybox /bin/mount" #" "))

  (println "Downloading guix and unpacking it inside the WSL distro...")
  (terminate-distro)
  (download-guix)
  (busybox-write-file (io/file distro-name "guix.tar.xz") "/tmp/guix.tar.xz")
  (busybox-exec (s/split "tar -C / -xvJf /tmp/guix.tar.xz" #" "))

  (busybox-write-file etc-services "/etc/services")
  (busybox-write-file guix-initial-bootstrap  "/root/guix-initial-bootstrap.sh")

  (busybox-write-file boot-script  "/root/boot.sh")
  (busybox-exec (s/split "chmod 744 /root/boot.sh" #" "))
  (busybox-write-file wsl-system-config "/root/wsl-config.scm")
  (terminate-distro)

  (println "All done, see what the final steps should be at https://github.com/giuliano108/guix-packages/blob/master/notes/Guix-on-WSL2.md ..."))


(main)


;; -*- mode: clojure; -*-
;; vim: set ft=clojure:
