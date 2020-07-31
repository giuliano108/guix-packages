(define-module (oracle-jdk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix build-system binary))


;;; To install this package:
;;;
;;; * Manually download the JDK from Oracle's website.
;;;   This is a manual step because Oracle want you to accept their EULA before allowing you
;;;   to download anything...
;;; * Choose the plain .tar.gz (Linux Compressed Archive), save it somewhere.
;;; * Run:
;;;   guix install oracle-jdk --with-source=oracle-jdk=/tmp/jdk-14.0.2_linux-x64_bin.tar.gz
;;; * The JDK will be installed under share/oracle-jdk-* . To use it,
;;;   change your $PATH/$JAVA_HOME accordingly or let jEnv do that for you.
;;;
;;; For more info check: https://github.com/giuliano108/guix-packages/blob/master/notes/oracle-jdk.md

(define make-license (@@ (guix licenses) license))

(define-public oracle-jdk
  (package
    (name "oracle-jdk")
    (version "14.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://127.0.0.1/jdk-" version "_linux-x64_bin.tar.gz"))
              (sha256
               (base32
                "1pixb6c6kadrrmsiggav29skxdszwb97pv3bs4lzbh3cja31m0fb"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:patchelf-plan
       `(("bin/jstatd"
          ("zlib"))
         ("bin/jstat"
          ("zlib"))
         ("lib/libawt_xawt.so"
          ("libxext" "libx11" "libxrender" "libxtst" "libxi"))
         ("bin/jshell"
          ("zlib"))
         ("bin/jfr"
          ("zlib"))
         ("bin/javadoc"
          ("zlib"))
         ("lib/libsaproc.so"
          ("libstdc++" "gcc:lib"))
         ("bin/jimage"
          ("zlib"))
         ("bin/jar"
          ("zlib"))
         ("lib/libzip.so"
          ("zlib"))
         ("bin/serialver"
          ("zlib"))
         ("bin/rmic"
          ("zlib"))
         ("bin/jinfo"
          ("zlib"))
         ("bin/javac"
          ("zlib"))
         ("bin/java"
          ("zlib"))
         ("lib/libsplashscreen.so"
          ("libx11" "libxext" "zlib"))
         ("bin/jdeps"
          ("zlib"))
         ("bin/jps"
          ("zlib"))
         ("bin/javap"
          ("zlib"))
         ("bin/jaotc"
          ("zlib"))
         ("bin/jpackage"
          ("zlib"))
         ("lib/libjli.so"
          ("zlib"))
         ("lib/libjsound.so"
          ("alsa-lib"))
         ("bin/jdeprscan"
          ("zlib"))
         ("bin/jmod"
          ("zlib"))
         ("bin/rmid"
          ("zlib"))
         ("lib/libjawt.so"
          ("libxext" "libx11" "libxrender" "libxtst" "libxi"))
         ("bin/jconsole"
          ("zlib"))
         ("lib/libfontmanager.so"
          ("freetype"))
         ("bin/jrunscript"
          ("zlib"))
         ("lib/libinstrument.so"
          ("zlib"))
         ("bin/jmap"
          ("zlib"))
         ("bin/jarsigner"
          ("zlib"))
         ("bin/jhsdb"
          ("zlib"))
         ("bin/rmiregistry"
          ("zlib"))
         ("bin/jdb"
          ("zlib"))
         ("bin/jstack"
          ("zlib"))
         ("bin/jjs"
          ("zlib"))
         ("bin/keytool"
          ("zlib"))
         ("bin/jlink"
          ("zlib"))
         ("bin/jcmd"
          ("zlib")))
       #:install-plan
                                        ;`(("." (".") (string-append "share/" ,name "-" ,version)))
       `(("." (".") "share/oracle-jdk-14.0.2/"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'validate-runpath 'fix-runpath
           (lambda* (. rest)
             (use-modules (ice-9 popen))
             (use-modules (ice-9 rdelim))
             (let ((binaries-to-fix (list "jaotc" "jar" "jarsigner" "java" "javac" "javadoc" "javap" "jcmd" "jconsole" "jdb"
                                          "jdeprscan" "jdeps" "jfr" "jhsdb" "jimage" "jinfo" "jjs" "jlink" "jmap" "jmod"
                                          "jpackage" "jps" "jrunscript" "jshell" "jstack" "jstat" "jstatd" "keytool" "rmic"
                                          "rmid" "rmiregistry" "serialver" "libattach.so" "libawt_headless.so" "libawt.so"
                                          "libawt_xawt.so" "libextnet.so" "libfontmanager.so" "libinstrument.so" "libjavajpeg.so"
                                          "libjava.so" "libjawt.so" "libjdwp.so" "libjimage.so" "libjsound.so" "liblcms.so"
                                          "libmanagement_agent.so" "libmanagement_ext.so" "libmanagement.so" "libmlib_image.so"
                                          "libnet.so" "libnio.so" "libprefs.so" "librmi.so" "libsctp.so" "libsplashscreen.so"
                                          "libverify.so" "libzip.so")))
               (for-each (lambda (binary)
                           (let* ((dest (string-append "share/" ,name "-" ,version))
                                  (prefix (string-append dest (if (string-contains binary ".so")
                                                                  "/lib"
                                                                  "/bin")))
                                  (file-to-patch (string-join (list %output prefix binary) "/"))
                                  (current-runpath (read-line (open-pipe (string-append "patchelf --print-rpath " file-to-patch) OPEN_READ)))
                                  (fixed-runpath (string-append %output "/" dest "/lib:" current-runpath)))
                             (invoke "patchelf" "--set-rpath" fixed-runpath file-to-patch)))
                         binaries-to-fix))
             #t)))))
    (inputs
     `(("libstdc++" ,(make-libstdc++ gcc))
       ("gcc:lib" ,gcc "lib")
       ("alsa-lib" ,alsa-lib)
       ("freetype" ,freetype)
       ("libx11" ,libx11)
       ("libxi" ,libxi)
       ("libxrender" ,libxrender)
       ("libxtst" ,libxtst)
       ("zlib" ,zlib)))
    (synopsis "Oracle Java development kit")
    (description "This package provides the Java development kit Oracle JDK.")
    (home-page "https://github.com/borkdude/oracle-jdk")
    (license (make-license "Oracle Technology Network License Agreement for Oracle Java SE"
                           "https://www.oracle.com/downloads/licenses/javase-license1.html"
                           ""))))
