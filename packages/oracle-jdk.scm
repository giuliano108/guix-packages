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
          ("zlib"))
	 ("lib/jspawnhelper"
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
             (let ((binaries-to-fix (list "bin/jaotc" "bin/jar" "bin/jarsigner" "bin/java" "bin/javac" "bin/javadoc" "bin/javap" "bin/jcmd"
                                          "bin/jconsole" "bin/jdb" "bin/jdeprscan" "bin/jdeps" "bin/jfr" "bin/jhsdb" "bin/jimage" "bin/jinfo"
                                          "bin/jjs" "bin/jlink" "bin/jmap" "bin/jmod" "bin/jpackage" "bin/jps" "bin/jrunscript" "bin/jshell"
                                          "bin/jstack" "bin/jstat" "bin/jstatd" "bin/keytool" "bin/rmic" "bin/rmid" "bin/rmiregistry"
                                          "bin/serialver" "lib/libattach.so" "lib/libawt_headless.so" "lib/libawt.so" "lib/libawt_xawt.so"
                                          "lib/libextnet.so" "lib/libfontmanager.so" "lib/libinstrument.so" "lib/libjavajpeg.so" "lib/libjava.so"
                                          "lib/libjawt.so" "lib/libjdwp.so" "lib/libjimage.so" "lib/libjsound.so" "lib/jspawnhelper"
                                          "lib/liblcms.so" "lib/libmanagement_agent.so" "lib/libmanagement_ext.so" "lib/libmanagement.so"
                                          "lib/libmlib_image.so" "lib/libnet.so" "lib/libnio.so" "lib/libprefs.so" "lib/librmi.so"
                                          "lib/libsctp.so" "lib/libsplashscreen.so" "lib/libverify.so" "lib/libzip.so")))
               (for-each (lambda (binary)
                           (let* ((dest (string-append "share/" ,name "-" ,version))
                                  (file-to-patch (string-join (list %output dest binary) "/"))
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
    (home-page "https://www.oracle.com/java/technologies/javase-downloads.html")
    (license (make-license "Oracle Technology Network License Agreement for Oracle Java SE"
                           "https://www.oracle.com/downloads/licenses/javase-license1.html"
                           ""))))
