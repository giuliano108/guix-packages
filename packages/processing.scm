(add-to-load-path "..")

(define-module (processing)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages video)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix build-system binary)
  #:use-module (giuliano108 utils))


(define make-license (@@ (guix licenses) license))

(define-public processing
  (package
    (name "processing")
    (version "3.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.processing.org/processing-" version "-linux64.tgz"))
              (sha256
               (base32
                "0nq2wdfi2kpswf6smi9n4nys9l57gb58knp49wwgrimkkl34bm6y"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (native-inputs
     `(("unzip" ,unzip)
       ("zip" ,zip)))
    (arguments
     `(#:imported-modules ((giuliano108 utils)
                           ,@%binary-build-system-modules)
       #:patchelf-plan
       `(("java/lib/amd64/libjsoundalsa.so"
          ("alsa-lib"))
         ("modes/java/libraries/serial/library/linux64/libjSSC-2.8.so"
          ("libstdc++" "gcc:lib"))
         ("java/lib/amd64/libavplugin-ffmpeg-57.so"
          ("ffmpeg-3.4"))
         ("java/lib/amd64/libgstreamer-lite.so"
          ("alsa-lib"))
         ("java/lib/amd64/libglassgtk3.so"
          ("gtk+" "pango" "atk" "cairo" "gdk-pixbuf" "glib" "libxtst"))
         ("java/lib/amd64/libjfxmedia.so"
          ("alsa-lib"))
         ("java/lib/amd64/libjavafx_font_freetype.so"
          ("freetype"))
         ("java/lib/amd64/libavplugin-57.so"
          ("ffmpeg-3.4"))
         ("java/lib/amd64/libt2k.so"
          ("gcc:lib"))
         ("java/lib/amd64/libnpjp2.so"
          ("gcc:lib"))
         ("java/lib/amd64/libsplashscreen.so"
          ("libx11" "libxext"))
         ("java/lib/amd64/libsunec.so"
          ("gcc:lib"))
         ("java/lib/amd64/libawt_xawt.so"
          ("libxext" "libx11" "libxrender" "libxtst" "libxi"))
         ("java/lib/amd64/libglassgtk2.so"
          ("pango" "atk" "cairo" "gdk-pixbuf" "glib" "freetype" "fontconfig" "libxtst"))
         ("java/lib/amd64/libprism_es2.so"
          ("libx11" "libxxf86vm" "mesa"))
         ("java/lib/amd64/libjawt.so"
          ("libxext" "libx11" "libxrender" "libxtst" "libxi"))
         ("java/lib/amd64/libjavafx_font_pango.so"
          ("pango" "glib" "freetype" "fontconfig"))
         ("java/lib/amd64/libglass.so"
          ("libx11"))
         ("java/lib/amd64/libfontmanager.so"
          ("gcc:lib"))
         ("java/lib/amd64/libunpack.so"
          ("gcc:lib"))

         ;; a few libraries are packaged up in a .jar
         ;; we extract them to a temporary folder so that #:patchelf-plan can fix those too
         ("guix-jogl-all-natives-linux-amd64.jar/natives/linux-amd64/libnativewindow_x11.so"
          ("libx11" "libxxf86vm" "libxrender"))
         ("guix-jogl-all-natives-linux-amd64.jar/natives/linux-amd64/libjogl_mobile.so"
          ("libx11"))
         ("guix-jogl-all-natives-linux-amd64.jar/natives/linux-amd64/libjogl_desktop.so"
          ("libx11"))
         ("guix-jogl-all-natives-linux-amd64.jar/natives/linux-amd64/libnativewindow_awt.so"
          ("libx11" "libxxf86vm" "libxrender"))
         ("guix-jogl-all-natives-linux-amd64.jar/natives/linux-amd64/libnewt.so"
          ("libx11" "libxcursor" "libxrandr")))
       #:install-plan
       `(("." ,(string-append "share/" ,name "-" ,version)))
       #:phases
       (modify-phases %standard-phases

         (add-after 'unpack 'unpack-native-libraries-jars
           (lambda _
             (let* ((jar-dir "core/library")
                    (jar-name "jogl-all-natives-linux-amd64.jar")
                    (jar-path (string-append jar-dir "/" jar-name))
                    (unpacked-jar-dir (string-append "guix-" jar-name)))
               (invoke "unzip" "-d" unpacked-jar-dir jar-path))))

         ;; Find all the binaries that include $ORIGIN in their rpath.
         ;; Save the results to /rpath-origin-components.scm for later use.
         (add-before 'patchelf 'collect-rpath-origin-components
           (lambda _
             (use-modules (ice-9 popen))
             (use-modules (ice-9 rdelim))
             (use-modules (ice-9 regex))
             (use-modules (ice-9 pretty-print))
             (use-modules (srfi srfi-1))
             (use-modules (giuliano108 utils))

             (define (get-runpath binary)
               (read-line (open-pipe (string-append "patchelf --print-rpath " binary) OPEN_READ)))

             (let* ((out (assoc-ref %outputs "out"))
                    (binaries-pattern ".*\\.so$")
                    (binaries-to-fix (find-files "." binaries-pattern))
                    (all-origin-components
                     (remove
                      (lambda (x) (null? (car (cdr x))))
                      (map (lambda (binary)
                             (let* ((input-runpath (get-runpath binary))
                                    (input-runpath-components (string-split input-runpath #\:))
                                    (input-origin-components (filter (lambda (x) (string-contains x "$ORIGIN"))
                                                                     input-runpath-components)))
                               (list binary input-origin-components)))
                           binaries-to-fix))))
               (mkdir out)
               (call-with-output-file (string-append out "/rpath-origin-components.scm")
                 (lambda (p) (pretty-print all-origin-components p))))
             #t))

         ;; Fix binaries that included, before the patchelf phase, $ORIGIN in their rpath.
         (add-before 'validate-runpath 'fix-runpath
           (lambda* (#:key inputs #:allow-other-keys)
             (use-modules (ice-9 popen))
             (use-modules (ice-9 rdelim))
             (use-modules (ice-9 regex))

             (define %out (string-append (assoc-ref %outputs "out") "/share/" ,name "-" ,version))

             (define (get-runpath binary)
               (read-line (open-pipe (string-append "patchelf --print-rpath " binary) OPEN_READ)))

             (define (fix-origin store-path input-filename origin-rpath-component)
               (let* ((path (regexp-substitute #f
                                               (string-match "^\\$ORIGIN" origin-rpath-component)
                                               'pre
                                               (string-append store-path "/" (dirname input-filename))
                                               'post))
                      (canonicalized-path (canonicalize-path path)))
                 canonicalized-path))

             (let* ((out (assoc-ref %outputs "out"))
                    (rpath-origin-components (read (open-file (string-append out "/rpath-origin-components.scm") "r"))))
               (for-each (lambda (x)
                           (let* ((binary (car x))
                                  (output-file (string-append %out "/" binary))
                                  (origin-components (car (cdr x)))
                                  (current-runpath (get-runpath output-file))
                                  (current-runpath-components
                                   (remove
                                    (lambda (x) (string-contains x "$ORIGIN"))
                                    (string-split current-runpath #\:)))
                                  (fixed-origin-components (map (lambda (x) (fix-origin %out binary x))
                                                                origin-components))
                                  (fixed-runpath (string-join
                                                  (append fixed-origin-components
                                                          current-runpath-components)
                                                  ":")))
                             (invoke "patchelf" "--set-rpath"
                                     fixed-runpath
                                     (string-append %out "/" binary))))
                         rpath-origin-components)
               ; if java/bin/java was part of the patchelf-plan, manual --set-interpreter would not be necessary
               (let ((interpreter (car (find-files (assoc-ref inputs "libc") "ld-linux.*\\.so"))))
                 (invoke "patchelf" "--set-interpreter" interpreter (string-append %out "/java/bin/java")))
               (delete-file (string-append out "/rpath-origin-components.scm")))
             #t))

         (add-after 'fix-runpath 'unpack-native-libraries-jars
           (lambda* (#:key inputs outputs #:allow-other-keys)

             (let* ((out-base (assoc-ref outputs "out"))
                    (out (string-append out-base "/share/" ,name "-" ,version))
                    (mesa-libs (string-append (assoc-ref inputs "mesa") "/lib")))
               (with-directory-excursion "guix-jogl-all-natives-linux-amd64.jar"
                 (invoke "zip"  "-r" (string-append out "/jogl-all-natives-linux-amd64.jar") "."))
               (rename-file (string-append out "/jogl-all-natives-linux-amd64.jar")
                            (string-append out "/core/library/jogl-all-natives-linux-amd64.jar"))
               (wrap-program (string-append out "/processing")
                 `("LD_LIBRARY_PATH" ":" prefix (,mesa-libs)))))))))

    (inputs
     `(("libstdc++" ,(make-libstdc++ gcc))
       ("gcc:lib" ,gcc "lib")
       ("alsa-lib" ,alsa-lib)
       ("atk" ,atk)
       ("cairo" ,cairo)
       ("ffmpeg-3.4" ,ffmpeg-3.4)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libx11" ,libx11)
       ("libxcursor" ,libxcursor)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxtst" ,libxtst)
       ("libxxf86vm" ,libxxf86vm)
       ("mesa" ,mesa)
       ("pango" ,pango)
       ("zlib" ,zlib)))
    (synopsis "Programming environment for creating images and animations")
    (description "Processing is a flexible software sketchbook and a language
for learning how to code within the context of the visual arts")
    (home-page "https://www.processing.org")
    (license (make-license "It's complicated"
                           "https://github.com/processing/processing/wiki/FAQ#is-processing-open-source-how-bout-some-code"
                           "Processing ships with the JDK plus other variously licensed software"))))

processing
