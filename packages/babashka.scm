(define-module (babashka)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix download)
	       #:use-module (guix utils)
               #:use-module (gnu packages base)
               #:use-module (gnu packages compression)
               #:use-module (gnu packages gcc)
	       #:use-module (nonguix build-system binary))


(define-public babashka
  (package
    (name "babashka")
    (version "1.3.188")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/borkdude/babashka/releases/download/v" version "/babashka-" version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "06gm3m7jw1if08p9h3ar4vqwbdl362c8i6bkbqy9l7z873x5flsk"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux" "i686-linux"))
    (arguments
     `(#:patchelf-plan
       `(("bb"
          ("libc" "zlib" "libstdc++")))
       #:install-plan
       `(("." ("bb") "bin/"))
       #:phases
       (modify-phases %standard-phases
		      ;; this is required because standard unpack expects
		      ;; the archive to contain a directory with everything inside it,
		      ;; while babashka's release .tar.gz only contains the `bb` binary.
		      (replace 'unpack
			       (lambda* (#:key inputs #:allow-other-keys)
					(system* (which "tar")
						 "-xf"
						 (assoc-ref inputs "source"))
					#t)))))
    (inputs
      `(("libstdc++" ,(make-libstdc++ gcc))
        ("zlib" ,zlib)))
    (native-inputs
      `(("tar" ,tar)))
    (synopsis "A Clojure babushka for the grey areas of Bash")
    (description
      "The main idea behind babashka is to leverage Clojure in places
where you would be using bash otherwise.")
    (home-page "https://github.com/borkdude/babashka")
    (license license:epl1.0)))
