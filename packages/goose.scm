(define-module (goose)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary)
  )

(define-public goose
  (package
    (name "goose")
    (version "1.29.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/block/goose/releases/download/"
                                  version "/goose-x86_64-unknown-linux-gnu.tar.bz2"))
              (sha256 (base32 "13s5b4xqanji6dljiv9zccr6zq9sf8j9sz8krzvvq8v11141ar3i"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:patchelf-plan
       `(("goose"
          ("glibc" "gcc" "zlib" "libomp")))
       #:install-plan
       `(("." ("goose") "bin/"))
       #:phases
       (modify-phases %standard-phases
		      ;; this is required because standard unpack expects
		      ;; the archive to contain a directory with everything inside it,
		      ;; while goose's release .zip only contains the `goose` binary.
		      (replace 'unpack
			       (lambda* (#:key inputs #:allow-other-keys)
					(system* (which "tar") "-xjf"
						 (assoc-ref inputs "source"))
					#t)))))
    (inputs
     (list glibc
           (list gcc "lib")
           libomp
           zlib))
    (native-inputs
      `(("tar" ,tar)))
    (home-page "https://block.github.io/goose/")
    (synopsis "AI agent CLI")
    (description "an open source, extensible AI agent that
goes beyond code suggestions - install, execute, edit, and test with any LLM")
    (license license:asl2.0)))
