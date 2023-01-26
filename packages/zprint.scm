(define-module (zprint)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix download)
	       #:use-module (guix utils)
               #:use-module (gnu packages compression)
               #:use-module (gnu packages gcc)
	       #:use-module (nonguix build-system binary))


(define-public zprint
  (package
    (name "zprint")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/kkinnear/zprint/releases/download/" version "/zprintl-" version))
              (sha256
               (base32
                "0m73pfkx7bijv3vfxyjw0sgmnm21d5f2fcf9dbd0klw5jfa5gda2"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux" "i686-linux"))
    (arguments
     `(#:patchelf-plan
       `(("zprint"
          ("libc" "zlib" "libstdc++")))
       #:install-plan
       `(("." ("zprint") "bin/"))
       #:phases
       (modify-phases %standard-phases
		      (replace 'unpack (lambda* (#:key source #:allow-other-keys)
						(invoke "cp" source "zprint")
						(chmod "zprint" #o755))))))
    (inputs
      `(("libstdc++" ,(make-libstdc++ gcc))
        ("zlib" ,zlib)))
    (synopsis "Beautifully format Clojure and Clojurescript source code and s-expressions")
    (description
      "zprint is a library and command line tool providing a variety of pretty printing
capabilities for both Clojure code and Clojure/EDN structures. It can meet almost anyone's needs.
As such, it supports a number of major source code formatting approaches.")
    (home-page "https://github.com/kkinnear/zprint")
    (license license:expat)))  ; MIT
