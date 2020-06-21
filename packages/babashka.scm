(define-module (babashka)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix download)
	       #:use-module (guix utils)
               #:use-module (gnu packages gcc)
	       #:use-module (gnu packages compression)
	       #:use-module (nonguix build-system binary))


(define-public babashka
  (package
    (name "babashka")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/borkdude/babashka/releases/download/v" version "/babashka-" version "-linux-amd64.zip"))
              (sha256
               (base32
                "030dvfwcz8q8im4h0jm9400d8i0fg46crp0r45xcdd48xg47jn1i"))))
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
		      ;; while babashka's release .zip only contains the `bb` binary.
		      (replace 'unpack
			       (lambda* (#:key inputs #:allow-other-keys)
					(system* (which "unzip")
						 (assoc-ref inputs "source"))
					#t)))))
    (inputs
      `(("libstdc++" ,(make-libstdc++ gcc))
        ("zlib" ,zlib)))
    (native-inputs
      `(("unzip" ,unzip)))
    (synopsis "A Clojure babushka for the grey areas of Bash")
    (description
      "The main idea behind babashka is to leverage Clojure in places
where you would be using bash otherwise.")
    (home-page "https://github.com/borkdude/babashka")
    (license license:epl1.0)))
