(define-module (spire)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix download)
	       #:use-module (guix utils)
	       #:use-module (gnu packages base)
               #:use-module (gnu packages gcc)
	       #:use-module (gnu packages compression)
	       #:use-module (nonguix build-system binary))


(define-public spire
  (package
    (name "spire")
    (version "0.1.0-alpha.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/epiccastle/spire/releases/download/v" version "/spire-" version "-linux-amd64.tgz"))
              (sha256
		(base32
		  "0jwrq4wh2c96x3zvffj1w94hqxhzfhh7payxxh580rb554iifffn"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:patchelf-plan
       `(("spire"
          ("libc" "zlib" "libstdc++")))
       #:install-plan
       `(("." ("spire") "bin/"))
       #:phases
       (modify-phases %standard-phases
		      ;; this is required because standard unpack expects
		      ;; the archive to contain a directory with everything inside it,
		      ;; while spire's release .zip only contains the `spire` binary.
		      (replace 'unpack
			       (lambda* (#:key inputs #:allow-other-keys)
					(system* (which "tar") "-xzf"
						 (assoc-ref inputs "source"))
					#t)))))
    (inputs
      `(("libstdc++" ,(make-libstdc++ gcc))
        ("zlib" ,zlib)))
    (native-inputs
      `(("tar" ,tar)))
    (synopsis "Pragmatic Provisioning Using Clojure")
    (description
      "A Clojure domain specific language tailored to idempotently
orchestrate machines in parallel over SSH.")
    (home-page "https://github.com/epiccastle/spire")
    (license license:epl2.0)))
