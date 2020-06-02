(define-module (rust-newer)
	       #:use-module (guix packages)
	       #:use-module (guix utils)
	       #:use-module (gnu packages rust))

(define rust-bootstrapped-package (@@ (gnu packages rust) rust-bootstrapped-package))
(define %cargo-reference-hash (@@ (gnu packages rust) %cargo-reference-hash))

(define-public rust-1.40
  (let ((base-rust
         (rust-bootstrapped-package rust-1.39 "1.40.0"
           "1ba9llwhqm49w7sz3z0gqscj039m53ky9wxzhaj11z6yg1ah15yx")))
    (package
      (inherit base-rust)
      (source
        (origin
          (inherit (package-source base-rust))
          (snippet '(begin
                      (delete-file-recursively "src/llvm-project")
                      (delete-file-recursively "vendor/jemalloc-sys/jemalloc")
                      #t))))
      (arguments
       (substitute-keyword-arguments
	 (append '(#:validate-runpath? #f)
		 (package-arguments base-rust))
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'patch-cargo-checksums
               ;; The Cargo.lock format changed.
               (lambda* _
                 (use-modules (guix build cargo-utils))
                 (substitute* "Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor")
                 #t))
	     (delete 'remove-unsupported-tests))))))))
