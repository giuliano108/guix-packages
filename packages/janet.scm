(define-module (janet)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix build-system meson)
	       #:use-module (guix git-download))


(define-public janet
  (package
    (name "janet")
    (version "1.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janet-lang/janet")
                     (commit (string-append "v" version))))
            (file-name (git-file-name name version))
              (sha256
               (base32
                "1zdiwddnppwg5zrizy2ypd449zj4mivib76h73xhvr1syl7dk7sc"))))
    (build-system meson-build-system)
    (synopsis "A dynamic language and bytecode vm")
    (description
      "Janet is a functional and imperative programming language and bytecode
interpreter. It is a lisp-like language, but lists are replaced by other data
structures (arrays, tables (hash table), struct (immutable hash table), tuples).
The language also supports bridging to native code written in C, meta-programming
with macros, and bytecode assembly.")
    (home-page "https://janet-lang.org")
    (license license:expat)))  ; MIT
