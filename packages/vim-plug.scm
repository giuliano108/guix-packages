(define-module (vim-plug)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download))

;; TODO: "helptags ALL" hook
;;
;; The documentation for the plugin does get installed, but it won't be
;; available via vim's :help . This is because vim needs a `tags` file in
;; ~/.guix-profile/share/vim/vimfiles/doc
;; containing the tags for all the plugins installed around there, and we don't
;; generate that. This seems to also be true for all the vim plugins that come with Guix.
;;
;; Debian/Ubuntu use: https://manpages.debian.org/testing/vim-common/helpztags.1.en.html
;; it would be great to rewrite that in scheme...

(define-public vim-plug
  (package
    (name "vim-plug")
    (version "0.10.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/junegunn/vim-plug")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "11x10l75q6k4z67yyk5ll25fqpgb2ma88vplrakw3k41g79xn9d9"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("plug.vim" "share/vim/vimfiles/autoload/")
         ("doc" "share/vim/vimfiles/"))))
    (home-page "https://github.com/junegunn/vim-plug")
    (synopsis "Minimalist Vim Plugin Manager")
    (description "A minimalist Vim plugin manager.")
    (license license:expat)))  ; MIT
