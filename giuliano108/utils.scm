(define-module (giuliano108 utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (guix build utils)
  #:export (collect-rpath-origin-components))

(define (get-runpath binary)
  (read-line (open-pipe (string-append "patchelf --print-rpath " binary) OPEN_READ)))

(define (collect-rpath-origin-components out)
  "Under the current directory, find all the binaries that include $ORIGIN
in their rpath. Save the results to `out'/`rpath-origin-components.scm' for later use."
  (let* ((binaries-pattern ".*\\.so$")
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
    (call-with-output-file (string-append out "/rpath-origin-components.scm")
      (lambda (p) (pretty-print all-origin-components p)))))
