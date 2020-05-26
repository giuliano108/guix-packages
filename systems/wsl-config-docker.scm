;(use-modules (oop goops))
;(add-to-load-path "/root/.guix-profile/share/guile/site/3.0/")
;(use-modules (ice-9 readline))
;(activate-readline)

(use-modules
  (gnu)
  (gnu system linux-container)
  (gnu services shepherd)
  (guix profiles)
  (guix packages)
  (srfi srfi-1))
 
(use-service-modules base ssh desktop dbus docker)
(use-package-modules screen vim certs)

(define dummy-networking-service-type
  (shepherd-service-type
    'dummy-networking
    (const (shepherd-service
	     (documentation "Provide loopback and networking without actually
			    doing anything.")
	     (provision '(loopback networking))
	     (start #~(const #t))))
    #f))

(define os
(operating-system
  (host-name "scarpa")
  (timezone "Europe/London")
  (locale "en_US.utf8")

  (kernel hello)  ; dummy package
  (initrd (lambda* (. rest) (plain-file "dummyinitrd" "dummyinitrd")))
  (initrd-modules '())
  (firmware '())

  (bootloader
    (bootloader-configuration
      (bootloader
        (bootloader
          (name 'dummybootloader)
          (package hello)
          (configuration-file "/dev/null")
	  (configuration-file-generator (lambda* (. rest) (computed-file "dummybootloader" #~(mkdir #$output))))
          (installer #~(const #t))))))

  (file-systems (list (file-system
                        (device "/dev/sdb")
                        (mount-point "/")
                        (type "ext4")
                        (mount? #t))  ; saying #f here doesn't work :(
		      (file-system
			(device "cgroup")
			(mount-point "/sys/fs/cgroup/rdma")
			(type "cgroup")
			(check? #f)
			(options "rdma")
			(create-mount-point? #t)
			(mount? #t))))

  (users (cons (user-account
                (name "giuliano")
                (group "users")
                (supplementary-groups '("wheel" "docker")))
               %base-user-accounts))

  (packages (append (list screen  ; global packages to add
                          vim
			  nss-certs)
              (remove
                  (lambda (x)
                    (member (package-name x)
                            (list "zile"  ; global packages to not add
                                  "nano"
                                  "info-reader"
                                  "pciutils"
                                  "usbutils"
                                  "util-linux-with-udev"
                                  "kmod"
                                  "eudev"
                                  "isc-dhcp"
                                  "iw"
                                  "wireless-tools")))
                  %base-packages)))

  (essential-services
    (remove
      (lambda (x)
        (member (service-type-name (service-kind x))
                (list 'firmware 'linux-bare-metal)))
      (operating-system-default-essential-services this-operating-system)))

  (services (list (service guix-service-type)
                  (service nscd-service-type)
                  (service dummy-networking-service-type)
                  (syslog-service)
                  (service elogind-service-type)
                  (service dbus-root-service-type)
		  (service docker-service-type)))))

; Hackish way to avoid building/including linux-module-database in the system,

(define hooks-modifier
  (eval '(record-modifier <profile> 'hooks)
    (resolve-module '(guix profiles))))

(define my-essential-services (operating-system-essential-services os))

(define system-service (car my-essential-services))
(unless (eq? 'system (service-type-name (service-kind system-service)))
  (raise-exception "The first essential service is not 'system'"))


(define kernel-profile (car (cdr (car (service-value system-service)))))
(unless (string=? "hello" (manifest-entry-name (car (manifest-entries (profile-content kernel-profile)))))
  (raise-exception "I was expecting 'hello' as the (dummy) kernel"))

(hooks-modifier kernel-profile '())

(define os
(operating-system
  (inherit os)
  (essential-services my-essential-services)))

os
