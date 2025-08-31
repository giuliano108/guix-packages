;(use-modules (oop goops))
;(add-to-load-path "/root/.guix-profile/share/guile/site/3.0/")
;(use-modules (ice-9 readline))
;(activate-readline)

(use-modules
  (gnu)
  (gnu system linux-container)
  (gnu services shepherd)
  (gnu packages docker)
  (guix profiles)
  (guix packages)
  (srfi srfi-1))

(use-service-modules base ssh desktop dbus docker)
(use-package-modules screen vim certs bash)

(define dummy-networking-service-type
  (shepherd-service-type
    'dummy-networking
    (const (shepherd-service
	     (documentation "Provide loopback and networking without actually
			    doing anything.")
	     (provision '(loopback networking))
	     (start #~(const #t))))
    #f
    (description "dummy networking")))

(define make-cgroup-fs
  (lambda (name)
    (file-system
      (device "cgroup")
      (mount-point (string-append "/sys/fs/cgroup/" name))
      (type "cgroup")
      (check? #f)
      (options name)
      (create-mount-point? #t)
      (mount? #t))))

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

  (file-systems (append
		  (list (file-system
			  (device "/dev/sdb")
			  (mount-point "/")
			  (type "ext4")
			  (mount? #t)))  ; saying #f here doesn't work :(
		  ))

  (users (cons (user-account
                (name "giuliano")
                (group "users")
                (supplementary-groups '("wheel" "docker")))
               %base-user-accounts))

  (packages (append (list screen  ; global packages to add
                          vim)
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
                  (service dummy-networking-service-type)
                  (service syslog-service-type)
                  (service elogind-service-type)
                  (service dbus-root-service-type)
		  (service docker-service-type)
		  (service containerd-service-type)
                  (service special-files-service-type
                         `(("/bin/sh" ,(file-append bash "/bin/sh"))
                           ("/bin/bash" ,(file-append bash "/bin/bash"))
                           ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))))))

os
