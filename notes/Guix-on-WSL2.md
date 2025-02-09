# Guix on WSL2

_Originally a gist [here](https://gist.github.com/giuliano108/49ec5bd0a9339db98535bc793ceb5ab4). This document applies to Guix 1.4.0. For Guix 1.1.0, check a [previous version](https://github.com/giuliano108/guix-packages/blob/5bb057baf290455b11ab4a748e15c8293d086146/notes/Guix-on-WSL2.md)._

This will show you how to get Guix running on WSL2.
We're going to go as "minimal" as possible, without starting off one of the readily available WSL2 distros.
Parts of this guide should help with understanding how to set up any custom distro on WSL, not just Guix.

_Disclaimer_: I'm a Guix nOOb! (hence going through the trouble of installing it on WSL2)

## Table of contents

<!--ts-->
   * [Guix on WSL2](#guix-on-wsl2)
      * [Table of contents](#table-of-contents)
      * [About WSL](#about-wsl)
      * [Conventions](#conventions)
      * [Minimal rootfs archive](#minimal-rootfs-archive)
      * [Installing Guix](#installing-guix)
      * [Let Guix take over](#let-guix-take-over)
      * [Booting the Guix WSL distro as if it were a GuixSD system](#booting-the-guix-wsl-distro-as-if-it-were-a-guixsd-system)
      * [A script to automate distro creation](#a-script-to-automate-distro-creation)
      * [Misc](#misc)
         * [Can only run one "wsl -d guix" terminal at a time](#can-only-run-one-wsl--d-guix-terminal-at-a-time)
         * [How much disk space does the Guix WSL distro take?](#how-much-disk-space-does-the-guix-wsl-distro-take)
         * [/dev/null turns into a file](#devnull-turns-into-a-file)
      * [Docker](#docker)

<!-- Added by: giuliano, at: Fri Sep  9 11:52:08 PM BST 2022 -->

<!--te-->

## About WSL

WSL2 distros are, in fact, more like OS containers:

* A single instance of the [Microsoft-shipped](https://github.com/microsoft/WSL2-Linux-Kernel) Linux Kernel is running at any given time (under Hyper-V).
* Running, from a command prompt, `wsl -d distroname` instructs the Kernel to create one such container, the root filesystem coming an `ext4.vhdx` disk image.
* The Kernel executes an `/init` binary which, again, is supplied by Microsoft and cannot be changed/customized. Together with the Kernel itself, `init` can be found in `%systemroot%\System32\lxss\tools`.
* `/init` executes a shell or the command supplied to `wsl.exe`
* `/init` also takes care of mounting all the file systems, setting up `/dev`, ...
* A very useful side-effect of the "single Kernel for all distros" thing is that if, say, you've got a working Ubuntu WSL distro, and a Guix WSL distro that doesn't boot, you can run `dmesg` under Ubuntu and see what's wrong with Guix. The Kernel is shared, the Kernel ring buffer is also shared.

In this guide we'll use the `wsl --import` command. Here's what that does:

* Takes a `.tar` or `.tar.gz` _rootfs_ archive.
* Creates an Hyper-V disk image from it: the `ext4.vhdx` file we mentioned before.
* Extracts the contents of the archive into the disk image.
* Adds a bunch of files/directories to the image, including `/init`.
* Stopping the `LxssManager` service allows you to "mount" `ext4.vhdx` in Windows. The disk image contents can be inspected with f.e. [DiskInternals Linux Reader](https://www.diskinternals.com/linux-reader/).
* Not going to go into the details here, but `docker save` generates an archive which can then be fed to `wsl --import`. That's a great way to turn a Docker container into a WSL distro.

## Conventions

I'll prefix each command with the "place" it's been run from.

* `C:\` for a Windows command prompt.
* `(ubuntu) $` for things that are run on an Ubuntu WSL distro. This is used only to create the rootfs. Doesn't have to be Ubuntu, any distro will do. You can also get creative and do everything from Windows.
* If no "place" is specified, assume the command is run from the Guix WSL distro.

## Minimal rootfs archive

Let's start by adding [busybox](https://busybox.net/) to the archive and nothing else.


```
C:\Users\Giuliano\Documents\WSL>mkdir guix
```

```
(ubuntu) /mnt/c/Users/Giuliano/Documents/WSL/guix $ mkdir rootfs
(ubuntu) /mnt/c/Users/Giuliano/Documents/WSL/guix $ cd rootfs
(ubuntu) /mnt/c/Users/Giuliano/Documents/WSL/guix/rootfs $ curl -LO https://busybox.net/downloads/binaries/1.35.0-i686-linux-musl/busybox
(ubuntu) /mnt/c/Users/Giuliano/Documents/WSL/guix/rootfs $ cd ..
(ubuntu) /mnt/c/Users/Giuliano/Documents/WSL/guix $ tar -C rootfs -cvf rootfs.tar .
./
./busybox
```

Now we can try to `wsl --import`.

Our main working folder contains:

```
C:\Users\Giuliano\Documents\WSL\guix>dir
05/01/2020  05:54 PM    <DIR>          .
05/01/2020  05:51 PM    <DIR>          ..
05/01/2020  05:53 PM    <DIR>          rootfs
05/01/2020  05:54 PM           983,040 rootfs.tar
```

And we only have one Ubuntu distro available:

```
C:\Users\Giuliano\Documents\WSL\guix>wsl -l
Windows Subsystem for Linux Distributions:
Ubuntu-18.04
```

`--import` command causes `ext4.vhdx` to be created:

```
C:\Users\Giuliano\Documents\WSL\guix>wsl --import guix . rootfs.tar
C:\Users\Giuliano\Documents\WSL\guix>dir
05/01/2020  05:58 PM    <DIR>          .
05/01/2020  05:51 PM    <DIR>          ..
05/01/2020  05:58 PM        66,060,288 ext4.vhdx
05/01/2020  05:53 PM    <DIR>          rootfs
05/01/2020  05:54 PM           983,040 rootfs.tar
```

...and registers the `guix` distro:

```
C:\Users\Giuliano\Documents\WSL\guix>wsl -l
Windows Subsystem for Linux Distributions:
Ubuntu-18.04
guix
```

Can we actually run the `guix` distro? Remember there's nothing but `busybox` in there so we're going to try busybox's own `sh` implementation:

```
C:\Users\Giuliano\Documents\WSL\guix>wsl -d guix /busybox sh

C:\Users\Giuliano\Documents\WSL\guix>
```

Doesn't work, we are immediately taken back to the Windows prompt. :( Let's try the `dmesg`-from-Ubuntu trick explained above:

```
(ubuntu) $ dmesg
[..]
[29242.265446] EXT4-fs (sde): mounted filesystem with ordered data mode. Opts: discard,errors=remount-ro,data=ordered
[29242.270345] init: (1) ERROR: ConfigUpdateInformation:2623: creat /etc/hostname failed: 2
[29242.270349] init: (1) ERROR: ConfigUpdateInformation:2657: creat /etc/hosts failed 2
[29242.270494] init: (2) ERROR: UtilCreateProcessAndWait:635: /bin/mount failed with 2
[29242.270583] init: (1) ERROR: UtilCreateProcessAndWait:655: /bin/mount failed with status 0x
[29242.270585] ff00
[29242.270589] init: (1) ERROR: ConfigMountFsTab:2110: Processing fstab with mount -a failed.
[29242.271405] init: (3) ERROR: UtilCreateProcessAndWait:635: /bin/mount failed with 2
[29242.271506] init: (1) ERROR: UtilCreateProcessAndWait:655: /bin/mount failed with status 0x
[29242.271508] ff00
```

`/etc/hostname` and `/etc/hosts` don't exist. `/bin/mount` seems to "fail".

According to `wsl --help`, the "default shell" is used well, by default. Our rootfs doesn't have an `/etc/passwd` yet, Microsoft's `/init` can't check it to know what the root user shell should be. `--exec` promises to run a command as is.

```
C:\Users\Giuliano>wsl --help
Usage: wsl.exe [Argument] [Options...] [CommandLine]

Arguments for running Linux binaries:

    If no command line is provided, wsl.exe launches the default shell.

    --exec, -e <CommandLine>
        Execute the specified command without using the default Linux shell.
[..]
```

Does `--exec` work?

```
C:\Users\Giuliano\Documents\WSL\guix>wsl -d guix --exec /busybox sh
/ #
```

It does! We've got shell access to the hopefully-soon-to-be `guix` distro.

Now that we're in, we can check what WSL (`--import`) added our bare `rootfs.tar`. Notably, `/root`, `/etc`, `/tmp` and `/var` are missing...

```
/ # /busybox find / | /busybox grep -v '^.proc\|^.dev\|^.sys'
find: /usr/lib/wsl/drivers: Value too large for defined data type
/
/busybox
/mnt
/mnt/wsl
/mnt/c
/usr
/usr/lib
/usr/lib/wsl
/usr/lib/wsl/drivers
find: /usr/lib/wsl/lib: Value too large for defined data type
/usr/lib/wsl/lib
/sbin
/sbin/mount.drvfs
/bin
/bin/wslpath
/run
/run/WSL
/run/WSL/7_interop
/run/user
/run/shm
/run/lock
/init
/lost+found
```

Despite the `mount` errors in `dmesg`, things look OK:

```
/ # /busybox mount
/dev/sde on / type ext4 (rw,relatime,discard,errors=remount-ro,data=ordered)
tmpfs on /mnt/wsl type tmpfs (rw,relatime)
tools on /init type 9p (ro,dirsync,relatime,aname=tools;fmask=022,loose,access=client,trans=fd,rfd=6,wfd=6)
none on /dev type devtmpfs (rw,nosuid,relatime,size=3194532k,nr_inodes=798633,mode=755)
sysfs on /sys type sysfs (rw,nosuid,nodev,noexec,noatime)
proc on /proc type proc (rw,nosuid,nodev,noexec,noatime)
devpts on /dev/pts type devpts (rw,nosuid,noexec,noatime,gid=5,mode=620,ptmxmode=000)
none on /run type tmpfs (rw,nosuid,noexec,noatime,mode=755)
none on /run/lock type tmpfs (rw,nosuid,nodev,noexec,noatime)
none on /run/shm type tmpfs (rw,nosuid,nodev,noatime)
none on /run/user type tmpfs (rw,nosuid,nodev,noexec,noatime,mode=755)
[..]
```

## Installing Guix

Guix refuses to perform certain operations as `root` so let's add the users/group it needs. `groupadd`/`useradd` are not available yet, so we're going to do it manually:

```
### Make sure you're running these from the Guix WSL distro!
# /busybox mkdir -p /root /etc /tmp /var/run /run /home
# /busybox chmod 1777 /tmp

# /busybox cat <<EOM >> /etc/passwd
root:x:0:0:root:/root:/bin/bash
guixbuilder01:x:999:999:Guix build user 01:/var/empty:/usr/sbin/nologin
guixbuilder02:x:998:999:Guix build user 02:/var/empty:/usr/sbin/nologin
guixbuilder03:x:997:999:Guix build user 03:/var/empty:/usr/sbin/nologin
guixbuilder04:x:996:999:Guix build user 04:/var/empty:/usr/sbin/nologin
guixbuilder05:x:995:999:Guix build user 05:/var/empty:/usr/sbin/nologin
guixbuilder06:x:994:999:Guix build user 06:/var/empty:/usr/sbin/nologin
guixbuilder07:x:993:999:Guix build user 07:/var/empty:/usr/sbin/nologin
guixbuilder08:x:992:999:Guix build user 08:/var/empty:/usr/sbin/nologin
guixbuilder09:x:991:999:Guix build user 09:/var/empty:/usr/sbin/nologin
guixbuilder10:x:990:999:Guix build user 10:/var/empty:/usr/sbin/nologin
EOM

# /busybox cat <<EOM >> /etc/group
root:x:0:
guixbuild:x:999:guixbuilder01,guixbuilder02,guixbuilder03,guixbuilder04,guixbuilder05,guixbuilder06,guixbuilder07,guixbuilder08,guixbuilder09,guixbuilder10
EOM
```

At this point `/etc` contains:

```
/ # /busybox find /etc
/etc
/etc/passwd
/etc/group
```

WSL distros usually have the host `C:\` drive automagically mounted on `/mnt/c` (by the usual suspect: `/init`), but we don't:

```
/tmp # /busybox mount | /busybox grep mnt
tmpfs on /mnt/wsl type tmpfs (rw,relatime)
/tmp #
```

Copy `/busybox` to `/bin/mount`:

```
/tmp # /busybox cp /busybox /bin/mount
```

Logout of the Guix WSL distro, terminate it, make sure it's stopped:

```
C:\Users\Giuliano>wsl -l -v
  NAME            STATE           VERSION
  Ubuntu-18.04    Running         2
  guix            Running         2
C:\Users\Giuliano>wsl -t guix
C:\Users\Giuliano>wsl -l -v
  NAME            STATE           VERSION
  Ubuntu-18.04    Running         2
  guix            Stopped         2
```

Log back in and notice how WSL (now that `/etc` exists) created `resolv.conf`, `hosts` and `hostname` for us:

```
C:\Users\Giuliano>wsl -d guix --exec /busybox sh
/ # /busybox find /etc
/etc
/etc/resolv.conf
/etc/hosts
/etc/passwd
/etc/hostname
/etc/group
```

The `/busybox -> /bin/mount` trick caused `/mnt/c` to appear:

```
/ #  /busybox mount | /busybox grep mnt
tmpfs on /mnt/wsl type tmpfs (rw,relatime)
C:\134 on /mnt/c type 9p (rw,dirsync,noatime,aname=drvfs;path=C:\;uid=0;gid=0;symlinkroot=/mnt/,mmap,access=client,msize=65536,trans=fd,rfd=10,wfd=10)
/ #
```

Networking works now. If `/busybox wget` properly supported HTTPS, we could use it to download the Guix binary tarball.

```
/tmp # /busybox wget https://ftp.gnu.org/gnu/guix/guix-bynary-1.4.0.x86_64-linux.tar.xz
Connecting to ftp.gnu.org (209.51.188.20:443)
wget: note: TLS certificate validation not implemented
saving to 'guix-bynary-1.4.0.x86_64-linux.tar.xz'
guix-bynary-1.4.0.x8 100% | 86.8M  0:00:00 ETA
'guix-bynary-1.4.0.x86_64-linux.tar.xz' saved
/tmp #
```

and extract it:

```
/ # /busybox tar -C / -xvJf /tmp/guix-bynary-1.4.0.x86_64-linux.tar.xz
```

If `/busybox wget` fails on a TLS error, remember you can download the tarball with Windows and extract it from `/mnt/c` instead, i.e.:

```
/ # /busybox tar -C / -xvJf /mnt/c/Users/Giuliano/Downloads/guix-bynary-1.4.0.x86_64-linux.tar.xz
```


`/gnu` and `/var/guix` are in place:

```
/ # /busybox ls -ld /gnu /var/guix
drwxr-xr-x    3 root     root          4096 Feb 23 22:30 /gnu
drwxr-xr-x    5 root     root          4096 Feb 23 22:30 /var/guix
/ #
```

As per the [Binary Installation](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html) instructions, activate the profile included in the tarball:

```
/ # /busybox mkdir -p ~root/.config/guix
/ # /busybox ln -sf /var/guix/profiles/per-user/root/current-guix ~root/.config/guix/current
/ # GUIX_PROFILE="`echo ~root`/.config/guix/current"
/ # source $GUIX_PROFILE/etc/profile
```

And finally start the daemon:

```
/ # guix-daemon --build-users-group=guixbuild &
```

Great, in theory we can now start using Guix to install stuff!

Make sure to enable subtitutes, unless you want Guix to buid everything from source:

```
/ # guix archive --authorize < /var/guix/profiles/per-user/root/current-guix/share/guix/ci.guix.gnu.org.pub
```

But `guix pull` doesn't work... :(

```
/ # guix pull
accepted connection from pid 23, user root
substitute: guix substitute: warning: ci.guix.gnu.org: host not found: Servname not supported for ai_socktype
[..]
build of /gnu/store/dd5mga544azygz2hhs3ksp1d8bgmi38j-isrgrootx1.pem.drv failed
View build log at '/var/log/guix/drvs/dd/5mga544azygz2hhs3ksp1d8bgmi38j-isrgrootx1.pem.drv.bz2'.
cannot build derivation `/gnu/store/j814i78jg0832akx856ha6za526rdvy6-le-certs-0.drv': 1 dependencies couldn't be built
guix pull: error: build of `/gnu/store/j814i78jg0832akx856ha6za526rdvy6-le-certs-0.drv' failed
```

The build log also mentions those `Servname not supported for ai_socktype` errors. Googling tells us they are caused by a broken/missing `/etc/services` which indeed does not exist. Let's populate it with the basics:

```
/ # /busybox cat <<EOM >> /etc/services
ftp-data        20/tcp
ftp             21/tcp
ssh             22/tcp                          # SSH Remote Login Protocol
domain          53/tcp                          # Domain Name Server
domain          53/udp
http            80/tcp          www             # WorldWideWeb HTTP
https           443/tcp                         # http protocol over TLS/SSL
ftps-data       989/tcp                         # FTP over SSL (data)
ftps            990/tcp
http-alt        8080/tcp        webcache        # WWW caching service
http-alt        8080/udp
EOM
```

`guix pull` now works...

While we are at it, let's populate `/etc/nsswitch.conf` too. I don't know how/when but at some point this file disappeared (either WSL or Guix are no longer creating it). Without it, the initial `guix system reconfigure` (see below) fails.

```
/ # /busybox cat <<EOM >> /etc/nsswitch.conf
group:  files
hosts:  files dns [!UNAVAIL=return]
networks:       files dns [!UNAVAIL=return]
passwd: files
shadow: files
EOM
```

## Let Guix take over

Now it'd be great if we could get our Guix WSL distro to be more like GuixSD. F.e., Guix itself should be able to make a "more proper" `/etc/passwd` than the one we manually edited, fix `/etc/services`, ...

To achieve that, we're going to try and put together a Guix [System Configuration](https://guix.gnu.org/manual/en/html_node/Using-the-Configuration-System.html#Using-the-Configuration-System) file. A WSL distro, unlike a VM or a bare metal server, doesn't need a lot of the stuff that Guix's [operating-system](https://guix.gnu.org/manual/en/html_node/Using-the-Configuration-System.html#Using-the-Configuration-System) declaration expects. In my configuration file I tried to convince Guix that it's fine to not care about the kernel/bootloader, deal with disks, ...

[`systems/wsl-config.scm`](https://github.com/giuliano108/guix-packages/blob/master/systems/wsl-config.scm) is the hacky/uneducated operating system declaration I'm using.

Passing that file to `guix system reconfigure`...

```
~ # guix system reconfigure --no-bootloader wsl-config.scm
```

...creates a new "instance" of the entire OS and switches to it (that's the beauty of GuixSD and NixOS!).

```
[..]
building /gnu/store/68wyiashnhc05wspcn26p3fw0vg2wn2l-switch-to-system.scm.drv...
making '/gnu/store/b544x90ncmxm95344m0c4cygz98w2azr-system' the current system...
setting up setuid programs in '/run/setuid-programs'...
populating /etc from /gnu/store/5s856h5r91xslm90ymm3gn179q6z4hmi-etc...
substitute: updating substitutes from 'https://ci.guix.gnu.org'... 100.0%
0.0 MB will be downloaded:
   /gnu/store/q9wrh0rrhq3sy3n2i6m73ijql7a1m2nm-module-import-compiled
downloading from https://ci.guix.gnu.org/nar/lzip/q9wrh0rrhq3sy3n2i6m73ijql7a1m2nm-module-import-compiled ...
 module-import-compiled  24KiB                                                1.3MiB/s 00:00 [##################] 100.0%

guix system: warning: while talking to shepherd: No such file or directory
~ #
[..]
```

## Booting the Guix WSL distro as if it were a GuixSD system

After the previous step the system is functional. *But*, when the Guix WSL distro starts afresh (because it was stopped with `wsl -t guix`, Windows was restarted or other), you'll notice that f.e. `/run/setuid-programs` disappeared (`sudo` stops working). Some important Guix things live in `/run`, which is mounted by WSL on a `tmpfs` (as per the [FHS](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard)). Because WSL distros don't boot like normal VMs/servers, Guix doesn't have a chance to populate `/run` at boot time.

The [source](https://github.com/guix-mirror/guix/blob/83460433b94487198750ad0bcc6f3869f68a8c8f/gnu/build/activation.scm#L325) and [this](https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00139.html) thread help with understanding how to "boot GuixSD by hand":

```
export GUIX_NEW_SYSTEM=$(/busybox readlink -f /var/guix/profiles/system)
# $GUIX_NEW_SYSTEM/boot needs this to exist even though /run is expected to be empty.
# I installed GuixSD in a proper VM and /run is not on tmpfs, so I'm not sure.
/busybox ln -s none /run/current-system
/var/guix/profiles/system/profile/bin/guile --no-auto-compile $GUIX_NEW_SYSTEM/boot &
```

Running the above populates `/run` and starts `shepherd`.

```
~ # . /etc/profile
~ # pstree
init─┬─init───init───busybox─┬─pstree
     │                       └─shepherd─┬─containerd───10*[{containerd}]
     │                                  ├─dbus-daemon
     │                                  ├─dockerd───9*[{dockerd}]
     │                                  ├─elogind
     │                                  ├─guix-daemon
     │                                  ├─syslogd
     │                                  ├─udevd
     │                                  └─4*[{shepherd}]
     └─{init}
~ #
```

## A script to automate distro creation

This repo contains a [babashka](https://babashka.org/) script that automates 99% of the steps we discussed. Babashka can be installed via scoop (instructions [here](https://github.com/babashka/babashka#scoop)). You can also install GNU tar, which is required too, via `scoop install tar`.

Here's a full example. It creates the `guixtest` WSL distro (the distro name can be changed by editing the script).

```
C:\Users\Giuliano\Documents\WSL>bb ..\code\guix-packages\scripts\make-wsl2-guix-distro.bb
Downloading busybox, creating the rootfs and the base WSL distro...
Downloading guix and unpacking it inside the WSL distro...
[..]
./var/guix/profiles/per-user/root/current-guix
./var/guix/profiles/per-user/root/current-guix-1-link
All done, see what the final steps should be at https://github.com/giuliano108/guix-packages/blob/master/notes/Guix-on-WSL2.md ...

C:\Users\Giuliano\Documents\WSL>wsl -d guixtest --exec /busybox sh
Processing fstab with mount -a failed.

/mnt/c/Users/Giuliano/Documents/WSL # cd
~ # . ./guix-initial-bootstrap.sh
~ # guix pull
accepted connection from pid 20, user root
substitute: updating substitutes from 'https://ci.guix.gnu.org'... 100.0%
Updating channel 'guix' from Git repository at 'https://git.savannah.gnu.org/git/guix.git'...
[..]
building /gnu/store/k0k1la0j0v6kh339k6zkpf9bg4npcm7j-guix-36f57d071.drv...
building CA certificate bundle...
listing Emacs sub-directories...
building fonts directory...
building directory of Info manuals...
building profile with 1 package...
building /gnu/store/fllgsfp81rqw03jjbymwdz3qkdmcmk6k-inferior-script.scm.drv...
building package cache...
building profile with 1 package...

~ # guix system reconfigure --no-bootloader wsl-config.scm
[..]
guix system: warning: cannot determine provenance for current system
[..]
making '/var/guix/profiles/system-1-link' the current system...
populating /etc from /gnu/store/f6xzszx5i51wqvma9b5amscqga563wxq-etc...
setting up privileged programs in '/run/privileged/bin'...
creating /etc/machine-id...
Please wait while gathering entropy to generate the key pair;
this may take time...
substitute: updating substitutes from 'https://ci.guix.gnu.org'... 100.0%
0.0 MB will be downloaded
 module-import-compiled  23KiB                                                417KiB/s 00:00 100.0%
guix system: warning: while talking to shepherd: No such file or directory
substitute: updating substitutes from 'https://ci.guix.gnu.org'... 100.0%
The following derivation will be built:
  /gnu/store/530bbi0gyxay32rsnnjrzz0r05a0b15x-kexec-load-system.scm.drv

0.2 MB will be downloaded
 module-import-compiled  179KiB                                               1.5MiB/s 00:00 100.0%
building /gnu/store/530bbi0gyxay32rsnnjrzz0r05a0b15x-kexec-load-system.scm.drv...
WARNING: (guile-user): imported module (guix build utils) overrides core binding `delete'
guix system: warning: failed to load operating system for kexec: In procedure open-fdes: No such file or directory
~ #
~ # ./boot.sh
[..]
Service pam started.
Starting service dockerd...
Service pam running with value #t.
Service pam has been started.
Service dockerd has been started.
Service dockerd started.
The following services could not be started in the background: file-system-/sys/fs/cgroup/rdma file-system-/sys/fs/cgroup/net_prio file-system-/sys/fs/cgroup/net_cls file-system-/sys/fs/cgroup/misc file-system-/sys/fs/cgroup/hugetlb.
Service dockerd running with value #<<process> id: 2814 command: ("/gnu/store/5ljlzb29jyc2k113lrlpq1rgd0bwnnm1-docker-20.10.27/bin/dockerd" "-p" "/var/run/docker.pid" "--userland-proxy=true" "--userland-proxy-path=/gnu/store/2hz8yairfpd33qx0hcvc4nspxja50mg9-docker-libnetwork-cmd-proxy-20.10-3.3797618/bin/proxy" "--iptables" "--containerd" "/run/containerd/containerd.sock")>.
bash-5.1# herd status
Started:
 + containerd
 + dbus-system
 + dockerd
 + elogind
 + file-system-/run/systemd
 + file-system-/run/user
 + file-system-/sys/fs/cgroup
 + file-system-/sys/fs/cgroup/elogind
 + file-system-/sys/fs/cgroup/hugetlb
 + file-system-/sys/fs/cgroup/misc
 + file-system-/sys/fs/cgroup/net_cls
 + file-system-/sys/fs/cgroup/net_prio
 + file-system-/sys/fs/cgroup/rdma
 + file-systems
 + guix-daemon
 + loopback
 + pam
 + root
 + root-file-system
 + syslogd
 + udev
 + user-file-systems
 + user-processes
Running timers:
 + log-rotation
One-shot:
 * host-name
 * user-homes
bash-5.1#
```


## Misc

### Can only run one "wsl -d guix" terminal at a time

EDIT: stopping `nscd` works around this issue, see https://github.com/giuliano108/guix-packages/issues/4 .

I don't know why but, after `shepherd` is started with the recipe above, you can't open another "terminal" (I mean: open `cmd.exe`, run `wsl -d guix --exec /busybox sh `. The `wsl` command returns immediately back to prompt. `dmesg` contains:

```
[ 3541.851862] init: (48) ERROR: CreateProcessEntryCommon:600: initgroups failed 29
[ 3541.851865] init: (48) ERROR: CreateProcessEntryCommon:645: Create process not expected to return
```

This isn't really a problem for me, one terminal is enough and I use `tmux` anyway.
I start Guix with `wsl -d guix --exec /busybox sh /root/boot.sh`, where `boot.sh` contains:

```shell
#!/busybox sh

cd /root

export GUIX_NEW_SYSTEM=$(/busybox readlink -f /var/guix/profiles/system)
# $GUIX_NEW_SYSTEM/boot needs this to exist even though /run is expected to be empty.
# I installed GuixSD in a proper VM and /run is not on tmpfs, so I'm not sure.
/busybox ln -s none /run/current-system
/var/guix/profiles/system/profile/bin/guile --no-auto-compile $GUIX_NEW_SYSTEM/boot &

/busybox sleep 3

# WSL mounts /run with the noexec and nosuid mount flags, preventing the
# binaries in /run/setuid-programs from being useful. As a workaround, we can
# copy those binaries to /var/run/setuid-programs and bind-mount on top.
/busybox rm -rf /var/run/setuid-programs
/busybox mkdir -p /var/run/setuid-programs
/busybox cp -p /run/setuid-programs/* /var/run/setuid-programs/
/busybox mount -o bind /var/run/setuid-programs /run/setuid-programs

source /etc/profile

su -l giuliano -c tmux

bash
```

### How much disk space does the Guix WSL distro take?

Freshly installed:

```
bash-5.1# df -h
Filesystem      Size  Used Avail Use% Mounted on
/dev/sdc        251G  4.2G  235G   2% /
```

### &nbsp;`/dev/null` turns into a file

At seemingly random times, `/dev/null` goes from being a device, to being a plain file. That upsets a lot of things, like `git` or `tmux`:

```
$ git status
fatal: open /dev/null or dup failed: Permission denied
```

The fix is:

```
# ls -l /dev/null
-r-xr-xr-x 1 root root 0 May 30 18:03 /dev/null*
# rm /dev/null
# mknod /dev/null c 1 3
# chmod 666 /dev/null
# ll /dev/null
crw-rw-rw- 1 root root 1, 3 May 30 18:08 /dev/null
```

Note that running the fix within Ubuntu, also fixes Guix (if both WSL distros are active at the same time).

## Docker

The [`systems/wsl-config.scm`](https://github.com/giuliano108/guix-packages/blob/master/systems/wsl-config.scm) operating system declaration also includes Docker. You'll just need to install the `docker-cli` package in your (non-root) user's Guix profile to be able to use it.

In some cases, depending on the version of WSL2 and the version of Guix, you might find that some cgroup filesystems have not been mounted. That's because Guix uses static list of cgroups names to know what to mount. For example, at the time of writing `rdma` (which Docker uses) is missing from [the list](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/system/file-systems.scm?h=39959735e58f32357bbf474bb82a6cf41f803901#n511).

To address this issue, I've opted to leave a bunch of potentially redundant cgroup `(filesystem ...)` definitions in [`systems/wsl-config.scm`](https://github.com/giuliano108/guix-packages/blob/master/systems/wsl-config.scm). They might or might not be needed, they might cause shepherd to complain that it `failed to start service 'file-system-/sys/fs/cgroup/$cgroup_name'`, but at least Docker seems to reliably work for me.

On Ubuntu, all the `/sys/fs/cgroup` are set up by `/usr/bin/cgroupfs-mount`, which just looks at `/proc/cgroups` and mounts everything it finds there...
