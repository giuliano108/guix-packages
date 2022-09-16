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

# su -l yourusername -c tmux

bash
