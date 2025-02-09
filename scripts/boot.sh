#!/busybox sh

cd /root

export GUIX_NEW_SYSTEM=$(/busybox readlink -f /var/guix/profiles/system)
# $GUIX_NEW_SYSTEM/boot needs this to exist even though /run is expected to be empty.
# I installed GuixSD in a proper VM and /run is not on tmpfs, so I'm not sure.
/busybox ln -s none /run/current-system
/var/guix/profiles/system/profile/bin/guile --no-auto-compile $GUIX_NEW_SYSTEM/boot &

/busybox sleep 3

# WSL mounts /run with the noexec and nosuid mount flags, preventing the
# binaries in /run/privileged/bin from being useful. As a workaround, we can
# copy those binaries to /var/run/privileged/bin and bind-mount on top.
/busybox rm -rf /var/run/privileged/bin
/busybox mkdir -p /var/run/privileged/bin
/busybox cp -p /run/privileged/bin/* /var/run/privileged/bin/
/busybox mount -o bind /var/run/privileged/bin /run/privileged/bin

source /etc/profile

# su -l yourusername -c tmux

bash
