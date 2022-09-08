#!/busybox sh

cd /root

export GUIX_NEW_SYSTEM=$(/busybox readlink -f /var/guix/profiles/system)
# $GUIX_NEW_SYSTEM/boot needs this to exist even though /run is expected to be empty.
# I installed GuixSD in a proper VM and /run is not on tmpfs, so I'm not sure.
/busybox ln -s none /run/current-system
/var/guix/profiles/system/profile/bin/guile --no-auto-compile $GUIX_NEW_SYSTEM/boot &

/busybox sleep 3
source /etc/profile

# why are these permissions not there in the first place?
for f in ping su sudo; do
    chmod 4755 $(readlink -f $(which $f))
done

# su -l yourusername -c tmux

bash
