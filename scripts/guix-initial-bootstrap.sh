/busybox mkdir -p ~root/.config/guix
/busybox ln -sf /var/guix/profiles/per-user/root/current-guix ~root/.config/guix/current
GUIX_PROFILE="`echo ~root`/.config/guix/current"
source $GUIX_PROFILE/etc/profile
guix-daemon --build-users-group=guixbuild &
guix archive --authorize < /var/guix/profiles/per-user/root/current-guix/share/guix/ci.guix.gnu.org.pub
