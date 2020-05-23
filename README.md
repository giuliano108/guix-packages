Some Guix packages not available (yet) in the main distro.

Use this repo as a [channel](https://guix.gnu.org/manual/en/html_node/Channels.html):

```
(cons (channel
        (name 'giuliano108-guix-packages)
        (url "https://github.com/giuliano108/guix-packages"))
;;      (url "file:///home/giuliano/Documents/code/guix-packages"))
      %default-channels)
```
