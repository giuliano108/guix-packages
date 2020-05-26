This repo contains:

* Some Guix packages not available (yet) in the main distro.
* Scattered notes. Particularly about the steps involved in running it under WSL2.
* [Operating system](https://guix.gnu.org/manual/en/html_node/Using-the-Configuration-System.html) declaration (check under `systems/`).

Use this repo as a [channel](https://guix.gnu.org/manual/en/html_node/Channels.html):

``` scheme
(cons (channel
        (name 'giuliano108-guix-packages)
        (url "https://github.com/giuliano108/guix-packages"))
;;      (url "file:///home/giuliano/Documents/code/guix-packages"))
      %default-channels)
```
