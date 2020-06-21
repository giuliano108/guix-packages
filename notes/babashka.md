# `babashka` and nonguix `binary-build-system`

[`babashka`](https://github.com/borkdude/babashka) is distributed as an almost static binary. On my system only a couple libraries can't be found out of the box.

```
~/temp ❯ ldd bb
        linux-vdso.so.1 (0x00007fffa515d000)
        libstdc++.so.6 => not found
        libpthread.so.0 => /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/libpthread.so.0 (0x00007fb9b09eb000)
        libdl.so.2 => /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/libdl.so.2 (0x00007fb9b09e6000)
        libz.so.1 => not found
        librt.so.1 => /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/librt.so.1 (0x00007fb9b09dc000)
        libc.so.6 => /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/libc.so.6 (0x00007fb9b081f000)
        /lib64/ld-linux-x86-64.so.2 => /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/ld-linux-x86-64.so.2 (0x00007fb9b4bf8000)
~/temp ❯
```

The missing libraries can be introduced manually using patchelf (as decribed [here](rust-starship.md#manually-fixing-the-rpath-dynamic-linker-search-path)), but it turns out there's a much better way.

[`nonguix`](https://gitlab.com/nonguix/nonguix/) is a Guix channel that provides packages that can't be included in the main repo for licensing reasons. Among the packages themselves, it comes with various extremely usefuls modules, like the `binary-build-system`.

This build system allows you to take something which is distributed as a binary, describe its inputs, namely the missing libraries from `ldd` and fix the [rpath](https://en.wikipedia.org/wiki/Rpath) automatically.

I've used it for the first time to package `babashka` (.scm [here](../packages/babashka.scm)).

Because channels can specify their dependencies, I just had to edit [.guix-channel](../.guix-channel) for `binary-build-system` to be available in my packages. The [guix-gaming-channel](https://gitlab.com/guix-gaming-channels/games) also makes extensive use of nonguix's modules, those two channels are an excellent reference when it comes to figuring out how to package weird things.
