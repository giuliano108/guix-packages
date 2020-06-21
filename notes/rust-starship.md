# `starship` and building rust in Guix

[starship](https://starship.rs) is a "cross-shell prompt". Compared to roll-your-own or things like [Oh My Bash](https://ohmybash.github.io/), it's a simpler/faster way to get a "smart" shell prompt that understands git, python virtualenv, etc.

Let's try to build it.

## Table of contents

<!--ts-->
   * [starship and building rust in Guix](#starship-and-building-rust-in-guix)
      * [Table of contents](#table-of-contents)
      * [First steps](#first-steps)
      * [Manually fixing the rpath (dynamic linker search path)](#manually-fixing-the-rpath-dynamic-linker-search-path)
      * [A newer Rust](#a-newer-rust)
      * [Cleanup](#cleanup)

<!-- Added by: giuliano, at: Tue 02 Jun 2020 11:12:19 PM BST -->

<!--te-->

## First steps

Put `rust` and `cargo` into a throwaway profile:

```
$ guix install -p ~/temp/build-starship rust rust:cargo
The following packages will be installed:
   rust       1.39.0
   rust:cargo 1.39.0

The following derivation will be built:
   /gnu/store/pcgw7vmxazjh1xp9i5bpvs8x8jjl0y9d-profile.drv
The following profile hooks will be built:
   /gnu/store/1qilh56yp3v54hmzx51kacsq9sf213pa-fonts-dir.drv
   /gnu/store/b6f3zhwklnjgkxkw78m5lyb6l7f79ly2-ca-certificate-bundle.drv
   /gnu/store/v1ipyi07vl8km9y3qalvab9wc4x7sj5k-manual-database.drv
   /gnu/store/znqm6rzmkarvaxvn0a4yp13fw4393h3q-info-dir.drv
building CA certificate bundle...
building fonts directory...
building directory of Info manuals...
building database for manual pages...
building profile with 2 packages...
hint: Consider setting the necessary environment variables by running:

     GUIX_PROFILE="/home/giuliano/temp/build-starship"
     . "$GUIX_PROFILE/etc/profile"

Alternately, see `guix package --search-paths -p "/home/giuliano/temp/build-starship"'.
```

I've cloned the starship repo and checked out the most recent version:

```
$ pwd
/home/giuliano/temp/starship
$ git status
HEAD detached at v0.41.3
```

From here on I'll be running `cargo build --release` repeatedly, making note of each blocker. Round 1:

```
Error { kind: ToolNotFound, message: "Failed to find tool. Is `cc` installed?" }
```

This is easy, just `export CC=gcc`. If `gcc` is not available, then `guix install gcc-toolchain`.

```
error[E0658]: the `#[non_exhaustive]` attribute is an experimental feature
 --> /home/giuliano/.cargo/registry/src/github.com-1ecc6299db9ec823/os_info-2.0.5/src/os_type.rs:7:1
  |
7 | #[non_exhaustive]
  | ^^^^^^^^^^^^^^^^^
  |
  = note: for more information, see https://github.com/rust-lang/rust/issues/44109

   Compiling toml v0.5.6
error: aborting due to 2 previous errors

For more information about this error, try `rustc --explain E0658`.
error: could not compile `os_info`.
warning: build failed, waiting for other jobs to finish...
error: build failed
```

`rustc --explain E0658` says that the code we're trying to build is using some experimental feature, which is only supported by "some" Rust compilers.

We can do two things here:

* Build in Docker and fix the resulting executable so that it uses the right Guix libraries.
* Use Guix to build a suitable Rust compiler.

Let's try Docker first. Before writing a Guix package, we'll want to know what's the earliest Rust compiler that works anyway. Docker is the quickest way to get that information.

```
$ docker pull rust:1.40.0
$ pwd
/home/giuliano/temp/starship
$ docker run -ti -v $PWD:/code rust:1.40.0 /bin/bash
root@fcfa2543de72:/# cd /code
root@fcfa2543de72:/code# cargo build --release
[..]
    Finished release [optimized] target(s) in 0.10s
root@fcfa2543de72:/code# find target -executable -name starship
target/release/starship
root@fcfa2543de72:/code# target/release/starship prompt

code on  HEAD (0dfe3f7) is 📦 v0.41.3 via 🦀 v1.40.0
```

`rust@1.40.0` works! Should be easy to make a Guix package for that (at the time of writing, Guix ships with `rust@1.39.0`).

## Manually fixing the `rpath` (dynamic linker search path)

_Update_: I wrote this section before discovering [nonguix](https://gitlab.com/nonguix/nonguix/). Its `binary-build-system` automates the patchelf gymnastics described below in a safe and reproducible way. I've used it for the first time to package `babashka` (some notes about that [here](babashka.md)).

Of course, trying to run `starship` outside of the Docker container yields an error:

```
./target/release/starship prompt
-bash: ./target/release/starship: No such file or directory
```

That's because the same "interpreter" (for a binary, interpreter == dynamic linker) that existed in the container, can't be found outside of it. Compare:

```
$ file ./target/release/starship
./target/release/starship: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 3.2.0, BuildID[sha1]=5cdc194bfcd64eeceac80dfe13d99620bb557fc3, with debug_info, not stripped
$ ls /lib64/ld-linux-x86-64.so.2
ls: cannot access '/lib64/ld-linux-x86-64.so.2': No such file or directory
$ file $(readlink -f $(which bash))
/gnu/store/87kif0bpf0anwbsaw0jvg8fyciw4sz67-bash-5.0.16/bin/bash: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/ld-linux-x86-64.so.2, for GNU/Linux 2.6.32, not stripped
$ ls /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/ld-linux-x86-64.so.2
/gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/ld-linux-x86-64.so.2
```

We can use `patchelf` (available in Guix) to change the binary and put the right interpreter in place:

```
$ sudo chown -R giuliano:users target
$ patchelf --set-interpreter /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/ld-linux-x86-64.so.2 target/release/starship
$ file ./target/release/starship
./target/release/starship: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, interpreter /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/ld-linux-x86-64.so.2, BuildID[sha1]=5cdc194bfcd64eeceac80dfe13d99620bb557fc3, for GNU/Linux 3.2.0, with debug_info, not stripped
$ ./target/release/starship
$ ./target/release/starship: error while loading shared libraries: libssl.so.1.1: cannot open shared object file: No such file or directory
```

Ok, pretty sure we've got `libssl.so.1.1` in our profile, we just have to tell `starship` where to find it.

```
$ find -L ~/.guix-profile -name libssl.so.1.1
/home/giuliano/.guix-profile/lib/libssl.so.1.1
$ readlink -f /home/giuliano/.guix-profile/lib/libssl.so.1.1
/gnu/store/hcxpkksmbql6s4al8yy2myr25kh4cic0-openssl-1.1.1g/lib/libssl.so.1.1
```

We can do this by changing the [rpath](https://en.wikipedia.org/wiki/Rpath). The starship binary doesn't even set it:

```
$ patchelf --print-rpath target/release/starship
$
```

So let's give it the path to `libssl.so.1.1`:

```
$ patchelf --set-rpath /gnu/store/hcxpkksmbql6s4al8yy2myr25kh4cic0-openssl-1.1.1g/lib target/release/starship
$ target/release/starship
target/release/starship: error while loading shared libraries: libz.so.1: cannot open shared object file: No such file or directory
```

Cool, it breaks on another libary, `libz.so.1`. We repeat the same procedure and add another library path to `rpath`, separating it with a colon.

```
$ readlink -f /home/giuliano/.guix-profile/lib/libz.so.1
/gnu/store/rykm237xkmq7rl1p0nwass01p090p88x-zlib-1.2.11/lib/libz.so.1.2.11
$ patchelf --set-rpath /gnu/store/hcxpkksmbql6s4al8yy2myr25kh4cic0-openssl-1.1.1g/lib:/gnu/store/rykm237xkmq7rl1p0nwass01p090p88x-zlib-1.2.11/lib target/release/starship
$ target/release/starship
target/release/starship: error while loading shared libraries: libgcc_s.so.1: cannot open shared object file: No such file or directory
```

`libgcc_s.so.1` isn't in an obvious place, maybe we can just take its path from `bash`:

```
$ find -L ~/.guix-profile -name libgcc_s.so.1
$
$ ldd $(readlink -f $(which bash)) | grep libgcc_s.so.1
        libgcc_s.so.1 => /gnu/store/01b4w3m6mp55y531kyi1g8shh722kwqm-gcc-7.5.0-lib/lib/libgcc_s.so.1 (0x00007ff8ee2a6000)
$ patchelf --set-rpath /gnu/store/hcxpkksmbql6s4al8yy2myr25kh4cic0-openssl-1.1.1g/lib:/gnu/store/rykm237xkmq7rl1p0nwass01p090p88x-zlib-1.2.11/lib:/gnu/store/01b4w3m6mp55y531kyi1g8shh722kwqm-gcc-7.5.0-lib/lib target/release/starship
$ target/release/starship
starship 0.41.3
Matan Kushner <hello@matchai.me>
The cross-shell prompt for astronauts. ☄🌌️

USAGE:
    starship <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    bug-report    Create a pre-populated GitHub issue with information about your configuration    config        Edit the starship configuration
    explain       Explains the currently showing modules
    help          Prints this message or the help of the given subcommand(s)
    init          Prints the shell function used to execute starship
    module        Prints a specific prompt module
    prompt        Prints the full starship prompt

https://github.com/starship/starship
```

We've got a working `starship`, built in a Docker container, running on Guix.

While this exercise was interesting, it's not a sustainable way to build things, even manually. Upgrading any of the packages we "borrowed" library paths from, would break `starship` too.

## A newer Rust

[packages/rust-newer.scm](../packages/rust-newer.scm) builds `rust@1.40.0`. That takes 2 hours on my system (WSL2, i5). The package might be poor quality (Guix n00b here) and for reasons I don't understand, I had to `(#:validate-runpath? #f)`. Anyway, let's install this new Rust in the throwaway profile we created at the beginning of this document:

```
$ guix package -p ~/temp/build-starship --install rust@1.40.0 rust@1.40.0:cargo
$ guix package -p ~/temp/build-starship -I
rust    1.40.0  out     /gnu/store/58rizji545xlfg1xzzga3qifh75yy2qj-rust-1.40.0
rust    1.40.0  cargo   /gnu/store/zfqxvz9vqbqz3by56iib5n2w90fyn7xf-rust-1.40.0-cargo
$
```

Try to build starship outside of Docker:

```
$ pwd
/home/giuliano/temp/starship
$ export CC=gcc
$ cargo build --release
```

It works!

Compare the rpath for the docker-built starship:

```
$ patchelf --print-rpath starship.docker | tr ':' '\n' | sort
/gnu/store/01b4w3m6mp55y531kyi1g8shh722kwqm-gcc-7.5.0-lib/lib
/gnu/store/01b4w3m6mp55y531kyi1g8shh722kwqm-gcc-7.5.0-lib/lib/gcc/x86_64-unknown-linux-gnu/7.5.0/../../..
/gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib
/gnu/store/hcxpkksmbql6s4al8yy2myr25kh4cic0-openssl-1.1.1g/lib
/gnu/store/rykm237xkmq7rl1p0nwass01p090p88x-zlib-1.2.11/lib
starship/target/release on  HEAD (0dfe3f7) ❯
```

...with the one built directly in Guix, using the newer Rust:

```
$ patchelf --print-rpath starship | tr ':' '\n' | sort
/gnu/store/01b4w3m6mp55y531kyi1g8shh722kwqm-gcc-7.5.0-lib/lib/gcc/x86_64-unknown-linux-gnu/7.5.0/../../..
/gnu/store/hcxpkksmbql6s4al8yy2myr25kh4cic0-openssl-1.1.1g/lib
/gnu/store/rykm237xkmq7rl1p0nwass01p090p88x-zlib-1.2.11/lib
/home/giuliano/.guix-profile/lib
```

## Cleanup

Now that we no longer need the `~/temp/build-starship` profile, let's remove it. Running `guix gc` gets rid of the newer Rust we've built (it removes it from the store).

```
$ rm ~/temp/build-starship
$ guix gc
$ ll /gnu/store/58rizji545xlfg1xzzga3qifh75yy2qj-rust-1.40.0
total 12
dr-xr-xr-x 4 root root 4096 Jun  2 22:07 share/
dr-xr-xr-x 3 root root 4096 Jun  2 22:07 lib/
dr-xr-xr-x 2 root root 4096 Jun  2 22:07 bin/
$
```

Wait, Rust is still in the store?

That's because I've deleted the current `~/temp/build-starship` profile, but left the older generations around:

```
$ ls -1d ~/temp/build-starship*
/home/giuliano/temp/build-starship-1-link
/home/giuliano/temp/build-starship-2-link
/home/giuliano/temp/build-starship-3-link
/home/giuliano/temp/build-starship-4-link
/home/giuliano/temp/build-starship-5-link
/home/giuliano/temp/build-starship.lock
```

So try again by removing all the links (but leaving the `.lock` file untouched, I don't know what it is).

```
$ rm ~/temp/build-starship*link
$ ls -1d ~/temp/build-starship*
/home/giuliano/temp/build-starship.lock
$ guix gc
finding garbage collector roots...
[..]
$ ll /gnu/store/58rizji545xlfg1xzzga3qifh75yy2qj-rust-1.40.0
ls: cannot access '/gnu/store/58rizji545xlfg1xzzga3qifh75yy2qj-rust-1.40.0': No such file or directory
```

All gone.
