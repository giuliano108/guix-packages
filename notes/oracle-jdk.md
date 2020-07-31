# Oracle JDK

Similar to what we did for `babashka` (notes [here](babashka.md)), we can use [`nonguix`](https://gitlab.com/nonguix/nonguix/)'s `binary-build-system` to fix all the binaries (executables and libraries) included in the JDK. By "fixing" I mean teaching the binaries where to find, in the Guix store, the libraries they depend on. Here's a quick way to list them all (I've extracted the JDK tarball as is, under `~/temp`).

```
~/temp/jdk-14.0.2 ❯ find . -type f \( -executable -or -name '*.so' \) | xargs ldd 2>/dev/null | grep not.found | sort | uniq
        libasound.so.2 => not found
        libfreetype.so.6 => not found
        libgcc_s.so.1 => not found
        libjvm.so => not found
        libstdc++.so.6 => not found
        libX11.so.6 => not found
        libXext.so.6 => not found
        libXi.so.6 => not found
        libXrender.so.1 => not found
        libXtst.so.6 => not found
        libz.so.1 => not found
~/temp/jdk-14.0.2 ❯
```

Where do all these dependencies come from?

This one is obvious, it's part of the JDK distribution itself:

```
~/temp/jdk-14.0.2 ❯ find . | grep libjvm.so
./lib/server/libjvm.so
```

All the others happen to already be in my store, making it easy to know where they're from (f.e. `libasound` comes from the `alsa-lib`package, etc.).

```
~/temp/jdk-14.0.2 ❯ for f in \
'libasound.so.2' \
'libfreetype.so.6' \
'libX11.so.6' \
'libXext.so.6' \
'libXi.so.6' \
'libXrender.so.1' \
'libXtst.so.6' \
'libz.so.1' ; do find /gnu/store -name $f | grep -v -- -profile/ | head -1; done
/gnu/store/zcjdb23gbhl0pcnvvm8rnlprkfl43cv5-alsa-lib-1.2.2/lib/libasound.so.2
/gnu/store/haaam6v8l4s75mj9xmpb9gc78xk001y9-freetype-2.10.1/lib/libfreetype.so.6
/gnu/store/nab7hhw326cpmpwyc1rgm0q8sk464qry-libx11-1.6.9/lib/libX11.so.6
/gnu/store/2hpgzimhi1vg9n8xagvmqh5sf96svz8q-libxext-1.3.4/lib/libXext.so.6
/gnu/store/r08g4srlbr8p8i9ram2jl6izs75pb5wa-libxi-1.7.10/lib/libXi.so.6
/gnu/store/2mbh4bhhmkhm7188c7rqwzbm5lap0cc9-libxrender-0.9.10/lib/libXrender.so.1
/gnu/store/dnaxy9x4y40n1r2mhnf0ws0m7z5dlsk4-libxtst-1.2.3/lib/libXtst.so.6
/gnu/store/qx7p7hiq90mi7r78hcr9cyskccy2j4bg-zlib-1.2.11/lib/libz.so.1
~/temp/jdk-14.0.2 ❯
```

I didn't include these in the `for` loop above, they both stem from `gcc` (I knew from previous Guix packaging attempts).

```
libstdc++.so.6
libgcc_s.so.1
```

## patchelf-plan

Alright, let's try to make a [babashka](https://github.com/borkdude/babashka) script to generate the patchelf plan for us:

```clojure
#!/usr/bin/env bb

(def basedir "/home/giuliano/temp/jdk-14.0.2")

(def dep->input
  {"libasound.so.2"   "alsa-lib"
   "libfreetype.so.6" "freetype"
   "libX11.so.6"      "libx11"
   "libXext.so.6"     "libxext"
   "libXi.so.6"       "libxi"
   "libXrender.so.1"  "libxrender"
   "libXtst.so.6"     "libxtst"
   "libz.so.1"        "zlib"
   "libjvm.so"        "SKIP"         ; don't care about this one
   "libgcc_s.so.1"    "gcc:lib"
   "libstdc++.so.6"   "libstdc++"})

(def binaries (-> (str "find " basedir " -type f ( -executable -or -name *.so )")
                  (str/split #" ")
                  (as-> args (apply shell/sh args))
                  :out
                  (str/split #"\n")))

(defn missing-deps-for-binary [path]
  (let [get-lib #(-> (re-matches #"\t(.*) => not found$" %) last)
        ldd-output (-> (shell/sh "ldd" path)
                       :out
                       (str/split #"\n"))
        not-found (filter #(re-find #" not found$" %) ldd-output)
        libs (distinct (map get-lib not-found))]
    libs))

(def binary->missing-deps (apply hash-map
                                 (mapcat #(let [binary (str/replace % (str basedir "/") "")
                                                missing-deps (missing-deps-for-binary %)]
                                            (if (seq missing-deps)
                                              [binary missing-deps]
                                              [])) binaries)))

(defn patchelf-spec-for-binary
  [[binary missing-deps]]
  [binary missing-deps]
  (let [inputs (->> missing-deps (map #(get dep->input %)) distinct (remove #(= "SKIP" %)))]
    (when (seq inputs)
      (str "(\"" binary "\"\n " (pr-str inputs) ")\n"))))

(defn patchelf-plan []
  (let [specs (map #(patchelf-spec-for-binary %) binary->missing-deps)]
    specs))

(print (str/join "" (patchelf-plan)))
```

The script spews the `#:patchelf-plan` that I used in the [package](../packages/oracle-jdk.scm).

## `fix-runpath`

JDK's binaries come with an [rpath](https://en.wikipedia.org/wiki/Rpath) set to `$ORIGIN` and/or `$ORIGIN:$ORIGIN/../lib`. That allows them to dynamically link other binaries that are included in the JDK, no matter where that ends up in the filesystem. In a sense, the JDK is "relocatable".

The `binary-build-system` replaces the rpath completely getting rid of those `$ORIGIN` pseudo-paths. That stops, say, `java` from being able to import `libjli.so` under `../lib`.

```
temp/jdk-14.0.2/bin ❯ patchelf --print-rpath java
$ORIGIN:$ORIGIN/../lib
temp/jdk-14.0.2/bin ❯ ldd java | grep libjli
        libjli.so => /home/giuliano/temp/jdk-14.0.2/bin/./../lib/libjli.so (0x00007f49a1e32000)
```

The custom `fix-runpath` phase (in the [package](../packages/oracle-jdk.scm)) addresses that by injecting, where needed, the full store path to `lib/` into the rpath, preserving the changes that `patchelf-plan` already did. I probably over-complicated things by hardcoding a list of binaries that need fixing into the package (might have patched all binaries regardless)...

You might've noticed this line:

```
   "libjvm.so"        "SKIP"         ; don't care about this one
```

That library is under `lib/server`, instead of `lib/` like all the others. Even under Ubuntu, it can't be resolved. I decided to not give it any special treatment.

```
giuliano@ubuntu:/usr/lib/jvm/java-11-oracle/lib $ ldd libjava.so
        linux-vdso.so.1 (0x00007fffa2351000)
        libjvm.so => not found
        libverify.so => /usr/lib/jvm/java-11-oracle/lib/./libverify.so (0x00007f7195772000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f719556e000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f719517d000)
        /lib64/ld-linux-x86-64.so.2 (0x00007f7195bac000)
        libjvm.so => not found
giuliano@ubuntu:/usr/lib/jvm/java-11-oracle/lib $
```

## Installation

These instructions are also included in the package:

```
;;; To install this package:
;;;
;;; * Manually download the JDK from Oracle's website.
;;;   This is a manual step because Oracle want you to accept their EULA before allowing you
;;;   to download anything...
;;; * Choose the plain .tar.gz (Linux Compressed Archive), save it somewhere.
;;; * Run:
;;;   guix install oracle-jdk --with-source=oracle-jdk=/tmp/jdk-14.0.2_linux-x64_bin.tar.gz
;;; * The JDK will be installed under share/oracle-jdk-* . To use it,
;;;   change your $PATH/$JAVA_HOME accordingly or let jEnv do that for you.
;;;
;;; For more info check: https://github.com/giuliano108/guix-packages/blob/master/notes/oracle-jdk.md
```
