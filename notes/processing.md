# Processing

Same as with [babashka](babashka.md) and the [Oracle JDK](oracle-jdk.md), I used [`nonguix`](https://gitlab.com/nonguix/nonguix/)'s `binary-build-system` to fix the binaries (executables and libraries) included in the Processing distribution.

So far, this was the trickiest patchelf-based repackage I've made.

## `$ORIGIN`

Some of the binaries used `$ORIGIN` in their [rpath](https://en.wikipedia.org/wiki/Rpath). The [man page](https://man7.org/linux/man-pages/man8/ld.so.8.html) expains what this does:

```
$ORIGIN (or equivalently ${ORIGIN})
      This expands to the directory containing the program or
      shared object.  Thus, an application located in
      somedir/app could be compiled with

          gcc -Wl,-rpath,'$ORIGIN/../lib'

      so that it finds an associated shared object in
      somedir/lib no matter where somedir is located in the
      directory hierarchy.  This facilitates the creation of
      "turn-key" applications that do not need to be installed
      into special directories, but can instead be unpacked into
      any directory and still find their own shared objects.
```

In other words, it enables finding dinamic libraries relative to the binary location in the filesystem.  
I had to write some code (in the `(giuliano108 utils)` module) to turn those `$ORIGIN`-based rpath components into absolute paths.

## Dynamic libraries packaged in .jar files

```
java.lang.UnsatisfiedLinkError: /tmp/jogamp_0000/file_cache/jln1976543742187674208/jln9081750750926925924/natives/linux-amd64/libnativewindow_awt.so: libXxf86vm.so.1: cannot open shared object file: No such file or directory
```

A few `.so` libraries are contained in `.jar` files. Processing extracts them under `/tmp` at runtime, then tries to `dlopen()` them.  
I had to extract the `.jar`, include the libraries there in the `patchelf-plan`, recreate it and replace the original. I only bothered about `jogl-all-natives-linux-amd64.jar`, so there might be more un-Guix-ified libraries lurking around the Processing binary distribution...

## More `dlopen()`

At this point some Processing examples were working, while others still triggered an obscure exception:

```
java.lang.RuntimeException: java.lang.ClassNotFoundException: Failed to find NEWT Display Class <.x11.DisplayDriver>
        at jogamp.newt.DisplayImpl.create(DisplayImpl.java:314)
        at com.jogamp.newt.NewtFactory.createDisplay(NewtFactory.java:167)
        at com.jogamp.newt.NewtFactory.createDisplay(NewtFactory.java:149)
        at processing.opengl.PSurfaceJOGL.initDisplay(PSurfaceJOGL.java:149)
        at processing.opengl.PSurfaceJOGL.initFrame(PSurfaceJOGL.java:135)
        at processing.core.PApplet.initSurface(PApplet.java:11035)
        at processing.core.PApplet.runSketch(PApplet.java:10922)
        at processing.core.PApplet.main(PApplet.java:10620)
Caused by: java.lang.ClassNotFoundException: Failed to find NEWT Display Class <.x11.DisplayDriver>
        at jogamp.newt.DisplayImpl.getDisplayClass(DisplayImpl.java:277)
        at jogamp.newt.DisplayImpl.create(DisplayImpl.java:285)
        ... 7 more
RuntimeException: java.lang.ClassNotFoundException: Failed to find NEWT Display Class <.x11.DisplayDriver>
```

`libGL` and other OpenGL-related libraries are also `dlopen()`'ed at runtime. Guix provides a `wrap-program` helper which I used to set the required `LD_LIBRARY_PATH`.
