```
java.lang.UnsatisfiedLinkError: /tmp/jogamp_0000/file_cache/jln1976543742187674208/jln9081750750926925924/natives/linux-amd64/libnativewindow_awt.so: libXxf86vm.so.1: cannot open shared object file: No such file or directory
        at java.lang.ClassLoader$NativeLibrary.load(Native Method)
        at java.lang.ClassLoader.loadLibrary0(ClassLoader.java:1941)
        at java.lang.ClassLoader.loadLibrary(ClassLoader.java:1824)
        at java.lang.Runtime.load0(Runtime.java:809)
        at java.lang.System.load(System.java:1086)
        at com.jogamp.common.jvm.JNILibLoaderBase.loadLibraryInternal(JNILibLoaderBase.java:603)
        at com.jogamp.common.jvm.JNILibLoaderBase.access$000(JNILibLoaderBase.java:63)
        at com.jogamp.common.jvm.JNILibLoaderBase$DefaultAction.loadLibrary(JNILibLoaderBase.java:106)
        at com.jogamp.common.jvm.JNILibLoaderBase.loadLibrary(JNILibLoaderBase.java:487)
        at jogamp.nativewindow.NWJNILibLoader.access$000(NWJNILibLoader.java:39)
        at jogamp.nativewindow.NWJNILibLoader$1.run(NWJNILibLoader.java:49)
        at jogamp.nativewindow.NWJNILibLoader$1.run(NWJNILibLoader.java:41)
        at java.security.AccessController.doPrivileged(Native Method)
        at jogamp.nativewindow.NWJNILibLoader.loadNativeWindow(NWJNILibLoader.java:41)
        at jogamp.nativewindow.jawt.JAWTUtil.<clinit>(JAWTUtil.java:336)
        at java.lang.Class.forName0(Native Method)
        at java.lang.Class.forName(Class.java:348)
        at com.jogamp.nativewindow.NativeWindowFactory$3.run(NativeWindowFactory.java:346)
        at com.jogamp.nativewindow.NativeWindowFactory$3.run(NativeWindowFactory.java:342)
        at java.security.AccessController.doPrivileged(Native Method)
        at com.jogamp.nativewindow.NativeWindowFactory.initSingleton(NativeWindowFactory.java:342)
        at com.jogamp.newt.NewtFactory$1.run(NewtFactory.java:68)
        at java.security.AccessController.doPrivileged(Native Method)
        at com.jogamp.newt.NewtFactory.<clinit>(NewtFactory.java:65)
        at processing.opengl.PSurfaceJOGL.initIcons(PSurfaceJOGL.java:498)
        at processing.opengl.PSurfaceJOGL.initFrame(PSurfaceJOGL.java:134)
        at processing.core.PApplet.initSurface(PApplet.java:11035)
        at processing.core.PApplet.runSketch(PApplet.java:10922)
        at processing.core.PApplet.main(PApplet.java:10620)
UnsatisfiedLinkError: /tmp/jogamp_0000/file_cache/jln1976543742187674208/jln9081750750926925924/natives/linux-amd64/libnativewindow_awt.so: libXxf86vm.so.1: cannot open shared object file: No such file or directory
UnsatisfiedLinkError: /tmp/jogamp_0000/file_cache/jln1976543742187674208/jln9081750750926925924/natives/linux-amd64/libnativewindow_awt.so: libXxf86vm.so.1: cannot open shared object file: No such file or directory
A library relies on native code that's not available.
Or only works properly when the sketch is run as a 32-bit application.
```

`%load-path`

```
(
/gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import
/gnu/store/0m0vd873jp61lcm4xa3ljdgx381qa782-guile-3.0.2/share/guile/3.0
/gnu/store/0m0vd873jp61lcm4xa3ljdgx381qa782-guile-3.0.2/share/guile/site/3.0
/gnu/store/0m0vd873jp61lcm4xa3ljdgx381qa782-guile-3.0.2/share/guile/site
/gnu/store/0m0vd873jp61lcm4xa3ljdgx381qa782-guile-3.0.2/share/guile
)

nonguix on master ❯ find /gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import -type f
/gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import/guix/elf.scm
/gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import/guix/build/gremlin.scm
/gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import/guix/build/gnu-build-system.scm
/gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import/guix/build/utils.scm
/gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import/guix/build/copy-build-system.scm
/gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import/nonguix/build/binary-build-system.scm
/gnu/store/2f9z9v2ibix5hp51w7qwgpkhhsiyj6xc-module-import/nonguix/build/utils.scm
nonguix on master ❯
```