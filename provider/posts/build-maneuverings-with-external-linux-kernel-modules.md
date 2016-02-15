---
author: dan
title: Build maneuverings with external Linux kernel modules
excerpt: Linux Kernel
published: 2014-12-12
---

Much of the material relating to writing [Linux kernel](http://en.wikipedia.org/wiki/Linux_kernel) modules does not discuss the scenario where you would like to replace existing kernel code or a driver with a wrapping interface, or a whole new implementation of the same component, or another scenario where you have one external kernel module that depends on another. Our Linux kernel is a standalone component and it doesn't like these sort of tricks, but its build system is advanced enough to allow to implement them cleanly.

# First, just a plain-old module #

To demonstrate the topics I am about to discuss, let's create a simple kernel module that we can work with:

> ##### `foo/Makefile` #####
```makefile
foo-objs += foo-main.o
obj-m += foo.o
```

The following .gitignore file can be used:

> ##### `.gitignore` #####
```
*.o
*.mod.o
*.ko
.*
modules.order
Module.symvers
```

The C file can be very minimal for now:

`foo/foo-main.c`:
```C
#include <linux/kernel.h>

void foo_export(void)
{
	printk(KERN_DEBUG "foo_export called\n");
}
EXPORT_SYMBOL(foo_export);
```

Building it for the currently running kernel is quite simple:

```
$ make -C /lib/modules/`uname -r`/source M=`pwd`/foo
make: Entering directory `/usr/src/kernels/3.17.3-200.fc20.x86_64'
  CC [M]  /home/dan/module/foo/foo-main.o
  LD [M]  /home/dan/module/foo/foo.o
  Building modules, stage 2.
  MODPOST 1 modules
  LD [M]  /home/dan/module/foo/foo.ko
make: Leaving directory `/usr/src/kernels/3.17.3-200.fc20.x86_64'
```

We should be able to load it via `insmod foo/foo.ko`. But, since the module does not do anything, and does not register on any subsystem, it is effectively just a library. A novice reader can add call to `foo_export` on module init. For now we will continue to focus on the building scriptology in this post.

# Prepare for external access #

We would like for another module to use our kernel code. But first, we need to export it via the standard C means, because `EXPORT_MODULE` is not enough - this just tells the kernel that it is okay to link against `foo_export` in module load time.

So we add this header under `foo/include`:

`foo/include/foo/foo.h`:

```C
#ifndef __FOO_MAIN_H__
#define __FOO_MAIN_H__

void foo_export(void);

#endif
```

We could have added a `foo.h` directly under `foo`, but that is not good because we would like to separate the interface from implementation. We expect other users to include the location of the header file via `-I`. Let's take care of our internal user `foo-main.c` too, by modifying the Makefile:

`foo/Makefile`:
```makefile
ifneq (${LINUXINCLUDE},)
# If we being invoked from kbuild, prepend the proper include paths
LINUXINCLUDE := \
        -I${M}/include \
        ${LINUXINCLUDE}
endif

foo-objs += foo-main.o
obj-m += foo.o

```

We can now proceed to also add `#include <foo/foo.h>` in `foo-main.c`.

**NOTE:** The `LINUXINCLUDE` directive in kbuild is very useful - it allows to insert new include paths before or after other paths or the kernel headers themselves. Here, we needed abstraction for our private module include paths. The module's code is now agnostic to where headers are located, which is a good preparation for any future point in time where the headers might move, and perhaps these headers can move into the kernel itself if our kernel code is really dandy and useful - who knows.

# A second, dependent kernel module #

Similarly to `foo`, we have created `bar`. However, in bar we insert a run-time dependency over foo:

`bar/bar-main.c`:
```C
#include <linux/kernel.h>
#include <bar/bar.h>
#include <foo/foo.h>

void bar_export(void)
{
	foo_export();
	printk(KERN_DEBUG "bar_export called\n");
}
EXPORT_SYMBOL(bar_export);
```

But we would like to avoid the following error:

```C
/home/dan/test/bar/bar-main.c:2:21: fatal error: foo/foo.h: No such file or directory
 #include <foo/foo.h>
```

Let's take the first step. We need to depend on `foo` in `bar`. If we would like to be completely flexible, we can support the modes where foo arrives from the kernel itself or from another external kernel module.

`bar/Makefile`:
```makefile
ifneq (${FOO_PATH},)
FOO_INCLUDE=-I${FOO_PATH}/include
endif

ifneq (${LINUXINCLUDE},)
# If we being invoked from kbuild, prepend the proper include paths
LINUXINCLUDE := \
        -I${M}/include \
        ${FOO_INCLUDE} \
        ${LINUXINCLUDE}
endif

bar-objs += bar-main.o
obj-m += bar.o
```

By adding `foo` to `LINUXINCLUDE` we can now have a working build, if we point `FOO_PATH` to the correct place when building `bar`.

```
make -C /lib/modules/`uname -r`/source M=`pwd`/bar FOO_PATH=`pwd`/foo
```

But is it not over yet, because we get an error from `modpost` (although the build is successful):

```
WARNING: "foo_export" [/home/dan/test/bar/bar.ko] undefined!
```

The kernel keeps track of which modules are exported by which binary code. By default the program `modpost` that runs during the build process performs a lookup on all undefined symbosl via the kernel's own `Module.symvers` located under `/lib/modules` in our case. `foo` is not there. Let's extend our kernel module's Makefile. Luckily we can pass `KBUILD_EXTRA_SYMBOLS` to kbuild, but it is ineffectual to do so from within the Makefile itself when it is invoked by kbuild.


`bar/Makefile`:
```makefile
ifneq (${LINUXINCLUDE},)
# If we being invoked from kbuild, prepend the proper include paths
ifneq (${FOO_PATH},)
FOO_INCLUDE=-I${FOO_PATH}/include
endif
LINUXINCLUDE := \
        -I${M}/include \
        ${FOO_INCLUDE} \
        ${LINUXINCLUDE}
else
export FOO_PATH
export KBUILD_EXTRA_SYMBOLS=${FOO_PATH}/Module.symvers
all:
	make -C ${KDIR} M=$(shell pwd)
endif

bar-objs += bar-main.o
obj-m += bar.o
```

In order to not make the command line any longer, we have inserted a proxy target 'all'. Also, `KBUILD_EXTRA_SYMBOLS` and `FOO_PATH` are forwarded to kbuild, and `KDIR` receives the path of the kernel tree from the top level invocation.

```shell
$ make -C bar KDIR=/lib/modules/`uname -r`/source FOO_PATH=`pwd`/foo
make: Entering directory `/home/dan/test/bar'
make -C /lib/modules/3.17.3-200.fc20.x86_64/source M=/home/dan/test/bar
make[1]: Entering directory `/usr/src/kernels/3.17.3-200.fc20.x86_64'
  LD      /home/dan/test/bar/built-in.o
  CC [M]  /home/dan/test/bar/bar-main.o
  LD [M]  /home/dan/test/bar/bar.o
  Building modules, stage 2.
  MODPOST 1 modules
  CC      /home/dan/test/bar/bar.mod.o
  LD [M]  /home/dan/test/bar/bar.ko
make[1]: Leaving directory `/usr/src/kernels/3.17.3-200.fc20.x86_64'
make: Leaving directory `/home/dan/test/bar'
```
There are several things consider. `foo` must already be built when `bar` is built, otherwise `foo`'s `Module.symvers` is missing, and you would get a warning. The other thing to keep in mind is that the Makefile is evaluated twice - once in our direct execution and a second time when kbuild evaluates it. The current directory (\`pwd\`) in the second evaluation is the kernel tree, and it already contains a lots of useful bits such as `LINUXINCLUDE`. We should be careful about kbuild's own evaluation so that we don't accidently override stuff that we did not intend or insert makefile targets that don't belong under the kbuild environment.

# Careful replacement #

Note that the use of `LINUXINCLUDE` to replace existing kernel headers should be used with care. It is powerful enough to allow re-packaging of whole stacks of code that overlap with existing kernel software stack. Some kernel subsystems are spreading their headers over paths that don't necessarily start with `linux/`, for example, `uapi/linux` (user space headers), and `asm/` (architecture specific headers, actually located under `arch/*/include`). Ordering of paths when extending `LINUXINCLUDE` is key in that case.

One should mind including the proper headers so that the API are validated in compile time and match in run-time. For example, it is possible for an external package X to replace some modular existing kernel subystem X with modified APIs, but when any other piece of code - userspace and kernel space - tries to compile against X, it should refer to the intended headers.
