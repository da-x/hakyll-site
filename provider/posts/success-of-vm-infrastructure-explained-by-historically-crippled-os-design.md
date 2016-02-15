---
title: Success of VM infrastructure explained by historically crippled OS design
excerpt: OSes
link: http://www.blog.aloni.org/success-of-vm-infrastructure-explained-by-historically-crippled-os-design/
author: dan
post_id: 217
published: 2010-12-22
created: 2010/12/22 14:47:38
created_gmt: 2010/12/22 14:47:38
comment_status: open
post_name: success-of-vm-infrastructure-explained-by-historically-crippled-os-design
status: publish
post_type: post
---

During the last decade we have seen the rise of server and desktop virtualization infrastructure as the official and standard means of creating services and resources isolation at both the client and server side. System virtualization provided rigid management of computing resources over standardized PC and server hardware for the first time.

However when one thinks about it from an engineering perspective - why does a full PC system virtualization needed in order to achieve 'rigid management of computing resources'? Why couldn't [OS](http://en.wikipedia.org/wiki/Operating_system) designers create those abilities in the first place? Why do we need hyper-visors such as [Xen](http://www.xen.org/) and software such as one provided by [VMware](http://www.vmware.com) in order to attain these abilities?

Short answer is that all major OS writers did not provide the proper infrastructure for running sub-instances of the OS **efficiently** and managing those instances with Quality of Service and fail-over on a network of hardware. A standard OS only provides the means of running unmigratable processes. A standard OS is there just to provide isolation between processes, scheduling, memory management, hardware abstraction, implementation of file systems and networking protocols. In the past there were attempts to extend the OS concept in order to accommodate clustering capabilities.

Linux serves as a good ground for these kind of experiments. Take for example - [MOSIX](http://www.mosix.org), which is a project that extends Linux in a sense that one Linux system encompasses more than one PC, allowing for process migration. However process migration on its own is inadequate since a process is just one part that composes 'service', and you would actually want to migrate whole services efficiently in a fully isolated manner. Another project is '[User Mode Linux](http://user-mode-linux.sourceforge.net)' (often confused with [UML](http://en.wikipedia.org/wiki/Unified_Modeling_Language) which is something else), which allows running the Linux kernel as a Linux process. At some point in time there was a resource project that added suspend/resume support to 'User Mode Linux', but except that ability, 'User Model Linux' did not gain ground in the virtualization field.

Take another example - [Cooperative Linux](http://www.colinux.org), written by yours truly. Except for helping lots of Windows users fiddling with Linux on their desktop machines, it did not gain ground in the virtualization industry on servers (even though it also runs on Linux, with the best opportunity for performance). All those projects are just partial hacks in the direction of VM infrastructure and none seem to provide it in a productive sense. All major OS designs are crippled in a sense that you need to virtualized them (i.e. put them in "a box") in order to attain VM infrastructure capabilities. Key for VM infrastructure is that you must provide the ability to manage your VMs on a cluster in an enterprise level manner and capabilities. Of course, I don't blame OS writers for not detecting these needs in the corporate world. Actually, there were a few engineers that did detect it, and even awhile back. IBM already [came up with the idea](http://www.networkworld.com/news/2009/043009-ibm-virtualization.html) of virtualization awhile back on their mainframes. However, that was the beginning - and later on when traditional OS designs for single, desktop users emerged those features were not needed. Unfortunately those OSes also propagated to the server side, and only few saw the potential to bring the virtualization feature back under the cloth of the 'full system virtualization' method, adding a VM infrastructure.


Rethinking the Operating System concept, one would come up with a '**Super Operating System**' concept (i.e. SOS, also a funny overlap with the acronym for - "Save Our Souls"), that would define a new Operating System from the ground up that would also conform to all the attributes that are currently provided by standard 'VM infrastructure'. SOS should be able to run instances of itself, connecting those instances with their dependencies. For example, I would be able to run two instances and connect them with a virtual Ethernet link (of course, I'd need a separate network stack for each instance). It should also make it possible to migrate its sub-instances, and other aspects of current-day's VM management.

I know that designing a new OS as a standalone mission is somewhat unessential, but I have other ideas to pursue in that venue that would might improve the software world a little, but that's a topic for another post.
