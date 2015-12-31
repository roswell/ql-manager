# ql-manager

A Quicklisp environment manager.

# Overview

This is a tiny library for downloading and managing multiple Quicklisp
installations. It's a pretty flexible library, and as such, doesn't do certain
things like installing those QL setups, but you can easily extend it to do that.

ql-manager downloads the Quicklisp installer to a directory, then you can use it
to install Quicklisp environments into subdirectories, and record those installs
in a database.

# Usage

```lisp
CL-USER> (ql:quickload :ql-manager)
To load "ql-manager":
  Load 1 ASDF system:
    ql-manager
; Loading "ql-manager"
.....................
(:QL-MANAGER)

;;; Create a manager object, with the directory where you want to store the QL
;;; installs
CL-USER> (defvar *manager* (make-instance 'ql-manager:manager
                                          :directory #p"/home/eudoxia/.quicklisp/"))
*MANAGER*

;;; Download Quicklisp

CL-USER> (ql-manager:download-quicklisp-installer *manager*)
Downloading the Quicklisp installer...
Downloading "http://beta.quicklisp.org/quicklisp.lisp" (57.144 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Verifying installer...
NIL
```

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.
