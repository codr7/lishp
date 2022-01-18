# lishp
## a Common Lispy text based shell

### intro
```lishp``` aims to implement a text based shell that's implemented and programmable in Common Lisp, while talking the bold step to carefully tweak Common Lisp's syntax to fit the purpose without burning any bridges. Or, more metaphorically; the extendability and power of Emacs applied to solving the text based shell problem but starting from a sane, mature Lisp standard/implementation; offered under a less conditional license.

### virtual
True to it's Lisp heritage and host; `lishp` implements a virtual world, a namespace clearly separated from the rest of your computer. It's not an isolated world, by any means; but separate. It doesn't support listing files in a regular directory out of the box, for example; not that it couldn't be extended by adding a command to do just that, it just tries to solve the more general problem in a more programmable style. Lisp images represent the world, directories are packages with a touch of syntax to represent hierarchies; functions represents executables, variables data etc.

### syntax

#### directories
Directories are represented by packages, angled brackets are used to indicate hirerchy.

```
$1 ls
$2 md foo
$3 md baz
$3 ls
foo>
baz>
$6 md foo>bar
$7 cd foo>bar
foo>bar>
$7 cd >
>
$8 cd foo
foo>
$10 rm bar
$11 ls
$10 cd <baz
baz>
```

### status
All functionality described in this document is intended to work reliably, but `lishp` is still very much a work in progress with huge gaps all over the place. The good news is that it is trivial to extend from Common Lisp; which hopefully will allow more experimentaion and a more constructive as well as spedient evolution than less programmable approaches to solving similar use cases, regular shells included.

### support
Should you wish to support this effort and allow me to spend more of my time and energy on evolving `lishp`, feel free to [help]() make that economically feasible. Rest assured that any contributions are most appreciated, as I much prefer writing code entirely on my own terms (who doesn't?).