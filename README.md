## lishp
#### a Common Lispy, text based shell

```
lishp v1,
may the source be with you!

$0 say "hello"
hello
$1 q
arrivederci!
```

### intro
```lishp``` aims to implement a text based shell that's programm(ed/able) in CL, while talking the bold step to carefully tweak CL's syntax in shell context to fit the purpose without burning any bridges. An attempt to apply the extendability and power of Emacs to solving the text based shell problem; but starting from a sane, mature Lisp standard/implementation and offered under a less conditional license. Back to the original Lisp machines in some ways; sans graphics for now, one step at a time.

#### vision
What you see at this point is a tiny step in the direction where I'm aiming with this, I still don't feel like I'm even done tracing the idea to it's logical conclusions, this is very much an explorative adventure with no end in neither mind nor sight.

### virtual reality
True to it's inner Lisp; `lishp` implements a virtual world, a namespace clearly separated from the rest of your computer and all it's pesky little details. It's not an isolated world, by any means; but it doesn't support listing files in its host environment out of the box, for example; not that it couldn't be trivially extended by the user to do that, it just tries to solve the more general problem in a more programmable style.

### syntax

#### directories

```
$0
lishp>
$1 ls
Contents of lishp>:
$2 md foo>bar baz
$3 ls
Contents of lishp>:
foo> (1)
baz> (0)
$4 cd foo
lishp>foo>
$5 ls
Contents of lishp>foo>:
bar> (0)
$6 rm bar
$7 cd <
lishp>
```

### status
All functionality described in this document is intended to work reliably, but `lishp` is still very much a work in progress with huge gaps all over the place. The good news is that it is trivial to extend from CL; which hopefully will allow more experimentaion and a more constructive as well as spedient evolution than less programmable approaches to solving similar use cases, regular shells included.

### support
Should you wish to support this effort and allow me to spend more of my time and energy on evolving `lishp`, feel free to [help](https://liberapay.com/andreas7/donate) make that economically feasible. Rest assured that any contributions are most appreciated, as I much prefer writing code that means something to me on my own terms (who doesn't?).