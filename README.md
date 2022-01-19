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
```lishp``` aims to implement a text based shell that's programm(ed/able) in CL, while talking the bold step to carefully tweak CL's syntax in shell context to fit the purpose without burning any bridges.

### status
All functionality described in this document is intended to work reliably; but `lishp` is still very raw, with missing features and weird edge cases all over the place.

### vision
What you see at this point is a tiny step in the direction where I'm aiming with this, I'm still tracing the idea to it's logical conclusions, this is very much an explorative adventure with no end in neither mind nor sight.

### virtual reality
True to it's inner Lisp; `lishp` implements a virtual world, a namespace clearly separated from the rest of your computer and all it's pesky little details. It's not an isolated world, by any means; but it doesn't support listing files in its host environment out of the box, for example; not that it couldn't be trivially extended by the user to do that, it just tries to solve the more general problem in a more programmable style.

### directories
Directories work more or less as you would expect; but using `>` as separator, and it's opposite instead of `..`.

```
$0
>
$1 ls
Contents of >:
$2 md foo>bar baz
$3 ls
Contents of >:
foo> (1)
baz> (0)
$4 cd foo
foo>
$5 ls
Contents of foo>:
bar> (0)
$6 rm bar
$7 cd <
>
```

### support
Should you wish to support this effort and allow me to spend more of my time and energy on evolving `lishp`, feel free to [help](https://liberapay.com/andreas7/donate) make that economically feasible. Rest assured that any contributions are most appreciated, as I much prefer writing code that means something to me on my own terms (who doesn't?).