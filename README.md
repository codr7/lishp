## lishp
#### a Common Lispy, text based shell

```
$0 say "hello"
hello
$1 q
```

### intro
```lishp``` aims to implement a text based shell that's programm(ed/able) in CL, while talking the bold step to carefully tweak CL's syntax in shell context to fit the purpose without burning any bridges.

### setup
A prebuilt binary is provided for macOS, building requires SBCL:

```
$ sbcl
> (asdf:make "lishp")
...
$ ./lishp
lishp v1,
may the source be with you!

$0 q
saving the world...
```

### status
All functionality described in this document is intended to work reliably; but `lishp` is still very raw, with missing features and weird edge cases all over the place.

### vision
What you see is a tiny step in the direction where I'm aiming, this is very much an explorative adventure with no end in neither mind nor sight.

### virtual reality
True to it's inner Lisp; `lishp` implements a virtual world, contained in the executable image; a namespace clearly separated from the rest of your computer and all it's pesky little details. It's not an isolated world, by any means; but it doesn't support listing files in its host environment out of the box, for example; not that it couldn't be trivially extended by the user to do that, it just tries to solve the more general problem in a more programmable style.

### bindings
`get` returns the value of the first binding found for the specified key starting from the current directory and recursing out to root.

```
$0 get say
>say:
say()
```

`set` may be used to set/override the value of a binding in the current directory.

```
$0 set x 42
$1 get x
>x:
42
```

### directories
Directories work more or less as you would expect; but using `>` as separator, and it's opposite instead of `..`.
The actual directory lives independently from its path in a specific context in the form of a regular hash table.

```
$0 ls
contents of >:
$1 md foo bar
$2 ls
contents of >:
bar> (0)
foo> (0)
$3 cd bar
bar>
$4 md baz
$5 cd <
>
$6 ls
contents of >:
bar> (1)
foo> (0)
```

### support
Should you wish to support this effort and allow me to spend more of my time and energy on evolving `lishp`, feel free to [help](https://liberapay.com/andreas7/donate) make that economically feasible. Rest assured that any contributions are most appreciated, as I much prefer writing code that means something to me on my own terms (who doesn't?).