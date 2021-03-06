## lishp
#### a Common Lispy, text based shell

```
0 say "hello"
hello
1 q
```

### intro
```lishp``` aims to implement a text based shell that's programm(ed/able) in CL, while talking the bold step to carefully tweak CL's syntax in shell context to fit the purpose without burning any bridges.

### setup
A prebuilt binary is provided for macOS, building requires SBCL:

```
 sbcl
> (asdf:make "lishp")
...
 ./lishp
lishp v1,
may the source be with you!

0 q
saving world...
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
0 get say
>say:
say()
```

`set` may be used to set/override the value of a binding in the current directory.

```
0 set x 42
1 get x
>x:
42
```

Bindings may be moved using `mv`,

```
0 set x 42
1 mv x y
2 get y
>y:
42
3 get x
not found: >x
```

and copied using `cp`.

```
0 set x 42
1 cp x y
2 get y
>y:
42
3 get x
>x:
42
```

### directories
Directories work more or less as you would expect; but using `>` as separator, and it's opposite instead of `..`.
The actual directory lives independently from its path in a specific context in the form of a regular hash table.

```
0 ls
contents of >:
1 md foo>bar baz
2 ls
contents of >:
baz> (0)
foo> (1)
3 cd foo
foo>
4 ls
contents of foo>:
bar> (0)
5 cd <
>
6 rm foo>bar
7 ls
contents of >:
baz> (0)
foo> (0)
```

### time
`now`/`today` may be used to obtain the current time/date.
`time-format'/'time-sonze' may be bound to change i/o format.

```
0 get time-format
>time-format:
year
-
month
2
-
day
2
 
hour
2
:
min
2a
1 get time-zone
>time-zone:
#<TIMEZONE UTC>
2 now
2022-01-21 08:47
```

### support
Should you wish to support this effort and allow me to spend more of my time and energy on evolving `lishp`, feel free to [help](https://liberapay.com/andreas7/donate) make that economically feasible. Rest assured that any contributions are most appreciated, as I much prefer writing code that means something to me on my own terms (who doesn't?).