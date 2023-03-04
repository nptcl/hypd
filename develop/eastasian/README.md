Get Unicode EastAsian Width.


# C

- Source
  - [eastasian.c](eastasian.c)
  - [eastasian.h](eastasian.h)
  - [unicode.h](unicode.h)


```c
/* Get width */
x = eastasian_width('A');     /* x is 1. */
x = eastasian_width(0x3042);  /* x is 2. */

/* Get type */
y = eastasian_symbol('A');     /* y is EastAsian_Na. */
y = eastasian_symbol(0x3042);  /* y is EastAsian_W. */
```


# Common Lisp

- Source
  - [eastasian.lisp](eastasian.lisp)


```lisp
* (load "eastasian.lisp")
T
* (eastasian:eastasian-width #x3042)
2
* (eastasian:eastasian-symbol #x3042)
EASTASIAN:EASTASIAN-W
```


# Distribution
[https://github.com/nptcl/hypd](https://github.com/nptcl/hypd)

