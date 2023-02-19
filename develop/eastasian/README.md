Get Unicode EastAsian Width.


# File

- Source
  - [eastasian.c](eastasian.c)
  - [eastasian.h](eastasian.h)
  - [unicode.h](unicode.h)


# Example

```c
/* Get width */
x = eastasian_width('A');     /* x is 1. */
x = eastasian_width(0x3042);  /* x is 2. */

/* Get type */
y = eastasian_symbol('A');     /* y is EastAsian_Na. */
y = eastasian_symbol(0x3042);  /* y is EastAsian_W. */
```


# Distribution

[https://github.com/nptcl/hypd](https://github.com/nptcl/hypd)

