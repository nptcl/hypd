Reading Unicode File Format.


# File

- Source
  - [unicode.c](unicode.c)
  - [unicode.h](unicode.h)


# Format

- UTF-8
- UTF-16  (`uint16_t` array)
- UTF-16LE
- UTF-16BE
- UTF-32  (`unicode` array)
- UTF-32LE
- UTF-32BE


# Example

```c
int r;
unicode c;
struct state_utf8 x;

/* ASCII */
init_utf8(&x);
r = decode_utf8(&x, 'A', &c);  /* r=1, c='A' */
r = decode_utf8(&x, 'B', &c);  /* r=1, c='B' */

/* Mahjong Tile East Wind, U+1F000, F0 9F 80 80 */
r = decode_utf8(&x, 0xF0, &c);  /* r=0 */
r = decode_utf8(&x, 0x9F, &c);  /* r=0 */
r = decode_utf8(&x, 0x80, &c);  /* r=0 */
r = decode_utf8(&x, 0x80, &c);  /* r=1, c=0x01F000 */

/* Error */
r = decode_utf8(&x, 0xF0, &c);  /* r=0 */
r = decode_utf8(&x, 0x7F, &c);  /* r=-1, ERROR */

/* Error */
r = decode_utf8(&x, 'A', &c);  /* r=-1, ERROR */

/* Recovery */
init_utf8(&x);
r = decode_utf8(&x, 'A', &c);  /* r=1, c='A' */
```


# Distribution

[https://github.com/nptcl/hypd](https://github.com/nptcl/hypd)

