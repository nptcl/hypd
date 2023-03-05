Get a random number.


# File

- Source
  - [random.c](random.c)
  - [random.h](random.h)
  - [md5encode.c](md5encode.c)
  - [md5encode.h](md5encode.h)


# Compile

```sh
# for Unix, Linux, FreeBSD  (use /dev/urandom)
#   cc -DHYPD_UNIX -c random.c
#   cc -DHYPD_LINUX -c random.c
#   cc -DHYPD_FREEBSD -c random.c
# for Windows  (use RtlGenRandom function)
#   cc -DHYPD_WINDOWS -c random.c

$ cc -DHYPD_UNIX random.c md5encode.c main.c
```


# Example

```c
#include <stdio.h>
#include "random.h"

int main(void)
{
    struct random_state s;

    random_seed_string(&s, "Hello");
    printf("%X\n", (unsigned)random_number_64bit(&s));
    printf("%X\n", (unsigned)random_number_64bit(&s));

    random_seed_string(&s, "Hello");
    printf("%X\n", (unsigned)random_number_64bit(&s));
    printf("%X\n", (unsigned)random_number_64bit(&s));

    random_seed_randomly(&s);
    printf("%d\n", (int)random_equal_64bit(&s, 10)); /* 0..10 */
    printf("%d\n", (int)random_less_64bit(&s, 5));   /* 0..4 */

    /* 0.0 (inclusive) .. 1.0 (exclusive) */
    printf("%f\n", float_random_64bit(&s));
    printf("%f\n", double_random_64bit(&s));
    printf("%Lf\n", long_random_64bit(&s));

    return 0;
}
```


# Distribution
[https://github.com/nptcl/hypd](https://github.com/nptcl/hypd)

