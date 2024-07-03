Advanced Encryption Standard (AES)


# File

- Source
  - [aes.c](aes.c)
  - [aes.h](aes.h)


# Example

```c
#include "aes.h"
#include <stdio.h>
#include <string.h>

int main(void)
{
    int i;
    struct aes a;

    /* key */
    init_aes256(&a);
    memset(a.key, 0, AES_KEY);
    memcpy(a.key, "Hello", 5);
    aes_key(&a);

    /* encode */
    memset(a.state, 0, AES_SIZE);
    memcpy(a.state, "ABCD", 4);
    aes_cipher1(&a);
    for (i = 0; i < AES_SIZE; i++)
        printf("%02x", a.state[i]);
    printf("\n");

    /* decode */
    aes_cipher2(&a);
    printf("%s\n", a.state);

    return 0;
}
```

```
$ cc aes.c main.c
$ ./a.out
67e47efeb27f95fe9d4088250c38c50b
ABCD
```


# Distribution

[https://github.com/nptcl/hypd](https://github.com/nptcl/hypd)

