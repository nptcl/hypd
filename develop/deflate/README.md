DEFLATE decompression code.


# Source

DEFLATE decompression

- `deflate_decode.h`
- `deflate_decode.c`


DEFLATE compression  [Incomplete]

- `deflate_encode.h`
- `deflate_encode.c`


# Format

- `gzip`
  - `format/deflate_gzip.h`
  - `format/deflate_gzip.c`

- `zlib`
  - `format/deflate_zlib.h`
  - `format/deflate_zlib.c`


# Defective Compression

`deflate_encode.c` can't compress.

The encoding always use non-compressed blocks (BTYPE=00).
See 3.2.4. Non-compressed blocks (BTYPE=00),
RFC1951 DEFLATE Compressed Data Format Specification.


# Example

See `inflate_FILE` function in `example_decode.c`.

```c
int main(void)
{
	FILE *fin, *fout;

	fin = fopen("input.deflate", "rb");
	fout = fopen("output.binary", "wb");
	inflate_FILE(fin, fout);
	fclose(fout);
	fclose(fin);

	return 0;
}
```


# Distribution

[https://github.com/nptcl/hypd](https://github.com/nptcl/hypd)

