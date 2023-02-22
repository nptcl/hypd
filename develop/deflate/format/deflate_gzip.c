#include "deflate_gzip.h"
#include <stdio.h>
#include <string.h>

#define GzipError(x) { \
	fprintf(stderr, "[ERROR] %s\n", x); \
}
#define GzipError1(x,y) { \
	char __message[256]; \
	snprintf(__message, 256, x, y); \
	GzipError(__message); \
}

/*****************************************************************************
 *  CRC32
 *****************************************************************************/
static const uint32_t gzip_crc32[256] = {
	0x00000000UL, 0x77073096UL, 0xEE0E612CUL, 0x990951BAUL,
	0x076DC419UL, 0x706AF48FUL, 0xE963A535UL, 0x9E6495A3UL,
	0x0EDB8832UL, 0x79DCB8A4UL, 0xE0D5E91EUL, 0x97D2D988UL,
	0x09B64C2BUL, 0x7EB17CBDUL, 0xE7B82D07UL, 0x90BF1D91UL,
	0x1DB71064UL, 0x6AB020F2UL, 0xF3B97148UL, 0x84BE41DEUL,
	0x1ADAD47DUL, 0x6DDDE4EBUL, 0xF4D4B551UL, 0x83D385C7UL,
	0x136C9856UL, 0x646BA8C0UL, 0xFD62F97AUL, 0x8A65C9ECUL,
	0x14015C4FUL, 0x63066CD9UL, 0xFA0F3D63UL, 0x8D080DF5UL,
	0x3B6E20C8UL, 0x4C69105EUL, 0xD56041E4UL, 0xA2677172UL,
	0x3C03E4D1UL, 0x4B04D447UL, 0xD20D85FDUL, 0xA50AB56BUL,
	0x35B5A8FAUL, 0x42B2986CUL, 0xDBBBC9D6UL, 0xACBCF940UL,
	0x32D86CE3UL, 0x45DF5C75UL, 0xDCD60DCFUL, 0xABD13D59UL,
	0x26D930ACUL, 0x51DE003AUL, 0xC8D75180UL, 0xBFD06116UL,
	0x21B4F4B5UL, 0x56B3C423UL, 0xCFBA9599UL, 0xB8BDA50FUL,
	0x2802B89EUL, 0x5F058808UL, 0xC60CD9B2UL, 0xB10BE924UL,
	0x2F6F7C87UL, 0x58684C11UL, 0xC1611DABUL, 0xB6662D3DUL,
	0x76DC4190UL, 0x01DB7106UL, 0x98D220BCUL, 0xEFD5102AUL,
	0x71B18589UL, 0x06B6B51FUL, 0x9FBFE4A5UL, 0xE8B8D433UL,
	0x7807C9A2UL, 0x0F00F934UL, 0x9609A88EUL, 0xE10E9818UL,
	0x7F6A0DBBUL, 0x086D3D2DUL, 0x91646C97UL, 0xE6635C01UL,
	0x6B6B51F4UL, 0x1C6C6162UL, 0x856530D8UL, 0xF262004EUL,
	0x6C0695EDUL, 0x1B01A57BUL, 0x8208F4C1UL, 0xF50FC457UL,
	0x65B0D9C6UL, 0x12B7E950UL, 0x8BBEB8EAUL, 0xFCB9887CUL,
	0x62DD1DDFUL, 0x15DA2D49UL, 0x8CD37CF3UL, 0xFBD44C65UL,
	0x4DB26158UL, 0x3AB551CEUL, 0xA3BC0074UL, 0xD4BB30E2UL,
	0x4ADFA541UL, 0x3DD895D7UL, 0xA4D1C46DUL, 0xD3D6F4FBUL,
	0x4369E96AUL, 0x346ED9FCUL, 0xAD678846UL, 0xDA60B8D0UL,
	0x44042D73UL, 0x33031DE5UL, 0xAA0A4C5FUL, 0xDD0D7CC9UL,
	0x5005713CUL, 0x270241AAUL, 0xBE0B1010UL, 0xC90C2086UL,
	0x5768B525UL, 0x206F85B3UL, 0xB966D409UL, 0xCE61E49FUL,
	0x5EDEF90EUL, 0x29D9C998UL, 0xB0D09822UL, 0xC7D7A8B4UL,
	0x59B33D17UL, 0x2EB40D81UL, 0xB7BD5C3BUL, 0xC0BA6CADUL,
	0xEDB88320UL, 0x9ABFB3B6UL, 0x03B6E20CUL, 0x74B1D29AUL,
	0xEAD54739UL, 0x9DD277AFUL, 0x04DB2615UL, 0x73DC1683UL,
	0xE3630B12UL, 0x94643B84UL, 0x0D6D6A3EUL, 0x7A6A5AA8UL,
	0xE40ECF0BUL, 0x9309FF9DUL, 0x0A00AE27UL, 0x7D079EB1UL,
	0xF00F9344UL, 0x8708A3D2UL, 0x1E01F268UL, 0x6906C2FEUL,
	0xF762575DUL, 0x806567CBUL, 0x196C3671UL, 0x6E6B06E7UL,
	0xFED41B76UL, 0x89D32BE0UL, 0x10DA7A5AUL, 0x67DD4ACCUL,
	0xF9B9DF6FUL, 0x8EBEEFF9UL, 0x17B7BE43UL, 0x60B08ED5UL,
	0xD6D6A3E8UL, 0xA1D1937EUL, 0x38D8C2C4UL, 0x4FDFF252UL,
	0xD1BB67F1UL, 0xA6BC5767UL, 0x3FB506DDUL, 0x48B2364BUL,
	0xD80D2BDAUL, 0xAF0A1B4CUL, 0x36034AF6UL, 0x41047A60UL,
	0xDF60EFC3UL, 0xA867DF55UL, 0x316E8EEFUL, 0x4669BE79UL,
	0xCB61B38CUL, 0xBC66831AUL, 0x256FD2A0UL, 0x5268E236UL,
	0xCC0C7795UL, 0xBB0B4703UL, 0x220216B9UL, 0x5505262FUL,
	0xC5BA3BBEUL, 0xB2BD0B28UL, 0x2BB45A92UL, 0x5CB36A04UL,
	0xC2D7FFA7UL, 0xB5D0CF31UL, 0x2CD99E8BUL, 0x5BDEAE1DUL,
	0x9B64C2B0UL, 0xEC63F226UL, 0x756AA39CUL, 0x026D930AUL,
	0x9C0906A9UL, 0xEB0E363FUL, 0x72076785UL, 0x05005713UL,
	0x95BF4A82UL, 0xE2B87A14UL, 0x7BB12BAEUL, 0x0CB61B38UL,
	0x92D28E9BUL, 0xE5D5BE0DUL, 0x7CDCEFB7UL, 0x0BDBDF21UL,
	0x86D3D2D4UL, 0xF1D4E242UL, 0x68DDB3F8UL, 0x1FDA836EUL,
	0x81BE16CDUL, 0xF6B9265BUL, 0x6FB077E1UL, 0x18B74777UL,
	0x88085AE6UL, 0xFF0F6A70UL, 0x66063BCAUL, 0x11010B5CUL,
	0x8F659EFFUL, 0xF862AE69UL, 0x616BFFD3UL, 0x166CCF45UL,
	0xA00AE278UL, 0xD70DD2EEUL, 0x4E048354UL, 0x3903B3C2UL,
	0xA7672661UL, 0xD06016F7UL, 0x4969474DUL, 0x3E6E77DBUL,
	0xAED16A4AUL, 0xD9D65ADCUL, 0x40DF0B66UL, 0x37D83BF0UL,
	0xA9BCAE53UL, 0xDEBB9EC5UL, 0x47B2CF7FUL, 0x30B5FFE9UL,
	0xBDBDF21CUL, 0xCABAC28AUL, 0x53B39330UL, 0x24B4A3A6UL,
	0xBAD03605UL, 0xCDD70693UL, 0x54DE5729UL, 0x23D967BFUL,
	0xB3667A2EUL, 0xC4614AB8UL, 0x5D681B02UL, 0x2A6F2B94UL,
	0xB40BBE37UL, 0xC30C8EA1UL, 0x5A05DF1BUL, 0x2D02EF8DUL
};

static uint32_t gzip_crc32_begin(uint32_t crc32)
{
	return crc32 ^ 0xFFFFFFFFUL;
}

static uint32_t gzip_crc32_end(uint32_t crc32)
{
	return crc32 ^ 0xFFFFFFFFUL;
}

static void gzip_crc32_putc(uint32_t *ptr, uint8_t v)
{
	*ptr = gzip_crc32[(*ptr ^ v) & 0xFFUL] ^ (*ptr >> 8UL);
}

static uint32_t gzip_crc32_to_crc16(uint32_t crc32)
{
	crc32 = gzip_crc32_end(crc32);
	return (uint16_t)(crc32 & 0xFFFFUL);
}


/*****************************************************************************
 *  decode
 *****************************************************************************/
void gzip_decode_init(struct gzip_decode *inst)
{
	struct inflate *decode;

#ifdef HYPD_DEBUG
	memset(inst, 0xAA, sizeof(struct gzip_decode));
#endif
	decode = &(inst->decode);
	inflate_init(decode);
	inst->flush = 1;
	inst->header_ftext = 0;  /* binary */
	inst->header_fhcrc = 1;  /* crc16 */
	inst->header_xfl = 3;    /* normal */
	inst->header_os = 255;   /* unknown */
	inst->header_mtime = 0;
	inst->state = GzipDecode_start;
}

static int gzip_decode_start(struct gzip_decode *inst)
{
	struct inflate *decode;

	decode = &(inst->decode);
	decode->output_callback = NULL;
	decode->output_instance = NULL;
	inst->crc32 = gzip_crc32_begin(0);
	inst->state_index = 0;
	inst->state = GzipDecode_header;

	return 0;
}

const void *gzip_decode_input(struct gzip_decode *inst,
		const void *ptr, size_t size, size_t *ret)
{
	return inflate_input(&(inst->decode), ptr, size, ret);
}

void *gzip_decode_output(struct gzip_decode *inst,
		void *ptr, size_t size, size_t *ret)
{
	return inflate_output(&(inst->decode), ptr, size, ret);
}

static int gzip_decode_readv1(struct gzip_decode *inst, uint8_t *ret)
{
	int check;
	uint8_t v;
	struct inflate *decode;

	decode = &(inst->decode);
	check = inflate_byte_getc(decode, &v);
	if (check < 0) {
		GzipError("gzip_decode_readv1 error.");
		return -1;
	}
	if (check)
		return 1;
	gzip_crc32_putc(&(inst->crc32), v);
	*ret = v;

	return 0;
}

static int gzip_decode_readvn(struct gzip_decode *inst, unsigned size, unsigned *ret)
{
	uint8_t v;
	int check;
	unsigned i, value;
	struct inflate *decode;

	/* Little endian */
	decode = &(inst->decode);
	check = inflate_byte_read_p(decode, size);
	if (check < 0) {
		GzipError("inflate_byte_read_p error.");
		return -1;
	}
	if (check)
		return 1;
	value = 0;
	for (i = 0; i < size; i++) {
		(void)inflate_byte_getc(decode, &v);
		value |= (v << (i * 8));
		gzip_crc32_putc(&(inst->crc32), v);
	}
	*ret = value;

	return 0;
}

static int gzip_decode_readv2(struct gzip_decode *inst, uint16_t *ret)
{
	int check;
	unsigned value;

	check = gzip_decode_readvn(inst, 2, &value);
	if (check)
		return check;
	*ret = (uint16_t)value;

	return 0;
}

static int gzip_decode_readv4(struct gzip_decode *inst, uint32_t *ret)
{
	int check;
	unsigned value;

	check = gzip_decode_readvn(inst, 4, &value);
	if (check)
		return check;
	*ret = (uint32_t)value;

	return 0;
}

static int gzip_decode_header(struct gzip_decode *inst)
{
	int check;
	uint8_t v;
	uint32_t v4;
	struct inflate *decode;

	decode = &(inst->decode);
	switch (inst->state_index) {
		case 0: goto header_id1;
		case 1: goto header_id2;
		case 2: goto header_cm;
		case 3: goto header_flg;
		case 4: goto header_mtime;
		case 5: goto header_xfl;
		case 6: goto header_os;
		default: break;
	}
	/* error */
	GzipError("state_index error.");
	return -1;

header_id1:
	check = gzip_decode_readv1(inst, &v);
	if (check < 0) {
		GzipError("gzip_decode_readv1 ID1 error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	if (v != 0x1F) {
		GzipError("gzip header ID1 error.");
		return -1;
	}
	inst->state_index = 1;

header_id2:
	check = gzip_decode_readv1(inst, &v);
	if (check < 0) {
		GzipError("gzip_decode_readv1 ID2 error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	if (v != 0x8B) {
		GzipError("gzip header ID2 error.");
		return -1;
	}
	inst->state_index = 2;

header_cm:
	check = gzip_decode_readv1(inst, &v);
	if (check < 0) {
		GzipError("gzip_decode_readv1 CM error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	if (v != 8) {
		GzipError("gzip header is not DEFLATE encoding.");
		return -1;
	}
	inst->state_index = 3;

header_flg:
	check = gzip_decode_readv1(inst, &v);
	if (check < 0) {
		GzipError("gzip_decode_readv1 FLG error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	inst->header_ftext    = (v >> 0) & 0x01;
	inst->header_fhcrc    = (v >> 1) & 0x01;
	inst->header_fextra   = (v >> 2) & 0x01;
	inst->header_fname    = (v >> 3) & 0x01;
	inst->header_fcomment = (v >> 4) & 0x01;
	if (v >> 5) {
		GzipError("FLG reserved error.");
		return -1;
	}
	inst->state_index = 4;

header_mtime:
	check = gzip_decode_readv4(inst, &v4);
	if (check < 0) {
		GzipError("gzip_decode_readv4 MTIME error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	inst->header_mtime = v4;
	inst->state_index = 5;

header_xfl:
	check = gzip_decode_readv1(inst, &v);
	if (check < 0) {
		GzipError("gzip_decode_readv1 XFL error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	inst->header_xfl = v;
	inst->state_index = 6;

header_os:
	check = gzip_decode_readv1(inst, &v);
	if (check < 0) {
		GzipError("gzip_decode_readv1 OS error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	inst->header_os = v;

	/* next */
	inst->state = GzipDecode_extra;
	return 0;
}

static int gzip_decode_extra_loop(struct gzip_decode *inst)
{
	uint8_t ignore;
	int check;
	unsigned size;

	/* read */
	size = inst->loop_size;
	while (inst->state_index < size) {
		check = gzip_decode_readv1(inst, &ignore);
		if (check < 0) {
			GzipError("gzip_decode_readv1 error.");
			return -1;
		}
		if (check) {
			inst->decode.input_call = 1;
			return 1;
		}
		inst->state_index++;
	}

	/* next */
	inst->state = GzipDecode_name;
	return 0;
}

static int gzip_decode_extra(struct gzip_decode *inst)
{
	int check;
	uint16_t v2;

	/* next */
	if (! inst->header_fextra) {
		inst->state = GzipDecode_name;
		return 0;
	}

	/* skip */
	check = gzip_decode_readv2(inst, &v2);
	if (check < 0) {
		GzipError("gzip_decode_readv2 error.");
		return -1;
	}
	if (check) {
		inst->decode.input_call = 1;
		return 1;
	}
	inst->loop_size = (unsigned)v2;
	inst->state_index = 0;
	inst->state = GzipDecode_extra_loop;
	return gzip_decode_extra_loop(inst);
}

static int gzip_decode_string(struct gzip_decode *inst)
{
	uint8_t v;
	int check;

	for (;;) {
		check = gzip_decode_readv1(inst, &v);
		if (check < 0) {
			GzipError("gzip_decode_readv1 error.");
			return -1;
		}
		if (check) {
			inst->decode.input_call = 1;
			return 1;
		}
		if (v == 0)
			break;
	}

	return 0;
}

static int gzip_decode_name(struct gzip_decode *inst)
{
	int check;

	/* string */
	if (inst->header_fname) {
		check = gzip_decode_string(inst);
		if (check)
			return check;
	}

	/* next */
	inst->state = GzipDecode_comment;
	return 0;
}

static int gzip_decode_comment(struct gzip_decode *inst)
{
	int check;

	/* next */
	if (inst->header_fcomment) {
		check = gzip_decode_string(inst);
		if (check)
			return check;
	}

	/* next */
	inst->state = GzipDecode_crc16;
	return 0;
}

static int gzip_decode_crc16(struct gzip_decode *inst)
{
	int check;
	uint16_t x, y;

	if (inst->header_fhcrc) {
		check = gzip_decode_readv2(inst, &x);
		if (check < 0) {
			GzipError("gzip_decode_readv2 error.");
			return -1;
		}
		if (check) {
			inst->decode.input_call = 1;
			return 1;
		}
		y = gzip_crc32_to_crc16(inst->crc32);
		if (x != y) {
			GzipError("crc16 error.");
			return -1;
		}
	}

	/* next */
	inst->state = GzipDecode_block;
	return 0;
}

static void gzip_decode_callback(void *ptr, uint8_t v)
{
	struct gzip_decode *inst;

	inst = (struct gzip_decode *)ptr;
	gzip_crc32_putc(&(inst->crc32), v);
	inst->input_size++;
}

static int gzip_decode_block(struct gzip_decode *inst)
{
	int check;
	struct inflate *decode;

	/* alignment */
	decode = &(inst->decode);
	check = inflate_alignment(decode);
	if (check)
		return check;

	/* crc32 */
	decode->output_callback = gzip_decode_callback;
	decode->output_instance = (void *)inst;
	inst->input_size = 0;
	inst->crc32 = gzip_crc32_begin(0);

	/* next */
	inst->state = GzipDecode_inflate;
	return 0;
}

static int gzip_decode_inflate(struct gzip_decode *inst)
{
	int check;
	struct inflate *decode;

	decode = &(inst->decode);
	check = inflate_execute(decode);
	if (check < 0) {
		GzipError("inflate_execute error.");
		return -1;
	}
	if (check)
		return 1;
	if (! inflate_final(decode))
		return 0;

	/* next */
	decode->output_callback = NULL;
	decode->output_instance = NULL;
	inst->state = GzipDecode_crc32;
	return 0;
}

static int gzip_decode_crc32(struct gzip_decode *inst)
{
	int check;
	uint32_t x, y;
	struct inflate *decode;

	/* input alignment */
	decode = &(inst->decode);
	if (inflate_alignment(decode)) {
		GzipError("inflate_alignment error.");
		return -1;
	}

	/* CRC32 check */
	x = gzip_crc32_end(inst->crc32);
	check = gzip_decode_readv4(inst, &y);
	if (check < 0) {
		GzipError("gzip_decode_readv4 CRC32 error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	if (x != y) {
		GzipError("crc32 error.");
		return -1;
	}

	/* next */
	inst->state = GzipDecode_size;
	return 0;
}

static int gzip_decode_size(struct gzip_decode *inst)
{
	int check;
	uint32_t x, y;

	/* CRC32 check */
	check = gzip_decode_readv4(inst, &x);
	if (check < 0) {
		GzipError("gzip_decode_readv4 ISIZE error.");
		return -1;
	}
	if (check) {
		inst->decode.input_call = 1;
		return 1;
	}
	y = (uint32_t)inst->input_size;
	if (x != y) {
		GzipError("isize error.");
		return -1;
	}

	/* next */
	inst->state = GzipDecode_flush;
	return 0;
}

static int gzip_decode_flush(struct gzip_decode *inst)
{
	struct inflate *decode;

	/* flush */
	if (inst->flush) {
		decode = &(inst->decode);
		if (decode->outputc) {
			decode->output_call = 1;
			return 1;
		}
	}

	/* next */
	inst->state = GzipDecode_final;
	return 0;
}

int gzip_decode_final(struct gzip_decode *inst)
{
	return inst->state == GzipDecode_final;
}

int gzip_decode_restart(struct gzip_decode *inst)
{
	struct inflate *decode;

	/* gzip */
	if (inst->state != GzipDecode_final) {
		GzipError("gzip_decode state error.");
		return -1;
	}

	/* inflate */
	decode = &(inst->decode);
	if (inflate_restart(decode)) {
		GzipError("inflate state error.");
		return -1;
	}

	/* restart */
	inst->state = GzipDecode_start;
	return 0;
}

static int gzip_decode_state(struct gzip_decode *inst)
{
	switch (inst->state) {
		case GzipDecode_start:
			return gzip_decode_start(inst);

		case GzipDecode_header:
			return gzip_decode_header(inst);

		case GzipDecode_extra:
			return gzip_decode_extra(inst);

		case GzipDecode_extra_loop:
			return gzip_decode_extra_loop(inst);

		case GzipDecode_name:
			return gzip_decode_name(inst);

		case GzipDecode_comment:
			return gzip_decode_comment(inst);

		case GzipDecode_crc16:
			return gzip_decode_crc16(inst);

		case GzipDecode_block:
			return gzip_decode_block(inst);

		case GzipDecode_inflate:
			return gzip_decode_inflate(inst);

		case GzipDecode_crc32:
			return gzip_decode_crc32(inst);

		case GzipDecode_size:
			return gzip_decode_size(inst);

		case GzipDecode_flush:
			return gzip_decode_flush(inst);

		case GzipDecode_final:
			return 0;

		case GzipDecode_error:
		default:
			return -1;
	}
}

int gzip_decode_execute(struct gzip_decode *inst)
{
	int check;
	struct inflate *decode;

	/* call */
	decode = &(inst->decode);
	decode->input_call = 0;
	decode->output_call = 0;

	/* state */
	check = gzip_decode_state(inst);
	if (check < 0)
		inst->state = GzipDecode_error;

	return check;
}


/*****************************************************************************
 *  encode
 *****************************************************************************/
void gzip_encode_init(struct gzip_encode *inst)
{
#ifdef HYPD_DEBUG
	memset(inst, 0xAA, sizeof(struct gzip_encode));
#endif
	deflate_init(&(inst->encode));
	inst->flush = 1;
	inst->state = GzipEncode_start;
}

static int gzip_encode_start(struct gzip_encode *inst)
{
	struct deflate *encode;

	encode = &(inst->encode);
	encode->input_callback = NULL;
	encode->input_instance = NULL;
	inst->crc32 = gzip_crc32_begin(0);
	inst->state_index = 0;
	inst->state = GzipEncode_header;

	return 0;
}

const void *gzip_encode_input(struct gzip_encode *inst,
		const void *ptr, size_t size, size_t *ret)
{
	return deflate_input(&(inst->encode), ptr, size, ret);
}

void *gzip_encode_output(struct gzip_encode *inst,
		void *ptr, size_t size, size_t *ret)
{
	return deflate_output(&(inst->encode), ptr, size, ret);
}

static int gzip_encode_writev1(struct gzip_encode *inst, uint8_t v)
{
	int check;
	struct deflate *encode;

	encode = &(inst->encode);
	check = deflate_byte_putc(encode, v);
	if (check < 0) {
		GzipError("deflate_byte_putc error.");
		return -1;
	}
	if (check)
		return 1;
	gzip_crc32_putc(&(inst->crc32), v);

	return 0;
}

static int gzip_encode_writevn(struct gzip_encode *inst, unsigned size, unsigned value)
{
	uint8_t v;
	int check;
	unsigned i;
	struct deflate *encode;

	/* Little endian */
	encode = &(inst->encode);
	check = deflate_byte_write_p(encode, size);
	if (check)
		return check;
	for (i = 0; i < size; i++) {
		v = (value >> (i * 8)) & 0xFF;
		(void)deflate_byte_putc(encode, v);
		gzip_crc32_putc(&(inst->crc32), v);
	}

	return 0;
}

static int gzip_encode_writev2(struct gzip_encode *inst, uint16_t value)
{
	return gzip_encode_writevn(inst, 2, (unsigned)value);
}

static int gzip_encode_writev4(struct gzip_encode *inst, uint32_t value)
{
	return gzip_encode_writevn(inst, 4, (unsigned)value);
}

static int gzip_encode_header(struct gzip_encode *inst)
{
	int check;
	uint8_t v;
	uint32_t v4;
	struct deflate *encode;

	encode = &(inst->encode);
	switch (inst->state_index) {
		case 0: goto header_id1;
		case 1: goto header_id2;
		case 2: goto header_cm;
		case 3: goto header_flg;
		case 4: goto header_mtime;
		case 5: goto header_xfl;
		case 6: goto header_os;
		default: break;
	}
	/* error */
	GzipError("state_index error.");
	return -1;

header_id1:
	check = gzip_encode_writev1(inst, 0x1F);
	if (check < 0) {
		GzipError("gzip_encode_writev1 ID1 error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}
	inst->state_index = 1;

header_id2:
	check = gzip_encode_writev1(inst, 0x8B);
	if (check < 0) {
		GzipError("gzip_encode_writev1 ID2 error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}
	inst->state_index = 2;

header_cm:
	check = gzip_encode_writev1(inst, 8);
	if (check < 0) {
		GzipError("gzip_encode_writev1 CM error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}
	inst->state_index = 3;

header_flg:
	v = 0;
	v |= (inst->header_ftext & 0x01) << 0;
	v |= (inst->header_fhcrc & 0x01) << 1;
	check = gzip_encode_writev1(inst, v);
	if (check < 0) {
		GzipError("gzip_encode_writev1 FLG error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}
	inst->state_index = 4;

header_mtime:
	v4 = inst->header_mtime;
	check = gzip_encode_writev4(inst, v4);
	if (check < 0) {
		GzipError("gzip_encode_writev4 MTIME error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}
	inst->state_index = 5;

header_xfl:
	v = inst->header_xfl;
	check = gzip_encode_writev1(inst, v);
	if (check < 0) {
		GzipError("gzip_encode_writev1 XFL error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}
	inst->state_index = 6;

header_os:
	v = inst->header_os;
	check = gzip_encode_writev1(inst, v);
	if (check < 0) {
		GzipError("gzip_encode_writev1 OS error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}

	/* next */
	inst->state = GzipEncode_crc16;
	return 0;
}

static int gzip_encode_crc16(struct gzip_encode *inst)
{
	int check;
	uint16_t x;

	if (inst->header_fhcrc) {
		x = gzip_crc32_to_crc16(inst->crc32);
		check = gzip_encode_writev2(inst, x);
		if (check < 0) {
			GzipError("gzip_encode_writev2 error.");
			return -1;
		}
		if (check) {
			inst->encode.output_call = 1;
			return 1;
		}
	}

	/* next */
	inst->state = GzipEncode_block;
	return 0;
}

static void gzip_encode_callback(void *ptr, uint8_t v)
{
	struct gzip_encode *inst;

	inst = (struct gzip_encode *)ptr;
	gzip_crc32_putc(&(inst->crc32), v);
	inst->output_size++;
}

static int gzip_encode_block(struct gzip_encode *inst)
{
	int check;
	struct deflate *encode;

	/* alignment */
	encode = &(inst->encode);
	check = deflate_alignment(encode);
	if (check)
		return check;

	/* crc32 */
	encode->input_callback = gzip_encode_callback;
	encode->input_instance = (void *)inst;
	inst->output_size = 0;
	inst->crc32 = gzip_crc32_begin(0);

	/* next */
	inst->state = GzipEncode_deflate;
	return 0;
}

static int gzip_encode_deflate(struct gzip_encode *inst)
{
	int check;
	struct deflate *encode;

	encode = &(inst->encode);
	check = deflate_execute(encode);
	if (check < 0) {
		GzipError("deflate_execute error.");
		return -1;
	}
	if (check)
		return 1;
	if (! deflate_final(encode))
		return 0;

	/* next */
	encode->input_callback = NULL;
	encode->input_instance = NULL;
	inst->state = GzipEncode_crc32;
	return 0;
}

static int gzip_encode_crc32(struct gzip_encode *inst)
{
	int check;
	uint32_t x;

	/* CRC32 check */
	x = gzip_crc32_end(inst->crc32);
	check = gzip_encode_writev4(inst, x);
	if (check < 0) {
		GzipError("gzip_encode_writev4 CRC32 error.");
		return -1;
	}
	if (check) {
		inst->encode.output_call = 1;
		return 1;
	}

	/* next */
	inst->state = GzipEncode_size;
	return 0;
}

static int gzip_encode_size(struct gzip_encode *inst)
{
	int check;
	uint32_t x;

	/* CRC32 check */
	x = (uint32_t)inst->output_size;
	check = gzip_encode_writev4(inst, x);
	if (check < 0) {
		GzipError("gzip_encode_writev4 ISIZE error.");
		return -1;
	}
	if (check) {
		inst->encode.output_call = 1;
		return 1;
	}

	/* next */
	inst->state = GzipEncode_flush;
	return 0;
}

static int gzip_encode_flush(struct gzip_encode *inst)
{
	struct deflate *encode;

	if (inst->flush) {
		encode = &(inst->encode);
		if (encode->outputc) {
			encode->output_call = 1;
			return 1;
		}
	}

	/* next */
	inst->state = GzipEncode_final;
	return 0;
}

int gzip_encode_state(struct gzip_encode *inst)
{
	switch (inst->state) {
		case GzipEncode_start:
			return gzip_encode_start(inst);

		case GzipEncode_header:
			return gzip_encode_header(inst);

		case GzipEncode_crc16:
			return gzip_encode_crc16(inst);

		case GzipEncode_block:
			return gzip_encode_block(inst);

		case GzipEncode_deflate:
			return gzip_encode_deflate(inst);

		case GzipEncode_crc32:
			return gzip_encode_crc32(inst);

		case GzipEncode_size:
			return gzip_encode_size(inst);

		case GzipEncode_flush:
			return gzip_encode_flush(inst);

		case GzipEncode_error:
		default:
			return -1;
	}
	return -1;
}

int gzip_encode_execute(struct gzip_encode *inst)
{
	int check;
	struct deflate *encode;

	/* call */
	encode = &(inst->encode);
	encode->input_call = 0;
	encode->output_call = 0;

	/* state */
	check = gzip_encode_state(inst);
	if (check < 0)
		inst->state = GzipEncode_error;

	return check;
}

int gzip_encode_break(struct gzip_encode *inst)
{
	return deflate_break(&(inst->encode));
}

int gzip_encode_final(struct gzip_encode *inst)
{
	return inst->state == GzipEncode_final;
}

int gzip_encode_restart(struct gzip_encode *inst)
{
	if (inst->state != GzipEncode_final) {
		GzipError("gzip state error.");
		return -1;
	}
	if (deflate_break(&(inst->encode)))
		return -1;

	/* restart */
	inst->state = GzipEncode_start;
	return 0;
}

