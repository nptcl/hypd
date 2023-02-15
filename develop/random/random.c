#include "md5encode.h"
#include "random.h"
#include <inttypes.h>
#include <locale.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

/*****************************************************************************
 *  xorshift+
 *****************************************************************************/
#define RANDOM_MASK32BIT 0xFFFFFFFFUL
#define RANDOM_MASK64BIT 0xFFFFFFFFFFFFFFFFULL

static uint32_t xorshift128_32bit(uint32_t *x, uint32_t *y, uint32_t *z, uint32_t *w)
{
	/*
	 *  Xorshift RNGs
	 *  George Marsaglia, The Florida State University,
	 *  Journal of Statistical Software, 2003.
	 *  http://www.jstatsoft.org/v08/i14/paper
	 */
	uint32_t v;

	v = (*x ^ (*x << 11/*a*/));
	*x = *y;
	*y = *z;
	*z = *w;
	*w = (v ^ (v >> 8/*b*/)) ^ (*w ^ (*w >> 19/*c*/));

	return *w;
}

static uint64_t xorshift128plus_64bit(uint64_t *s0, uint64_t *s1)
{
	/*
	 *  Further scramblings of Marsaglia's xorshift generators
	 *  Sebastiano Vigna, Universit`a degli Studi di Milano, Italy,
	 *  arXiv:1404.0390v3 [cs.DS] 23 May 2016.
	 *  https://arxiv.org/abs/1404.0390
	 *  http://vigna.di.unimi.it/ftp/papers/xorshiftplus.pdf
	 *
	 *  Table I.
	 *  a23-b17-c26: s34-r30+64-w61 (failures)
	 *  a23-b18-c5 : s38-r20+70-w65 (weight)
	 */
	uint64_t x, y, z;

	x = *s0;
	y = *s1;
	z = x + y;
	*s0 = y;
	x ^= x << 23/*a*/;
	*s1 = x ^ y ^ (x >> 18/*b*/) ^ (y >> 5/*c*/);

	return z;
}

/* random */
uint32_t random_number_32bit(struct random_state *state)
{
	return xorshift128_32bit(
			&state->seed.u32[0], &state->seed.u32[1],
			&state->seed.u32[2], &state->seed.u32[3]);
}

uint64_t random_number_64bit(struct random_state *state)
{
	return xorshift128plus_64bit(&state->seed.u64[0], &state->seed.u64[1]);
}

/* 0 ... value */
uint32_t random_equal_32bit(struct random_state *state, uint32_t value)
{
	int shift;
	uint32_t check, result;

	/* shift */
	if (value == 0UL) return 0UL;
	check = (value >> 1UL);
	for (shift = 1; check; shift++)
		check >>= 1UL;

	/* generate */
	check = (32 <= shift)? RANDOM_MASK32BIT: (1UL << shift) - 1UL;
	do {
		result = check & random_number_32bit(state);
	} while (value < result);

	return result;
}

uint64_t random_equal_64bit(struct random_state *state, uint64_t value)
{
	int shift;
	uint64_t check, result;

	/* shift */
	if (value == 0ULL) return 0ULL;
	check = (value >> 1ULL);
	for (shift = 1; check; shift++)
		check >>= 1ULL;

	/* generate */
	check = (64 <= shift)? RANDOM_MASK64BIT: (1ULL << shift) - 1ULL;
	do {
		result = check & random_number_64bit(state);
	} while (value < result);

	return result;
}

/* 0 ... value-1 */
uint32_t random_less_32bit(struct random_state *state, uint32_t value)
{
	if (value <= 1UL) return 0UL;
	return random_equal_32bit(state, value - 1UL);
}

uint64_t random_less_64bit(struct random_state *state, uint64_t value)
{
	if (value <= 1UL) return 0UL;
	return random_equal_64bit(state, value - 1UL);
}

/* seed */
void random_seed_buffer(struct random_state *state, const void *ptr, size_t size)
{
	sequence_md5encode(ptr, size, state->seed.u32);
}

void random_seed_string(struct random_state *state, const char *word)
{
	sequence_md5encode(word, strlen(word), state->seed.u32);
}

/* check */
int random_state_equal(struct random_state *a, struct random_state *b)
{
	return memcmp(&(a->seed.u32), &(b->seed.u32),
			(size_t)(sizeof(uint32_t) * 4)) == 0;
}


/*****************************************************************************
 *  random state
 *****************************************************************************/
#define zeroset_random(p,s) memset((void *)(p), 0, (size_t)(s))
#define readmd5_random(m,p,s) read_md5encode((m), (const void *)(p), (size_t)(s))

#define RANDOM_DEVICE			"/dev/urandom"
#define RANDOM_DEVICE_SIZE		256

static volatile int RandomState_Counter = 0;

/****************************************
 *  Unix
 ****************************************/
#if defined(RANDOM_UNIX)
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>

/* hostname */
static size_t gethostname_random(char *ptr, size_t size)
{
	int result;

	result = gethostname(ptr, size);
	if (result < 0) {
		size--;
		ptr[size] = '\0';
	}
	else {
		size = strlen(ptr);
	}

	return size;
}

#define RANDOM_GETHOSTNAME_SIZE 256
static void seed_hostname_random(struct md5encode *md5)
{
	volatile char buffer[RANDOM_GETHOSTNAME_SIZE];
	size_t size;

	size = gethostname_random((char *)buffer, RANDOM_GETHOSTNAME_SIZE);
	readmd5_random(md5, buffer, size);
	zeroset_random(buffer, RANDOM_GETHOSTNAME_SIZE);
}

static int readf_random(int file, void *ptr, size_t size, size_t *ret)
{
	ssize_t check;
	size_t count, rsize, diff;
	unsigned char *pos;

	pos = (unsigned char *)ptr;
	for (count = 0; count < size; count += rsize) {
		diff = size - count;
retry:
		check = read(file, (void *)pos, diff);
		/* Error */
		if (check < 0) {
			if (errno == EINTR)
				goto retry;
			*ret = count;
			return -1;
		}
		/* EOF */
		if (check == 0) {
			*ret = count;
			return 1;
		}
		/* Next */
		rsize = (size_t)check;
		pos += rsize;
	}
	*ret = count;
	return 0;
}

static int read_urandom_random(struct md5encode *md5)
{
	volatile unsigned char buffer[RANDOM_DEVICE_SIZE];
	int file, check;
	size_t size;

retry:
	file = open(RANDOM_DEVICE, O_RDONLY | O_NONBLOCK);
	if (file < 0) {
		if (errno == EINTR)
			goto retry;
		fprintf(stderr, "file " RANDOM_DEVICE " is not exist.");
		return 1;
	}
	check = readf_random(file, (void *)buffer, RANDOM_DEVICE_SIZE, &size);
	if (check) {
		close(file);
		fprintf(stderr, "read error");
		return -1;
	}
	readmd5_random(md5, buffer, size);
	zeroset_random(buffer, RANDOM_DEVICE_SIZE);
	if (close(file) < 0) {
		fprintf(stderr, "close error");
		return -1;
	}

	return 0;
}

static int seed_os_random(struct md5encode *md5)
{
	volatile pid_t pid;
	volatile struct timeval now;

	/* hostname */
	seed_hostname_random(md5);
	/* time */
	gettimeofday((struct timeval *)&now, NULL);
	readmd5_random(md5, &now, sizeof(now));
	zeroset_random(&now, sizeof(now));
	/* process id */
	pid = getpid();
	readmd5_random(md5, &pid, sizeof(pid));
	zeroset_random(&pid, sizeof(pid));
	/* read /dev/urandom */
	return read_urandom_random(md5);
}


/****************************************
 *  Windows
 ****************************************/
#elif defined(RANDOM_WINDOWS)
#include <windows.h>
#include <ntsecapi.h>

#define RANDOM_GETHOSTNAME_SIZE 256
#define gethostwin_random(p,s) \
	GetComputerNameExA(ComputerNameDnsFullyQualified,(p),(s))
static int seed_hostname_random(struct md5encode *md5)
{
	volatile char buffer[RANDOM_GETHOSTNAME_SIZE];
	BOOL result;
	DWORD size;
	char *ptr;

	size = RANDOM_GETHOSTNAME_SIZE;
	result = gethostwin_random((LPSTR)buffer, &size);
	if (result == 0) {
		ptr = (char *)malloc(size + 1UL);
		if (ptr == NULL)
			return 1;
		result = gethostwin_random(ptr, &size);
		readmd5_random(md5, ptr, size);
		zeroset_random(buffer, RANDOM_GETHOSTNAME_SIZE);
		free(ptr);
		if (result == 0) {
			fprintf(stderr, "GetComputerName error");
			return 1;
		}
	}
	else {
		ptr = (char *)buffer;
		readmd5_random(md5, ptr, size);
		zeroset_random(buffer, RANDOM_GETHOSTNAME_SIZE);
	}

	return 0;
}

#define RANDOM_RTLGENRANDOM_SIZE 256
static BOOLEAN rtlgenrandom(PVOID buffer, ULONG length)
{
	typedef BOOLEAN (WINAPI *apicalltype)(PVOID, ULONG);
	HMODULE hModule;
	BOOLEAN result;
	apicalltype call;

	hModule = LoadLibraryA("Advapi32.dll");
	if (hModule == NULL) {
		fprintf(stderr, "LoadLibrary Advapi32 error");
		return FALSE;
	}
	call = (apicalltype)GetProcAddress(hModule, "SystemFunction036");
	if (call == NULL) {
		fprintf(stderr, "GetProcAddress SystemFunction036 error");
		FreeLibrary(hModule);
		return FALSE;
	}
	result = (*call)(buffer, length);
	FreeLibrary(hModule);

	return result;
}

static int read_windows_random(struct md5encode *md5)
{
	volatile unsigned char buffer[RANDOM_RTLGENRANDOM_SIZE];
	BOOLEAN result;

	result = rtlgenrandom((PVOID)buffer, RANDOM_RTLGENRANDOM_SIZE);
	if (result == FALSE) {
		fprintf(stderr, "RtlGenRandom error");
		return 1;
	}
	readmd5_random(md5, buffer, RANDOM_RTLGENRANDOM_SIZE);
	SecureZeroMemory((PVOID)buffer, RANDOM_RTLGENRANDOM_SIZE);

	return 0;
}

static int seed_os_random(struct md5encode *md5)
{
	volatile DWORD value;
	volatile SYSTEMTIME time;

	/* hostname */
	if (seed_hostname_random(md5))
		return 1;
	/* time */
	GetSystemTime((LPSYSTEMTIME)&time);
	readmd5_random(md5, &time, sizeof(time));
	zeroset_random(&time, sizeof(time));
	/* process id */
	value = GetCurrentProcessId();
	readmd5_random(md5, &value, sizeof(value));
	value = 0;
	/* thread id */
	value = GetCurrentThreadId();
	readmd5_random(md5, &value, sizeof(value));
	value = 0;
	/* RtlGenRandom */
	return read_windows_random(md5);
}


/****************************************
 *  Others
 ****************************************/
#else
#include <time.h>

static int read_urandom_random(struct md5encode *md5)
{
	volatile unsigned char buffer[RANDOM_DEVICE_SIZE];
	FILE *file;
	size_t size;

#ifdef _MSC_VER
	__pragma(warning(push)) __pragma(warning(disable:4996));
#endif
	file = fopen(RANDOM_DEVICE, "rb");
#ifdef _MSC_VER
	__pragma(warning(pop));
#endif
	if (file == NULL) {
		/* Device is not exist, OK. */
		return 1;
	}
	size = fread((void *)buffer, RANDOM_DEVICE_SIZE, 1, file);
	if (size == 0) {
		fclose(file);
		fprintf(stderr, "fread error");
		return -1;
	}
	readmd5_random(md5, buffer, size);
	zeroset_random(buffer, RANDOM_DEVICE_SIZE);
	if (fclose(file) < 0) {
		fprintf(stderr, "fclose error");
		return -1;
	}

	return 0;
}

static int seed_os_random(struct md5encode *md5)
{
	volatile time_t now;

	/* time */
	time((time_t *)&now);
	readmd5_random(md5, &now, sizeof(now));
	zeroset_random(&now, sizeof(now));
	/* read /dev/urandom */
	return read_urandom_random(md5) < 0;
}
#endif


/*
 *  interface
 */
static int make_seed_random(struct md5encode *md5)
{
	volatile const void *ptr;
	int (*call)(struct md5encode *);

	/* environment */
	if (seed_os_random(md5))
		return 1;
	/* counter */
	RandomState_Counter++;
	readmd5_random(md5, &RandomState_Counter, sizeof(RandomState_Counter));
	/* function pointer */
	call = make_seed_random;  /* Own function address */
	memcpy((void *)&ptr, (const void *)&call, sizeof(ptr));
	readmd5_random(md5, &ptr, sizeof(ptr));
	ptr = NULL;

	return 0;
}

int random_seed_randomly(struct random_state *ptr)
{
	volatile uint8_t result[MD5ENCODE_SIZE];
	struct md5encode md5;

	clear_md5encode(&md5);
	if (make_seed_random(&md5))
		return 1;
	calc_md5encode(&md5, (void *)result);
	clear_md5encode(&md5);
	memcpy(ptr, (const void *)result, sizeof(result));
	zeroset_random(result, MD5ENCODE_SIZE);

	return 0;
}


/*****************************************************************************
 *  random float
 *****************************************************************************/
/*
 *  locale
 */
const char *setlocale_c_random(int category)
{
	const char *ptr;

	ptr = setlocale(category, NULL);
	if (ptr && (ptr[0] != 'C' || ptr[1] != 0)) {
		return setlocale(category, "C");
	}

	return NULL;
}

static int sscanc_random(const char *buffer, const char *fmt, ...)
{
	int result;
	const char *check;
	va_list args;

	/* setlocale C */
	check = setlocale_c_random(LC_NUMERIC);

	/* sscanf */
	va_start(args, fmt);
#ifdef _MSC_VER
	result = vsscanf_s(buffer, fmt, args);
#else
	result = vsscanf(buffer, fmt, args);
#endif
	va_end(args);

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	return result;
}


/*
 *  random float
 */
static char *strfloat_front_random(char *ptr, const char *data)
{
	for (;;) {
		*ptr = *data;
		if (*ptr == 0)
			break;
		data++;
		ptr++;
	}

	return ptr;
}

static char *strfloat_start_random(char *ptr)
{
	return strfloat_front_random(ptr, "0x0.");
}

static void strfloat_end_random(char *ptr)
{
	strfloat_front_random(ptr, "p0");
}


/*
 *  32bit float
 */
#define RANDOM_PRINTF32		8

static char *strfloat_random_32bit(char *ptr, uint32_t value)
{
	char buffer[32];
	snprintf(buffer, 32, "%08" PRIX32, value);
	return strfloat_front_random(ptr, buffer);
}


/* [32bit] (23+1) * 2 = 48 -> 64 -> 2 times */
#define RANDOM_FLOAT32_TIMES	2
#define RANDOM_FLOAT32_BUFFER	(RANDOM_PRINTF32*RANDOM_FLOAT32_TIMES)
#define RANDOM_FLOAT32_DATA		(4+2+1 + RANDOM_FLOAT32_BUFFER)
float float_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	char data[RANDOM_FLOAT32_DATA];
	char *ptr;
	unsigned i;
	uint32_t bit;
	float value;

	ptr = strfloat_start_random(data);
	for (i = 0; i < RANDOM_FLOAT32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_random_32bit(ptr, bit);
	}
	strfloat_end_random(ptr);
	sscanc_random(data, "%A", &value);

	return value;
}


/* [32bit] (52+1) * 2 = 106 -> 128 -> 4 times */
#define RANDOM_DOUBLE32_TIMES	4
#define RANDOM_DOUBLE32_BUFFER	(RANDOM_PRINTF32*RANDOM_DOUBLE32_TIMES)
#define RANDOM_DOUBLE32_DATA	(4+2+1 + RANDOM_DOUBLE32_BUFFER)
double double_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_DOUBLE32_DATA];
	char *ptr;
	uint32_t bit;
	double value;

	ptr = strfloat_start_random(data);
	for (i = 0; i < RANDOM_DOUBLE32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_random_32bit(ptr, bit);
	}
	strfloat_end_random(ptr);
	sscanc_random(data, "%lA", &value);

	return value;
}


/*
 *  IEEE754 binary128
 *    (112+1) * 2 = 226 -> 256 -> 8 times
 *  Intel long-double
 *     (64+0) * 2 = 128 -> 128 -> 4 times
 */
#define RANDOM_LONG32_TIMES		8
#define RANDOM_LONG32_BUFFER	(RANDOM_PRINTF32*RANDOM_LONG32_TIMES)
#define RANDOM_LONG32_DATA		(4+2+1 + RANDOM_LONG32_BUFFER)
long double long_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_LONG32_DATA];
	char *ptr;
	uint32_t bit;
	long double value;

	ptr = strfloat_start_random(data);
	for (i = 0; i < RANDOM_LONG32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_random_32bit(ptr, bit);
	}
	strfloat_end_random(ptr);
	sscanc_random(data, "%LA", &value);

	return value;
}


/*
 *  64bit float
 */
#define RANDOM_PRINTF64		16

static char *strfloat_random_64bit(char *ptr, uint64_t value)
{
	char buffer[32];
	snprintf(buffer, 32, "%016" PRIX64, value);
	return strfloat_front_random(ptr, buffer);
}


/* [64bit] (23+1) * 2 = 48 -> 64 -> 1 times */
#define RANDOM_FLOAT64_TIMES	1
#define RANDOM_FLOAT64_BUFFER	(RANDOM_PRINTF64*RANDOM_FLOAT64_TIMES)
#define RANDOM_FLOAT64_DATA		(4+2+1 + RANDOM_FLOAT64_BUFFER)
float float_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	char data[RANDOM_FLOAT64_DATA];
	char *ptr;
	unsigned i;
	uint64_t bit;
	float value;

	ptr = strfloat_start_random(data);
	for (i = 0; i < RANDOM_FLOAT64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_random_64bit(ptr, bit);
	}
	strfloat_end_random(ptr);
	sscanc_random(data, "%A", &value);

	return value;
}


/* [64bit] (52+1) * 2 = 106 -> 128 -> 2 times */
#define RANDOM_DOUBLE64_TIMES	2
#define RANDOM_DOUBLE64_BUFFER	(RANDOM_PRINTF64*RANDOM_DOUBLE64_TIMES)
#define RANDOM_DOUBLE64_DATA	(4+2+1 + RANDOM_DOUBLE64_BUFFER)
double double_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_DOUBLE64_DATA];
	char *ptr;
	uint64_t bit;
	double value;

	ptr = strfloat_start_random(data);
	for (i = 0; i < RANDOM_DOUBLE64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_random_64bit(ptr, bit);
	}
	strfloat_end_random(ptr);
	sscanc_random(data, "%lA", &value);

	return value;
}


/*
 *  IEEE754 binary128
 *    (112+1) * 2 = 226 -> 256 -> 4 times
 *  Intel long-double
 *     (64+0) * 2 = 128 -> 128 -> 2 times
 */
#define RANDOM_LONG64_TIMES		4
#define RANDOM_LONG64_BUFFER	(RANDOM_PRINTF64*RANDOM_LONG64_TIMES)
#define RANDOM_LONG64_DATA		(4+2+1 + RANDOM_LONG64_BUFFER)
long double long_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_LONG64_DATA];
	char *ptr;
	uint64_t bit;
	long double value;

	ptr = strfloat_start_random(data);
	for (i = 0; i < RANDOM_LONG64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_random_64bit(ptr, bit);
	}
	strfloat_end_random(ptr);
	sscanc_random(data, "%LA", &value);

	return value;
}

