/*
 *  xorshift+
 *
 *  Xorshift RNGs
 *    George Marsaglia, The Florida State University,
 *    Journal of Statistical Software, 2003.
 *    http://www.jstatsoft.org/v08/i14/paper
 *
 *  Further scramblings of Marsaglia's xorshift generators
 *    Sebastiano Vigna, Universit`a degli Studi di Milano, Italy,
 *    arXiv:1404.0390v3 [cs.DS] 23 May 2016.
 *    https://arxiv.org/abs/1404.0390
 *    http://vigna.di.unimi.it/ftp/papers/xorshiftplus.pdf
 */
#ifndef __RANDOM_HEADER__
#define __RANDOM_HEADER__

/*
 *  32bit / 64bit
 */
#if defined(HYPD_RANDOM_32BIT)
#define RANDOM_32BIT_USE
#undef RANDOM_64BIT_USE
#else
#define RANDOM_64BIT_USE
#undef RANDOM_32BIT_USE
#endif

#ifdef RANDOM_32BIT_USE
#define random_number		random_number_32bit
#define random_equal		random_equal_32bit
#define random_less			random_less_32bit
#define float_random		float_random_32bit
#define double_random		double_random_32bit
#define long_random			long_random_32bit
#else
#define random_number		random_number_64bit
#define random_equal		random_equal_64bit
#define random_less			random_less_64bit
#define float_random		float_random_64bit
#define double_random		double_random_64bit
#define long_random			long_random_64bit
#endif


/*
 *  Linux / FreeBSD / Windows
 */
#if defined(HYPD_UNIX)
#define RANDOM_UNIX
#undef RANDOM_WINDOWS
#elif defined(HYPD_LINUX)
#define RANDOM_UNIX
#undef RANDOM_WINDOWS
#elif defined(HYPD_FREEBSD)
#define RANDOM_UNIX
#undef RANDOM_WINDOWS
#elif defined(HYPD_WINDOWS)
#undef RANDOM_UNIX
#define RANDOM_WINDOWS
#elif defined(HYPD_ANSIC)
#undef RANDOM_UNIX
#undef RANDOM_WINDOWS
#else
#undef RANDOM_UNIX
#undef RANDOM_WINDOWS
#endif

#include <stddef.h>
#include <stdint.h>

struct random_state {
	union {
		uint64_t u64[2];
		uint32_t u32[4];
	} seed;
};

/* random */
uint32_t random_number_32bit(struct random_state *state);
uint64_t random_number_64bit(struct random_state *state);
/* 0 ... value */
uint32_t random_equal_32bit(struct random_state *state, uint32_t value);
uint64_t random_equal_64bit(struct random_state *state, uint64_t value);
/* 0 ... value-1 */
uint32_t random_less_32bit(struct random_state *state, uint32_t value);
uint64_t random_less_64bit(struct random_state *state, uint64_t value);
/* seed */
void random_seed_buffer(struct random_state *state, const void *ptr, size_t size);
void random_seed_string(struct random_state *state, const char *word);
/* check */
int random_state_equal(struct random_state *a, struct random_state *b);

/* random_state */
int random_seed_randomly(struct random_state *ptr);

/* float (0 <= return < 1.0) */
float float_random_32bit(struct random_state *state);
float float_random_64bit(struct random_state *state);
double double_random_32bit(struct random_state *state);
double double_random_64bit(struct random_state *state);
long double long_random_32bit(struct random_state *state);
long double long_random_64bit(struct random_state *state);

#endif

