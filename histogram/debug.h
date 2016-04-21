/**
 * \file debug.h
 *
 *
 *
 * \author eaburns
 * \date 20-12-2009
 */
#if !defined(_DEBUG_H_)
#define _DEBUG_H_

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define NOT_REACHABLE()						\
	do {							\
		fprintf(stderr, "Should not reach here\n");	\
		assert(false);					\
		exit(EXIT_FAILURE);				\
	} while (0)


#define NOT_IMPLEMENTED()					\
	do {							\
		fprintf(stderr, "Not implemented\n");		\
		assert(false);					\
		exit(EXIT_FAILURE);				\
	} while (0)

#if !defined(NDEBUG)

/**
 * A simple floating point equality comparison.  Only to be used for
 * debugging.
 */
static inline bool effectively_equal(double a, double b)
{
	return ((a - 0.000001) < b) && ((a + 0.000001) > b);
}

#define DEBUG_DEFAULT_MASK 0
#define DEBUG_POINTS     1<<0
#define DEBUG_BINS       1<<1
#define DEBUG_CONVERT    1<<2
#define DEBUG_ADD        1<<3
#define DEBUG_CONVOLVE   1<<4
#define DEBUG_STUBS      1<<5
#define DEBUG_COPY       1<<6
#define DEBUG_BOUNDS     1<<7
#define DEBUG_PRUNE      1<<8

/* Enable, possibly expensive, sanity checks. */
#define DEBUG_SANITY     1<<9

/* Perform a sanity check (if it is enabled).  These are like asserts
 * but they may be a bit more expensive so we want to have the ability
 * to turn them off. */
#define SANITY_CHECK(x)				\
	do {					\
		if (DEBUG_SANITY & debug_mask)	\
			assert(x);		\
	} while (0)


/* The debug mask. */
extern unsigned long debug_mask;

#define DPRINTF(bit, ...)					\
	do {							\
		if ((bit) & debug_mask) {			\
			fprintf(stdout, "%s: ",__func__);	\
			fprintf(stdout, __VA_ARGS__);		\
			fflush(stdout);				\
		}						\
	} while (0)

#else

#define DPRINTF(bit, ...)

#define SANITY_CHECK(x)

#endif


#endif /* !_DEBUG_H_ */
