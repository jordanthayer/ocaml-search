/**
 * \file verb.h
 *
 * Various utilities for printing.
 *
 * \author eaburns
 * \date 27-01-2010
 */

#if !defined(_VERB_H_)
#define _VERB_H_

#define _POSIX_C_SOURCE 200112L

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


#define NOT_IMPLEMENTED()						\
	do {								\
		fprintf(stderr, "%s: Not implemented\n", __func__);	\
		assert(false);						\
		exit(EXIT_FAILURE);					\
	} while (0)


/* The current verbosity level. */
unsigned int verbosity;

/* Verbosity levels. */
enum {
	/* Always printed. */
	V_ALWAYS,

	/* The default level print. */
	V_OFTEN,

	/* Optional infornation for non-debug messages. */
	V_OPTIONAL,

	/* Debugging information (gets printed with function name) and
	 * forces a flush of the output stream. */
	V_DEBUG,
};

/* Print something if the verbosity level is sufficient. */
#define VPRINTF_LVL(lvl, ...)						\
	do {								\
		if (verbosity >= lvl) {					\
			if (lvl >= verbosity)				\
				fprintf(stdout, "%s: ",__func__);	\
			fprintf(stdout, __VA_ARGS__);			\
			if (lvl >= verbosity)				\
				fflush(stdout);				\
		}							\
	} while (0)

/* Print something at the default level. */
#define VPRINTF(...) VPRINTF_LVL(V_OFTEN, __VA_ARGS__)

/* Print optional information. */
#define OPRINTF(...) VPRINTF_LVL(V_OPTIONAL, __VA_ARGS__)

/* Print debugging information. */
#define DPRINTF(...) VPRINTF_LVL(V_DEBUG, __VA_ARGS__)


#endif /* !_VERB_H_ */

