/**
 * \file test_bins.c
 *
 *
 *
 * \author eaburns
 * \date 21-12-2009
 */

#define _POSIX_C_SOURCE 200112L

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "debug.h"
#include "hist.h"
#include "hist_bins.h"


/* Random number between 0 and max (exclusive on max) with a few
 * fecimal places. */
static double random_double(unsigned int max)
{
	int decimal_fact = 10000000;
	int r = rand() % (max * decimal_fact);
	return r / decimal_fact;
}

/* Make a bins histogram. */
static bool make(struct histogram *h)
{
	h->max = 100;
	if (!bins_create(h, 0, 100)) {
		perror("");
		return false;;
	}
	return true;
}

/* Copy a bins histogram. */
static bool copy(struct histogram *to, struct histogram *from)
{
	to->max = 100;
	if (!bins_copy(to, from)) {
		perror("");
		return false;
	}
	return true;
}

/* Populate a histogram with random garbage. */
static bool populate(struct histogram *h)
{
	unsigned int j;

	if (!make(h))
		return false;;
	for (j = 0; j < 100; j += 1)
		bins_add_mass(h, random_double(100), random_double(10));

	return true;
}

static double bound_for_wted_combo(struct histogram *a, struct histogram *b,
				   double factor, double desired)
{
	struct histogram accum, scaled;
	double bound;

	if (!copy(&accum, a))
		return EXIT_FAILURE;
	if (!copy(&scaled, b))
		return EXIT_FAILURE;

	bins_scale(&scaled, factor);

	if (!bins_add_bins(&accum, &scaled)) {
		perror("");
		exit(EXIT_FAILURE);
	}

	bound = bins_val_for_weight(&accum, desired);

	bins_free(&accum);
	bins_free(&scaled);

	return bound;
}

int main(void)
{
	unsigned int i;
	unsigned int times = 100;

/*
	debug_mask = DEBUG_SANITY | DEBUG_PRUNE;
*/
	srand(0);

	for (i = 0; i < times; i += 1) {
		struct histogram g, h;
		double desired, bound;

		if (!populate(&g))
			return EXIT_FAILURE;
		if (!populate(&h))
			return EXIT_FAILURE;

		desired = random_double(floor(bins_total_mass(&h)));
		bound = bound_for_wted_combo(&g, &h, 1.0, desired);
		printf("bound for wt=%f is %f\n", desired, bound);

		if (!bins_add_bins(&g, &h)) {
			perror("");
			return EXIT_FAILURE;
		}
		assert(effectively_equal(bins_weight_left_of(&g, bound),
					 desired));

		bins_prune_above(&g, bound);
		printf("pruned at %f, wt=%f\n", bound, bins_total_mass(&g));
		assert(bins_total_mass(&g) >= desired);

		if (i == times - 1)
			bins_output(stdout, &g);

		bins_free(&g);
		bins_free(&h);
	}

	return EXIT_SUCCESS;
}
