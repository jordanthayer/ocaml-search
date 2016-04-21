/**
 * \file test_convolve.c
 *
 *
 *
 * \author eaburns
 * \date 22-12-2009
 */

#define _POSIX_C_SOURCE 200112L

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "hist.h"
#include "hist_bins.h"
#include "hist_points.h"


static double random_double(void)
{
	int r = rand() % 100;
	return r / 10.0;
}

bool test_points_bins(void)
{
	bool ret = true;
	unsigned int i;
	double bound;
	struct histogram a, b, res;

	/*
	 * points histogram
	 */
	a.max = 100;
	a.total = 0;
	if (!points_create(&a)) {
		perror("");
		ret = false;
		goto out_a;
	}
	for (i = 0; i < 4; i += 1)
		histogram_add_mass(&a, random_double(), random_double());
	histogram_output(stdout, &a);

	/*
	 * bins histogram
	 */
	b.max = 100;
	b.total = 0;
	if (!bins_create(&b, 5, 10)) {
		perror("");
		ret = false;
		goto out_b;
	}
	for (i = 0; i < 4; i += 1)
		histogram_add_mass(&b, random_double(), random_double());
	histogram_output(stdout, &b);

	printf("Convolving with bound INFINITY\n");
	if (!histogram_convolve(&res, &a, &b, INFINITY)) {
	    ret = false;
	    goto out;
	}
	histogram_output(stdout, &res);
	bound = histogram_val_for_weight(&res, histogram_total_mass(&res) / 2);
	histogram_free(&res);

	printf("Convolving with bound %f\n", bound);
	if (!histogram_convolve(&res, &a, &b, bound)) {
	    ret = false;
	    goto out;
	}
	histogram_output(stdout, &res);
	histogram_free(&res);

	/*
	 * cleanup
	 */
out:
	histogram_free(&a);
out_b:
	histogram_free(&b);
out_a:

	return ret;
}

bool test_bins_points(void)
{
	bool ret = true;
	unsigned int i;
	double bound;
	struct histogram a, b, res;

	/*
	 * points histogram
	 */
	a.max = 100;
	a.total = 0;
	if (!points_create(&a)) {
		perror("");
		ret = false;
		goto out_a;
	}
	for (i = 0; i < 4; i += 1)
		histogram_add_mass(&a, random_double(), random_double());
	histogram_output(stdout, &a);

	/*
	 * bins histogram
	 */
	b.max = 100;
	b.total = 0;
	if (!bins_create(&b, 5, 10)) {
		perror("");
		ret = false;
		goto out_b;
	}
	for (i = 0; i < 4; i += 1)
		histogram_add_mass(&b, random_double(), random_double());
	histogram_output(stdout, &b);

	printf("Convolving with bound INFINITY\n");
	if (!histogram_convolve(&res, &b, &a, INFINITY)) {
	    ret = false;
	    goto out;
	}
	histogram_output(stdout, &res);
	bound = histogram_val_for_weight(&res, histogram_total_mass(&res) / 2);
	histogram_free(&res);

	printf("Convolving with bound %f\n", bound);
	if (!histogram_convolve(&res, &b, &a, bound)) {
	    ret = false;
	    goto out;
	}
	histogram_output(stdout, &res);
	histogram_free(&res);

	/*
	 * cleanup
	 */
out:
	histogram_free(&a);
out_b:
	histogram_free(&b);
out_a:

	return ret;
}

bool test_points_points(void)
{
	bool ret = true;
	unsigned int i;
	double bound;
	struct histogram a, b, res;

	/*
	 * points histogram
	 */
	a.max = 100;
	a.total = 0;
	if (!points_create(&a)) {
		perror("");
		ret = false;
		goto out_a;
	}
	for (i = 0; i < 4; i += 1)
		histogram_add_mass(&a, random_double(), random_double());
	histogram_output(stdout, &a);

	/*
	 * points histogram
	 */
	b.max = 100;
	b.total = 0;
	if (!points_create(&b)) {
		perror("");
		ret = false;
		goto out_b;
	}
	for (i = 0; i < 4; i += 1)
		histogram_add_mass(&b, random_double(), random_double());
	histogram_output(stdout, &b);

	printf("Convolving with bound INFINITY\n");
	if (!histogram_convolve(&res, &a, &b, INFINITY)) {
	    ret = false;
	    goto out;
	}
	histogram_output(stdout, &res);
	bound = histogram_val_for_weight(&res, histogram_total_mass(&res) / 2);
	histogram_free(&res);

	printf("Convolving with bound %f\n", bound);
	if (!histogram_convolve(&res, &a, &b, bound)) {
	    ret = false;
	    goto out;
	}
	histogram_output(stdout, &res);
	histogram_free(&res);

	/*
	 * cleanup
	 */
out:
	histogram_free(&a);
out_b:
	histogram_free(&b);
out_a:

	return ret;
}

bool test_bins_bins(void)
{
	bool ret = true;
	unsigned int i;
	double bound;
	struct histogram a, b, res;

	/*
	 * points histogram
	 */
	a.max = 100;
	a.total = 0;
	if (!bins_create(&a, 5, 10)) {
		perror("");
		ret = false;
		goto out_a;
	}
	for (i = 0; i < 4; i += 1)
		histogram_add_mass(&a, random_double(), random_double());
	histogram_output(stdout, &a);

	/*
	 * points histogram
	 */
	b.max = 100;
	b.total = 0;
	if (!bins_create(&b, 1, 4)) {
		perror("");
		ret = false;
		goto out_b;
	}
	for (i = 0; i < 4; i += 1)
		histogram_add_mass(&b, random_double(), random_double());
	histogram_output(stdout, &b);

	printf("Convolving with bound INFINITY\n");
	if (!histogram_convolve(&res, &a, &b, INFINITY)) {
	    ret = false;
	    goto out;
	}
	histogram_output(stdout, &res);
	bound = histogram_val_for_weight(&res, histogram_total_mass(&res) / 2);
	printf("Pruning above %f\n", bound);
	histogram_prune_above(&res, bound);
	histogram_output(stdout, &res);
	histogram_free(&res);

	printf("Convolving with bound %f\n", bound);
	if (!histogram_convolve(&res, &a, &b, bound)) {
	    ret = false;
	    goto out;
	}
	histogram_output(stdout, &res);
	histogram_free(&res);

	/*
	 * cleanup
	 */
out:
	histogram_free(&a);
out_b:
	histogram_free(&b);
out_a:

	return ret;
}

int main(void)
{
	srand(0);

#if !defined(NDEBUG)
	debug_mask = DEBUG_CONVOLVE;
#endif

	printf("Testing points and bins --------------------\n");
	if (!test_points_bins())
		return EXIT_FAILURE;

	printf("Testing bins and points --------------------\n");
	if (!test_bins_points())
		return EXIT_FAILURE;

	printf("\n\nTesting points and points --------------------\n");
	if (!test_points_points())
		return EXIT_FAILURE;

	printf("\n\nTesting bins and bins --------------------\n");
	if (!test_bins_bins())
		return EXIT_FAILURE;

	return EXIT_SUCCESS;
}
