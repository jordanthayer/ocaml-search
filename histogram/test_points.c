/**
 * \file test.c
 *
 *
 *
 * \author eaburns
 * \date 20-12-2009
 */

#define _POSIX_C_SOURCE 200112L

#include <stdio.h>
#include <stdlib.h>

#include "hist.h"
#include "hist_points.h"


static double random_double(void)
{
	int r = rand() % 100;
	return r / 10.0;
}

int main(void)
{
	unsigned int i;
	struct histogram g, h;

	srand(0);

	g.max = h.max = 100;
	if (!points_create(&h)) {
		perror("");
		return EXIT_FAILURE;
	}

	for (i = 0; i < 50; i += 1) {
		double vl, wt;

		vl = random_double();
		wt = random_double();
		if (!points_add_mass(&h, vl, wt)) {
			perror("");
			return EXIT_FAILURE;
		}
	}

	points_output(stdout, &h);
	i = points_max_chain_length(&h);
	printf("max chain length=%u\n", i);


	struct histogram_pt **ary = points_sorted_array(&h, true);
	printf("Got an array copy %p\n", ary);
	if (!ary) {
		perror("");
		return EXIT_FAILURE;
	}
	for (i = 0; i < 20; i += 1) {
		struct histogram_pt *p = ary[i];
		printf("point %p\n", p);
		printf("removing value %f\n", p->vl);
		if (!points_add_mass(&h, p->vl, -p->wt)) {
			perror("");
			return EXIT_FAILURE;
		}
	}
	free(ary);
	points_output(stdout, &h);

	points_prune_above(&h, 7.0, false);
	points_output(stdout, &h);

	if (!points_add_mass(&h, 9.8, 1.0)) {
		perror("");
		return EXIT_FAILURE;
	}
	points_output(stdout, &h);

	printf("Copying\n");
	points_copy(&g, &h);
	points_output(stdout, &g);

	printf("Scaling by 2\n");
	points_scale(&g, 2);
	points_output(stdout, &g);

	printf("Finding bounds by 2\n");
	points_val_for_weight(&h, points_weight_left_of(&h, 6.9, true), true);
	points_val_for_weight(&h, points_weight_left_of(&h, 6.9, false), false);
	points_free(&h);

	return EXIT_SUCCESS;
}
