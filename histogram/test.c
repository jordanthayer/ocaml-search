/**
 * \file test.c
 *
 *
 *
 * \author eaburns
 * \date 21-12-2009
 */

#define _POSIX_C_SOURCE 200112L

#include <stdio.h>
#include <stdlib.h>

#include "hist.h"
#include "hist_bins.h"


static double random_double(void)
{
	int r = rand() % 10000;
	return r / 10.0;
}

int main(void)
{
	unsigned int i;
	struct histogram g, h;

	srand(0);

	histogram_create(&h, 10);
	histogram_create(&g, 50);

	if (!histogram_add_mass(&h, 1., random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 2, random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 3, random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 4, random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 5, random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 6, random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 7, random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 8, random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 9, random_double()))
		return EXIT_FAILURE;
	if (!histogram_add_mass(&h, 10, random_double()))
		return EXIT_FAILURE;

	for (i = 0; i < 1000; i += 1)
		if (!histogram_add_mass(&h, random_double(), random_double()))
			return EXIT_FAILURE;

	printf("Added %f total mass in h\n", histogram_total_mass(&h));

	histogram_prune_above(&h, 5.0);

	printf("Mass after pruning h above 5.0 is: %f\n",
	       histogram_total_mass(&h));

	for (i = 0; i < 1000; i += 1)
		if (!histogram_add_mass(&g, random_double(), random_double()))
			return EXIT_FAILURE;

	printf("Added %f total mass in g\n", histogram_total_mass(&g));

	if (!histogram_add(&g, &h))
		return EXIT_FAILURE;

	printf("%f total mass after adding both g and h\n",
	       histogram_total_mass(&g));

	histogram_free(&g);

	histogram_copy(&g, &h);
	printf("Scaling by 2\n");
	histogram_scale(&g, 2);

	histogram_free(&g);
	histogram_free(&h);

	return EXIT_SUCCESS;
}
