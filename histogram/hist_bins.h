/**
 * \file hist_bins.h
 *
 * Functions that operate on bins histograms.
 *
 * \author eaburns
 * \date 21-12-2009
 */
#if !defined(_HIST_BINS_H_)
#define _HIST_BINS_H_

#include <stdbool.h>

/**
 * Get the index of the bin for value 'vl'.
 */
static inline unsigned int bin_index(struct histogram *h, double vl)
{
	unsigned int ind = (vl - h->as.bins.base) / h->as.bins.width;
	assert(ind < h->max);
	return ind;
}

/**
 * Get the beginning value of the bin.
 */
static inline double bin_start(struct histogram *h, unsigned int bin)
{
	return h->as.bins.base + (h->as.bins.width * bin);
}


/**
 * Get the final value of the bin.
 */
static inline double bin_end(struct histogram *h, unsigned int bin)
{
	return bin_start(h, bin + 1);
}

/**
 * Get the value of the center of the bin.
 */
static inline double bin_center(struct histogram *h, unsigned int bin)
{
	return bin_start(h, bin) + (h->as.bins.width / 2);
}

/**
 * Makes a new bins histogram.  'min' and 'max' are the minimum and
 * maximum values that will be contained in the histogram.  The base
 * and widths are setup so there are one quarter of the bins on the
 * left side of the bin centered on min and one quarter of the bins to
 * the rigth side of the bin centered on max.
 *
 * This assumes that the max field of the histogram is properly filled
 * out.
 *
 * Returns true on success and false on failure.
 */
bool bins_create(struct histogram *h, double min, double max);

/**
 * Frees the memory associated with the bins histogram.
 */
void bins_free(struct histogram *h);

/**
 * Copy a bins histogram.
 *
 * Returns true on success and false on failure.
 */
bool bins_copy(struct histogram *to, struct histogram *from);

/**
 * Add mass to a bins histogram.
 */
bool bins_add_mass(struct histogram *h, double vl, double wt);

/**
 * Prunes all of the weight above the given bound.  If 'equal' is true
 * then the weight equal to the bound is pruned too.
 *
 * Returns the amount of weight that was pruned.
 */
double bins_prune_above(struct histogram *h, double bound);

/**
 * Get the total amount of mass in the histogram.
 */
double bins_total_mass(struct histogram *h);

/**
 * Get the minimum value in the bins.
 */
static inline double bins_start(struct histogram *h)
{
	return bin_start(h, 0);
}

/**
 * Get the maximum value in the bins.
 */
static inline double bins_end(struct histogram *h)
{
	return bin_end(h, h->max - 1);
}

/**
 * Get the minimum value in the histogram.
 */
double bins_min_value(struct histogram *h);

/**
 * Get the maximum value in the histogram.
 */
double bins_max_value(struct histogram *h);

/**
 * Test if the histogram is empty.
 */
bool bins_is_empty(struct histogram *h);

/**
 * Adds weight to the histogram between 'min' and 'max'.
 */
void bins_sprinkle_mass(struct histogram *h, double min,
			double max, double wt);

/**
 * Adds together two bins histograms.
 */
bool bins_add_bins(struct histogram *a, struct histogram *b);

/**
 * Scale the weights.
 */
void bins_scale(struct histogram *h, double fact);

/**
 * Find the weight to the left of the given value.
 */
double bins_weight_left_of(struct histogram *h, double vl);

/**
 * Find the bound that will have the given weight to its left.
 */
double bins_val_for_weight(struct histogram *h, double wt);

/**
 * Print the histogram to the given FILE.
 */
void bins_output(FILE* f, struct histogram *h);

#endif	/* !_HIST_BINS_H_ */
