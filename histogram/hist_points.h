/**
 * \file hist_points.h
 *
 *
 *
 * \author eaburns
 * \date 20-12-2009
 */
#if !defined(_HIST_POINTS_H_)
#define _HIST_POINTS_H_

#include "hist.h"

/**
 * Makes a new histogram that is a hash table of the various points.
 *
 * This assumes that the max field of the histogram is properly filled
 * out.
 *
 * Returns true on success and false on failure.
 */
bool points_create(struct histogram *h);

/**
 * Free the memory allocated for a points histogram.
 */
void points_free(struct histogram *h);

/**
 * Copy the data for a points histogram.
 *
 * Assumes that the 'max' field of the 'to' histogram is properly set.
 */
bool points_copy(struct histogram *to, struct histogram *from);

/**
 * Add a mass to the points histogram.  Returns true on success and
 * false on error.
 */
bool points_add_mass(struct histogram *h, double vl, double wt);

/**
 * Get an array of the historgam points which is (optionally) sorted
 * on ascending values.
 *
 * If the histogram 'h' is not going to be modified during the time
 * that this array is used then pass false for copy to get the array
 * directly from the histogram.
 *
 * If the histogram 'h' might be modified then pass true to the
 * argument copy to get a copy of this array that must be freed by the
 * caller.
 */
struct histogram_pt **points_array(struct histogram *h,
				   bool sorted, bool copy);

/**
 (* Get an array of the historgam points which is sorted
 * on ascending values.
 *
 * See notes on the 'points_array' function for more usage info.
 */
static inline struct histogram_pt **points_sorted_array(struct histogram *h,
							bool copy)
{
	return points_array(h, true, copy);
}

/**
 * Prunes all of the weight above the given bound.  If 'equal' is true
 * than weight equal to the bound is pruned too.
 *
 * Returns the amount of weight that was pruned.
 */
double points_prune_above(struct histogram *h, double bound, bool equal);

/**
 * Test if the histogram is full.
 */
static inline bool points_is_full(const struct histogram *h)
{
	return h->max == h->as.points.fill;
}

/**
 * Get the total mass in the histograms.
 */
double points_total_mass(struct histogram *h);

/**
 * Test if the histogram is empty.
 */
static inline bool points_is_empty(const struct histogram *h)
{
	return h->as.points.fill == 0;
}

/**
 * Get the minimum value in the points histogram.
 */
double points_min_value(struct histogram *h);

/**
 * Get the maximum value in the points histogram.
 */
double points_max_value(struct histogram *h);

/**
 * Output the points to the given file.
 */
void points_output(FILE *out, struct histogram *h);

/**
 * Get the maximum length of a chain in the points hash table.  This
 * is really just for debugging that the hash function is getting a
 * descent spread.
 */
unsigned int points_max_chain_length(struct histogram *h);

/**
 * Scale the histogram by a given factor.
 */
void points_scale(struct histogram *h, double fact);

/**
 * Get the amount of weight to the left of the given value.
 *
 * If equal is true than the weight for the value is accounted too.
 */
double points_weight_left_of(struct histogram *h, double vl, bool equal);

/**
 * Find the bound that gives (close to) the desired weight.
 *
 * If equal is true then the weight of the bound is included.  If
 * equal is false than only the weight to the left is included.
 */
double points_val_for_weight(struct histogram *h, double wt, bool equal);

#endif /* !_HIST_POINTS_H_ */
