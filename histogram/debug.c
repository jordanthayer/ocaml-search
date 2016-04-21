/**
 * \file debug.c
 *
 *
 *
 * \author eaburns
 * \date 20-12-2009
 */

#define _POSIX_C_SOURCE 200112L
#include "debug.h"

#if !defined(NDEBUG)

unsigned long debug_mask = DEBUG_DEFAULT_MASK;

#endif	/* !NDEBUG */

