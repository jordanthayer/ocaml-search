/**
 * \file common.c
 *
 * Common routines used in any of the stubs
 *
 * \author eaburns
 * \date 27-01-2010
 */

#define _POSIX_C_SOURCE 200112L

#include <libplayerc/playerc.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "common.h"
#include "verb.h"

static value *playerc_error = NULL;

void exception_playerc_error(void)
{
	const char *error_string;

	error_string = playerc_error_str();

	if (!playerc_error)
		playerc_error = caml_named_value("exception Playerc_error");

	caml_raise_with_string(*playerc_error, error_string);
}

int get_access_mode(int variant_num)
{
	if (variant_num == AM_OPEN) {
		DPRINTF("access mode OPEN\n");
		return PLAYERC_OPEN_MODE;
	} else if (variant_num == AM_CLOSE) {
		DPRINTF("access mode CLOSE\n");
		return PLAYERC_CLOSE_MODE;
	} else if (variant_num == AM_ERROR) {
		DPRINTF("access mode ERROR\n");
		return PLAYERC_ERROR_MODE;
	} else  {
		DPRINTF("invalid access mode %d\n", variant_num);
		caml_invalid_argument("accessmode");
	}
}
