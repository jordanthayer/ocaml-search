/**
 * \file common.h
 *
 * Some common functionality.
 *
 * \author eaburns
 * \date 30-01-2010
 */

#if !defined(_COMMON_H_)
#define _COMMON_H_

#include <libplayerc/playerc.h>
#include <caml/mlvalues.h>

/* Must match the order of the OCaml variant. */
enum accessmode {
	AM_OPEN,
	AM_CLOSE,
	AM_ERROR,
};

/* Must match the order of the OCaml variant. */
enum datamode {
	DM_PUSH,
	DM_PULL,
};

/* Must match the order of the OCaml variant. */
enum transport {
	TR_TCP,
	TR_UDP,
};

/* Throws a Failure with a string of the last error message. */
void exception_playerc_error(void);

/* Get the PLAYERC access model from the corresponding OCaml variant
 * value. */
int get_access_mode(int variant_val);

/* Casting from custom block values into C pointers. */
#define Client_val(v) (*(playerc_client_t **) Data_custom_val(v))
#define Position2d_val(v) (*(playerc_position2d_t **) Data_custom_val(v))
#define Laser_val(v) (*(playerc_laser_t **) Data_custom_val(v))
#define Graphics2d_val(v) (*(playerc_graphics2d_t **) Data_custom_val(v))
#define Map_val(v) (*(playerc_map_t **) Data_custom_val(v))
#define Simulation_val(v) (*(playerc_simulation_t **) Data_custom_val(v))

#endif /* !_COMMON_H_ */
