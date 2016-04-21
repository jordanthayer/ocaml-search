/**
 * \file graphics2d_stubs.c
 *
 * Stub functions for the graphics2d proxy.
 *
 * \author Kevin Rose
 * \date 06-09-2010
 */

#define _POSIX_C_SOURCE 200112L

#include <libplayerc/playerc.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "common.h"
#include "verb.h"


enum color_fields {
  COLOR_ALPHA,
  COLOR_RED,
  COLOR_GREEN,
  COLOR_BLUE
};

enum point_fields {
  POINT_X,
  POINT_Y
};


/* cleanup a graphics2d structure. */
static void graphics2d_finalize(value v);


/**
 * Operations for a graphics2d custom block.
 */
static struct custom_operations graphics2d_ops = {
	.identifier = "playerc graphics2d",
	.finalize = graphics2d_finalize,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

static void graphics2d_finalize(value g2d_val)
{
	playerc_graphics2d_t *g2d;
	g2d = Graphics2d_val(g2d_val);

	if (g2d) {
		DPRINTF("destroying graphics2d %p\n", g2d);
		playerc_graphics2d_destroy(g2d);
	}
}

value graphics2d_create_stub(value client_val, value index_val)
{
	CAMLparam2(client_val, index_val);
	CAMLlocal1(g2d_val);
	int index = Int_val(index_val);
	playerc_client_t *client = Client_val(client_val);
	playerc_graphics2d_t *g2d;

	DPRINTF("creat graphics2d on client %p\n", client);

	g2d = playerc_graphics2d_create(client, index);
	if (!g2d)
		exception_playerc_error();

	DPRINTF("created graphics2d %p on clien %p\n", g2d, client);

	g2d_val = caml_alloc_custom(&graphics2d_ops, sizeof(g2d), 0, 1);
	Graphics2d_val(g2d_val) = g2d;

	CAMLreturn(g2d_val);
}

void graphics2d_destroy_stub(value g2d_val)
{
	CAMLparam1(g2d_val);
	playerc_graphics2d_t *g2d = Graphics2d_val(g2d_val);

	DPRINTF("destroying graphics2d %p\n", g2d);
	playerc_graphics2d_destroy(g2d);

	CAMLreturn0;
}

void graphics2d_subscribe_stub(value g2d_val, value access_val)
{
	CAMLparam2(g2d_val, access_val);
	playerc_graphics2d_t *g2d = Graphics2d_val(g2d_val);
	int access = get_access_mode(Int_val(access_val));


	DPRINTF("subscribing to graphics2d %p\n", g2d);

	if (playerc_graphics2d_subscribe(g2d, access))
		exception_playerc_error();

	DPRINTF("graphics2d %p subscribed\n", g2d);

	CAMLreturn0;
}

void graphics2d_unsubscribe_stub(value g2d_val)
{
	CAMLparam1(g2d_val);
	playerc_graphics2d_t *g2d = Graphics2d_val(g2d_val);

	DPRINTF("unsubscribing from graphics2d %p\n", g2d);

	if (playerc_graphics2d_unsubscribe(g2d))
		exception_playerc_error();

	DPRINTF("graphics2d %p unsubscribed\n", g2d);

	CAMLreturn0;
}


void graphics2d_setcolor_stub(value g2d_val, value color_val)
{
  CAMLparam2(g2d_val, color_val);
  playerc_graphics2d_t *g2d = Graphics2d_val(g2d_val);
  player_color_t pcolor;
  pcolor.alpha = Int_val(Field(color_val, COLOR_ALPHA));
  pcolor.red   = Int_val(Field(color_val, COLOR_RED));
  pcolor.green = Int_val(Field(color_val, COLOR_GREEN));
  pcolor.blue  = Int_val(Field(color_val, COLOR_BLUE));

  DPRINTF("setting graphics2d %p color a=%d r=%d g=%d b=%d\n",
    g2d, pcolor.alpha, pcolor.red, pcolor.green, pcolor.blue);

  if(playerc_graphics2d_setcolor(g2d, pcolor))
    exception_playerc_error();

  DPRINTF("graphics2d %p color set to a=%d r=%d g=%d b=%d\n",
    g2d, pcolor.alpha, pcolor.red, pcolor.green, pcolor.blue);

  CAMLreturn0;
}

void graphics2d_draw_points_stub(value g2d_val, value points_val, value count_val)
{
  CAMLparam3(g2d_val, points_val, count_val);
  playerc_graphics2d_t *g2d = Graphics2d_val(g2d_val);

  int count = Int_val(count_val);

  player_point_2d_t pts[count];
  value point_val;
  int i;

  for(i = 0; i < count; i++)
  {
    point_val = Field(points_val, i);
    pts[i].px = Double_field(point_val, 0);
    pts[i].py = Double_field(point_val, 1);
  }

  DPRINTF("graphics2d %p drawing points: ", g2d);
  for(i = 0; i < count; i++)
  {
    DPRINTF("(%f, %f) ", pts[i].px, pts[i].py); 
  }
  DPRINTF("\n");

  playerc_graphics2d_draw_points(g2d, pts, count);

  DPRINTF("graphics2d %p points drawn\n", g2d);

  CAMLreturn0;
}

void graphics2d_draw_polyline_stub(value g2d_val, value points_val, value count_val)
{
  CAMLparam3(g2d_val, points_val, count_val);
  playerc_graphics2d_t *g2d = Graphics2d_val(g2d_val);

  int count = Int_val(count_val);

  player_point_2d_t pts[count];
  value point_val;
  int i;

  for(i = 0; i < count; i++)
  {
    point_val = Field(points_val, i);
    pts[i].px = Double_field(point_val, 0);
    pts[i].py = Double_field(point_val, 1);
  }

  DPRINTF("graphics2d %p drawing polylines: ", g2d);
  for(i = 0; i < count; i++)
  {
    DPRINTF("(%f, %f) ", pts[i].px, pts[i].py); 
  }
  DPRINTF("\n");

  playerc_graphics2d_draw_polyline(g2d, pts, count);

  DPRINTF("graphics2d %p polylines drawn\n", g2d);

  CAMLreturn0;
}

void graphics2d_draw_polygon_stub(value g2d_val, value points_val, value count_val, value filled_val, value color_val)
{
  CAMLparam5(g2d_val, points_val, count_val, filled_val, color_val);
  playerc_graphics2d_t *g2d = Graphics2d_val(g2d_val);

  int count = Int_val(count_val);
  int filled = Int_val(filled_val);

  player_color_t pcolor;

  player_point_2d_t pts[count];
  value point_val;
  int i;

  for(i = 0; i < count; i++)
  {
    point_val = Field(points_val, i);
    pts[i].px = Double_field(point_val, 0);
    pts[i].py = Double_field(point_val, 1);
  }

  pcolor.alpha = Int_val(Field(color_val, COLOR_ALPHA));
  pcolor.red   = Int_val(Field(color_val, COLOR_RED));
  pcolor.green = Int_val(Field(color_val, COLOR_GREEN));
  pcolor.blue  = Int_val(Field(color_val, COLOR_BLUE));

  DPRINTF("graphics2d %p drawing polygon ", g2d);

  playerc_graphics2d_draw_polygon(g2d, pts, count, filled, pcolor);

  DPRINTF("graphics2d %p polygon drawn\n", g2d);

  CAMLreturn0;
}

void graphics2d_clear_stub(value g2d_val)
{
  CAMLparam1(g2d_val);
  playerc_graphics2d_t *g2d = Graphics2d_val(g2d_val);

  DPRINTF("graphics2d %p clearing\n", g2d);

  playerc_graphics2d_clear(g2d);

  DPRINTF("graphics2d %p cleared\n", g2d);

  CAMLreturn0;
}


// EOF
