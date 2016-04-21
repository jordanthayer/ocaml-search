/**
 * \file client_stubs.c
 *
 * Stub functions for playerc_client_X() calls.
 *
 * \author eaburns
 * \date 27-01-2010
 */

#define _POSIX_C_SOURCE 200112L

#include <libplayerc/playerc.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include "common.h"
#include "verb.h"

/* cleanup a client structure. */
static void client_finalize(value v);


/**
 * Operations for a client custom block.
 */
static struct custom_operations client_ops = {
	.identifier = "playerc client",
	.finalize = client_finalize,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

static void client_finalize(value client_val)
{
	playerc_client_t *client;
	client = Client_val(client_val);

	DPRINTF("destroying client %p\n", client);
	playerc_client_destroy(client);
}


value client_create_stub(value mclient_val, value host_val, value port_val)
{
	CAMLparam3(mclient_val, host_val, port_val);
	CAMLlocal1(client_val);
	playerc_mclient_t *mclient = NULL;
	char *host = String_val(host_val);
	int port = Int_val(port_val);
	playerc_client_t *client;

	if (Is_long(mclient_val))
		DPRINTF("mclient = %ld (const)\n", Long_val(mclient_val));
	else
		DPRINTF("mclient = %d (non-const)\n", Tag_val(mclient_val));

	if (Long_val(mclient_val) != 0)
		caml_failwith("Unimplemented: mclient <> None");

	DPRINTF("creating client %s:%d\n", host, port);

	client = playerc_client_create(mclient, host, port);
	if (!client)
		exception_playerc_error();

	DPRINTF("created client %p\n", client);

	client_val = caml_alloc_custom(&client_ops, sizeof(client), 0, 1);
	Client_val(client_val) = client;

	CAMLreturn(client_val);
}

void client_set_transport_stub(value client_val, value transport_val)
{
	CAMLparam2(client_val, transport_val);
	playerc_client_t *client = Client_val(client_val);
	unsigned int transport = Int_val(transport_val);
	unsigned int tr;

	if (transport == TR_TCP) {
		DPRINTF("client %p setting transport to TCP\n", client);
		tr = PLAYERC_TRANSPORT_TCP;
	} else if (transport == TR_UDP) {
		DPRINTF("client %p setting transport to UDP\n", client);
		tr = PLAYERC_TRANSPORT_UDP;
	} else {
		DPRINTF("client %p invalid transport %d\n", client, transport);
		caml_invalid_argument("transport");
	}

	playerc_client_set_transport(client, tr);

	CAMLreturn0;
}


void client_connect_stub(value client_val)
{
	CAMLparam1(client_val);
	playerc_client_t *client = Client_val(client_val);

	DPRINTF("connecting client %p\n", client);

	if (playerc_client_connect(client))
		exception_playerc_error();

	DPRINTF("%p connected\n", client);

	CAMLreturn0;
}

void client_disconnect_stub(value client_val)
{
	CAMLparam1(client_val);
	playerc_client_t *client = Client_val(client_val);

	DPRINTF("disconnecting client %p\n", client);

	if (playerc_client_disconnect(client))
		exception_playerc_error();

	DPRINTF("client %p disconnected\n", client);

	CAMLreturn0;
}

void client_disconnect_retry_stub(value client_val)
{
	CAMLparam1(client_val);
	playerc_client_t *client = Client_val(client_val);

	DPRINTF("disconnecting client %p with retry\n", client);

	if (playerc_client_disconnect_retry(client))
		exception_playerc_error();

	DPRINTF("client %p disconnected\n", client);

	CAMLreturn0;
}

void client_datamode_stub(value client_val, value datamode_val)
{
	CAMLparam2(client_val, datamode_val);
	playerc_client_t *client = Client_val(client_val);
	unsigned int datamode = Int_val(datamode_val);
	unsigned int dm;

	if (datamode == 0) {
		DPRINTF("client %p, setting datamode to PUSH\n", client);
		dm = PLAYERC_DATAMODE_PUSH;
	} else if (datamode == 1) {
		DPRINTF("client %p, setting datamode to PULL\n", client);
		dm = PLAYERC_DATAMODE_PULL;
	} else {
		DPRINTF("client %p, invalid datamode %d\n", client, datamode);
		caml_invalid_argument("datamode");
	}

	if (playerc_client_datamode(client, dm))
		exception_playerc_error();

	DPRINTF("client %p datamode set\n", client);

	CAMLreturn0;
}

void client_requestdata_stub(value client_val)
{
	CAMLparam1(client_val);
	playerc_client_t *client = Client_val(client_val);

	DPRINTF("client %p requesting data\n", client);

	if (playerc_client_requestdata(client))
		exception_playerc_error();

	DPRINTF("client %p got data\n", client);

	CAMLreturn0;
}

/*
   int playerc_client_set_replace_rule(playerc_client_t * client,
   int interf,
   int index,
   int type,
   int subtype,
   int replace
   )
*/

/*
  int playerc_client_get_devlist (playerc_client_t *client)
 */

value client_peek_stub(value client_val, value timeout_val)
{
	CAMLparam2(client_val, timeout_val);
	playerc_client_t *client = Client_val(client_val);
	int timeout = Int_val(timeout_val);
	int ret;

	DPRINTF("client %p peeking for data\n", client);

	ret = playerc_client_peek(client, timeout);
	if (ret < 0)
		exception_playerc_error();

	DPRINTF("client %p data = %d\n", client,  ret);

	CAMLreturn(Val_bool(ret));
}

value client_internal_peek_stub(value client_val, value timeout_val)
{
	CAMLparam2(client_val, timeout_val);
	playerc_client_t *client = Client_val(client_val);
	int timeout = Int_val(timeout_val);
	int ret;

	DPRINTF("client %p peeking for data\n", client);

	ret = playerc_client_internal_peek(client, timeout);
	if (ret < 0)
		exception_playerc_error();

	DPRINTF("client %p data = %d\n", client, ret);

	CAMLreturn(Val_bool(ret));
}

void client_read_stub(value client_val)
{
	CAMLparam1(client_val);
	playerc_client_t *client = Client_val(client_val);

	DPRINTF("reading from client %p\n", client);

	caml_enter_blocking_section();
	if (!playerc_client_read(client))
		exception_playerc_error();
	caml_leave_blocking_section();

	DPRINTF("read from client %p\n", client);

	CAMLreturn0;
}

void client_read_nonblock_stub(value client_val)
{
	CAMLparam1(client_val);
	playerc_client_t *client = Client_val(client_val);

	if (playerc_client_read_nonblock(client))
		exception_playerc_error();

	CAMLreturn0;
}

void client_set_request_timeout_stub(value client_val, value seconds_val)
{
	CAMLparam2(client_val, seconds_val);
	playerc_client_t *client = Client_val(client_val);
	unsigned int seconds = Int_val(seconds_val);

	DPRINTF("client %p setting request timeout to %u\n", client, seconds);

	playerc_client_set_request_timeout(client, seconds);

	CAMLreturn0;
}

void client_set_retry_limit_stub(value client_val, value limit_val)
{
	CAMLparam2(client_val, limit_val);
	playerc_client_t *client = Client_val(client_val);
	int limit = Int_val(limit_val);

	DPRINTF("client %p setting retry limit to %d\n", client, limit);

	playerc_client_set_retry_limit(client, limit);

	CAMLreturn0;
}

void client_set_retry_time_stub(value client_val, value time_val)
{
	CAMLparam2(client_val, time_val);
	playerc_client_t *client = Client_val(client_val);
	double time = Double_val(time_val);

	DPRINTF("client %p setting retry time to %lf\n", client, time);

	playerc_client_set_retry_time(client, time);

	CAMLreturn0;
}
