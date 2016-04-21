// I (Jordan) got this from Rob Holte, buth the code belongs to
// Neil Burch
#ifndef _GAME_H
#define _GAME_H

#include <inttypes.h>
#include <string.h>
#include "game_common.h"


#define CONSTANT_MOVE_LIST 1
#define UNIT_MOVE_COST 1
#define HAVE_REVERSE_MOVES 1
#define USE_UNDO_MOVES 0
#define MAX_DEPTH 64
#define MOVE_LIST 5
#define HAVE_MOVE_SKIP_DB 1
#if HAVE_MOVE_SKIP_DB
// we could actually do both of these, and catch a very few
// more, as the skip masks can catch some very deep cycles,
// but the move skip database catches 99.9% of what the masks
// do, as well ss some other things.  Putting both together
// is a win in nodes, but a loss in time
#define SKIP_TYPE NO_SKIP
#else
#define SKIP_TYPE SKIP_MASKS
#endif
#define HAVE_DUAL 1
#define MIN_DEPTH_INCREASE 1


#define NO_COMPRESSION 0
#define DIV_COMPRESSION 1
#define MOD_COMPRESSION 2

#define COMPRESS_PDB NO_COMPRESSION
#define USE_MR_PDB_RANK 1
#define MAX_TILES 24
/* 1 = use a nibble per PDB entry, byte per entry otherwise */
#define PACK_PDB 0
#define USE_REVERSE 1
#define MAX_REFLECTIONS 3
#define NUM_REFLECTIONS 2
/* break out of evaluation early if we find a sufficiently large value */
#define USE_EVAL_MIN_CUT 1

#if USE_DUAL_SEARCH
#undef USE_EVAL_MIN_CUT
#define USE_EVAL_MIN_CUT 0
#endif


typedef struct {
  int num_tiles;
  int8_t tiles[ MAX_REFLECTIONS + 1 ][ MAX_TILES ];
#if USE_REVERSE
  int8_t rev_tiles[ MAX_REFLECTIONS + 1 ][ MAX_TILES ];
#endif
  uint8_t *table;
#if ( COMPRESS_PDB == DIV_COMPRESSION )
  int divisor;
#elif ( COMPRESS_PDB == MOD_COMPRESSION )
  int modulus;
#endif
} game_pdb_t;

typedef struct {
	int num_tiles;  // number of tiles on the top-spin puzzle
	int spin_size;  // number of tiles that can be reversed at once
	uint32_t *skip_masks; // used to control branching factor
	uint32_t *keep_masks;
	uint32_t *reverse_skip_masks;
	uint32_t *reverse_keep_masks;
	int num_pdbs;
	game_pdb_t *pdbs;
	uint32_t init_move_list;
	uint32_t max_new_move_list;
#if HAVE_MOVE_SKIP_DB
	uint32_t *move_skip_db;
	uint32_t *reverse_move_skip_db;
#endif
} game_t;

typedef int8_t *state_t;
typedef int64_t state_rank_t;
typedef int64_t pdb_rank_t;


/* standard game functions */
int init_game( game_t *game, const int num_args, char **args, int need_eval );
state_t new_state( const game_t *game );
#define state_size( game ) ((game)->num_tiles*2)
#define copy_state( game, dest_state, source_state ) memcpy( dest_state, source_state, (game)->num_tiles * 2 )
#define states_unequal( game, state_a, state_b ) memcmp( state_a, state_b, (game)->num_tiles * 2 )
void destroy_state( state_t state );
void goal_state( const game_t *game, state_t state );
void write_state( const game_t *game, const state_t state, FILE *file );
int read_state( const game_t *game, state_t state, FILE *file );
state_rank_t state_to_rank( const game_t *game, const state_t state );
void rank_to_state( const game_t *game, state_rank_t rank, state_t state );
state_rank_t max_rank( const game_t *game );
int eval( const game_t *game, const state_t state
#if USE_EVAL_MIN_CUT
	  , const int heur_thresh
#endif
#if HAVE_DUAL
	  , int *dual_is_greater
#endif
	  );
int is_goal( const game_t *game, const state_t state );

/* CONSTANT_MOVE_LIST == 1  */
#define num_moves( game_ptr ) ((game_ptr)->num_tiles)
void make_move( const game_t *game, state_t state, const int move );
#define move_cost( game, state, move ) 1
#define get_undo_move( game, state, move ) (move)
#define num_reverse_moves( game_ptr ) ((game_ptr)->num_tiles)
#define make_reverse_move( game_ptr, state, move ) make_move(game,state,move)
#define reverse_move_cost( game, state, move ) 1
#define get_undo_reverse_move( game, state, move ) (move)

/* MOVE_LIST != 0 */
#define max_move_list( game ) ((game)->init_move_list+1)
#define max_reverse_move_list( game ) max_move_list(game)
#define init_move_list( game ) ((game)->init_move_list)
#define init_reverse_move_list( game ) init_move_list(game)
#define new_move_list( game, move_list, move ) \
(((move_list)%(game)->max_new_move_list)*((game)->num_tiles+1)+(move))
#define new_reverse_move_list( game, move_list, move ) \
new_move_list(game,move_list,move)
#define MAX_MOVE_SKIP_DB_NAME 32
#define get_move_skip_dbname( game, name ) \
sprintf((name),"Moves%dSize%dSpin%dSkip",MOVE_LIST,(game)->num_tiles,(game)->spin_size);

/* HAVE_DUAL == 1 */
void switch_to_dual( const game_t *game, state_t state );


/* PDB FUNCTIONS */
int parse_pdb_name( const game_t *game, game_pdb_t *pdb, const char *name );
void write_pdb_state( const game_t *game, const game_pdb_t *pdb,
		      const state_t state, FILE *file );
pdb_rank_t state_to_pdb_rank( const game_t *game, const game_pdb_t *pdb,
			      const state_t state );
uint8_t pdb_lookup( const game_t *game, const game_pdb_t *pdb,
		    pdb_rank_t index );
void pdb_rank_to_pdb_state( const game_t *game, const game_pdb_t *pdb,
			    state_rank_t rank, state_t state );
pdb_rank_t max_pdb_rank( const game_t *game, const game_pdb_t *pdb );
#endif
