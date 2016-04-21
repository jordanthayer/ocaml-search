// I (Jordan) got this from Rob Holte, buth the code belongs to
// Neil Burch
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "util.h"
#include "game.h"
#if HAVE_MOVE_SKIP_DB
#include <netinet/in.h>
#endif


static const int reflections[ MAX_REFLECTIONS + 1 ] = { 0, 2, 4, 6 };


/* set the pdb parameters from the given name
   returns 1 on success, 0 otherwise */
int parse_pdb_name( const game_t *game, game_pdb_t *pdb, const char *name )
{
  int pos, tile, t;

  pos = 0;

  while( name[ pos ] && !isdigit( name[ pos ] ) ) pos++;
  if( sscanf( &name[ pos ], "%d%n", &tile, &t ) < 1 ) {
    printf( "could not get puzzle size from %s\n", name );
    return 0;
  }
  if( tile != game->num_tiles ) {
    printf( "%s does not match number of tiles in game\n", name );
    return 0;
  }
  pos += t;

  while( name[ pos ] && !isdigit( name[ pos ] ) ) pos++;
  if( sscanf( &name[ pos ], "%d%n", &tile, &t ) < 1 ) {
    printf( "could not get puzzle spin size from %s\n", name );
    return 0;
  }
  if( tile != game->spin_size ) {
    printf( "%s does not match spin size in game\n", name );
    return 0;
  }
  pos += t;

  for( pdb->num_tiles = 0;
       pdb->num_tiles < game->num_tiles; pdb->num_tiles++ ) {
    while( name[ pos ] && !isdigit( name[ pos ] ) ) pos++;
    if( sscanf( &name[ pos ], "%d%n", &tile, &t ) < 1 ) {
      if( !pdb->num_tiles ) {
	printf( "could not find any tiles in %s\n", name );
	return 0;
      }
      break;
    }
    if( tile < 0 || tile >= game->num_tiles ) {
      printf( "invalid tile %d in %s\n", pdb->num_tiles + 1, name );
      return 0;
    }
    pos += t;

    if( !tile ) {
      printf( "ignoring tile 0 in %s\n", name );
      pdb->num_tiles--;
      continue;
    }
    for( t = 0; t <= NUM_REFLECTIONS; t++ ) {
      pdb->tiles[ t ][ pdb->num_tiles ]
	= ( tile + reflections[ t ] ) % game->num_tiles;
#if USE_REVERSE
      pdb->rev_tiles[ t ][ pdb->num_tiles ]
	= ( game->num_tiles - tile + reflections[ t ] ) % game->num_tiles;
#endif
    }
  }

  if( !pdb->num_tiles ) { return 0; }

  return 1;
}

void write_pdb_state( const game_t *game, const game_pdb_t *pdb,
		      const state_t state, FILE *file )
{
  int i, used[ game->num_tiles ];

  memset( used, 0, sizeof( *used ) * game->num_tiles );
  for( i = 0; i != pdb->num_tiles; ++i ) { used[ pdb->tiles[ 0 ][ i ] ] = 1; }
  for( i = 0; i != game->num_tiles; ++i ) {
    if( used[ state[ i ] ] || !state[ i ] ) {
      fprintf( file, i ? " %d" : "%d", (int)state[ i ] );
    } else {
      fprintf( file, i ? " *" : "*" );
    }
  }
  fprintf( file, "\n");
}

pdb_rank_t max_pdb_rank( const game_t *game, const game_pdb_t *pdb )
{
  pdb_rank_t size;
  int i;
  
  size = 1;
  for( i = game->num_tiles - pdb->num_tiles; i < game->num_tiles; i++ ) {
    size *= i;
  }
  return size;
}


/* returns 1 on success if the pdb is valid for game, 0 otherwise */
static int read_game_pdb( const game_t *game, game_pdb_t *pdb,
			  const char *name )
{
  pdb_rank_t file_size, table_size;
  FILE *file;
#if COMPRESS_PDB
  int64_t total_read, idx;
  int i, num_read;
  uint8_t buf[ 8192 ];
#endif

  if( !parse_pdb_name( game, pdb, name ) ) { return 0; }

  if( ( file = fopen( name, "rb" ) ) == 0 ) {
    printf( "could not open %s\n", name );
    return 0;
  }

  file_size = max_pdb_rank( game, pdb );
#if ( COMPRESS_PDB == DIV_COMPRESSION )
  if( pdb->divisor < 1 || pdb->divisor > file_size ) {
    printf( "invalid pdb divisor\n" );
    fclose( file );
    return 0;
  }
  table_size = ( file_size + pdb->divisor - 1 ) / pdb->divisor;
#elif ( COMPRESS_PDB == MOD_COMPRESSION )
#if PACK_PDB
  pdb->modulus *= 2;
#endif
  if( pdb->modulus < 1 || pdb->modulus > file_size ) {
    printf( "invalid pdb modulus\n" );
    fclose( file );
    return 0;
  }
  table_size = pdb->modulus;
#else
  table_size = file_size;
#endif

#if PACK_PDB
  file_size = ( file_size + 1 ) / 2;
  table_size = ( table_size + 1 ) / 2;
#endif

  pdb->table = xmalloc( table_size );

#if COMPRESS_PDB
  idx = 0;
  memset( pdb->table, 255, table_size );
  total_read = 0;
  while( total_read < file_size ) {
    i = file_size - total_read; if( i > 8192 ) i = 8192;
    if( ( num_read = fread( buf, 1, i, file ) ) < 0 ) {
      printf( "error reading %s\n", name );
      free( pdb->table ); fclose( file ); return 0;
    }

    for( i = 0; i < num_read; i++ ) {
#if PACK_PDB

#if ( COMPRESS_PDB == DIV_COMPRESSION )
      idx = ( ( total_read + i ) * 2 ) / pdb->divisor;
#elif ( COMPRESS_PDB == MOD_COMPRESSION )
      idx = ( ( total_read + i ) * 2 ) % pdb->modulus;
#else
#error BAD COMPRESSION TYPE
#endif
      if( idx & 1 ) {
	if( ( buf[ i ] >> 4 ) < ( pdb->table[ idx / 2 ] & 15 ) ) {
	  pdb->table[ idx / 2 ] = ( pdb->table[ idx / 2 ] & 240 )
	    | ( buf[ i ] >> 4 );
	}
      } else {
	if( ( buf[ i ] >> 4 ) < ( pdb->table[ idx / 2 ] >> 4 ) ) {
	  pdb->table[ idx / 2 ] = ( pdb->table[ idx / 2 ] & 15 )
	    | ( buf[ i ] & 240 );
	}
      }

#if ( COMPRESS_PDB == DIV_COMPRESSION )
      idx = ( ( total_read + i ) * 2 + 1 ) / pdb->divisor;
#elif ( COMPRESS_PDB == MOD_COMPRESSION )
      idx = ( ( total_read + i ) * 2 + 1 ) % pdb->modulus;
#else
#error BAD COMPRESSION TYPE
#endif
      if( idx & 1 ) {
	if( ( buf[ i ] & 15 ) < ( pdb->table[ idx / 2 ] & 15 ) ) {
	  pdb->table[ idx / 2 ] = ( pdb->table[ idx / 2 ] & 240 )
	    | ( buf[ i ] & 15 );
	}
      } else {
	if( ( buf[ i ] & 15 ) < ( pdb->table[ idx / 2 ] >> 4 ) ) {
	  pdb->table[ idx / 2 ] = ( pdb->table[ idx / 2 ] & 15 )
	    | ( ( buf[ i ] & 15 ) << 4 );
	}
      }
#else /* !PACK_PDB */

#if ( COMPRESS_PDB == DIV_COMPRESSION )
      idx = ( total_read + i ) / pdb->divisor;
#elif ( COMPRESS_PDB == MOD_COMPRESSION )
      idx = ( total_read + i ) % pdb->modulus;
#else
#error BAD COMPRESSION TYPE
#endif
      if( buf[ i ] < pdb->table[ idx ] ) {
	pdb->table[ idx ] = buf[ i ];
      }
#endif
    }

    total_read += num_read;
  }
#else

  if( fread( pdb->table, 1, file_size, file ) != file_size ) {
    printf( "error reading %s\n", name );
    free( pdb->table );
    fclose( file );
    return 0;
  }
#endif

  fclose( file );
  printf( "using %s\n", name );
  return 1;
}

#if USE_MR_PDB_RANK
static pdb_rank_t pdb_index( const int8_t *pdbpos, const int num_tiles,
			     const int pdb_size )
{
  pdb_rank_t rank;
  int i, end;
  int8_t tiles[ MAX_TILES ], loc[ MAX_TILES ];

  end = pdb_size;
  memset( loc, pdb_size, num_tiles );
  i = 0; do loc[ tiles[ i ] = pdbpos[ i ] ] = i; while( ++i != pdb_size );

  rank = tiles[ i = 0 ];
  end = num_tiles - 1;
  while( i != pdb_size - 1 ) {
    tiles[ loc[ end ] ] = tiles[ i ];
    loc[ tiles[ i ] ] = loc[ end ];
    i++; end--;

    rank = rank * ( num_tiles - i ) + tiles[ i ];
  }

  return rank;
}
#else
static pdb_rank_t pdb_index( const int8_t *pdbpos, const int num_tiles,
			     const int pdb_size )
{
  pdb_rank_t idx;
  int i, j, digit;

  idx = 0;
  for( i = 0; i < pdb_size; i++ ) {
    digit = pdbpos[ i ];
    for( j = 0; j < i; j++ )
      if( pdbpos[ j ] < pdbpos[ i ] )
	digit--; 
    idx = idx * ( num_tiles - i ) + digit;
  }
  return idx;
}
#endif

uint8_t pdb_lookup( const game_t *game, const game_pdb_t *pdb,
		    pdb_rank_t index )
{
#if ( COMPRESS_PDB == DIV_COMPRESSION )
  index = index / pdb->divisor;
#elif ( COMPRESS_PDB == MOD_COMPRESSION )
  index = index % pdb->modulus;
#endif

#if PACK_PDB
  if( index & 1 ) { return pdb->table[ index / 2 ] & 15; }
  else { return pdb->table[ index / 2 ] >> 4; }
#else
  return pdb->table[ index ];
#endif
}

static void get_pdbpos( const game_t *game, const int8_t *tilepos,
			const int reflection, const game_pdb_t *pdb,
			int8_t *pdbpos )
{
  int t, i;

  t = game->num_tiles - ( tilepos[ reflections[ reflection ] ] + 1 );
  for( i = 0; i < pdb->num_tiles; i++ ) {
    pdbpos[ i ] = ( tilepos[ pdb->tiles[ reflection ][ i ] ] + t )
      % game->num_tiles;
  }
}

#if USE_REVERSE
static void get_pdbpos_rev( const game_t *game, const int8_t *tilepos,
			    const int reflection, const game_pdb_t *pdb,
			    int8_t *pdbpos )
{
  int t, i;

  t = game->num_tiles - 1 + tilepos[ reflections[ reflection ] ];
  for( i = 0; i < pdb->num_tiles; i++ ) {
    pdbpos[ i ] = ( t - tilepos[ pdb->rev_tiles[ reflection ][ i ] ] )
      % game->num_tiles;
  }
}
#endif

pdb_rank_t state_to_pdb_rank( const game_t *game, const game_pdb_t *pdb,
			      const state_t state )
{
  int8_t pdbpos[ MAX_TILES ];

  get_pdbpos( game, &state[ game->num_tiles ], 0, pdb, pdbpos );
  return pdb_index( pdbpos, game->num_tiles - 1, pdb->num_tiles );
}

#if USE_MR_PDB_RANK
void pdb_rank_to_pdb_state( const game_t *game, const game_pdb_t *pdb,
			    state_rank_t rank, state_t state )
{
  int i, end, j, t, num_tiles = game->num_tiles - 1;
  int8_t pdbpos[ MAX_TILES ], loc[ MAX_TILES ];

  memset( loc, pdb->num_tiles, num_tiles );

  i = pdb->num_tiles - 1;
  end = num_tiles - pdb->num_tiles;
  do {

    t = rank % ( num_tiles - i );
    rank /= num_tiles - i;

    loc[ end ] = loc[ t ];
    loc[ t ] = i;
    pdbpos[ loc[ end ] ] = end;
    pdbpos[ i ] = t;
  } while( ++end, --i );

  t = rank;
  pdbpos[ loc[ t ] ] = end;
  pdbpos[ i ] = t;

  /* convert the pdb tile positions into a "state" */
  memset( state, -1, game->num_tiles * 2 );
  state[ game->num_tiles - 1 ] = 0;
  state[ game->num_tiles ] = game->num_tiles - 1;
  for( i = 0; i < pdb->num_tiles; i++ ) {
    state[ game->num_tiles + pdb->tiles[ 0 ][ i ] ] = pdbpos[ i ];
    state[ pdbpos[ i ] ] = pdb->tiles[ 0 ][ i ];
  }
  j = -1;
  for( i = 1; i != game->num_tiles; i++ ) {
    if( state[ game->num_tiles + i ] < 0 ) {
      while( state[ ++j ] >= 0 );
      state[ j ] = i;
      state[ game->num_tiles + i ] = j;
    }
  }
}
#else
void pdb_rank_to_pdb_state( const game_t *game, const game_pdb_t *pdb,
			    state_rank_t rank, state_t state )
{
  int i, j, t, num_tiles = game->num_tiles - 1;
  int8_t pdbpos[ MAX_TILES ];

  i = pdb->num_tiles - 1;
  do {
    pdbpos[ i ] = rank % ( num_tiles - i );
    rank /= num_tiles - i;
    t = pdbpos[ i ];
    for( j = i + 1; j != pdb->num_tiles; j++ ) {
      if( pdbpos[ j ] >= t ) {
	pdbpos[ j ]++;
      }
    }
  } while( --i );
  pdbpos[ i ] = rank;
  t = pdbpos[ i ];
  for( j = pdb->num_tiles - 1; j != i; j-- ) {
    if( pdbpos[ j ] >= t ) {
      pdbpos[ j ]++;
    }
  }

  /* convert the pdb tile positions into a "state" */
  memset( state, -1, game->num_tiles * 2 );
  state[ game->num_tiles - 1 ] = 0;
  state[ game->num_tiles ] = game->num_tiles - 1;
  for( i = 0; i < pdb->num_tiles; i++ ) {
    state[ game->num_tiles + pdb->tiles[ 0 ][ i ] ] = pdbpos[ i ];
    state[ pdbpos[ i ] ] = pdb->tiles[ 0 ][ i ];
  }
  j = -1;
  for( i = 1; i != game->num_tiles; i++ ) {
    if( state[ game->num_tiles + i ] < 0 ) {
      while( state[ ++j ] >= 0 );
      state[ j ] = i;
      state[ game->num_tiles + i ] = j;
    }
  }
}
#endif


#if HAVE_MOVE_SKIP_DB
static void load_move_skip_db( game_t *game )
{
  int i, size;
  char name[ MAX_MOVE_SKIP_DB_NAME ];
  FILE *file;

  size = max_move_list( game );
  game->move_skip_db = xmalloc( sizeof( uint32_t ) * size );
  game->reverse_move_skip_db = game->move_skip_db;

  get_move_skip_dbname( game, name );
  if( !( file = fopen( name, "r" ) ) ) {
    printf( "could not open %d move skip database\n", MOVE_LIST );
    exit( -1 );
  }
  if( fread( game->move_skip_db, 1, size * sizeof( uint32_t ), file )
      != (size_t)size * sizeof( uint32_t ) ) {
    printf( "short read of %d move skip database\n", MOVE_LIST );
    exit( -1 );
  }
  fclose( file );

  for( i = 0; i < size; i++ ) {
    game->move_skip_db[ i ] = ntohl( game->move_skip_db[ i ] );
  }
}

#if 0
static void destroy_move_skip_db( game_t *game )
{
  free( game->move_skip_db );
}
#endif
#endif

/* returns 1 on success, 0 on failure */
int init_game( game_t *game, const int num_args, char **args, int need_eval )
{
  int i, j;

  if( reflections[ 0 ] != 0 ) {
    abort();
  }

  if( num_args < 2 ) {
    printf( "game requires at least two arguments\n" );
#if ( COMPRESS_PDB == DIV_COMPRESSION )
    printf( "  num_tiles spin_size [PDB_filename PDB_divisor ...]\n" );
#elif ( COMPRESS_PDB == MOD_COMPRESSION )
    printf( "  num_tiles spin_size [PDB_filename PDB_modulus ...]\n" );
#else
    printf( "  num_tiles spin_size [PDB_filename ...]\n" );
#endif
    return 0;
  }

  game->num_tiles = atoi( args[ 0 ] );
  if( game->num_tiles < 1 || game->num_tiles > MAX_TILES ) {
    printf( "%s is a bad number of tiles\n", args[ 0 ] );
    return 0;
  }

  game->spin_size = atoi( args[ 1 ] );
  if( game->spin_size < 2 || game->spin_size >= game->num_tiles ) {
    printf( "%s is a bad spin size\n", args[ 1 ] );
    return 0;
  }

  if( NUM_REFLECTIONS > MAX_REFLECTIONS ) {
    printf( "maximum of %d reflections, check define NUM_REFLECTIONS\n",
	    MAX_REFLECTIONS );
    abort();
  }

  game->num_pdbs = 0;
  if( need_eval ) {
    game->pdbs = xmalloc( sizeof( game_pdb_t ) * ( num_args - 2 ) );
    for( i = 2; i < num_args; i++ ) {
#if ( COMPRESS_PDB == DIV_COMPRESSION )
      if( i + 1 >= num_args ) break; /* need divisor */
      game->pdbs[ game->num_pdbs ].divisor = atoi( args[ i + 1 ] );
#elif ( COMPRESS_PDB == MOD_COMPRESSION )
      if( i + 1 >= num_args ) break; /* need modulus */
      game->pdbs[ game->num_pdbs ].modulus = atoi( args[ i + 1 ] );
#endif
      if( !read_game_pdb( game, &game->pdbs[ game->num_pdbs ], args[ i ] ) ) {
	printf( "skipping PDB %s\n", args[ i ] );
	continue;
      }

#if COMPRESS_PDB
      /* there's an extra argument to skip */
      i++;
#endif
      game->num_pdbs++;
    }
    if( !game->num_pdbs ) {
      printf( "WARNING! no pattern databases loaded\n" );
    }
  }

  game->skip_masks = xmalloc( sizeof(uint32_t) * game->num_tiles );
  game->keep_masks = xmalloc( sizeof(uint32_t) * game->num_tiles );
  game->reverse_skip_masks = game->skip_masks;
  game->reverse_keep_masks = game->keep_masks;

  for( i = 0; i < game->num_tiles; i++ ) {
    game->skip_masks[ i ] = 1 << i;
    j = i - game->spin_size;
    if( j >= 0 ) {
      game->skip_masks[ i ] |= ( 2 << j ) - 1;
    }
    j = i + game->spin_size - game->num_tiles;
    if( j >= 0 ) {
      game->skip_masks[ i ] &= ~( ( 1 << j ) - 1 );
    }
    game->keep_masks[ i ] = ~0;
    for( j = 1; j < game->spin_size; j++ ) {
      game->keep_masks[ i ] &= ~( 1 << ( ( i + j ) % game->num_tiles ) );
      game->keep_masks[ i ]
	&= ~( 1 << ( ( i - j + game->num_tiles ) % game->num_tiles ) );
    }
  }

  game->init_move_list = game->num_tiles + 1;
  game->max_new_move_list = 1;
  for( i = 1; i < MOVE_LIST; i++ ) {
    game->init_move_list *= game->num_tiles + 1;
    game->max_new_move_list *= game->num_tiles + 1;
  }
  game->init_move_list -= 1;

  if( need_eval ) {

#if HAVE_MOVE_SKIP_DB
    load_move_skip_db( game );
#endif
  }

  return 1;
}

state_t new_state( const game_t *game ) {
  return xmalloc( game->num_tiles * 2 );
}

void destroy_state( state_t state )
{
  free( state );
}

void goal_state( const game_t *game, state_t state )
{
  int i;

  i = game->num_tiles;
  while( i ){
    -- i;
    state[ i ]= i;
    state[ game->num_tiles + i ] = i;
  }
}

void write_state( const game_t *game, const state_t state, FILE *file )
{
  int i;

  for( i = 0; i < game->num_tiles; i++ ) {
    fprintf( file, i ? " %d" : "%d", (int)state[ i ] );
  }
  fprintf( file, "\n");
}

int read_state( const game_t *game, state_t state, FILE *file )
{
  char line[ 256 ];
  int i, l, t, tile;

  if( !fgets( line, 256, file ) ) return 0;
  for( l = 0, i = 0; i < game->num_tiles; i++ ) {
    if( sscanf( &line[ l ], "%d%n", &tile, &t ) < 1 ) {
      return 0;
    }
    if( tile < 0 || tile >= game->num_tiles ) {
      return 0;
    }
    l += t;
    state[ i ] = tile;
    state[ game->num_tiles + tile ] = i;
  }
  return 1;
}

state_rank_t state_to_rank( const game_t *game, const state_t state )
{
  state_rank_t rank;
  int i, end, num_tiles = game->num_tiles - 1;
  int8_t *tilepos, tiles[ num_tiles ], loc[ num_tiles ];

  /* convert from tiles [0,N) to tile positions of [1,N) */
  end = num_tiles - state[ game->num_tiles ];
  tilepos = &state[ game->num_tiles + 1 ];
  i = 0;
  do {
    tiles[ i ] = ( tilepos[ i ] + end ) % game->num_tiles;
    loc[ tiles[ i ] ] = i;
  } while( ++i != num_tiles );

  i = 0;
  rank = tiles[ i ];
  end = num_tiles - 1;
  while( end != 1 ) {
    tiles[ loc[ end ] ] = tiles[ i ];
    loc[ tiles[ i ] ] = loc[ end ];
    i++; end--;
    
    rank = rank * ( num_tiles - i ) + tiles[ i ];
  }

  return rank;
}

void rank_to_state( const game_t *game, state_rank_t rank, state_t state )
{
  int i, end;
  int num_tiles = game->num_tiles - 1;
  int8_t *tilepos, loc[ num_tiles ], t;

  state[ 0 ] = state[ game->num_tiles ] = 0;
  tilepos = &state[ game->num_tiles + 1 ];

  i = num_tiles - 1;
  loc[ end = 0 ] = i;
  tilepos[ i ] = 0;
  while( ++end, --i ) {

    t = rank % ( num_tiles - i );
    rank /= num_tiles - i;

    loc[ end ] = loc[ t ];
    loc[ t ] = i;
    tilepos[ loc[ end ] ] = end;
    tilepos[ i ] = t;
  }

  t = rank;
  loc[ end ] = loc[ t ];
  loc[ t ] = i;
  tilepos[ loc[ end ] ] = end;
  tilepos[ i ] = t;

  /* convert from tile positions of [1,N) to tiles [0,N) */
  for( i = 0; i < num_tiles; i++ ) {
    state[ ++tilepos[ i ] ] = i + 1;
  }
}

state_rank_t max_rank( const game_t *game )
{
  state_rank_t rank;
  int i;

  rank = 1;
  for( i = 2; i < game->num_tiles; i++ ) rank *= i;
  return rank;
}

int eval( const game_t *game, const state_t state
#if USE_EVAL_MIN_CUT
	  , const int min_cut
#endif
#if HAVE_DUAL
	  , int *used_dual
#endif	  
	  )
{
  int r, i;
  uint8_t h, h2;
  int8_t pdbpos[ MAX_TILES ];

  h = 0;
#if HAVE_DUAL
  *used_dual = 0;
#endif
  for( i = 0; i < game->num_pdbs; i++ ) {
    for( r = 0; r < NUM_REFLECTIONS + 1; r++ ) {
      get_pdbpos( game, &state[ game->num_tiles ],
		  r, &game->pdbs[ i ], pdbpos );
      h2 = pdb_lookup( game, &game->pdbs[ i ],
		       pdb_index( pdbpos, game->num_tiles - 1,
				  game->pdbs[ i ].num_tiles ) );
      if( h2 > h ) {
	h = h2;
#if HAVE_DUAL
	*used_dual = 0;
#endif
#if USE_EVAL_MIN_CUT
	if( h >= min_cut ) { break; }
#endif
      }

#if HAVE_DUAL
      get_pdbpos( game, state, r, &game->pdbs[ i ], pdbpos );
      h2 = pdb_lookup( game, &game->pdbs[ i ],
		       pdb_index( pdbpos, game->num_tiles - 1,
				  game->pdbs[ i ].num_tiles ) );
      if( h2 > h ) {
	h = h2;
	*used_dual = 1;
#if USE_EVAL_MIN_CUT
	if( h >= min_cut ) { break; }
#endif
      }
#endif

#if USE_REVERSE
      get_pdbpos_rev( game, &state[ game->num_tiles ],
		      r, &game->pdbs[ i ], pdbpos );
      h2 = pdb_lookup( game, &game->pdbs[ i ],
		       pdb_index( pdbpos, game->num_tiles - 1,
				  game->pdbs[ i ].num_tiles ) );
      if( h2 > h ) {
	h = h2;
#if HAVE_DUAL
	*used_dual = 0;
#endif
#if USE_EVAL_MIN_CUT
	if( h >= min_cut ) { break; }
#endif
      }

#if HAVE_DUAL
      get_pdbpos_rev( game, state, r, &game->pdbs[ i ], pdbpos );
      h2 = pdb_lookup( game, &game->pdbs[ i ],
		       pdb_index( pdbpos, game->num_tiles - 1,
				  game->pdbs[ i ].num_tiles ) );
      if( h2 > h ) {
	h = h2;
	*used_dual = 1;
#if USE_EVAL_MIN_CUT
	if( h >= min_cut ) { break; }
#endif
      }
#endif
#endif
    }
  }

  return h;
}

int is_goal( const game_t *game, const state_t state )
{
  int i, j;

  if( state[ 0 ] ) {
    for( i = game->num_tiles - 1, j = state[ 0 ] - 1; j >= 0; i--, j-- ) {
      if( state[ i ] != j ) {
	return 0;
      }
    }
    for( j += game->num_tiles; i; i--, j-- ) {
      if( state[ i ] != j ) {
	return 0;
      }
    }
  } else {

    for( i = game->num_tiles - 1; i; i-- ) {
      if( state[ i ] != i ) {
	return 0;
      }
    }
  }

  return 1;
}

void make_move( const game_t *game, state_t state, const int move )
{
  int end, a, b, t;

  end = ( move + game->spin_size / 2 ) % game->num_tiles;
  a = move;
  b = ( move + game->spin_size - 1 ) % game->num_tiles;
  while( a != end ) {
    state[ game->num_tiles + state[ a ] ] = b;
    state[ game->num_tiles + state[ b ] ] = a;
    t = state[ a ];
    state[ a ] = state[ b ];
    state[ b ] = t;

    a = ( a + 1 ) % game->num_tiles;
    if( !b )
      b = game->num_tiles;
    b--;
  }
}

void switch_to_dual( const game_t *game, state_t state )
{
#if 1
  int i;
  int8_t t;

  for( i = 0; i != game->num_tiles; i++ ) {
    t = state[ i ];
    state[ i ] = state[ i + game->num_tiles ];
    state[ i + game->num_tiles ] = t;
  }
#else
  int i;
  int8_t zero_shift;

  zero_shift = game->num_tiles - state[ game->num_tiles ];
  for( i = 0; i != game->num_tiles; i++ ) {
    state[ i ]
      = ( state[ i + game->num_tiles ] + zero_shift ) % game->num_tiles;
  }
  for( i = 0; i != game->num_tiles; i++ ) {
    state[ state[ i ] + game->num_tiles ] = i;
  }
#endif
}
