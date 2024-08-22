#ifndef _DEFS
#define _DEFS

const int MPI_ROOT_RANK = 0;

typedef struct
{
  char ids_name[132];
  int pulse;
  int run;
  int occurrence;
  int backend_id;
  int idx;
  char db_name[132];
  char user[132];
  char version[132];
} ids_description_t;

#endif // _DEFS
