#ifndef _DEFS
#define _DEFS

const int MPI_ROOT_RANK = 0;

typedef struct
{
  char ids_name[132];
  int occurrence;
  char uri[4096];
} ids_description_t;

#endif // _DEFS
