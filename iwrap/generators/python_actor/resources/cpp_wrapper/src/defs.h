#ifndef _DEFS
#define _DEFS

typedef struct
{
  char ids_name[132];
  int shot;
  int run;
  int occurrence;
  int idx;
  char machine[132];
  char user[132];
  char version[132];
} ids_description_t;


typedef struct
{
  const char* params = NULL;
  int params_size = 0;
} code_parameters_t;


typedef struct
{
    int code = 0;
    char* message = NULL;

} status_t;

#endif // _DEFS
