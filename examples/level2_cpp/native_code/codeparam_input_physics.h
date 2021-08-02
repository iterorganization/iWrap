#ifndef _CODEPARAM_INPUT_PHYSICS
#define _CODEPARAM_INPUT_PHYSICS


typedef struct
{
    int ntimes;
    double multiplication_factor;
} codeparam_physics_data_t;

void assign_codeparam(char* codeparam_string, codeparam_physics_data_t &codeparam_physics_data);

#endif // _CODEPARAM_INPUT_PHYSICS
