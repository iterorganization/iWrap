#ifndef _SERIALIZATION_TOOLS
#define _SERIALIZATION_TOOLS

void read_data(ifstream *stream, ids_description_t *ids_description);
void read_data(ifstream *stream, int *var);
void read_data(std::ifstream *stream, char *str, int size) ;

void write_data(ofstream *stream, int var);
void write_data(ofstream *stream, char *var);

#endif // _SERIALIZATION_TOOLS