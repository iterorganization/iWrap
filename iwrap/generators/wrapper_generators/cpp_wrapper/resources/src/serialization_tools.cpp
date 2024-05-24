#include <fstream>
#include <sstream>
#include <climits>
#include <iostream>
#include <cstring>
#include "defs.h"

void read_data(std::ifstream *stream, ids_description_t *ids_description) {
    stream->ignore(INT_MAX, '\n');
    *stream >> ids_description->ids_name;
    *stream >> ids_description->shot;
    *stream >> ids_description->run;
    *stream >> ids_description->occurrence;
    *stream >> ids_description->backend_id;
    *stream >> ids_description->idx;
    *stream >> ids_description->machine;
    *stream >> ids_description->user;
    *stream >> ids_description->version;
    stream->ignore(INT_MAX, '\n');
}

void read_data(std::ifstream *stream, int *var) {
  *stream >> *var;
  stream->ignore(INT_MAX, '\n');
}

void read_data(std::ifstream *stream, char *str, int size) {
  stream->read(str, size);
  stream->ignore(INT_MAX, '\n');
}


void write_data(std::ofstream *stream, int var) {
    *stream << var << std::endl;
}

void write_data(std::ofstream *stream, char *text) {
    size_t len = strlen(text);
    stream->write(text,  strlen(text));

    }




