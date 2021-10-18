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



void write_data(std::ofstream *stream, char *var) {
    *stream << *var << std::endl;
}

char* read_file(const char* file_path) {

    std::ifstream file(file_path);

    if (!file.is_open()) {
        std::cerr << "Could not open the file: '" << file_path << "'" << std::endl;
        exit(EXIT_FAILURE);
    }

    std::stringstream buffer;
    buffer << file.rdbuf();

    std::string str = buffer.str();

    char * cstr = new char [str.length()+1];
    std::strcpy (cstr, str.c_str());

    file.close();
    return cstr;
}


