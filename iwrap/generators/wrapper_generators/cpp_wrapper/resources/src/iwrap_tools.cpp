#include <fstream>
#include <sstream>
#include <cstring>
#include <cctype>

#include "iwrap_tools.h"
#include "serialization_tools.h"


char* iwrap_trim(char* text, int text_size = -1)
{

    int size = -1;

    if (*text == '\0')
        return text;

    char* last_char_ptr = text + strlen(text) -1;

    if (text_size > 0)
        size = text_size;
    else
        size = strlen(text);

    last_char_ptr = text + size -1;

    while(last_char_ptr >= text && isspace(*last_char_ptr))
    {
        *last_char_ptr = '\0';
        last_char_ptr--;
    }

    return text;
}

int read_input(const char* file_name, ids_description_t db_entry_desc_array[], int array_expected_size)
{
    ifstream fin;
    int array_read_size = -1;

    if (array_expected_size < 1)
        return 0;

    fin.open(file_name);
    fin.ignore(INT_MAX, '\n'); // skip line " === Arguments ===="
    fin.ignore(INT_MAX, '\n'); // skip line " Length:"
    read_data(&fin, &array_read_size);

    if ( array_expected_size != array_read_size)
    {
        printf("ERROR: Expected and read number of arguments differs ( %d  vs  %d )", array_expected_size, array_read_size);
        return -1;
    }

    for (int i=0; i < array_expected_size; i++)
    {
        read_data(&fin, &db_entry_desc_array[i]);
    }
    
    fin.close();

    return 0;
}

void read_code_parameters( char** xml_string)
{
    FILE *f = fopen("code_parameters.xml", "rb");
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    *xml_string = (char *)malloc(fsize + 1);
    fread(*xml_string, fsize, 1, f);
    fclose(f);

    (*xml_string)[fsize] = 0;
}

int write_output(const char* file_name, int status_code, char* status_message)
{
    ofstream fout;
    int str_len;

    fout.open(file_name);
    write_data(&fout, status_code);

    if ( status_message != NULL)
    {
        str_len = strlen(status_message) ;
        write_data(&fout, str_len);
        write_data(&fout, status_message);
    }
    else
    {
        write_data(&fout, 0);
        write_data(&fout, "");
    }

    fout.close();
    return 0;
}

void handle_status_info(int status_code, char* status_message, const char* actor_name, const char* method_name)
{
    const char* NO_MSG = "<No diagnostic message>";
    if(status_message == NULL)
    {
        int msg_size = strlen(NO_MSG) + 1;
        status_message = (char*) malloc(msg_size);
        strcpy(status_message, NO_MSG);
    }

    if(status_code !=0) {
      printf("---Diagnostic information returned from *** %s/%s ***:---\n", actor_name, method_name);
      printf("-------Status code    : %d\n", status_code);
      printf("-------Status message : %s\n", status_message);
      printf("---------------------------------------------------------\n");
      }
}

void release_status_info(char* status_message)
{
    if(status_message != NULL)
        free(status_message);
}

void convert_status_info(std::string in_status_msg, char** out_status_msg)
{

    //status message conversion
    int size = in_status_msg.length();

    *out_status_msg = (char*) malloc(size + 1);
    strcpy(*out_status_msg, in_status_msg.c_str());
}

IdsNs::IDS* init_db(ids_description_t* db_entry_desc)
{
    IdsNs::IDS* db_entry = NULL;

    if ( db_entry_desc->backend_id == MEMORY_BACKEND){
        db_entry = new IdsNs::IDS(db_entry_desc->idx);
    } else {
        db_entry = new IdsNs::IDS(db_entry_desc->pulse,  db_entry_desc->run, 0, 0);
   }

    return db_entry;
 }

int open_db(IdsNs::IDS* db_entry, ids_description_t* db_entry_desc)
{
    char* user = NULL;
    char* db_name = NULL;
    char* version = NULL;


    if ( db_entry_desc->backend_id == MEMORY_BACKEND){
        db_entry->setPulseCtx(db_entry_desc->idx);
        printf(">>MEM BE: SET CTX ENTRY %d\n", db_entry_desc->idx);
        }else{
            puts(">> Open ENV - begin ");
            user = iwrap_trim(db_entry_desc->user, sizeof db_entry_desc->user);
            db_name = iwrap_trim(db_entry_desc->db_name, sizeof db_entry_desc->db_name);
            version = iwrap_trim(db_entry_desc->version, sizeof db_entry_desc->version);

            db_entry->setBackend(static_cast<BACKEND>(db_entry_desc->backend_id));
            db_entry->openEnv(user, db_name, version);

                   printf(">>PF IDX: %d\n", db_entry->getPulseCtx());
                   puts(">>Open ENV - end ");
           }

    return 0;
 }



void close_db(IdsNs::IDS* db_entry)
{
    if ( db_entry->getBackend() == MEMORY_BACKEND)
    {
        puts(">>MEM BE: CLOSING ENTRY");
        return;
    }

    db_entry->close();
}
