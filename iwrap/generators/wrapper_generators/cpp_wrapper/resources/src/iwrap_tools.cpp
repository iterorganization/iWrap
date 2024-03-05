#include <fstream>
#include <sstream>
#include "iwrap_tools.h"
#include "serialization_tools.h"


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

int read_code_parameters( char** xml_string)
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

int handle_status_info(int status_code, char* status_message, const char* actor_name, const char* method_name)
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

int convert_status_info(std::string in_status_msg, char** out_status_msg)
{

    //status message conversion
    int size = in_status_msg.length();

    *out_status_msg = (char*) malloc(size + 1);
    strcpy(*out_status_msg, in_status_msg.c_str());
}


IdsNs::IDS** open_db_entries(ids_description_t* db_entry_desc_array, int array_size)
{
    if ( array_size < 1 )
        return NULL;

    IdsNs::IDS* *db_entry_array = new IdsNs::IDS* [array_size];

    for (int i=0; i<array_size; i++)
    {
        db_entry_desc_array[i].idx = -1;
        int j=0;
        // check if same entry already present
        while (j<i)
        {
        if ((db_entry_desc_array[j].shot == db_entry_desc_array[i].shot) &&
           (db_entry_desc_array[j].run == db_entry_desc_array[i].run) &&
           (db_entry_desc_array[j].user == db_entry_desc_array[i].user) &&
           (db_entry_desc_array[j].machine == db_entry_desc_array[i].machine) &&
           (db_entry_desc_array[j].version == db_entry_desc_array[i].version))
         break;
        else
         j++;
        }
        // open DB entry
        if (j==i)
        {
           IdsNs::IDS *ids =  new IdsNs::IDS(db_entry_desc_array[i].shot,db_entry_desc_array[i].run,0,0);
           ids->openEnv(db_entry_desc_array[i].user,db_entry_desc_array[i].machine,db_entry_desc_array[i].version);

           db_entry_array[j]  =    ids ;
           db_entry_desc_array[i].idx = ids->getIdx();
        }
        else // propagate already opened DB entry
            db_entry_desc_array[i].idx=db_entry_desc_array[j].idx;
    }
    
    return db_entry_array;
 }

void close_db_entries(IdsNs::IDS** db_entry_array, int array_size)
{

    if ( !db_entry_array )
        return;

    for (int i=0; i<array_size; i++)
    {
        IdsNs::IDS* db_entry = db_entry_array[i];
        if(db_entry != NULL)
            db_entry->close();
            delete db_entry;
    }

}
