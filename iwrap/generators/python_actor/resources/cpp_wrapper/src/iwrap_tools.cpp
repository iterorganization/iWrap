#include <fstream>
#include <sstream>
#include "UALClasses.h"
#include "defs.h"
#include "serialization_tools.h"


int read_input(ids_description_t db_entry_desc_array[], int array_expected_size)
{
    ifstream fin;
    int array_read_size = -1;

    if (array_expected_size < 1)
        return 0;

    fin.open("input.txt");
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

int write_output(status_t status_info)
{
    ofstream fout;
    int str_len;

    fout.open("output.txt");
    write_data(&fout, status_info.code);

    if ( status_info.message != NULL)
    {
        str_len = strlen(status_info.message) + 1;
        write_data(&fout, str_len);
        write_data(&fout, status_info.message);
    }
    else
    {
        write_data(&fout, 0);
        write_data(&fout, "");
    }

    fout.close();
    return 0;
}

int handle_status_info(status_t status_info, const char* actor_name)
{
    if(status_info.message == NULL)
        status_info.message = (char*)"<No diagnostic message>";

    if(status_info.code !=0) {
      printf("---Diagnostic information returned from *** %s ***:---\n", actor_name);
      printf("-------Status code    : %d\n", status_info.code);
      printf("-------Status message : %s\n", status_info.message);
      printf("---------------------------------------------------------\n");
      }
}

code_parameters_t read_codeparams(const char* param_dir, const char* xml_file, const char* xsd_file)
{
    code_parameters_t code_params;

    std::string str;


    if ( param_dir == NULL || strlen(param_dir) < 1
        || xml_file == NULL || strlen(xml_file) < 1
        || xsd_file == NULL || strlen(xsd_file) < 1
        ) return code_params;

    str.append(param_dir);
    str.append(xml_file);
    code_params.params = read_file(str.c_str());
    code_params.params_size = strlen(code_params.params);

    str.clear();
    str.append(param_dir);
    str.append(xsd_file);
    code_params.schema = read_file(str.c_str());
    code_params.schema_size = strlen(code_params.schema);

    return code_params;
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

int close_db_entries(IdsNs::IDS** db_entry_array, int array_size)
{

    for (int i=0; i<array_size; i++)
    {
        IdsNs::IDS* db_entry = db_entry_array[i];
        if(db_entry != NULL)
            db_entry->close();
    }

}
