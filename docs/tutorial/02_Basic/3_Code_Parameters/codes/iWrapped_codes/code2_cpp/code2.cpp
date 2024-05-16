#include "ALClasses.h"
#include "code2.h"
#include <libxml/parser.h>
#include <libxml/tree.h>

std::string farewell;

//===================================================================
// HELPER FUNCTION FOR GETTING FAREWELL MESSAGE FROM CODE PARAMETERS
//===================================================================
xmlChar* get_farewell(xmlDoc *doc, xmlNode * a_node)
{
    xmlNode *cur_node = a_node -> xmlChildrenNode;
   
    while( cur_node != NULL) {
        if (cur_node->type == XML_ELEMENT_NODE) {
            if ((!xmlStrcmp( cur_node->name, 
                             (const xmlChar *)"farewell") )) {
              return xmlNodeListGetString(doc, 
                                          cur_node->xmlChildrenNode, 1);
            }
        }
        cur_node = cur_node->next;
    }
    return (xmlChar*)"";
}

//===================================================================
// HELPER FUNCTION FOR PARSING CODE PARAMETERS FROM XML - LIBXML-2.0 
//===================================================================
xmlChar* parse_xml_and_get_farewell_message(codeparam_t cp)
{
    // XML DOC is built from a Code Parameters string that was passed 
    // to INIT method
    xmlDoc *doc           = xmlReadMemory( *(cp.parameters), 
                                           strlen(*(cp.parameters)), 
                                           "params.xml", 
                                           NULL, 
                                           0);

    // With the root element, we can look for a particular node and its value
    xmlNode *root_element = xmlDocGetRootElement(doc);

    // Retrieve farewell string from XML tree that was built based
    // on Code Parameters stored inside XML that were passed to 
    // the code
    unsigned char* farewell_str = get_farewell(doc, root_element );

    xmlFreeDoc(doc);

    return farewell_str;

}

//===================================================================
//                            INIT
//===================================================================

void code2_setup (codeparam_t code_parameters, int& status_code, std::string& status_message)
{
    printf("\n\n --- Code2 CPP: INIT ---");

    // Code specific initialization actions - in this case, we are reading
    // information from Code parameters
    unsigned char* farewell_str
        = parse_xml_and_get_farewell_message(code_parameters);

    // get value of <farewall> tag
    farewell.assign( (const char*) farewell_str,
                     strlen((const char*)farewell_str));

    std::cout << "\n Farewall text: " << farewell << std::endl;

    // Setting status flag and message
    status_code = 0;
    status_message = "OK";
}


//===================================================================
//                            MAIN
//===================================================================
void code2_step(const IdsNs::IDS::distribution_sources ids_in,
                    IdsNs::IDS::core_profiles& ids_out,
                    codeparam_t code_parameters,
                    int& status_code, std::string& status_message)
{
    printf("  Code2 CPP: MAIN ");

    // Computations specific to the code
    ids_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS;  // Mandatory field
    ids_out.ids_properties.comment = ids_in.ids_properties.comment + " C++  " + farewell + " | ";

    // Setting status flag and message
    status_code = 0;
    status_message = "OK";
}

//===================================================================
//                            FINALIZE
//===================================================================
void code2_cleanup( int& status_code, std::string& status_message)
{
    printf("\n\n --- Code2 CPP: FINALIZE --- \n ");

    // Code specific finalization actions
    farewell.clear();

    // Setting status flag and message
    status_code = 0;
    status_message = "OK";
}
