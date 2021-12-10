#include <memory>
#include "xmlIMAS.h"
#include "codeparam_input_physics.h"

void assign_codeparam(char* codeparam_string, codeparam_physics_data_t &codeparam_data)
{

    // The xml file contents is in codeparam_string.
    // Use libxml2 to parse the contents.
    // XmlImas is a simple class that does that for you,
    // its code is included in this example.

    // Create a xml doc and initialized it with the contents of codeparam_string:
    std::unique_ptr<XmlImas> doc; // libxml2 doc ptr
    doc = std::unique_ptr<XmlImas>(new XmlImas);
    doc->fromMemory(codeparam_string);
    // Extract the parameter values from the xml doc and assign them to codeparam_data:
    std::string xml_path;
    xml_path = "/*/ntimes";
    doc->getValue(xml_path, codeparam_data.ntimes);
    xml_path = "/*/multiplication_factor";
    doc->getValue(xml_path, codeparam_data.multiplication_factor);

}
