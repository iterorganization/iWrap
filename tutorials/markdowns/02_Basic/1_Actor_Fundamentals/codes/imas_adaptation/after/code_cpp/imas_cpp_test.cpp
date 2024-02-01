#include <iostream>
#include <string>
#include <memory>
#include "ALClasses.h"

std::unique_ptr<std::string> greeting()
{
    return std::unique_ptr<std::string>(new std::string("\nHello from C++ IDS!"));
}

/*
 * This is the process of code adaptation to IMAS in a nutshell.
 *
 */
IdsNs::IDS::distribution_sources greetingIDS()
{
    IdsNs::IDS::distribution_sources ids;

    ids.ids_properties.homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS;  // Mandatory field
    auto greetingMsg = greeting();                                      // Use auto for convenience
    ids.ids_properties.comment = *greetingMsg;                          // Dereference the unique_ptr

    return ids;
}

int main(int argc, char* argv[])
{
    std::cout << greetingIDS().ids_properties.comment << std::endl;
}
