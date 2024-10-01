#include <tuple>
#include <string>

// =======================================
//             HELPER FUNCTIONS
// =======================================
std::tuple<int,double> extract_xml_values(std::string parameters);
std::tuple<int,double> extract_json_values(std::string parameters);
std::tuple<int,double> extract_values(std::string parameters);