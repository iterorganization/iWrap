// =======================================
//             HELPER FUNCTIONS
// =======================================

#include "ParametersUtils.h"

#include <boost/algorithm/string.hpp>
#include <memory>
#include <stdexcept>
#include "json/json.h"
#include "xmlIMAS.h"
#include <stdexcept>
#include <tuple>
#include <iostream>

std::tuple<int,double> extract_xml_values(std::string parameters)
{
    int ntimes = 0;
    double multiplication_factor = 0.0;

    std::unique_ptr<XmlImas> doc; // libxml2 doc ptr
    doc = std::unique_ptr<XmlImas>(new XmlImas);
    doc->fromMemory(parameters);

    doc->getValue("/*/ntimes", ntimes);
    doc->getValue("/*/multiplication_factor", multiplication_factor);

    return std::make_tuple(ntimes,multiplication_factor);
}

std::tuple<int,double> extract_json_values(std::string parameters)
{
    const auto rawJsonLength = static_cast<int>(parameters.length());
    JSONCPP_STRING err;
    Json::Value root;

    Json::CharReaderBuilder builder;
    const std::unique_ptr<Json::CharReader> reader(builder.newCharReader());
    if (!reader->parse(parameters.c_str(), parameters.c_str() + rawJsonLength, &root, &err))
    {
        throw std::runtime_error("Cannot open JSON parameters file");
    }

    const int ntimes = root["parameters"]["ntimes"].asInt();
    const double multiplication_factor = root["parameters"]["multiplication_factor"].asFloat();

    return std::make_tuple(ntimes,multiplication_factor);
}

std::tuple<int,double> extract_values(std::string parameters)
{
    // This is general function for ntimes and multiplication_factor parameters extraction
    boost::algorithm::trim(parameters);

    std::tuple<int,double> ntimes_multiplicationFactor = std::make_tuple(1,1.0);
    if(parameters=="")return ntimes_multiplicationFactor;

    if(parameters[0] == '<')
    {
        ntimes_multiplicationFactor = extract_xml_values(parameters);
    }
    else if(parameters[0] == '{')
    {
        ntimes_multiplicationFactor = extract_json_values(parameters);
    }
    else if(parameters[0] == '&')
    {
        std::cerr<<"Fortran namelist parameters are supported only by fortran-based actors"<<std::endl;
    }
    else
    {
        std::cerr<<"Could not recognise parameters format"<<std::endl;
    }

    return ntimes_multiplicationFactor;
}