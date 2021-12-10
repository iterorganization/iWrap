#include <string>
#include <string.h>
#include <memory>
#include <iostream>
#include <cassert>

#include <libxml/parser.h>
#include <libxml/xpath.h>

#include "xmlIMAS.h"

int XmlImas::_count = 0;

XmlImas::XmlImas()
{
    _initialized = false;
}

XmlImas::XmlImas(const std::string filename)
{

    XmlImas();
    
    // We need to run xmlInitParser only once in the app;
    // also we need to call xmlCleanupParser only once,
    // when we destroy the last object.
    // _count has how many valid objects we have created.
    // Ideally we only increase _count at the end of the c'tor.
    // But the first object has to call xmlInitParser before any other calls,
    // so we increase _count here in that case.
    bool init_parser = false;
    if (_count<1) {
        xmlInitParser();
        init_parser = true;
        ++_count;
    }

    if (!createDocFromFile(filename)) {
        return;
    }
    if (!createContext()) {
        _doc.reset();
        return;
    }
    
    _initialized = true;
    // If we didn't call xmlInitParser, then increase the object count here;
    // otherwise, we already increased the object count above.
    if (!init_parser) ++_count;
    
#ifndef NDEBUG
    std::cerr << __func__ << " " << _count << std::endl;
#endif

}

XmlImas::~XmlImas()
{
    _initialized = false;
    if (_count > 0) --_count;
    if (_count < 1) xmlCleanupParser();
#ifndef NDEBUG
    std::cerr << __func__ << " " << _count << std::endl;
#endif
}

bool XmlImas::fromMemory(const std::string buffer)
{

    // Clean-up ourselves:
    if (_initialized) {
        if (_buffer) _buffer.reset();
        _bsize = 0;
        if (_ctx) _ctx.reset();
        if (_doc) _doc.reset();
        _initialized = false;
        assert(_count > 0);
        --_count;
        if (_count < 1) xmlCleanupParser();
    }

    // Start from the beginning:
    bool init_parser = false;
    if (_count<1) {
        xmlInitParser();
        init_parser = true;
        ++_count;
    }

    if (!createDocFromMemory(buffer)) {
        return false;
    }
    if (!createContext()) {
        _doc.reset();
        return false;
    }
    _initialized = true;
    if (!init_parser) ++_count;
    
#ifndef NDEBUG
    std::cerr << __func__ << " " << _count << std::endl;
#endif

    return true;
}

bool XmlImas::createDocFromMemory(const std::string buffer)
{
    if (_doc) _doc.reset();
    // Create a xmlDoc from the buffer:
    _doc = std::unique_ptr<xmlDoc, t_docDel>
        (xmlParseMemory(buffer.c_str(), buffer.size()), &xmlFreeDoc);
    if (_doc == nullptr) {
        std::cerr << "Error: memory does not seem to contain valid XML.\n";
        return false;
    }
    return true;
    
}

bool XmlImas::createDocFromFile(const std::string filename)
{
    if (_doc) _doc.reset();
    // Create a xmlDoc from the filename:
    _doc = std::unique_ptr<xmlDoc, t_docDel>
        (xmlParseFile(filename.c_str()), &xmlFreeDoc);
    if (_doc == nullptr) {
        std::cerr << "Error: can't open " << filename << " for reading.\n";
        return false;
    }
    return true;   
}

bool XmlImas::createContext()
{
    if (_ctx) _ctx.reset();
    // Create a XPath Context from the xml doc: 
    _ctx = std::unique_ptr<xmlXPathContext, t_ctxDel>
        (xmlXPathNewContext(_doc.get()), &xmlXPathFreeContext);
    if (_ctx == nullptr) {
        std::cerr << "Error: unable to create new XPath context.\n";
        return false;
    }
    return true;
}

const bool XmlImas::getValue(const std::string path, std::string& value)
{
    if (!_initialized) {
        std::cerr << "Not initialized.\n";
        return(false);
    }

    _obj = std::unique_ptr<xmlXPathObject, t_objDel>
        (xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(path.c_str()), _ctx.get()),
         &xmlXPathFreeObject);
    if (_obj == nullptr) {
        std::cerr << "Evaluation of " << path << " failed.\n";
        return(false);
    }
    auto node = std::make_unique<xmlNodeSet>(*_obj->nodesetval);
    if (node == nullptr) {
        std::cerr << "No nodes.\n";
        return(false);
    }
    char* kwrd = nullptr;;
    for (int i=0; i<node->nodeNr; ++i) {
        kwrd = reinterpret_cast<char*>
            (xmlNodeListGetString(_doc.get(),
                                  node->nodeTab[i]->xmlChildrenNode,
                                  1));
        if (strnlen(kwrd, 50)>0) break;
    }
    if (kwrd == nullptr || strnlen(kwrd, 50)<1)
        return(false);
    value = kwrd;
    free(kwrd);
    return(true);
}

const bool XmlImas::getValue(const std::string path, int& value)
{
    std::string raw;
    if (!getValue(path, raw)) return false;
    try {
        value = std::stoi(raw);
    } catch (...) {
        std::cerr << raw << " to int failed.\n";
        value = 0;
        return(false);
    }
    return(true);
}

const bool XmlImas::getValue(const std::string path, float& value)
{
    std::string raw;
    if (!getValue(path, raw)) return false;
    try {
        value = std::stof(raw);
    } catch (...) {
        std::cerr << raw << " to float failed.\n";
        value = 0.0;
        return(false);
    }
    return(true);
}

const bool XmlImas::getValue(const std::string path, double& value)
{
    std::string raw;
    if (!getValue(path, raw)) return false;
    try {
        value = std::stod(raw);
    } catch (...) {
        std::cerr << raw << " to double failed.\n";
        value = 0.0;
        return(false);
    }
    return(true);
}

const std::string* XmlImas::getText()
{

    if (!_initialized) return nullptr;
    if (_buffer) return _buffer.get();
    
    xmlChar* buffer = NULL;
    int size = 0;
    xmlDocDumpMemory(_doc.get(), &buffer, &size);
    if (buffer == NULL || size == 0) return nullptr;

    _buffer = std::make_unique<std::string>(std::string(reinterpret_cast<const char*>(buffer)));
    xmlFree(buffer);
    _bsize = size;
    return _buffer.get();

}

