#ifndef _XMLIMAS_H_
#define _XMLIMAS_H_

#include <string>
#include <memory>
#include <functional>
#include <libxml/parser.h>
#include <libxml/xpath.h>

///
/// \brief Minimal class to read values from XPaths
///
/// Initialize it with a xml filename and call the method
/// getValue with a XPath and the variable to store the value.
/// Return value is true if ok or false if something failed.
///
/// You can also initialize it with a std::string that contains a full xml.
/// Use fromMemory for that.
///
/// getText returns a pointer to a std::string with the xml file contents.
///
/// Uses libxml2.
///

class XmlImas {
 public:
    XmlImas();
    XmlImas(const std::string s);
    ~XmlImas();
    const bool getValue(const std::string s, std::string& v);
    const bool getValue(const std::string s, int& v);
    const bool getValue(const std::string s, float& v);
    const bool getValue(const std::string s, double& v);
    const std::string* getText();
    const bool getStatus(){ return _initialized; };
    bool fromMemory(const std::string buffer);
    
 private:

    bool createDocFromMemory(const std::string buffer);
    bool createDocFromFile(const std::string filename);
    bool createContext();
    
    static int _count;
    bool _initialized;
    std::unique_ptr<std::string> _buffer;
    int _bsize;

    // We use unique_ptr to the xml structs and assign
    // the deletors too. This way they are handled automatically.
    // First some shortcuts:
    using t_docDel = std::function<void(xmlDocPtr)>;
    using t_ctxDel = std::function<void(xmlXPathContextPtr)>;
    using t_objDel = std::function<void(xmlXPathObjectPtr)>;
    // And now the declarations:
    std::unique_ptr<xmlDoc, t_docDel> _doc;
    std::unique_ptr<xmlXPathContext, t_ctxDel> _ctx;
    std::unique_ptr<xmlXPathObject, t_objDel> _obj;
    std::unique_ptr<xmlNodeSet> _node;
};

#endif //_XMLIMAS_H_
