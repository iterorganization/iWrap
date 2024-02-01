---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.16.0
kernelspec:
  display_name: Python 3 (ipykernel)
  language: python
  name: python3
---

# Tutorial for beginners - Step 3 - Introduction

+++

```{admonition} What we have learned so far vs. What we will learn in the future
:class: hint

In the previous section you have learned how to: 
- construct and use iWrap methods: `INIT`, `MAIN`, `FINALIZE`
- compile, package, create actors using commands and makefiles
- prepare YAML configuration manually or using GUI 
- use actors in workflow

Now in this section you will learn: 
- what are **code parameters**, and how they are structured
- why to use them instead of hardcoding values in a code
- how to put them in a iwrap code
```

```{admonition} Codes used in this chapter.
Below is the repo layout of codes used in this chapter:
```

```{code-cell}
!find . -type d -name 'codes' | xargs tree
```

## Code parameters Introduction - `XML` & `XSD` quick reminder


````{admonition} Important!
:class: important

More about code parameters is described on official pages [here](https://confluence.iter.org/display/IMP/Getting+Started#GettingStarted-Level2:Addanxmlfileforuser-definedcodeparameters)

> Parameters to configure the code for a specific simulation are constant variables (they do not vary during the simulation), hence they can be stored in a file.  
> We chose xml input file to store code parameters since it can be associated to an xsd file, which defines results and provides definitions for each variable, and it can be interpreted by various languages to e.g. generate a GUI. An example of such a GUI is the H&CD workflow.

> Each xml file is associated to and xsd file that defines rules, restrictions and definitions for the code parameters. 

```{admonition} Note!
Note that writing an xsd file associated to the xml code parameters files is mandatory for the iWrap compilation:   
***iWrap will first check that the two files (xml and xsd) are consistent, and then it will proceed with the compilation.***
```

````

```{admonition} Attention!
:class: attention

In this section we will use these files. 
**You can name them as you want.**
```
```{admonition} XML
:class: hint

`XML` is a markup language that defines a set of rules for encoding documents in a format that is both human-readable and machine-readable.   
It's primarily used to transport and store data.
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<parameters>

    <!-- Greeting in chosen spoken language -->
    <greeting> Hola! </greeting>

    <!-- Farewell in chosen spoken language -->
    <farewell> Adios! </farewell>

</parameters>

```




```{admonition} XSD
:class: hint

`XSD` (XML Schema Definition) is a schema language for `XML`.   
`XSD` files are used to validate the structure and content of an `XML` file.
```

```xml
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
           elementFormDefault="qualified">
    <xs:annotation>
        <xs:documentation>Code parameters for iWrap tutorial</xs:documentation>
    </xs:annotation>

    <!-- document element -->
    <xs:element name="parameters">
        <xs:complexType>
            <xs:all>
                <xs:element name="greeting" type="xs:string" minOccurs="1" maxOccurs="1"/>
                <xs:element name="farewell" type="xs:string" minOccurs="1" maxOccurs="1"/>
            </xs:all>
        </xs:complexType>
    </xs:element>

    
</xs:schema>
 
```



```{admonition} Their Relationship
:class: note  

1. `XSD` Ensures that the data in XML files meet the predefined criteria, helping to maintain data consistency.
2. `XSD` specifies what types of data are allowed in specific XML elements and attributes.

+++

```{admonition} How to read XSD and XML files
:class: dropdown

1. **How to Read XML**
- `<greeting> Hello! </greeting>`: The greeting tag contains a string "Hello!".
- `<farewell> Bye! </farewell>`: The farewell tag contains a string "Bye!".

2. **How to Read XSD**
- `<xs:element name="greeting" type="xs:string" minOccurs="1" maxOccurs="1"/>`: This line in the XSD defines that there should be an element called `greeting` in the XML file.
- `type="xs:string"`: Data inside should be of string type.
- `minOccurs="1" and maxOccurs="1"`: The element must occur at least once and at most once.

- `elementFormDefault="qualified"`: Elements must be qualified by the namespace (in this case, "xs").
- `<xs:all>`: The elements can appear in any order, but they should appear exactly once.

```{note}
The XML structure is compliant with the XSD schema because it has both "greeting" and "farewell" tags, and both contain string data, fulfilling the conditions set by `minOccurs` and `maxOccurs` attributes.

```

+++

```{admonition} Why to use **code parameters**?
:class: attention

1. **Ease of update**

Changing a hardcoded value requires a change in the source code, which necessitates recompiling and redeploying the application. On the other hand, a value in an XML file can be changed without touching the code, reducing the deployment effort.

2. **Version Control**  

When configuration is in separate files, those files can also be version-controlled independently of the code. This provides a detailed history of configuration changes over time.

3. **Reusability**  

XML/XSD files can be reused across multiple projects or modules, thereby reducing duplication and enhancing consistency.

4. **Centralized Management**  

In larger systems, having a single, or a few, centralized configuration files can make management more straightforward. Multiple components can read their settings from a common source, ensuring uniformity.

5. **Validation**

With XSD, XML files can be validated for correctness before they are read by the application. This adds an additional layer of security and data integrity.
```

```{admonition} Code Parameters in Actor's Lifetime
:class: caution

When writing an actor, the developer assumes a specific XML structure.  
For instance, **certain parameters must be present in the XML**; otherwise, the code **may not function correctly or might even crash**.   
Therefore, even if the actor's user makes changes, the XML must comply with the structure initially envisioned by the developer.   
This compliance is ensured through validation with an XSD schema.
```

+++

```{admonition} Ready to go!
:class: hint

Now that we understand the value of experimenting with **code parameters**, let's proceed to the next lesson!
```
