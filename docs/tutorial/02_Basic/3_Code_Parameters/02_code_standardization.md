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

# Code configuration to read Code Params 

+++

```{admonition} What will you learn in this lesson
:class: note

1. How to add `code parameters` in your iWrap-compatible methods
2. How to properly parse data from XML files into IDS
```

+++

```{admonition} Step 1. Read values from code parameters file
:class: tip
In the most cases you will use your `code parameters` files in the `setup` process of your code (`INIT` method).
```


::::{tab-set}

:::{tab-item} Fortran
```fortran
!===================================================================
!                            INIT
!===================================================================
subroutine code1_setup (code_parameters, status_code, status_message)
    use ids_schemas, only: ids_parameters_input
    use xml2eg_mdl,  only: xml2eg_parse_memory, xml2eg_get, type_xml2eg_document, xml2eg_free_doc, set_verbose
    implicit none

    type(ids_parameters_input), intent(in) :: code_parameters
    integer, intent(out) :: status_code
    character(len=:), pointer, intent(out) :: status_message

    ! This variable will contain the string from code parameters
    character(len=50) :: greeting_string

    ! This is a tree structure that is built from XML string
    ! This structure can be later on traversed with 'get' methods
    type(type_xml2eg_document) :: doc

    write(*,*) "= = = = = Code1 Fortran: INIT = = = = ="


    ! Parse the string with code parameters and save the result inside doc
    call xml2eg_parse_memory(code_parameters%parameters_value,doc)

    ! Get the value of node 'greeting' and store it inside greeting_string
    ! variable; we can use it later on
    call xml2eg_get(doc,'greeting',greeting_string)

    ! Clean up after data are retrieved; This step can be done at the very
    ! end of your code as well.
    call xml2eg_free_doc(doc)

    ! Code specific initialization actions
    write(*,*) code_parameters%parameters_value
    ! SET GREETING TO value of <greeting> tag
    ! We will use this value later on inside code1_step routine
    allocate(character(60):: greeting)
    greeting = "CODE1: " // greeting_string

    ! Setting status to SUCCESS
    status_code = 0
    allocate(character(50):: status_message)
    status_message = 'OK'

end subroutine code1_setup



```
:::

:::{tab-item} C++


```c
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

```
:::
::::


`````{admonition} Step 2. Use imported values in code
:class: hint

Then we can use such imported value from **code parameters** in our `MAIN` method


::::{tab-set}

:::{tab-item} Fortran

```fortran
!===================================================================
!                            MAIN
!===================================================================
subroutine code1_step(ids_in, ids_out, code_parameters, status_code, status_message)

    use ids_schemas, only: ids_core_profiles, ids_distribution_sources, ids_parameters_input
    use ids_routines

    implicit none

    type(ids_core_profiles):: ids_in
    type(ids_distribution_sources):: ids_out
    type(ids_parameters_input), intent(in) :: code_parameters
    integer, intent(out) :: status_code
    character(len=:), pointer, intent(out) :: status_message


    write(*,*) "= = = = = Code1 Fortran: MAIN = = = = ="

     ! Computations specific to the code    
    ids_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS ! Mandatory field

    if (.not. associated(ids_out%ids_properties%comment)) allocate(ids_out%ids_properties%comment(1))   
    ids_out%ids_properties%comment(1) = trim(ids_in%ids_properties%comment(1)) // trim(greeting) // " - "

    ! Setting status flag and message
    status_code = 0
    allocate(character(5):: status_message)
    status_message = 'OK'

end subroutine code1_step


```
:::

:::{tab-item} C++

```c
//===================================================================
//                            MAIN
//===================================================================
void code2_step(const IdsNs::IDS::distribution_sources ids_in,
                    IdsNs::IDS::core_profiles& ids_out,
                    codeparam_t code_parameters, 
                    int& status_code, std::string& status_message)
{
    printf(" = = = = = Code2 CPP: MAIN = = = = =\n");

    // Computations specific to the code
    ids_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS;  // Mandatory field
    ids_out.ids_properties.comment = ids_in.ids_properties.comment + "CODE2: " + farewell + " - ";

    // Setting status flag and message
    status_code = 0;
    status_message = "OK";
}
```
:::
::::







`````



`````{admonition} Step 3. Clean environment
:class: hint

And the last thing is to clean everything up:


::::{tab-set}

:::{tab-item} Fortran

```fortran
!===================================================================
!                            FINALIZE
!===================================================================
subroutine code1_cleanup(status_code, status_message)
    implicit none
    integer, intent(out) :: status_code
    character(len=:), pointer, intent(out) :: status_message

    write(*,*) "= = = = = Code1 Fortran: FINALIZE = = = = ="

    ! Code specific finalization actions
    deallocate(greeting)
    greeting => null()

    ! Setting status flag and messageE
    status_code = 0
    allocate(character(5):: status_message)
    status_message = 'OK'

end subroutine code1_cleanup

```
:::

:::{tab-item} C++

```c
//===================================================================
//                            FINALIZE
//===================================================================
void code2_cleanup( int& status_code, std::string& status_message)
{
    printf(" = = = = = Code2 CPP: FINALIZE = = = = =\n");

    // Code specific finalization actions
    farewell.clear();

    // Setting status flag and message
    status_code = 0;
    status_message = "OK";
}
```
:::
::::



`````


```{admonition} Summary
:class: note

Voil'a! You now know how to use `code parameters` in your iWrapped code!

In the next section You will learn how to expand YAML file with `code parameters` settings
```