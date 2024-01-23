---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.16.0
kernelspec:
  display_name: Bash
  language: bash
  name: bash
---

# Managing Code Parameters in Worfklow




```{admonition} Documentation
:class: attention

Official IMAS documentation for Code Parameters is [here](https://confluence.iter.org/display/IMP/Getting+Started#GettingStarted-Level2:Addanxmlfileforuser-definedcodeparameters)

Official iWrap documentation of the [Actor API related to Code Parameters](https://sharepoint.iter.org/departments/POP/CM/IMDesign/Code%20Documentation/IWRAP-doc/resources/actor_usage.html#physics-model-parameters) before continuing.
```


## Accessing Code Parameters

These parameters, typically set as file paths during the generation of an actor, **can be modified at runtime**.   

But direct access to `code_parameters` is not possible. Instead, you must use a special getter method provided for this purpose:

```python
def get_code_parameters(self) -> CodeParameters:
    ...
```


The `CodeParameters` class includes several key attributes:

- `parameters (string)`: These are your XML parameters. This attribute is **read-only**.
- `schema (string)`: This is the XML schema used for XML validation. It is also **read-only**.
- `parameters_path (string)`: This is the path to your XML file. Unlike the other attributes, **this can be set by the user to overwrite the default XML parameters**.



### Methods for Single Parameter Manipulation

You have two primary methods at your disposal for interacting with XML nodes:

````{admonition} 1. **Get Parameter**:
:class: note

```{admonition} Tip!
:class: tip

Use this method to **retrieve the value of an XML node**, which is identified by the `path/to/node`.
```

```python
def get_parameter(self, path_to_node: str) -> str:
    ...
```
````

````{admonition} 2. Set Parameter: 

```{admonition} Tip!
:class: tip

This method allows you to **set a new value for an XML node** specified by  `path/to/node`.
```

```python
def set_parameter(self, path_to_node: str, value: str) -> str:
    ...
```


```` 

#### Navigating XML Nodes
When specifying `path_to_node`, it's formatted with XML node names separated by the `/` character.
````{admonition} Tip!
:class: tip

To access the `n-th` node within a group of nodes, use the `()` operator.  
 
 For example  this snippet accesses the third `multiplication_factor` node (counting starts from `0`).
 
```python  
  code_parameters.get_parameter('parameters/multiplication_factor(3)')
```

````

#### Setting Parameters with Objects or Lists
The `set_parameter` method is **versatile**.   
It can take any object that can be converted to a `string`.   
Moreover, you can pass a list of objects, all convertible to `strings`.     
Such a list will be transformed into a **space-separated** string value.

### Overwriting the Default XML File 

`````{admonition} Tip!
:class: tip

This method allows for the dynamic modification of the default XML configuration file path in `code_parameters`.    
This is useful for changing configurations at runtime, especially in different environments or testing scenarios."

````{admonition} Note!

To overwrite the existing path, assign a new file path directly to `code_parameters.parameters_path`, as shown: 
```python
code_parameters.parameters_path = '/gss_efgw_work/scratch/username/tmp/xml_new_location.xml'
```

````
`````


```{admonition} Wrapping up
:class: caution

1. The **schema (XSD)** of code_parameters, **once set initially, remains the same throughout the actor's lifecycle.**

2. The **user can only modify the values of fields (in XML)** and can do so in two ways:
    - By replacing a **single value** using `get_parameter` and `set_parameter` methods.
    - By replacing the **entire XML** file using the `parameters_path` attribute." 
 ```

```{admonition} Validation of Code Parameters
:class: important

It's important to note that code parameters are **validated when the actor's `initialize` method is called**.

This ensures that your parameters meet the necessary criteria **before the actor is used**.
```



So in this lesson we have provided four Python workflows in `codes` folder that demonstrate how to use code parameters in different ways:

1. The first workflow does not modify any code parameter values via the Actor API, relying instead on the default settings specified in the actor's XML file.  

2. The second workflow employs the `get_parameter` and `set_parameter` methods to alter the value of an individual XML node.
3. The third workflow utilizes the `parameters_path` attribute to replace the entire XML configuration file in **one actor**   
    It uses a different XML file, which is located in the `codes/iWrapped_codes/code_parameters/input_with_awesomer_values` folder.

4. The fourth workflow uses the above XML to change values in **two actors**.

```{code-cell}
cat codes/iWrapped_codes/code_parameters/input_with_awesomer_values.xml
```

Look at the differences in workflow codes:
`````{tab-set}

````{tab-item} Workflow 1

```python
# Creation of actors
actor1_fortran = actor1_fortran()
actor2_cpp = actor2_cpp()

# Initialization of actors
actor1_fortran.initialize()
actor2_cpp.initialize()

```

````

````{tab-item} Workflow 2
```python
# Creation of actors
actor1_fortran = actor1_fortran()
actor2_cpp = actor2_cpp()

### ADDED
# Code parameters single node for actor1_fortran editing
code_parameters = actor1_fortran.get_code_parameters()

#### METHOD USAGE
code_parameters.set_parameter('parameters/greeting', "Hallo!" )

### ADDED PARAMETER
# Initialization of actors
actor1_fortran.initialize(code_parameters=code_parameters)
actor2_cpp.initialize()

```
````

````{tab-item} Workflow 3

```python
# Creation of actors
actor1_fortran = actor1_fortran()
actor2_cpp = actor2_cpp()

### ADDED
# Code parameters entire file for actor1_fortran editing
code_parameters = actor1_fortran.get_code_parameters()

#### METHOD USAGE
code_parameters.parameters_path= 'codes/iWrapped_codes/code_parameters/input_with_awesomer_values.xml'

### ADDED PARAMETER
# Initialization of actors
actor1_fortran.initialize(code_parameters=code_parameters)
actor2_cpp.initialize()
```
````

````{tab-item} Workflow 4
```python
# Creation of actors
actor1_fortran = actor1_fortran()
actor2_cpp = actor2_cpp()

# Code parameters entire file for actor1_fortran editing
code_parameters = actor1_fortran.get_code_parameters()
code_parameters.parameters_path= 'codes/iWrapped_codes/code_parameters/input_with_awesomer_values.xml'

# Code parameters entire file for actor2_cpp editing
code_parameters = actor2_cpp.get_code_parameters()
code_parameters.parameters_path= 'codes/iWrapped_codes/code_parameters/input_with_awesomer_values.xml'

# Initialization of actors
actor1_fortran.initialize(code_parameters=code_parameters)
actor2_cpp.initialize(code_parameters=code_parameters)
```
````
`````

So now let's run these workflows and see the **differences in results**:

```{code-cell}
python3 codes/workflow-1-default.py
```

```{code-cell}
python3 codes/workflow-2-single-node.py
```

```{code-cell}
python3 codes/workflow-3-entire-file.py
```

```{code-cell}
python3 codes/workflow-4-entire-file-two-actors.py
```