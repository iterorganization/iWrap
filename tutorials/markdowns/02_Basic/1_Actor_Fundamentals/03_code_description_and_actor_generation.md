---
jupytext:
   text_representation:
      extension: .md
      format_name: myst
      format_version: 0.13
      jupytext_version: 1.16.0
kernelspec:
   display_name: bash
   language: bash
   name: bash
---


# 1. Actor and  code description

Please [refer to the docs](https://sharepoint.iter.org/departments/POP/CM/IMDesign/Code%20Documentation/IWRAP-doc/reproject_description.html).

```{admonition} What will you learn in this section
:class: note
1. How to tell iWrap about your iWrapfied code
2. What is the structure of configuration file
3. What are the mandatory fields of this file
4. How to properly created this config manually and using iwrap GUI
5. How to generate Python actor with CLI and GUI
```

## 1.1. How to tell iWrap about your **iWrap adapted** code

So iWrap needs a **configuration file** that holds information about your iWrap adapted methods.

This file is in **YAML** format.

It can be generated in two ways:
- **manually** by hand
- using **iWrap GU**I by filling proper information to boxes

```{admonition} Important! Fields are named the same in both ways!
:class: attention

So you can use the same configuration in `CLI` or `GUI`
```






## 1.2. What does iWrap need to know from configuration file

1. **Details about the Actor**: This is an output created by iWrap. iWrap needs to know its name, where to save it, and more.
2. **Details about Your Code**: Things like the programming language, the information being passed in and out, and the type of that information.


## 1.3. How does the configuration look?

Your configuration is split into two parts:
1. **Actor Description**: This is *optional*. It tells iWrap about the actor it's making.
2. **Code Description**: This is ***essential***. It tells iWrap about your code.

```{admonition} Caution!
:class: caution  

The following section details all the parameters and explains how to configure them in **either the GUI or directly in YAML file**.   
The examples provided below illustrate the *same settings being applied through these two different methods* for two codes, one in Fortran and one in C++.



```
## 1.4. Actor description
The actor is what iWrap creates based on your code.

- `actor_name`: the arbitrary, **user defined** name of the actor.

  It determines e.g. : **the name of class to be generated and directory where actor will be put**. For example: `barometry_actor`.

    ```{admonition} Actor name is mandatory!
    :class: caution

    It is essential to specify the `actor_name` to initiate the construction of an actor accurately.   
    This can be done either through the YAML file or via the GUI interface,** but it is a mandatory requirement**.
  
    If you don't specify the `actor_name`, iWrap will throw an error durin actor creation and stop the generation process.  
  
    ![](../../../sources/images/actor_name_error.png)
    ```

    ```
- `actor_type`: right now we will focus on the simple type `python` (other types are available throught the use of plugins but this will be covered in advanced parts of the tutorial.
- `data_type`: data type handled at the workflow level. Right now, only the type `legacy` (i.e static IDS structures) type has been implemented.


`````{tab-set}

````{tab-item} GUI 

![actor_description.png](../../../sources/images/actor_description.png)

````


````{tab-item} YAML 

```yaml
actor_description:
    actor_name: actor1_fortran
    actor_type: python
    data_type: legacy
```
````

`````

If you want to practice with the GUI, let's open it first:


```{code-cell}
:tags: [skip-execution]

iwrap-gui
```

## 1.5.  code description
This section tells iWrap about the program you've written.

````{admonition} Attention
:class: attention

Code description is devided into three main parts:
1. Arguments
2. implementation
3. settings 

```yaml
code_description:
    arguments:
        # some key-value pairs
        
    implementation:
        # some key-value pairs
        
    settings:
        # some key-value pairs
```
````



### 1.5.1. Arguments part:

These are parameters used in this part:

- `arguments`: List of methods arguments that your code needs or produces.
    - `name`:  user defined argument name (`barometry00`)
    - `type`: a type of an predefined IDS (`barometry`)
    - `intent`: determines if given argument is input or output one (`IN`/`OUT`)

`````{admonition} GUI explanation: Click Me! 👈
:class: dropdown


```{admonition} Arguments tab
:class: hint 

The table contains the following columns **[3]**:

- `Name`: A user-specified identifier for an argument.
- `Input/Output`: Classifies the argument as either input or output. {`IN`/`OUT`
- `Type`: Specifies the argument's IDS-based type, such as `equilibrium, barometry`, and so on.
```

````{admonition} Arguments control pane
:class: hint

Adjacent to the table on the right, there's a control panel featuring buttons **[4]**:

- `Add`: Introduces a new argument to the table.
- `Edit`: Modifies the attributes of an existing argument.
- `Up/Down`: Adjusts the rank or position of a selected argument in the table.
- `Remove`: Deletes the chosen argument from the table.


```{admonition} Adding New Arguments
:class: note



1. Navigate to the **Arguments** section and click on the `Add…` button.
2. This action will open a window titled **iWrap - Add new argument**.
   - Provide a name for the argument. (Note: An empty name is acceptable.)
   - Choose the appropriate `intent` and `type`.
3. To finalize and add the new argument, press the `Add` button.
4. To exit without any changes, simply click the `Cancel` button.

![add_arg.png](../../../sources/images/add_arg.png)
```
```{admonition}  Editing Arguments
:class: note


1. To modify an argument, click the `Edit…` button located in the **Arguments** section.
2. The **iWrap - Edit argument** window will appear.
   - Adjust the settings as necessary.
3. Confirm and save your changes by pressing the `Close` button.
4. To exit without any changes, click the `Cancel` button.
```
````


```{admonition} Change button state to **active**
:class: attention  

To change `Edit, Up/Down,` and `Remove`` buttons state to **active** - at least *one argument has to be added to the table*.

```

`````



#### 1.5.1.1. Fortran example

`````{tab-set}

````{tab-item} YAML 
```yaml
code_description:
    arguments:
    -   name: core_profiles_in
        type: core_profiles
        intent: IN
    -   name: distribution_sources_out
        type: distribution_sources
        intent: OUT
```

````

````{tab-item} GUI 
![code_description.png](../../../sources/images/code_description.png)     



````

`````



#### 1.5.1.2. C++ example


``````{tab-set}

````{tab-item} YAML 
```yaml
code_description:
    arguments:
    -   name: distribution_sources_in
        type: distribution_sources
        intent: IN
    -   name: core_profiles_out
        type: core_profiles
        intent: OUT
```
````

````{tab-item} GUI 
![actor_description.png](../../../sources/images/code_description_cpp.png)

````


``````


### 1.5.2. Implementation part

These are parameters used in **implementation** part:

- `implementation`:
    - `programming_language`: The language of your code, one of `Fortran` or `CPP`.
    - `data_dictionary_compliant`: The oldest version of Data Directory your actor can work with, e.g., `3.37.0`.
    - `subroutines`: These are procedures your actor can perform.
        - `main`: The main task. **The name here is also used as the actor's name and installation directory.**
    - `code_path`: Where the main code file is located, e.g., `./iWrapped_codes/code1_fortran/libcode1.a`.
    - `include_path`: Where the header or module file is, like `./iWrapped_codes/code1_fortran/mod_code1.mod`.
      note

         ```{admonition} Note
         :class: note
         
         Fortran doesn't care about uppercase or lowercase, **but the compiler does.**   
         Check the module name in the YAML matches the compiled one!
             
         ```


````{admonition} GUI - Implementation tab
:class: hint

This tab consists of three sections:
- **Implemenetation** [2]
- **Subroutines** [3]- Methods names in your  code
- **Code parameters** [4]

````

#### 1.5.2.1. Fortran Example

`````{tab-set}

````{tab-item} YAML
```yaml
code_description:
    implementation:
        subroutines:
            main:     code1_step
        code_path:      ./iWrapped_codes/code1_fortran/libcode_fortran.a
        include_path:   ./iWrapped_codes/code1_fortran/mod_code1.mod
        programming_language: fortran
        data_dictionary_compliant: 3.37.0
        data_type: legacy
```

````
````{tab-item} GUI 
![implementation_tab.png](../../../sources/images/implementation_tab.png)
````




`````

#### 1.5.2.2. C++ Example


```````{tab-set}


````{tab-item} YAML 
```yaml
code_description:
    implementation:
        subroutines:
            main:     code2_step
        code_path:      ./iWrapped_codes/code2_cpp/libcode_cpp.a
        include_path:   ./iWrapped_codes/code2_cpp/code2.h
        programming_language: cpp
        data_dictionary_compliant: 3.37.0
        data_type: legacy
```
````

``````{tab-item} GUI 
![implementation_tab.png](../../../sources/images/implementation_tab_cpp.png)
``````

```````





## 1.6. Language-Specific Settings

Specific settings based on the programming language of  code.


- `Compiler cmd` - the compiler command used to compile  codes `gfortran/ g++`

```{admonition} GUI - Warning!
:class: warning
To enable `settings` tab you need to choose **programming language** on **implementation** tab firtst!

![settings_empty.png](../../../sources/images/settings_empty.png)
```


### 1.6.1. Fortran Example

`````{tab-set}

````{tab-item} YAML 
```yaml
code_description:
    settings:
        compiler_cmd: gfortran
```

````

````{tab-item} GUI

![settings_not_empty.png](../../../sources/images/settings_not_empty.png)

````



`````

### 1.6.2. C++ Example



`````{tab-set}


````{tab-item} YAML
```yaml
code_description:
    settings:
        compiler_cmd: g++
```
````

  



````{tab-item} GUI

![settings_not_empty.png](../../../sources/images/settings_not_empty_cpp.png)

````

`````



## 1.7. Final YAML file



So the Final YAML files look like this:
```{admonition} Important!
:class: caution

The YAML content is the same whether it was implemented manually or the result of a saved settings by the GUI.
```


`````{tab-set}

````{tab-item} YAML

```yaml
actor_description:
    actor_name: actor1_fortran
    actor_type: python
    data_type: legacy
code_description:
    implementation:
        subroutines:
            main:     code1_step
        code_path:      ./iWrapped_codes/code1_fortran/libcode_fortran.a
        include_path:   ./iWrapped_codes/code1_fortran/mod_code1.mod
        programming_language: fortran
        data_dictionary_compliant: 3.37.0
        data_type: legacy
    arguments:
    -   name: core_profiles_in
        type: core_profiles
        intent: IN
    -   name: distribution_sources_out
        type: distribution_sources
        intent: OUT
    settings:
        compiler_cmd: gfortran
```
````


````{tab-item} YAML

```yaml
actor_description:
    actor_name: actor2_cpp
    actor_type: python
    data_type: legacy
code_description:
    implementation:
        subroutines:
            main:     code2_step
        code_path:      ./iWrapped_codes/code2_cpp/libcode_cpp.a
        include_path:   ./iWrapped_codes/code2_cpp/code2.h
        programming_language: cpp
        data_dictionary_compliant: 3.37.0
        data_type: legacy
    arguments:
    -   name: distribution_sources_in
        type: distribution_sources
        intent: IN
    -   name: core_profiles_out
        type: core_profiles
        intent: OUT
    settings:
        compiler_cmd: g++
```
````
`````
## 1.8. Actor Generation

### 1.8.1. Using CLI



```{code-cell}
:tags: [output_scroll, hide-output]

iwrap -h
```



```{admonition} Hint!
:class: hint

You should see this line at the end of logs:

    -------------------- GENERATION COMPLETE! --------------------
    ALL DONE!
        
        

```



- **Fortran**

```{code-cell}
:tags: [output_scroll, hide-output]


iwrap --actor-type python --actor-name actor1_fortran --file codes/actor1_fortran.yaml
```

- **C++**

```{code-cell}
:tags: [output_scroll, hide-output]

iwrap --actor-type python --actor-name actor2_cpp --file codes/actor2_cpp.yaml
```




### 1.8.2. Using GUI

```{admonition} Actor Generation using GUI
:class: attention

To generate actor  with all of the mandatory fields filled you need to click th `Generate` button.  [1]  
New window with generation logs will pop up. [2]


```

![actor_generation_gui.png](../../../sources/images/actor_generation_gui.png)




```{admonition} Summary
:class: note  

**What we have done so far:**
- created YAML files that describes our  codes manually and using GUI!
- generated iWrap actors using CLI and GUI!

**What is next?**  
- We would use these actors in Python workflow script!
```
