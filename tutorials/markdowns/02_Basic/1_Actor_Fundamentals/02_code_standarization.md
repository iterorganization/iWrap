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

# Explanation of iWrap  code implementation standards - part I

+++

```{admonition} What will you learn in this lesson
:class: note

1. What is the most basic iWrap API with only **mandatory** arguments explained
2. How to (re)design you **IMAS adapted** code to be **iWrap compatible**
3. How to compile iWrap code
4. Why you need to package your code to properly use iWrap actors
5. How to package your iWrap code into **Linux Library**

```
```{admonition} Documentation link
Docs is available [here](https://sharepoint.iter.org/departments/POP/CM/IMDesign/Code%20Documentation/IWRAP-doc/resources/_code.html) 
```
```{admonition} Caution!
:class: caution

***To use iWrap, your code must be rewritten in a specific way, often referred to as a `API`.***  
***If iWrap doesn't recognize this API, it won't be able to create what we call an `actor`.***  

You need properly (re)design your  code to interact with iWrap actor
```

+++

## The `MAIN` Method

```{admonition} Caution!
:class: caution

The "MAIN" method is a **mandatory** part of your original code. Here's what you need to know:
```
- **When to Use** - ***ALWAYS!*** - it's mandatory.  
- **How Often Can It Be Run?**: As many times as you want, like in a repeating loop.  
- **What Can You Name It?**: Any name you like!  Just remember this name for later.


### What this method needs (its arguments):



``````{admonition} IDS argument
:class: tip


`````{admonition} Note! This argument is listed first, because it **MUST** be listed first on method call!
:class: note
    
    1. **Input and Output IDSes:**
    - Do you have to include this? **Yes, it's mandatory**
    - How it works: These are **either** inputs (IN) or outputs (OUT) for the method.

 
```{admonition} Explanation of **IN, OUT and INOUT** arguments.
:class: dropdown

`IN, OUT and INOUT` are the arguments that are passed to the procedures.

- `IN` is a 'read only' argument and must be initialized.
- `OUT` is an uninitialized argument which must be initialized by a function.
- `INOUT` - A combination of the two above. That is, an initialized argument which can be written to
```

``````


1. **Status Code**:
   - Do you have to include this? **Yes, it's a mandatory**.
   - What kind of data? A number (an integer).
   - How it works: The method will give this number as an output to let you know how things went.

2. **Status Message**:
   - Do you have to include this? **Yes, it's a mandatory**.
   - What kind of data? A text string.
   - How it works: The method will provide this text message to give you more details about what happened.

+++

``````{admonition} How to construct methods

When writing your code to be iWrapped, **use procedures (also known as subroutines)** in *ANY* programming language.   
Avoid using `functions`.

`````{admonition} **Procedure** vs **Function**
:class: dropdown


The primary difference between them is that:


    
````{admonition} Procedures (aka Subroutines)
:class: tip
- `Procedures` (often called `Subroutines` in some languages) **do not return a value**.   


```cpp
void displayMessage() {
    std::cout << "Hello, World!" << std::endl;
}

int main() {
    displayMessage();  // prints "Hello, World!" to the console
    return 0;
}

```
````    


````{admonition} Functions
:class: tip
- `Functions` **return a value**.

```cpp
int add(int a, int b) {
    return a + b;
}

int main() {
    int result = add(3, 4);  // result gets the value 7
    return 0;
}

```

````

`````
``````



````{admonition} Naming Rules
:class: caution

**Method naming**: You can name your subroutines whatever you want.   
**Arguments naming:**

1. List them in a specific order.
2. Arguments are designated as either **input** or **output**; however, they **cannot simultaneously serve as both** input and output.

   
````



## Signature of `MAIN` method

### **C++**
```c

 #include "ALClasses.h"


 /* * * MAIN method * * */
 void <method name>(
                 [IdsNs::IDS::<ids_name>& ids1, ..., IdsNs::IDS::<ids_name>& idsN, ]
                 int& status_code, 
                 std::string& status_message)


```


```{admonition} Note
:class: note 

*This file doesn't have a `main` method!* It consists solely of **procedures** and isn't structured like a regular C++ program.   
It's crucial to understand this.   
If you accidentally include a `main` method, you'll run into compilation errors!
```

````{admonition} To generate an actor user has to provide a file containing **C++ header** of wrapped method.  
:class: caution

You can name this file whatever you like, but it's essential to make sure **it has the exact layout of the method**. (including the same `namespaces`)  

```cpp    
#ifndef _CODE1_CPP
#define _CODE1_CPP

#include "ALClasses.h"

using namespace IdsNs;

// MAIN
void code_cplus(const IDS::distribution_sources ids_in,
                      IDS::core_profiles& ids_out,
                      int& status_code, std::string& status_message);
```
    
````



### **Fortran**
```fortran
module mod_code_lifecycle

contains

    !    MAIN SUBROUTINE
    subroutine <method name>(
                            [ids_in, ... , ids_out],
                            status_code,
                            status_message)
    end subroutine main_code


```


```{admonition} Let's see our **IMASfied** & **iWrapfied** codes in action!
:class: tip

Both the `Fortran` and `C++` codes:
- define functions that initialize `greeting` and `farewell` variables,
- set a mandatory property, 
- update a comment field,
- and manage status-related variables


```

`````{tab-set}

````{tab-item} Fortran
```fortran
module mod_code1


character(len=:), pointer :: greeting

contains
    !===================================================================
    !                            MAIN
    !===================================================================
    subroutine code_fortran(ids_in, ids_out, status_code, status_message)

        use ids_schemas, only: ids_core_profiles, ids_distribution_sources
        use ids_routines

        implicit none

        type(ids_core_profiles):: ids_in
        type(ids_distribution_sources):: ids_out
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        write(*,*) "= = = = = Code1 Fortran: MAIN = = = = ="

        ! Code specific initialization actions
        allocate(character(50) :: greeting)
        greeting = "CODE1: Hello!"
        
        ! Computations specific to the code    
        ids_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS ! Mandatory field

        if (.not. associated(ids_out%ids_properties%comment)) allocate(ids_out%ids_properties%comment(1))   
        ids_out%ids_properties%comment(1) = trim(ids_in%ids_properties%comment(1)) // trim(greeting) // " - "


        ! Code specific finalization actions
        deallocate(greeting)

        ! Setting status flag and message
        status_code = 0
        allocate(character(5):: status_message)
        status_message = 'OK'

    end subroutine code_fortran


end module mod_code1
    
```
````

````{tab-item} C++
```c
#include "UALClasses.h"
#include "code2.h"

std::string farewell;

//===================================================================
//                            MAIN
//===================================================================
void code_cplus(const IdsNs::IDS::distribution_sources ids_in,
                    IdsNs::IDS::core_profiles& ids_out,
                    int& status_code, std::string& status_message)
{
    printf(" = = = = = Code2 CPP: MAIN = = = = =\n");
    
    // Code specific initialization actions
    farewell = "CODE2: Goodbye!";

    // Computations specific to the code
    ids_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS;  // Mandatory field
    ids_out.ids_properties.comment = ids_in.ids_properties.comment + farewell + " - ";


    // Code specific finalization actions
    farewell.clear();

    // Setting status flag and message
    status_code = 0;
    status_message = "OK";
}



 ```
````

````{tab-item} C++ header file

```cpp
#ifndef _CODE1_CPP
#define _CODE1_CPP

#include "ALClasses.h"

using namespace IdsNs;

// MAIN
void code_cplus(const IDS::distribution_sources ids_in,
                      IDS::core_profiles& ids_out,
                      int& status_code, std::string& status_message);

```

`````

+++

##  Code Rules Summarization 

```{admonition} Rules Summarization 
:class: warning


1. **Code Signature**: Ensure your code **follows the specific format that iWrap requires**, known as a signature. Without the correct format, iWrap can't make an actor.
2. **Naming**: Feel free to choose any name for your methods, but please **avoid** naming any method as `main`.
3. **Arguments**: When listing arguments in a method, ***follow the exact order provided in the documentation***.
4. **You MUST NOT use INOUT arguments**: Steer clear of using arguments of type `INOUT`.
5. **Header Consistency**: Your header file (ending in `*.h`) should match the structure, or "signature", of your methods in the `*.cpp` file. This includes using the same namespaces.
```

+++

## Code Compilation

We are using several flags to compile our source code into a object file:

```{admonition} Explanation of compilation flags used in process
:class: dropdown

- `-g`:  Enables debugging information in the output.
- `-fPIC`: Generate Position-Independent Code, usually needed for dynamic linking. 
- `-J`:  Place the `.mod` files into the specified directory.
- `-pthread`: Enable support for multi-threading using the POSIX threads library. **Used only in `C++`**
- `pkg-config`  Insert the correct compiler options on the command line rather than hard-coding values on where to desired package - **preferred option by ITER**
```

```{admonition} AL version
:class: tip

If you want to compile with `AL4` 
change `--cflags al-fortran` into `--cflags imas-ifort` or `--cflags imas-gfortran` as per compiler selection 
`--cflags al-cpp` to `--cflags imas-cpp` for cpp
```

- **Fortran**

```{code-cell}
:tags: [output_scroll, hide-output]

export FORTRAN_PATH="codes/iWrapped_codes/code1_fortran"
gfortran -g -fPIC -J $FORTRAN_PATH $FORTRAN_PATH/code1.f90 `pkg-config --cflags al-fortran` -c -o $FORTRAN_PATH/code_fortran.o
```

- **C++**

```{code-cell}
:tags: [output_scroll, hide-output]

export CPP_PATH="codes/iWrapped_codes/code2_cpp"
g++  -g -fPIC -pthread $CPP_PATH/code2.cpp `pkg-config --cflags al-cpp` -c -o $CPP_PATH/code_cpp.o
```

## Code Packaging

```{admonition} Important!
:class: attention

When you've written your code , you **have to package it** into a `static Linux library`, upon which **iWrap further extends this by adding its own components, resulting in the creation of a shared library**.  
It is essential to note that without the inclusion of the `-fPIC` (Position Independent Code) flag during compilation, this process would not be feasible.
```


```{admonition} How to Package?   
:class: tip

You can use tools like the `ar` tool, which is commonly available on Linux systems.  
This tool helps you bundle your code into a library file, which can then be linked to other programs.

So  to **package** our code into Linux Static Library -`.a` - we can use such command 
```



- **Fortran**

```{code-cell}
ar -rcs  $FORTRAN_PATH/libcode_fortran.a  $FORTRAN_PATH/code_fortran.o


if [ $? -eq 0 ]; then
    echo " Fortran Library created successfully."
else
    echo "Library creation failed."
fi
```

- **C++**

```{code-cell}
ar -rcs $CPP_PATH/libcode_cpp.a $CPP_PATH/code_cpp.o


if [ $? -eq 0 ]; then
    echo "C++ Library created successfully."
else
    echo "Library creation failed."
fi
```

```{admonition} Final Summary
:class: note

**What we have done so far:**
- prepared `MAIN` method that is `iWrap` compatible using **mandatory** arguments
- compiled code
- packed the code into Linux lbrary using `ar`


**What is next?**  
In the next lesson you will learn How to prepare proper `YAML` file, which is a iWrap configuration  
```
