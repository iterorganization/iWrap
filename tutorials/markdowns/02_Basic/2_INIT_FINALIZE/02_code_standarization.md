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

# Explanation of iWrap  code implementation standards - part II  


```{admonition} What will you learn in this lesson
:class: note

1. What are two optional iWrap API with only **mandatory** arguments explained
2. We will go through whole process of code preprocessing again.

```
  
```{admonition} Documentation link
Docs is available [here](https://sharepoint.iter.org/departments/POP/CM/IMDesign/Code%20Documentation/IWRAP-doc/resources/native_code.html) 
```


```{admonition} Caution!
:class: caution

***To use iWrap, your code must be written in a specific way, often referred to as a `API`.***  
***If iWrap doesn't recognize this signature, it won't be able to create what we call an `actor`.***  

You need properly design your  code to interact with iWrap actor
```

While the way you write these methods might change a bit based on the programming language you're using, the main ideas behind them stay the same.



## `INIT` method

Here's an extra method you can include to help set up your original code:

- **When to Use**: Only if you want to. It's *optional*.
- **When It's Called**: Just *once*, when the "actor" starts.
- **When Should It Run?**: Make sure this method runs *before* the `MAIN` method and any `FINALIZE` method.
- **What Can You Name It?**: You can name this method whatever you like. Just remember this name for later.

**What this method needs (its arguments)**:


1. **Status Code**:
   - Do you have to include this? **Yes, it's mandatory**.
   - What kind of data? A number (an integer).
   - What's it for? The method will give this number as an output once it's done.

2. **Status Message**:
   - Do you have to include this? **Yes, it's mandatory**.
   - What kind of data? A text string.
   - What's it for? The method will provide this text message as an output to give more details about its status or results.

+++

## The `FINALIZE` Method

The "FINALIZE" method is like the cleaning crew for your code - it tidies things up.

- **When to Use**:  you don't always have to include it — it's *optional*.
- **How Often Can It Run?**: As many times as you want.
- **What Can You Name It?**: Whatever you like! Just remember to mention its name in the code's YAML description.

**What this method needs (its arguments)**:

1. **Status Code**:
   - Do you have to include this? **Yes, it's a mandatory**.
   - What kind of data? A number (an integer).
   - How it works: The method will give this number as an output to let you know how the cleanup went.

2. **Status Message**:
   - Do you have to include this? **Yes, it's a mandatory**.
   - What kind of data? A text string.
   - How it works: The method will provide this text message to give more details about the cleanup process.

+++

```{admonition} How to construct methods
:class: hint

In fact *INIT*, *MAIN* and *FINALIZE* methods share the same API, 
so they could accept IDSes and code parameters among their arguments.
It will be explained more in details in tutorial for advanced users.

```

+++

## How to put these methods in a code

+++

```{admonition} How to construct methods
:class: hint

So when writing your code to be iWrapped, **use procedures (also known as subroutines)** in *ANY* programming language.   
Avoid using `functions`.

```
```{admonition} Arguments Naming Rules
:class: caution

**Naming**: You can name your subroutines whatever you want. 

**Rules for Arguments:**

1. List them in a specific order.
2. Arguments are designated as either **input** or **output**; however, they **cannot simultaneously serve as both** input and output.

```

+++

So the example looks like this:


`````{tab-set}
````{tab-item} Fortran

```fortran
module mod_code_lifecycle

contains
    <!--  -->
    <!-- ADDED -->
    !    INITIALISATION SUBROUTINE
    subroutine init_code (status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
    end subroutine init_code


    !    MAIN SUBROUTINE
    subroutine main_code(equilibrium_in, equilibrium_out, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_parameters_input, ids_is_valid
        use ids_routines, only: ids_copy

        implicit none

        type(ids_equilibrium):: equilibrium_in,equilibrium_out
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
     end subroutine main_code

    <!--  -->
    <!-- ADDED -->
    !    FINALISATION SUBROUTINE
    subroutine clean_up(status_code, status_message)
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
    
    
```
````


````{tab-item} C++
```cpp
 #include "ALClasses.h"

        // ADDED
 /* * *   INITIALISATION method   * * */
 void init_code (int& status_code, std::string& status_message)
 {
 ...
 // method body
 ...
 }

 /* * *   MAIN method   * * */
 void physics_ii_cpp(const IdsNs::IDS::equilibrium& in_equilibrium,
                           IdsNs::IDS::equilibrium& out_equilibrium,
                           int& status_code, std::string& status_message)
 {
 ...
 // method body
 ...
 }

        // ADDED
 /* * *   FINALISATION method   * * */
 void clean_up(int& status_code, std::string& status_message)
 {
 ...
 // method body
 ...
 }
 ```

````
````{tab-item} C++ Header file
```cpp

#ifndef _CODE1_CPP
#define _CODE1_CPP

#include "ALClasses.h"

using namespace IdsNs;

// INIT
void code2_setup(int& status_code, std::string& status_message);

// MAIN
void code2_step(const IDS::distribution_sources ids_in,
                      IDS::core_profiles& ids_out,
                      int& status_code, std::string& status_message);

// FINALIZATION
void code2_cleanup(int& status_code, std::string& status_message);

#endif // _CODE1_CPP
```
````
`````

+++

## Code Compilation and Packaging

`````{admonition} Important! **Makefile** usage
:class: important

Instead of running separate commands we will use `Makefiles` to automate this process

We provide makefiles for each programming language.  

These makefiles to compile and package our code are located in `Chapter_2/codes/iWrapped_codes/code*/Makefile`

````{admonition}  Makefile explanation
:class: dropdown

This Makefile is designed for **compiling code, creating a static library, and maintaining a clean build environment**.  
 Here's a breakdown of its contents:

1. **Default Target: All**
```makefile
all: | clean build lib
```

The `all` target is the default when you run make.     
It ensures a clean build environment, compiles the source code, and then creates a static library.  
 The `|` symbol indicates that c`lean, build`, and `lib` are independent targets.

2. **Compiling the Source Code**
```makefile
build: 
	<gfortran|g++> -g -fPIC <filename> `pkg-config --cflags al-*` -c -o code1.o
```

The `build` target compiles the source file `.f90 | .cpp` into an object file `*.o`.   
It uses the  compiler with debugging information `-g`` and generates position-independent code `-fPIC.   
The `pkg-config` command is used to include flags for the `al-*` library.

```{admonition} Additional flag **only** in **C++** codes! 
:class: attention

In **C++** makefile there is also `pthread` flag .
```

3. **Creating a Static Library**

```makefile
lib: 
	ar -rcs libcode1.a code1.o
```

The `lib` target uses the `ar` command to create a static library `.a` from the `.o` object file.   
The `-rcs` flags indicate replacing, creating, and sorting within the archive.

4. **Cleaning Up**
```makefile
clean:
	rm -f *.exe *.a *.mod *.o *~
```
The `clean` target removes all generated files like executables, static libraries, module files, object files, and temporary files, ensuring a clean start for the next build.
````
`````


```{admonition} **-C** flag
:class: hint

This `-C` flag is used to change the directory before reading the Makefile.   
Essentially, it tells make to go to a specific directory and then execute there.
```
- **Fortran**

```{code-cell}
!make -C codes/iWrapped_codes/code1_fortran
```

- **C++**

```{code-cell}
!make -C codes/iWrapped_codes/code2_cpp
```

```{admonition} Summary
:class: note
**What we have done so far**?
- learned new optional methods in iWrap ecosystem 

**What is next?**
- We expand our YAML file with this two additional methods
