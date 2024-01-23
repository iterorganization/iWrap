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

# iWrap Tutorial: Unveiling the Power of iWrap

```{admonition} Welcome!
Welcome to this comprehensive tutorial on iWrap, a tool to **generate standard _actors_ for IMAS towards building of larger and more complex integrated modelling simulations by coupling together several _actors_.**   
```

## Aim of This Tutorial

The primary objective of this tutorial is to equip you with the understanding and skills to leverage iWrap, and with it to create various IMAS actors combined within a workflow.    
*By the end of this tutorial, you should be capable to connect codes written in different language (e.g. `C++` and `Fortran`) seamlessly within a given workflow/coupling language (e.g. in a Python script).*

## What Will Be Included

*This tutorial is structured to provide both theoretical and practical knowledge.*     
It covers a variety of topics, ranging from setting up your environment and understanding iWrap's  code standards to advanced techniques like efficiently wrapping `MPI` codes.     
You'll find each section replete with code examples, explanations, and GUI walkthroughs to ensure a holistic learning experience.

## Why Change Your Way of Thinking

Making an actor for your code will **improve its ability to be shared, reused and integrated within a larger pool of IMAS workflows** maintained by different group of people.  
**iWrap automates the boring stuff so you can focus on the fun parts - like actually writing your code.     
Plus, it minimizes the chances of those pesky errors that can creep in when you're doing everything manually.**



## Prerequisites

iWrap knowledge is not required. However, some level of expertise in the following areas will help you follow the tutorial:

- Compilation methods
- Basic `Makefile` creation skills - [Makefile tutorial](https://makefiletutorial.com/)
- Elementary `Python` programming skills - [Python tutorial](https://edube.org/study/pe1)
- Basic `Fortran` (or `C++`) knowledge -  [Fortran tutorial](https://fortran-lang.org/learn/quickstart/)   |   [C++ Tutorial](https://www.w3schools.com/cpp/)
- Knowledge concerning `IMAS` essentials - [Official IMAS AL5 docs](https://sharepoint.iter.org/departments/POP/CM/IMDesign/Code%20Documentation/ACCESS-LAYER-doc/cpp/dev/index.html)
- Knowledge of `XSD` & `XML` file formats - [XML vs. XSD comparison](https://www.geeksforgeeks.org/difference-between-document-type-definition-dtd-and-xml-schema-definition-xsd/)




## Tutorial Structure

The tutorial is organized into separate folders for introductory content and beginner-level topics.  
Below is an outline of the folder structure (each folder can be composed of sub-folders or chapters, and 
of a set of notebooks that can be looked at in a sequence, 01 -> 02 -> ...):


### `01_Tutorial_Introduction_And_Environment_Set_Up`

This folder serves as an introduction to the tutorial as a whole and provides guides on setting up your environment.

### `02_Basic`

This folder contains chapters that are designed for beginners. Each chapter expands previous one.



#### 1_Actor_Fundamentals

This chapter introduces essential practices for adapting physics codes to IMAS and outlines the minimum standards for actor implementation.   
It details the process of describing codes in iWrap and generating basic Python actors.   
The chapter concludes by explaining the importation and sequential execution of these Python actors within a Python script.

***Keywords:*** `IMAS` adaptation, `MAIN` method,     code compilation,  code packaging, `iWrap` code description, `iWrap`  actor creation, `iWrap` actor usage, `Python` workflow


#### 2_INIT_FINALIZE

Building upon the initial creation of actors using iWrap, this chapter guides readers through enhancing their code interfaces with optional initialization and finalization functions.  
It covers their description in iWrap and the integration of these advanced actor functionalities within a Python workflow.  

***Keywords:*** `INIT` method, `FINALIZE` method

####  3_Code_Parameters

In this chapter you will learn that in addition to IDS data, you can pass _code parameters_ for the static configuration of 
individual codes. After explaining how to describe such code parameters in XML/XSD, the section will show how to adapt the code description 
in iWrap and how to check and modify these parameters for a given actor in a Python workflow.  

***Keywords:*** `XML` code parameters, `XSD` code parameters, static configuration