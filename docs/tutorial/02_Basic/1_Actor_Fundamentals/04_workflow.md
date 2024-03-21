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

# Workflow Introduction


## How to Use an Actor Class in Your Workflow Script

```{admonition} Step 1. Import Actor Class
:class: tip


To use an actor class in your script, you need to import it first. Here's how you do it:

    
    from actor_name.actor import actor_name
  
Here, replace `actor_name` with the name of the actor you want to use.

For example, if you have an actor named `actor1_fortran`, you would write:
```python
from actor1_fortran.actor import actor1_fortran
from actor2_cpp.actor import actor2_cpp
```



````{admonition} Step 2. Set Up Your Environment
:class: tip

Before importing an actor, you need to make sure Python knows where to find it. This is done by adding the directory containing all the actors to a special setting called `PYTHONPATH`.

In your terminal or command prompt, type:
```sh
export PYTHONPATH=path_to_actors_directory:$PYTHONPATH
```

```{admonition} Attention!
:class: attention

Make sure to replace `path_to_actors_directory` with the actual path to your actors' directory.
```
````






```{admonition} Step 3. Understand the Actor's Life Cycle
:class: tip

An actor goes through several steps in its life. It's important to follow these steps in the exact order:

1. **Creation**: This is when you first create an instance of the actor.

    ```{admonition} Configuration step omitted in the tutorial for beginners
    :class: note
    2. **Configuration**: Set up any settings or preferences for how the actor should run.  
    Now omitted, but described in detail in **For Advanced Users** chapter  
      
    ```

3. **Initialization**: The actor gets ready to perform its main tasks.
4. **Main Routine**: This is where the actor does its primary job. It's often referred to as the 'step' routine.
5. **Finalization**: Wrapping up and completing any final tasks.
```

+++

## Actor class API

```python

class actor_class_name:


   def initialize(self, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None) -> None:
       ...

   def run(self, *args)->None:
       ...

   def __call__(self, *args):
       return self.run( *args )

   def finalize(self) -> None:
       ...
```

+++

## Using the Actor in Your Workflow

```{admonition} Step 1. Creating the Actor
:class: tip

Once you've imported the actor class (as explained in the previous section), you can create an actor instance just like you'd create an object in Python:


    myActor = ActorName()

For instance, if you're using an actor named physics_ii, you would write:
```python
actor1_fortran = actor1_fortran()
actor2_cpp = actor2_cpp()
```


```{admonition} Step 2. Initializing the Actor:
:class: tip

The `initialize(...)` method prepares the actor for its main job. When you run this method, it:

- Updates the settings you've configured.
- Prepares the environment where the actor runs (called a "sandbox").
- Gets everything ready for the main task.
    
    ```{admonition} Note
    :class: note
    You usually run this **once** when your workflow starts:
    ```
    ```python
    actor1_fortran.initialize()
    actor2_cpp.initialize()
    ```
```

```{admonition} Step 3. Running the Main Task:
:class: tip

Now, it's time for the actor to do its primary job. You can call this main task in two ways:

- **Easy Way**: Just use the actor object like a function:  
    ```python
    ids2 = actor1_fortran(ids1)
    ids1 = actor2_cpp(ids2)
    ```
- **Alternate Way**: Using the `run(...)` method:

    ```python
   result = myActor.run(input_data)
    ```
    ```{note}
    You can run this main task **as many times as you need**.
    ```
```

```{admonition} Step 4. Finishing Up:
:class: tip

After the actor has done its job, you should clean up.   
Use the `finalize()` method:


    actor1_fortran.finalize()
    actor2_cpp.finalize()


This method cleans up any temporary data and resources the actor used.   

```{admonition} info
:class: note
Typically, you'd run this **once** your workflow is all done.
```

+++

## Understanding a Basic Workflow in Python: A Step-by-Step Guide
In this section, we'll walk you through a basic Python workflow.   
This `workflow.py` showcases a series of operations that involve actor objects, which  execute specific tasks.   Specifically, we will:

1. **Initialize Two Actor Objects**:
These are the primary entities we'll be working with. 

3. **Populate the Actors with Data**:
We'll be giving each actor some data to work with.

4. **Transfer Data Between the Actors**:
One actor will hand over some of its data to the other. This demonstrates how actors can communicate and share information in a workflow.

5. **Save the Actor's Data to a File**:
Finally, we'll take the data from one of the actors and save it to a file. This step is crucial for preserving results, logging, or sharing data outside of our immediate workflow.

By the end of this guide, you'll have a foundational understanding of how actor objects can be used in a Python workflow to manipulate and store data.

+++

```{code-cell}
!pygmentize codes/workflow.py
```

### Loop's Role in the Workflow
The loop serves **only in this example** as the core computational part of the workflow.    

It takes the initial `ids1` object, processes it through `actor1_fortran` and `actor2_cpp`, and then updates `ids1` based on those transformations.  
This cycle is repeated several times.  


After 5 iterations, `ids1` will have been transformed multiple times, and *this transformed object is then saved back into the IMAS database.*  
 
### What Each Actor Does
1. **Fortran Actor (actor1_fortran)**

- Reads the `ids_properties.comment` field from `ids1`
- Appends `Fortran: Hello!` to the comment field
- Sets `ids_properties.homogeneous_time` to `IDS_TIME_MODE_HETEROGENEOUS`
- Returns a new ids object, which is stored in `ids2`

2. **C++ Actor (actor2_cpp)**

- Reads the `ids_properties.comment` field from `ids2`
- Appends `C++: Goodbye!` to the comment field
- Sets `ids_properties.homogeneous_time` to `IDS_TIME_MODE_HETEROGENEOUS`
- Returns a new ids object, which replaces the original `ids1`

### What Happens Inside the Loop
Here's what happens within each iteration of the loop:



1. The script prints the current iteration number.
   ```
    print(" ITERATION: ", i)
    ```

2. `actor1_fortran` processes `ids1` and returns `ids2`.
    ```
    ids2 = actor1_fortran(ids1)
    ```

3. `actor2_cpp` processes `ids2` and returns a new `ids1`.
    ```
    ids1 = actor2_cpp(ids2)
    ```

+++

So, for example, let's say ids1.ids_properties.comment initially has the value "Initial Comment".   

After the `1st` iteration, it will be **START: Fortran: Hello! | C++:  Goodbye! |**  
After the `2nd` iteration, it will be **START: Fortran: Hello! | C++:  Goodbye! | Fortran: Hello! | C++:  Goodbye!**

...and so on for 5 iterations.

```{code-cell}
:tags: [output_scroll, hide-output]

!python3 codes/workflow.py
```

```{admonition} Summary
:class: note
**What we have done so far:**
- created first `Python` workflow using actors actors that wrapped codes in `Fortran` and `C++`
- finished first part of tutorial fo beginners, where you were shown the **most basic functionalities and mandatory settings**

**What is next?**
- In the next chapter we will dive more into iWrap, its **optional, but still basic** settings. 
- Let's Go back to `Chapter 2`!
