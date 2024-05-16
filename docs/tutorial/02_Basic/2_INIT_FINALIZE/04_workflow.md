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

# Workflow
```{admonition} What will you learn in this lesson
:class: note

How to run workflow using Makefile
```


```{admonition} Attention!
:class: attention

In both Chapter 1 and Chapter 2, the workflow remains the same, with the same processes and procedures being implemented.

```


To run workflow execute below command:

```{code-cell}
:tags: [output_scroll, hide-output]

!make -C codes wf-run
```

```{admonition} Summary of chapter 2
**1. What we have done in this Chapter 2:** 
- we have learned optional methods `INIT` & `FINALIZE`
- we have expanded our `yaml` file
- we had fun with GUI
- we have **compiled, packaged, created actors and run workflow** using `Makefiles`

**2. What is next:**  
Let's go next to Chapter 3 to see the mysterious world of **code parameters**!
``````
