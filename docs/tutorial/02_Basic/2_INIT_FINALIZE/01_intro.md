---
jupytext:
  formats: ipynb,md:myst
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

# `INIT` & `FINALIZE`



        
In the previous chapter you have learned the first and only required `MAIN` method in iWrap environment. 

Now we learn the remaining **two optional** methods:
- `INIT`
- `FINALIZE`

You will learn how to:
  - construct this methods to be iWrap compatible
  - use them in iWrap environment



```{admonition} Why to use this approach?
:class: important

The isolation of initialization and finalization procedures into distinct methods **enhances the efficiency of disk Input/Output operations.**
```


```{admonition}  codes in this chapter.
Below is the repo layout of codes used in this chapter:
```

```{code-cell}
!find . -type d -name 'codes' | xargs tree
```