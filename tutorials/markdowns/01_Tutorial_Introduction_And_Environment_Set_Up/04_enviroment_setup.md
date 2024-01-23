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


# Connect to a system where IMAS and required dependencies are available

##  How to connect to ITER SDCC via SSH
1. Open terminal window
2. Connect to SDCC with your credentials
![](../../sources/images/ssh_iter.gif)

## How to connect to Gateway via SSH
1. Open terminal window
2. Connect to gateway with your credentials
![](../../sources/images/ssh_gateway.gif)




# Environment Setup Check


+++

```{admonition} What you will do here
:class: note

In this section you will check if your tutorial environment is set up properly.

These scripts are designed to automatically change **username** to `system $USER`.
```


```{admonition} Check if you have **IMAS AL5** installed 
:class: tip

To check if we can use IMAS AL5 run:
```

```{code-cell}
echo $AL_VERSION
```


```{admonition} imasdbs 
:class: tip

To check if we have properly created our IDS we can use `imasdbs -u <username>` command:
```

```{code-cell}
imasdbs -u iwrap_user
```

```{admonition} idsdump
:class: tip

To list the content (all data) of an IDS,  use `idsdump`  script

Usage: `idsdump <USER> <TOKAMAK> <VERSION> <SHOT> <RUN> <IDS>`

```

```{code-cell}
:tags: [output_scroll, hide-output]

idsdump iwrap_user tutorial_db 3 1 1 core_profiles
```

```{admonition} Ready to go!
:class: hint

Now we have all environment set up and ready to go further with our tutorial!
```
