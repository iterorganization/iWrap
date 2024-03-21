
- [1. Welcome to `iWrap` tutorial!](#1-welcome-to-iwrap-tutorial)
  - [1.1. Summary Information:](#11-summary-information)
    - [1.1.1. Generation of tutorial content](#111-generation-of-tutorial-content)
    - [1.1.2. Environment Setup scripts](#112-environment-setup-scripts)
    - [1.1.3. You can view tutorial content in three ways:](#113-you-can-view-tutorial-content-in-three-ways)
  - [1.2. `JupyterBook` vs `JupyterLab`](#12-jupyterbook-vs-jupyterlab)
  - [1.3. Repository Layout](#13-repository-layout)
    - [1.3.1. Pure Repo Layout (Before Docker Build)](#131-pure-repo-layout-before-docker-build)
    - [1.3.2. Repo Layout After File Conversion (After `Jupyter Book` Build)](#132-repo-layout-after-file-conversion-after-jupyter-book-build)
    - [1.3.3. iWrap tutorial](#133-iwrap-tutorial)
    - [1.3.4. Tutorial Structure](#134-tutorial-structure)
  - [1.4. `Python Virtual Environment` - building iWrap tutorial without Docker](#14-python-virtual-environment---building-iwrap-tutorial-without-docker)
    - [1.4.1. Prerequisites](#141-prerequisites)
    - [1.4.2. Creating Virtual Environment](#142-creating-virtual-environment)
    - [1.4.3. Accessing `JupyterLab`](#143-accessing-jupyterlab)
    <!-- - [1.4.4. Environment Cleanup Script](#144-environment-cleanup-script) -->
  - [1.5. Docker based approach](#15-docker-based-approach)
    - [1.5.1. Using images from `Container registry`](#151-using-images-from-container-registry)
      - [1.5.1.1. Pulling Built Image](#1511-pulling-built-image)
      - [1.5.1.2. Running images from `container registry`](#1512-running-images-from-container-registry)
      - [1.5.1.3. Access JupyterLab](#1513-access-jupyterlab)
    - [1.5.2. Building Images from scratch](#152-building-images-from-scratch)
      - [1.5.2.1. Building images locally](#1521-building-images-locally)
        - [1.5.2.1.1. `build.sh` with **default** values](#15211-buildsh-with-default-values)
        - [1.5.2.1.2. `build.sh` with custom values](#15212-buildsh-with-custom-values)
      - [1.5.2.2. Running image that was built from scratch](#1522-running-image-that-was-built-from-scratch)
      - [1.5.2.3. Access JupyterLab](#1523-access-jupyterlab)
    - [1.5.3. Shuting down container](#153-shuting-down-container)
  - [1.6. `Jupyter Book` as another approach to tutorials](#16-jupyter-book-as-another-approach-to-tutorials)
    - [1.6.1. Pulling `Jupyter Book` from `CICD` artifacts](#161-pulling-jupyter-book-from-cicd-artifacts)
      - [1.6.1.1. Entering `CICD` job artifacts](#1611-entering-cicd-job-artifacts)
      - [1.6.1.2. Selecting `CICD` job artifacts with book](#1612-selecting-cicd-job-artifacts-with-book)
      - [1.6.1.3. Downloading `CICD` job artifacts with book](#1613-downloading-cicd-job-artifacts-with-book)
      - [1.6.1.4. Access to built `Jupyter Book`](#1614-access-to-built-jupyter-book)
    - [1.6.2. Creating `Jupyter Book` on your own](#162-creating-jupyter-book-on-your-own)
      - [1.6.2.1. Building the container](#1621-building-the-container)
      - [1.6.2.2. Running the Container](#1622-running-the-container)
      - [1.6.2.3. Building the HTML Output](#1623-building-the-html-output)

<!-- /TOC -->

# 1. Welcome to `iWrap` docs!

## 1.1. Summary Information:

### 1.1.1. Generation of tutorial content

**You can generate tutorial content in two ways:**

1. On `SDCC` & `Gateway`
   -  using  `Python Virtual Environment`



|                                                                  	                                                                  |                           **EUROfusion Gateway**                                       	                           |                          **ITER Computing Cluster**                                         	                           |
|:-----------------------------------------------------------------------------------------------------------------------------------:|:------------------------------------------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------------------------------:|
|                                                **How to get a user account**       	                                                | link >>  [Create account at Gateway](https://wiki.eufus.eu/doku.php?id=start)  <<                                	 | link >> [Create account on ITER](https://www.iter.org/accountcreation) <<                                             	 |
| **How to connect to**           	                                                                                                 	 |                                                         	                                                          |
|                                                    **NoMachine**               	                                                    |  link >> [Connecting to Gateway](https://wiki.eufus.eu/doku.php?id=namespace:connecting_to_the_gateway) <<     	   |   link >> [Connecting to ITER Computing Cluster](https://confluence.iter.org/display/IMP/ITER+Computing+Cluster)  <<	   |
|                                                     **Nodes**                 	                                                     |       `s51-s54.eufus.eu`                                                                                  	        |      `sdcc-login.iter.org`                                                                                      	       |
|                                            **SSH    (no private/public key support)** 	                                             |                        `ssh g2<username>@s51.eufus.eu`                                   	                         |                        `ssh <username>@sdcc-login.iter.org`                                    	                        |



2. On machine with `docker` installed
   - using pre-built images
   - building images on your own


### 1.1.2. Environemt Setup scripts

This tutorial provides several setup scripts in `docs/scripts` folder,  
so if you follow instructions in this file you should have your environment setup correctly ;)

### 1.1.3. You can view tutorial content in three ways:

1. using Markdown files `.md`
2. using Jupyter Notebook files `.ipynb` in `Jupyter Lab IDE`
3. using nested, user-friendly `HTML` file generated by `Jupyter Book`

The next chapter will describe deeply each point.


## 1.2. `JupyterBook` vs `JupyterLab`

JupyterBook and JupyterLab serve similar but distinct roles in the ecosystem of data science and scientific computing.  
 While both are based on Project Jupyter, they are designed for different stages of the data science workflow.



This tutorial is designed in two ways:
- `JupyterLab`  Provides a rich, interactive user interface to work with Jupyter notebooks, text editors, terminals, and custom components. **(preferred way)**
- `JupyterBook` A static, book-like presentation layer for Jupyter notebooks and other content like Markdown files.

<details>
<summary>JupyterBook vs JupyterLab Comparison</summary>

| Feature / Capability    | JupyterLab                                                                                      | JupyterBook                                                                         |
|-------------------------|-------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|
| **User Interface**      | Rich, interactive UI for Jupyter notebooks, text editors, terminals, and custom components.     | Static, book-like presentation layer for Jupyter notebooks and Markdown files.      |
| **Extensibility**       | Highly extensible through a variety of plugins.                                                 | Themable and customizable, but not as extensible in real-time features.             |
| **Language Support**    | Supports multiple languages like Python, R, Julia through kernels.                              | Designed to present outputs, not for real-time language execution.                  |
| **Data Viewing**        | Integrated data viewers for formats like CSV, JSON.                                             | Static representation aimed at displaying results.                                  |
| **Debugging**           | Built-in debugger for supported languages.                                                      | No debugging support.                                                              |
| **Real-time Collaboration** | In progress, but will soon allow real-time collaborative editing.                           | No real-time collaboration; can be hosted online for asynchronous collaboration.    |
| **Output Formats**      | `.ipynb`, `.py`, `.r`, `.md`, `.txt`, `.json`, can be converted to HTML, PDF, LaTeX, Markdown.  | Static web pages (HTML/CSS/JS), PDF through LaTeX compilation, ePub.                 |
| **Common Use-Cases**    | Data exploration, analysis, development, educational labs.                                      | Documentation, academic publishing, project showcase.                               |

</details>



## 1.3. Repository Layout

### 1.3.1. Pure Repo Layout (Before Docker Build)

Before building the Docker image, the repository has the following layout:

```shell
docs/
├── documentation             
├── tutorial
├── images                     
├── scripts                    
├── _config.yml                # Config file required by Jupyter-Book
├── _toc.yml                   # Table of Contents required by Jupyter-Book
├── build.sh                   # Shell script to build the project
├── Dockerfile                 # Dockerfile to build the Docker image
├── iWrap_introduction.md
├── requirements.txt
└── README.md

```

Markdown files within the `documentation` and `tutorial` directories **serves as the
source files**, which will later be **converted to** *Jupyter Notebook* `.ipynb` files and `HTML` files for
*JupyterBook*.

### 1.3.2. Repo Layout After File Conversion (After `Jupyter Book` Build)

After running the `Jupyter Book` build, additional directories `_build` and `docs_book_venv` are generated:

```shell
docs
## -----------------------
├── _build                  # this is where Jupyter Book build files are stored - HTML and .ipynb 
├── docs_book_venv          # Python virtual environment 
## -----------------------
├── documentation
├── images
├── scripts
├── tutorial
├── build.sh
├── _config.yml
├── Dockerfile
├── iWrap_introduction.md
├── README.md
├── requirements.txt
└── _toc.yml

```
```sh
> tree   -L 2  docs/_build  --dirsfirst 

docs/_build
├── html
│   ├── documentation
│   ├── _images
│   ├── _sources
│   ├── _sphinx_design_static
│   ├── _static
│   ├── tutorial
│   ├── genindex.html
│   ├── index.html                        # This is our nested HTML file
│   ├── iWrap_introduction.html
│   ├── objects.inv
│   ├── search.html
│   └── searchindex.js
└── jupyter_execute                       # This is where .ipynb files are stored               
    ├── tutorial
    └── iWrap_introduction.ipynb
                        
```

### 1.3.3. iWrap tutorial

The `docs/_build/jupyter_execute` folder consists of **interactive** iWrap tutorials in Jupyter notebooks format.  
The `docs/_build/html/index.html` file is a non-interacive nested HTML in a book format.

### 1.3.4. Tutorial Structure
This tutorial is designed to be followed **incrementally**.   
Begin with the `01_Tutorial_Introduction_And_Environment_Set_Up` folder and proceed to `02_Basic` for a structured learning experience.


## 1.4. `Python Virtual Environment` - building iWrap tutorial without Docker

If Docker is not an option or not preferred, you can still run iWrap tutorials using a **Python virtual environment**.  
   This guide will walk you through the process step-by-step.

### 1.4.1. Prerequisites

Before you start, make sure you have the correct version of Python installed. Here are the initial steps:

1. Navigate to the `docs/` directory.
2. Switch to `Bash` shell
3. Add the `scripts` directory to your `PATH` variable.   

   ```sh
   cd tutorials
   export PATH=$PATH:$PWD/scripts
    ```
4. Depending on your user type, execute the appropriate script:
    - For `SDCC` users
        ```sh
        source set-sdcc.sh
        ```
    - For `Gateway` users:
        ```sh
        source set-gw.sh
        ```

These scripts load necessary modules, such as `IMAS` and `iWrap`, and trigger the `create_db` script.


### 1.4.2. Creating Virtual Environment

Once you've completed the prerequisites, execute the virtual environment setup script:

```shell
> set-docs.sh -h

Build the JupyterBook and start JupyterLab.

Usage: ./scripts/set-docs.sh [OPTION]...

Options:
  -h, --help    display this help and exit
  --no-cache    delete _build/ (cache) directory before generating JupyterBook

```
```sh
source set-docs.sh
```
This script checks if a Python virtual environment directory exists.  
If not, it creates one and installs the required Python packages in the `venv` directory.   
Check for `--no-cache` argument and delete  `_build` directory if present.  
  Then, it converts the ``.md`` files (containing iWrap tutorials) into ``.html`` and ``.ipynb`` files.   
At the end, it runs `Jupyter Lab` with ready tutorials to learn from.


### 1.4.3. Accessing `JupyterLab`

Inside `Python virtual environment` we're installing `JupyterLab` which allows you to run tutorials interactively.   
In order to access it you need to:   

1. activate `Python virtual environment` from `docs` directory.
```sh
source docs_book_venv/bin/activate
```


2.  start `JupyterLab` instance.
```sh
jupyter lab --ip=0.0.0.0 --port=8888 --allow-root --IdentityProvider.token='' --ServerApp.allow_origin='*' --ServerApp.trust_xheaders='True' --browser=firefox
```

You can instead use script `scripts/start-tutorial-in-venv.sh` that includes above commands

```sh
source start-tutorial-in-venv.sh
```

To deactive virtual environment simply run
```sh
deactivate
```

<!-- ### 1.4.4. Environment Cleanup Script

This script, `scripts/clean-venv.sh`, is designed to efficiently clean up the tutorial environment by removing specific directories and symbolic links. It's particularly useful for resetting the state of the project or preparing for a fresh setup.

**What It Does**  
The script performs the following actions:

- Deletes the Virtual Environment Directory (`tutorials_venv/`)
- Removes the Markdowns Build Directory (`markdowns/_build/`)
- Deletes Symbolic Links (book and notebooks) -->



## 1.5. Docker based approach

This repository is used for `iWrap` tutorial based on `IMAS` docker image (`al`). In the following chapters you'll see
how to build image, run it, access JupiterLab via web browser and how to create python notebooks with `IMAS`. Every
command should be executed inside `docs/tutorial`.

Before you do steps from the following chapters make sure you have installed 'Docker' on your local machine. To check if you have installed `Docker` type in terminal:
```bash
docker --version
```

If you don't have `Docker` on your machine, you can download it from `Docker` website.\
https://docs.docker.com/desktop/install/windows-install/ - for Windows user\
https://docs.docker.com/engine/install/ubuntu/ - for Ubuntu user\
https://docs.docker.com/desktop/install/mac-install/ - for macOS user



You have two options to run our interactive tutorial in `Jupyter Lab` using `Docker`
1. Use already build and ready to pull images from our [Container registry](https://gitlab.eufus.psnc.pl/g2awisz/iwrap_tutorial/container_registry/52) - **You don't have to clone this repo**
2. Build those images by your own on your computer - **You MUST clone this repo**

### 1.5.1. Using images from `Container registry`

#### 1.5.1.1. Pulling Built Image

Before you pull docker image you need to login with your gateway credentials to docker registry to be able to pull that images.
```bash
docker login gitlab.eufus.psnc.pl:5050
```

Inside `Container Registry` you'll find two versions of docker image:  
1.  First One  is based on `AL 5.0.0`
2.  The second is based on `AL 4.11.7`.  

We're using `docker tags` to distinguish them.


After succesful login you can type following commands to pull desired images from `Container Registry`:
```bash
# AL 5
docker pull gitlab.eufus.psnc.pl:5050/g2awisz/iwrap_tutorial/iwrap_tutorial:AL-5.0.0_IWRAP-0.9.1

# AL 4
docker pull gitlab.eufus.psnc.pl:5050/g2awisz/iwrap_tutorial/iwrap_tutorial:AL-4.11.7_IWRAP-0.9.1
```



After that you can create docker tag for that image, so it'll be easier for you to use it.
```bash
# AL 5
docker tag gitlab.eufus.psnc.pl:5050/g2awisz/iwrap_tutorial/iwrap_tutorial:AL-5.0.0_IWRAP-0.9.1 iwrap_tutorial_al5

# AL 4
#docker tag gitlab.eufus.psnc.pl:5050/g2awisz/iwrap_tutorial/iwrap_tutorial:AL-4.11.7 iwrap_tutorial_al4
```

#### 1.5.1.2. Running images from `container registry`

Our docker tutorial enables user tu run iWrap GUI, but because of that, we need to give docker proper permissions first:
```bash
xhost +local:
```

And then you can run our docker with this command:
```bash
docker run -e DISPLAY=$DISPLAY \
           -p 8888:8888 \
           -v /tmp/.X11-unix:/tmp/.X11-unix:rw \
           -it iwrap_tutorial_al4   # CHANGE_CONTAINER_NAME
```

#### 1.5.1.3. Access JupyterLab

After running container with `iWrap` tutorial default browser should open. If it doesn't open type in web browser following:
```bash
localhost:8888/
```
In both scenarios it will open page with JupiterLab.


### 1.5.2. Building Images from scratch

If you don't want to use `Container Registry` which is recomended way, you can build that image on your own.   
To do that follow steps in this section.  

#### 1.5.2.1. Building images locally

Before building image with `iWrap` tutorial, you have to clone source repository.   
In order to do that paste in your terminal following command:
```bash
git clone https://gitlab.eufus.psnc.pl/g2awisz/iwrap_tutorial.git
```

After cloning repository to your machine navigate to directory with our tutorial. Inside that directory type in terminal:
```bash
docker login gitlab.eufus.psnc.pl:5050
```

Before building image with `iWrap` tutorial, you'll need to pull submodules to do that type in your terminal:
```bash
 git submodule update --init --recursive
```

Inside repository, you'll find `build.sh`, which is written to help you with building `iWrap` tutorial image.   

**Note!** It builds an image tailored for **Jupyter Books (HTML version)** OR **Jupyter Lab (interactive Version - preferred option)**.

You can execute it in two different ways:

1. with **default** values
2. with **custom** values

##### 1.5.2.1.1. `build.sh` with **default** values
If you chose to build docker image **default** values, your `iWrap` image will be:
- build for **Jupyter Lab**
- named `iwrap_tutorial`  
- and it'll be based on `AL 4.11.7`.  

To run it with **default** values type in terminal following command:
```bash
./build.sh
```

##### 1.5.2.1.2. `build.sh` with custom values
Second option allows you to specify name of image and version of `AL` on top of which your `iWrap` tutorial will be build.  
`build.sh` accepts two options:
- `-v` which allows you to choose version of `AL`   [`al4` / `al5`]. Default is `al4`  
- `-i` which allows you to change name of image.  Default is `iwrap_tutorial`
- `-t` which allows you to choose which target from Dockerfile should be build
    - `book`: Builds an image tailored for Jupyter Books.
    - `tutorial`: Builds an image tailored for Jupyter Tutorials.  
    Default is `tutorial`.
```
./build.sh -i iwrap_book_al5 -v al5 -t book
```
Command will create `iWrap`  Jupyter Book image named `iwrap_book_al5` and these image will be based on top of `AL 5.0.0`.   
Because it is `Book` image - you can create static `HTML` docs on your own, if you really want to ;)



#### 1.5.2.2. Running image that was built from scratch

After succesfull built of your tutorial image **with default options** you can run it by typing in your terminal following command:
```bash
> ./start-tutorial-in-docker.sh -h

Usage: ./start-tutorial-in-docker.sh [options]
Options:
  -i <IMAGE_NAME>        Set the Docker image name; default is 'iwrap_tutorial'
  -v <IMAGE_VERSION>     Set which Docker image version will be used; default is 'AL5'
  -c <CONTAINER_NAME>    Set the container name; if not specified, it will be the same as image name
  -p <HOST_PORT>         Set the host port to map to container's 8888; if not specified, defaults to 8888
  -h                     Display this help message and exit
```
The `-v` option enables users to select the version of the Docker image they wish to use.   
This is facilitated by employing a custom tagging format, `AL-X.Y.Z_IWRAP-X.Y.Z`, allowing the storage of multiple Docker images on the user's machine.   
Users can specify the Docker tag they intend to use, with the current options being `AL5` or `AL4`.   
If this option is not specified, the script defaults to using `AL5` for container initialization.


This script enables running python GUI apps inside docker.  
After starting the container `tutorial_db` test database will be created.

- firtst, export path to the script:
```sh
   export PATH=$PATH:$PWD/scripts
```
- If you have build container **with another name,** you can run existing container with `-i` option:
```
./start-tutorial-in-docker.sh -i iwrap_tutorial_al4
```

- you can have different tutorial containers run next to eachother by changing the mapping port `-p` and name of second container!
```sh
./start-tutorial-in-docker.sh -i iwrap_tutorial_al4 -c iwrap_tutorial_2 -p 8889
```  
#### 1.5.2.3. Access JupyterLab

After running container with `iWrap` tutorial default browser should open. If it doesn't open type in web browser following:
```bash
localhost:8888/
```
In both scenarios it will open page with JupiterLab.

### 1.5.3. Shuting down container

To close our container click in terminal `ctrl + c`.   
It will ask us closing our notebook server, we type `y` in terminal and after a few seconds our docker container will be shutdown.

## 1.6. `Jupyter Book` as another approach to tutorials
### 1.6.1. Pulling `Jupyter Book` from `CICD` artifacts
If you dont want to build `Jupyter Book` yourself, we prepared `CICD` pipeline to create `Jupyter Books` every time pipeline is triggered. In order to pull pre-built `Jupyter Book` follow staps given in next subchapters.

#### 1.6.1.1. Entering `CICD` job artifacts
`CICD` pipeline contain jobs responsible for building `Jupyter Book` based on `iWrap` tutorial. After successful job `Jupyter Books` are saved to job artifacts. You can find them here: [Job Artifacts](https://gitlab.eufus.psnc.pl/g2awisz/iwrap_tutorial/-/artifacts)

#### 1.6.1.2. Selecting `CICD` job artifacts with book
After entering `CICD` job artifacts you will see severall job names, which are:
- Build Image: [tutorial, `al4` / `al5`]
- Save Image: [tutorial, `al4` / `al5`]
- Deploy Book: [book, `al4` / `al5`]

We are interested in job named `Deploy Book`.   
These jobs containes job artifacts with `Jupyter Book` based on `AL 4.11.7` or `AL 5.0.0`

#### 1.6.1.3. Downloading `CICD` job artifacts with book



After Selecting `Jupyter Book` expand selected job artifacts. It should contain **3** files:
- artiacts.zip
- metadata.gz
- job.log

We're interested in donloading `artifacts.zip` archive.



#### 1.6.1.4. Access to built `Jupyter Book`
After donloading `artifacts.zip` to your local machine you're required to extract files from `.zip`. Inside extracted directory there will bi another directory named `book-al4` or `book-al5`.
When you extract files from `artifacts.zip` you can open `Jupyter Book` with browser of your choice. I order to do that type in terminal:
```bash
firefox path/to/extracted/directory/html/index.html #open index.html with firefox
```

### 1.6.2. Creating `Jupyter Book` on your own
If you wan't to create `Jupyter Book` yourself follow steps described in following subchapters.

#### 1.6.2.1. Building the container
To build image with JupyterBook inside use this command as an example
```sh
./build.sh -i iwrap_book_al4 -v al4 -t book
```

#### 1.6.2.2. Running the Container
To execute Jupyter Book in an AL 4 environment, use the following shell command:

```sh
./start-tutorial-in-docker.sh -i iwrap_book_al4 -c iwrap_book
```

This command initiates a terminal session within the container named iwrap_book.

You should see a prompt similar to the following:

```sh
[root@5acc97e814b3 notebooks]#
```

#### 1.6.2.3. Building the HTML Output
To compile multiple `.html` files into a single HTML output, execute:
```sh
jupyter-book build .
```

This will generate a unified HTML file using Jupyter Book.
