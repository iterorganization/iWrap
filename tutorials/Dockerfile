ARG IWRAP_DIR='/opt/iwrap'
ARG XMLLIB_DIR='/opt/xmllib'
ARG USERNAME="iwrap_user"
ARG JUPYTER_VER="jupyter-book"
ARG BASE

FROM ${BASE} AS install_jupiter

RUN yum install -y \
        nano \
        pandoc \
        python3.11-tkinter \
        firefox \
        tree \
    && pip3 install --upgrade --force-reinstall \
        jupyterlab==4.0.9 \
        jupyter_book==0.15.1 \
        bash_kernel==0.9.3 \
        docutils==0.18.1 \
        jupyterlab_myst==2.1.0 \ 
        lxml==4.9.3 \
        markdown_it_py==2.2.0 \
        mdit_py_plugins==0.3.5 \ 
        myst-nb==0.17.2 \
        myst-parser==0.18.1 \
        sphinx==5.0.2 \
        sphinx_inline_tabs==2023.4.21 \ 
        tomli==2.0.1 \
        typing-extensions==3.10.0.2 \
    && python3 -m bash_kernel.install \
    && ln --symbolic /usr/bin/python3 /usr/bin/python

FROM install_jupiter AS jupyter_book

ARG USERNAME
RUN useradd -m -U ${USERNAME} \
    && mkdir tutorials \
    && chown ${USERNAME}:${USERNAME} tutorials
        
COPY --chown=${USERNAME}:${USERNAME} sources/ /tutorials/sources/
COPY --chown=${USERNAME}:${USERNAME}  markdowns/ /tutorials/markdowns/

RUN chmod +x /tutorials/sources/scripts/book-entrypoint.sh

WORKDIR /tutorials

USER ${USERNAME}

ENV USER=${USERNAME} \
    PATH=${PATH}:/tutorials/sources/scripts

RUN git config --global --add safe.directory /opt/iwrap

ENTRYPOINT [ "./sources/scripts/book-entrypoint.sh" ]
CMD ["/bin/bash"]


#### deploy interactive JupiterLab
FROM install_jupiter AS jupyter_tutorial

ARG USERNAME
RUN useradd -m -U ${USERNAME} \
    && mkdir tutorials \
    && chown ${USERNAME}:${USERNAME} tutorials
        
COPY --chown=${USERNAME}:${USERNAME} sources/ /tutorials/sources/
COPY --chown=${USERNAME}:${USERNAME}  markdowns/ /tutorials/markdowns/

RUN chmod +x /tutorials/sources/scripts/tutorial-entrypoint.sh

WORKDIR /tutorials

USER ${USERNAME}

ENV USER=${USERNAME} \
    PATH=${PATH}:/tutorials/sources/scripts

RUN git config --global --add safe.directory /opt/iwrap

ENTRYPOINT [ "./sources/scripts/tutorial-entrypoint.sh" ]