# Use an ARG for common variables that don't change between builds
ARG BASE
ARG USERNAME="iwrap_user"

# Base stage for installing Jupyter and other dependencies
FROM ${BASE} AS base_install

RUN yum install -y \
        nano \
        pandoc \
        python3.11-tkinter \
        firefox \
        tree \
        epel-release \
        chromium && \
    pip3 install --upgrade --force-reinstall \
        jupyterlab==4.2.0 \
        jupyter_book==1.0.0 \
        bash_kernel==0.9.3 \
        docutils==0.20.1 \
        jupyterlab_myst==2.4.2 \
        lxml==5.2.1 \
        markdown_it_py==3.0.0 \
        mdit_py_plugins==0.4.0 \
        myst-nb==1.1.0 \
        myst-parser==2.0.0 \
        sphinx==7.3.7 \
        sphinx_inline_tabs==2023.4.21 \
        tomli==2.0.1 \
        typing-extensions==4.11.0 && \
    ln -s /usr/bin/python3 /usr/bin/python

# Common stage for setting up the user, copying documentation, and setting common configurations
FROM base_install AS setup_docs

ARG USERNAME
RUN useradd -m -U ${USERNAME} && \
    mkdir /docs && \
    chown ${USERNAME}:${USERNAME} /docs

COPY --chown=${USERNAME}:${USERNAME} scripts/tutorial-entrypoint.sh /docs/scripts/

# Setting common configurations for both Jupyter Book and Tutorial
WORKDIR /docs
USER ${USERNAME}
ENV USER=${USERNAME} \
    PATH=${PATH}:/docs/scripts
RUN git config --global --add safe.directory /opt/iwrap

# Specific stage for Jupyter Tutorial
FROM setup_docs AS jupyter_tutorial

RUN chmod +x /docs/scripts/tutorial-entrypoint.sh
ENTRYPOINT ["./scripts/tutorial-entrypoint.sh"]