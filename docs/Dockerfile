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
        typing-extensions==3.10.0.2 && \
    ln -s /usr/bin/python3 /usr/bin/python

# Common stage for setting up the user, copying documentation, and setting common configurations
FROM base_install AS setup_docs

ARG USERNAME
RUN useradd -m -U ${USERNAME} && \
    mkdir /docs && \
    chown ${USERNAME}:${USERNAME} /docs

COPY --chown=${USERNAME}:${USERNAME} documentation /docs/documentation
COPY --chown=${USERNAME}:${USERNAME} images/ /docs/images
COPY --chown=${USERNAME}:${USERNAME} scripts/ /docs/scripts
COPY --chown=${USERNAME}:${USERNAME} tutorial /docs/tutorial
COPY --chown=${USERNAME}:${USERNAME} _config.yml /docs/_config.yml
COPY --chown=${USERNAME}:${USERNAME} _toc.yml /docs/_toc.yml
COPY --chown=${USERNAME}:${USERNAME} iWrap_introduction.md /docs/iWrap_introduction.md

# Setting common configurations for both Jupyter Book and Tutorial
WORKDIR /docs
USER ${USERNAME}
ENV USER=${USERNAME} \
    PATH=${PATH}:/docs/scripts
RUN git config --global --add safe.directory /opt/iwrap

# Specific stage for Jupyter Book
FROM setup_docs AS jupyter_book

RUN chmod +x /docs/scripts/book-entrypoint.sh
ENTRYPOINT ["./scripts/book-entrypoint.sh"]
CMD ["/bin/bash"]

# Specific stage for Jupyter Tutorial
FROM setup_docs AS jupyter_tutorial

RUN chmod +x /docs/scripts/tutorial-entrypoint.sh
ENTRYPOINT ["./scripts/tutorial-entrypoint.sh"]
