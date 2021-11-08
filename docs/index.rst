.. iWrap documentation master file, created by
   sphinx-quickstart on Fri Mar 26 16:31:43 2021.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to iWrap's documentation!
=================================

iWrap is a modular component generator, implemented in Python, used for creating IMAS actors from physics models. This mechanism allows to integrate physics codes written in one language (Fortran, CPP) within complex computing scenarios designed in other language (e.g. Python).
It's plug-in based modular design with clear separation of concerns allows to generate various types of actors and easily change data access paradigm (from dataset descriptor for AL to direct HDC data for instance)

For user conveniency it provides two kinds of interfaces:

* user friendly graphical interface that allows non-experienced users to define an actor in intuitive way
* command line interface foreseen for more advanced users that may want to e.g. automatise actor generation process using scripts.

.. toctree::
   :maxdepth: 1
   :caption: For users:

   /iwrap_confluence_documentation/index.rst

.. toctree::
   :maxdepth: 1
   :caption: For developers:

   /documentation_files/docstring_documentation.rst



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

