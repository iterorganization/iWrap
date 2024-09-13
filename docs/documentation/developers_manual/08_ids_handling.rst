#######################################################################################################################
IDS Language-to-Language Conversions
#######################################################################################################################

Currently, iWrap handles only the so-called "legacy" IDS type (static structures). Legacy IDSes are very large and deep, 
tree-like objects defined in the Data Dictionary and implemented by IMAS Access Layer High-Level Interfaces. 
Unfortunately, there are no methods other than passing IDS via files to transfer IDSes from code implemented in one 
programming language to code written in another. This chapter describes how IDSes are passed between layers 
of a `Simple Python` actor.

IDS Converters
#######################################################################################################################

The Binder uses generic ``converters`` to translate IDS from the language of the actor to the language of the physics model 
and back.

.. note::

   Refer to the class :py:class:`iwrap.generators.binder_generators.python.common.data_storages.IDSConverter()`
   :py:mod:`iwrap.generators.binder_generators.python.common.data_storages`
                                 iwrap/generators/binder_generators/python/common/data_storages/__init__.py
   :py:class:`iwrap.generators.actor_generators.python_actor.resources.common.binder.Binder`
   to review the ``converter`` API, and :py:class:`iwrap.generators.binder_generators.python.common.data_storages.ids_converter.LegacyIDSConverter`
   to see the `legacy IDS` converter implementation.

.. code-block:: python

    class IDSConverter():

        @classmethod
        def data_type(cls) -> str:
            ...

        @classmethod
        def code_languages(cls) -> Set[str]:
            ...

        def convert_to_native_type(self, ids_description: IDSDescription, ids_intent, ids_object):
            ...

        def convert_to_actor_type(self, ids_description: IDSDescription, ids_intent, ids_object):
            ...

        def release(self, ids_description: IDSDescription):
            ...

Converter Methods
===========================================================================================
* ``prepare_native_type`` - Allocates required resources. (In ``LegacyIDSConverter``, this creates an IDS metadata description.)

* ``convert_to_native_type`` - In ``LegacyIDSConverter``, this saves the Python IDS object using the Access Layer.

* ``convert_to_actor_type`` - In ``LegacyIDSConverter``, this reads the Python IDS object using the Access Layer.

* ``release`` - Releases allocated resources (if any).

Converter Description
===========================================================================================

* ``data_type`` - Defines the type of IDSes handled by the converter (currently ``legacy`` only).
* ``code_languages`` - Defines the languages of the wrapped code that the converter handles.

Passing IDS Between Various Languages / Actor Layers
#######################################################################################################################

Two iWrap layers are particularly important for data conversion and management: the binding layer (implemented in the 
workflow/actor language, typically Python) and the wrapper layer (implemented in the code language, typically C++/Fortran).

The scenario for transferring an IDS from the top (workflow) layer to the lowest (code) layer is as follows:

1. **Workflow Layer (Python)**

   a. The IDS object is created or loaded by the workflow script.

   b. The workflow calls an actor method, passing the IDS object(s) as argument(s).

2. **Actor Layer (Python)**

   a. The actor's method receives the IDS object(s).

   b. No operations on IDSes are performed (the actor layer is IDS-type agnostic and simply passes IDS up and down).

   c. The actor calls the binder, passing the IDS object(s) as argument(s).

3. **Binder Layer (Python)**

   a. The binder receives the IDS object(s).

   b. The binder uses a temporary IDS storage to save the input IDS object(s), obtaining metadata describing the IDS(es).

   c. The binder requests the IDS storage to prepare a place for storing output IDS(es), obtaining metadata describing the IDS(es).

   d. The binder creates objects derived from ``ctypes.Structure`` and places the IDS metadata there.

   e. The binder calls the wrapper, passing the IDS metadata object(s) as argument(s) using the ``ctypes`` mechanism.

4. **Wrapper Layer (C++/Fortran)**

   a. The wrapper receives the IDS metadata from the binder.

   b. The wrapper uses the metadata to read IDSes from the IMAS AL.

   c. The wrapper passes the IDS object(s) to the code.

.. note::

  * To speed up data transfer, IMAS AL Memory Backend is used where possible.

  * For easy adoption of other IDS types (HDC, IMASPy), the wrapper uses Jinja2 macros for IDS-type specific operations.
