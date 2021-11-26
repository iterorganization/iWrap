==================================================
Scientific Worfklows : iWrap - graphical interface
==================================================

.. container::
   :name: page

   .. container:: aui-page-panel
      :name: main

            .. note::

               .. container:: confluence-information-macro-body

                  **This tutorial explains:**

                  -  iWrap graphic user interface
                  -  How to set up actor description
                  -  How to set up code description 

            .. note::

               .. container:: confluence-information-macro-body

                  All fields marked with \ **an
                  asterisk**\ **('*')**\ are mandatory.

            .. rubric:: 1.iWrap purpose
               :name: iWrapgraphicalinterface-iWrappurpose

            -  iWrap generates a Fortran/CPP wrapper, which
               intermediates between Kepler actor and user code in terms
               of:

               -  reading/writing of in/out physical data (IDS)
               -  passing other arguments to/from the actor

            -  iWrap creates a Python script (aka an actor) that:

               -  calls a user code
               -  provides error handling
               -  calls debugger (if run in "debug" mode)

            .. rubric:: 2.iWrap main window
               :name: iWrapgraphicalinterface-iWrapmainwindow

            |image1|

            .. rubric:: 3.Actor description
               :name: iWrapgraphicalinterface-Actordescription

            |image2|

            This group of graphical controls allows setting the
            description of the actor.

            -  Actor name - a user-defined name of the actor
            -  Actor type - a user-defined type of the actor
            -  Data type -  an actor data type

            .. rubric:: 4.Code description
               :name: iWrapgraphicalinterface-Codedescription

            .. rubric:: 4.1.Arguments
               :name: iWrapgraphicalinterface-Arguments

            |image3|

            In the table, there are columns like:

            -  **Name** - user defined name of an argument
            -  **Input/Output** - defines argument as \ *an
               input/output*
            -  **Type** - defines an IDS based type of argument (e.g.
               equilibrium, topinfo, etc.) 

            | 

            On the right side of the table, there is a section with
            buttons. Add button allows to add a new argument, edit to
            edit argument and up/down buttons are changing the position
            of selected argument in the table. The remove button removes
            selected argument.

            .. container::
            .. note::

               .. container:: confluence-information-macro-body

                  To change **Edit**, **Up**/**Down,** and **Remove**
                  buttons state to active at least one argument has to
                  be added to the table. 

            .. rubric:: 4.1.1. Add argument
               :name: iWrapgraphicalinterface-Addargument

            |image4|

            To add new arguments click **Add...** button in the
            *Arguments* section. A new window named *iWrap - Add new
            argument* will appear. Filling name (argument can be also
            added with empty name) and selecting intent and type allow
            adding new argument by clicking **Add** button. Click
            **Cancel** to exit this window. 

            .. rubric:: 4.1.2.Edit argument
               :name: iWrapgraphicalinterface-Editargument

            |image5|

            There is also possible to edit arguments. When **Edit...**
            button in the *Arguments* section is clicked, a new window
            *iWrap - Edit argument* will appear. Change settings and
            approve it by clicking **Close** button. To exit the window
            click **Cancel**.

            .. rubric:: 4.2.Implementation
               :name: iWrapgraphicalinterface-Implementation

            |image6|

            .. rubric:: 4.2.1.Implementation
               :name: iWrapgraphicalinterface-Implementation.1

            |image7|

            -  **Programming language** - a user code language
            -  **Data** **type** - data type handled by the physics code
            -  **Rood dir** - the root directory
            -  **Code** **path** - path to system library (C, CPP),
               script (Python), etc, containing the physics code and
               method/subroutine to be run
            -  **Include path** -  a module's / header's file path

            .. rubric:: 4.2.1.1.Subroutines tab
               :name: iWrapgraphicalinterface-Subroutinestab

            A user code should be provided as a subroutine.

            |image8|

            -  **Init** - a name of a subroutine that could be used to
               initialize the native code (optional)
            -  **Main** - a name of the main subroutine that will be
               called from actor (mandatory)
            -  **Finalize **- a name of a subroutine that could be used
               to finalize the native code (optional)

            .. rubric:: 4.2.1.2.Code parameters tab
               :name: iWrapgraphicalinterface-Codeparameterstab

            *Code parameters*\ are all parameters that are specific to
            the code (like switches, scaling parameters, and parameters
            for built-in analytical models) as well as parameters to
            explicitly overrule fields in the IMAS data structures.

            |image9|

            -  **Code parameters file** - XML file contains code
               parameters 
            -  **Schema file** - XSD file contains a schema

            .. rubric:: 4.3.Settings
               :name: iWrapgraphicalinterface-Settings

            |image10|

            .. rubric:: 4.3.1.Settings
               :name: iWrapgraphicalinterface-Settings.1

            |image11|

            -  **Compiler cmd** - the compiler command used to compile
               native codes
            -  **OpenMP switch** - theOpenMP switch
            -  **MPI compiler cmd** - the MPI compiler command

            .. rubric:: 4.3.2.Extra libraries
               :name: iWrapgraphicalinterface-Extralibraries

            |image12|

            .. rubric:: 4.3.2.1. pkg-config defined:
               :name: iWrapgraphicalinterface-pkg-configdefined:

            pkg-configs defined are system libraries handled by the
            pkg-config mechanism and required for building the user
            code.

            |image13|

            In the table there is information about:

            -  Name - a library name
            -  Info - information about library
            -  Description - a library description 

            pkg-config defined can be added and removed using buttons
            placed on the right side of the table.

            .. container::
            .. note::

               .. container:: confluence-information-macro-body

                  To change **Remove** button state to active at least
                  one system library has to be added and selected.

            .. rubric:: 4.3.2.1.1.Add pkg-config defined
               :name: iWrapgraphicalinterface-Addpkg-configdefined

            To add new arguments click **Add...** button in the
            *pkg-config defined*\ section. A new window named *Add
            system library* will appear. The *search *\ field allows for
            easy filtering of the list of libraries. To add a pkg-config
            definedselect your choice and click **Ok** button. To close
            the window click **Cancel**.

            |image14|

            .. rubric:: 4.3.2.2.Path defined
               :name: iWrapgraphicalinterface-Pathdefined

            *Path defined* are non-standard static libraries required
            for building the user code.

            |image15|

            Available operations on libraries list:

            -  **Add...** - Adds a new library to the list
            -  **Remove** - Removes a new library from the list

            | 

            .. container::
            .. note::

               .. container:: confluence-information-macro-body

                  To change **Remove** button state to active at least
                  one library path has to be added and selected.

            .. rubric:: 4.4.Documentation
               :name: iWrapgraphicalinterface-Documentation

            The\ *Documentation*\ tab specifies a user-defined actor
            description.

            |image16|






.. |image1| image:: attachments/70877876/77367779.png
   :class: confluence-embedded-image
   :width: 450px
.. |image2| image:: attachments/70877876/77367784.png
   :class: confluence-embedded-image
   :width: 450px
.. |image3| image:: attachments/70877876/77367785.png
   :class: confluence-embedded-image
   :width: 450px
.. |image4| image:: attachments/70877876/77367789.png
   :class: confluence-embedded-image
   :width: 450px
.. |image5| image:: attachments/70877876/77367790.png
   :class: confluence-embedded-image
   :width: 450px
.. |image6| image:: attachments/70877876/77367793.png
   :class: confluence-embedded-image
   :width: 450px
.. |image7| image:: attachments/70877876/77367794.png
   :class: confluence-embedded-image
   :width: 450px
.. |image8| image:: attachments/70877876/77367796.png
   :class: confluence-embedded-image
   :width: 450px
.. |image9| image:: attachments/70877876/77367798.png
   :class: confluence-embedded-image
   :width: 450px
.. |image10| image:: attachments/70877876/77370352.png
   :class: confluence-embedded-image
   :width: 450px
.. |image11| image:: attachments/70877876/77370355.png
   :class: confluence-embedded-image
   :width: 450px
.. |image12| image:: attachments/70877876/77367809.png
   :class: confluence-embedded-image
   :width: 450px
.. |image13| image:: attachments/70877876/77367810.png
   :class: confluence-embedded-image
   :width: 450px
.. |image14| image:: attachments/70877876/70878345.png
   :class: confluence-embedded-image
   :width: 500px
.. |image15| image:: attachments/70877876/77367815.png
   :class: confluence-embedded-image
   :width: 450px
.. |image16| image:: attachments/70877876/77367832.png
   :class: confluence-embedded-image
   :width: 450px
.. |image17| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image18| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image19| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image20| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image21| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image22| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image23| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image24| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image25| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image26| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image27| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image28| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image29| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image30| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image31| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image32| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image33| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image34| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image35| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image36| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image37| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image38| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image39| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image40| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image41| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image42| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image43| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image44| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image45| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image46| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image47| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image48| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image49| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image50| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image51| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image52| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image53| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image54| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image55| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image56| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image57| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image58| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image59| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image60| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image61| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image62| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image63| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image64| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image65| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image66| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image67| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image68| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image69| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image70| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image71| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image72| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image73| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image74| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image75| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image76| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image77| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image78| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image79| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image80| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image81| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image82| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image83| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image84| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image85| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image86| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image87| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image88| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
.. |image89| image:: images/icons/bullet_blue.gif
   :width: 8px
   :height: 8px
