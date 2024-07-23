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

# iWrap's MUSCLE3 Actor Generation

```{admonition} Key takeaways
:class: tip

- iWrap enables seamless integration of Fortran and Python code by generating compatible MUSCLE3 actors, facilitating multiscale simulations.
- The process involves creating a detailed YAML description of the code, which is then used by iWrap to generate actors that can be executed within the MUSCLE3 framework.
- Properly configuring the yMMSL file is essential for defining the simulation model, components, conduits, settings, and resources, ensuring smooth execution of the simulation.
```

In this chapter, we will focus on creating a macro model written in Fortran that communicates with a basic Python MUSCLE3 actor. We will wrap the Python actor using iWrap and execute it using the MUSCLE3 manager.

## Simple Example of an iWrap-Compatible Python Actor

Here's a simple example of an iWrap-compatible Python actor:

```{code-block} python
:caption: basic.py

# basic.py
code_state = 0

def get_code_state():
    state_str = code_state
    print('=======================================')
    print('Code lifecycle: GET CODE STATE called')
    print('STATE is:', state_str)
    print('=======================================')
    return state_str

def restore_code_state(state_str):
    global code_state
    code_state = float(state_str)
    print('=======================================')
    print('Code lifecycle: RESTORE STATE called')
    print('STATE TO BE RESTORED:', code_state)
    print('=======================================')

def get_timestamp():
    timestamp_out = code_state
    return timestamp_out

def init_code():
    print('=======================================')
    print('Code lifecycle: INITIALISATION called')
    print('=======================================')

def clean_up():
    print('=======================================')
    print('Code lifecycle: FINALISATION called')
    print('=======================================')

def code_step(core_profiles_in, distribution_sources_out):
    global code_state
    print('=======================================')
    print('START OF PHYSICS CODE')
    print('=======================================')
    print('Starting from:', code_state)

    for i in range(1, 20):
        code_state += 1

    print('Counting to:', code_state)
    print('=======================================')

    distribution_sources_out.ids_properties.homogeneous_time = 1
    distribution_sources_out.code.name = 'EXAMPLE: code_restart'
    distribution_sources_out.code.version = '1.0'

    distribution_sources_out.time.resize(1)
    distribution_sources_out.time[0] = code_state

    print('END OF PHYSICS CODE')
    print('=======================================')

```

This actor will serve as a micro model in our simulation setup.

### iWrap Code Description

To wrap the above Python code using iWrap, create a YAML file describing the code:

```{code-block} yaml
:caption: basic_python.yml

code_description:
    implementation:
        subroutines:
            init: init_code
            main:
                name: code_step
                arguments:
                -   name: core_profiles_in
                    type: core_profiles
                    intent: IN
                -   name: distribution_sources_out
                    type: distribution_sources
                    intent: OUT
            finalize: clean_up
            get_state: get_code_state
            set_state: restore_code_state
        programming_language: Python
        data_dictionary_compliant: 3.37.0
        data_type: legacy
        code_path: basic.py
        include_path: basic.py
    documentation: 'Simple actor'
```

To create an actor from this code description, execute the following command:

```{code-block} sh
iwrap --actor-type muscle3-python -a basic_actor -f basic_python.yml
```

This command generates a `basic_actor` MUSCLE3 micro model in the `IWRAP_ACTORS` folder.

## Example of a Macro Model in Fortran

Next, we will define a macro component in Fortran. Below is an example:

```{code-block} fortran
:caption: macro.f90

program helloworld
    use ids_routines
    use ymmsl
    use libmuscle
    implicit none

    type (ids_core_profiles) :: core_profiles_out
    type (ids_distribution_sources) :: distribution_sources_in

    real (selected_real_kind(15)) :: t_cur, t_next, t_max, dt
    character(len=1), dimension(:), allocatable :: serialized_ids
    real :: time

    type(LIBMUSCLE_PortsDescription) :: ports
    type(LIBMUSCLE_Instance) :: instance

    type(LIBMUSCLE_Message) :: rmsg
    type(LIBMUSCLE_DataConstRef) :: ritem

    type(LIBMUSCLE_Message) :: smsg
    type(LIBMUSCLE_Data) :: sitem

    write(*,*) "Starting Fortran M3 macro"

    open(10, file="./test.out", status="NEW")
    
    ports = LIBMUSCLE_PortsDescription_create()
    call LIBMUSCLE_PortsDescription_add(ports, YMMSL_OPERATOR_O_I, 'core_profiles_out')
    call LIBMUSCLE_PortsDescription_add(ports, YMMSL_OPERATOR_S, 'distribution_sources_in')
    instance = LIBMUSCLE_Instance_create(ports, &
            LIBMUSCLE_InstanceFlags(KEEPS_NO_STATE_FOR_NEXT_USE=.true.))
    call LIBMUSCLE_PortsDescription_free(ports)

    do while (LIBMUSCLE_Instance_reuse_instance(instance))
        ! F_INIT
        t_max = LIBMUSCLE_Instance_get_setting_as_real8(instance, 't_max')
        dt = LIBMUSCLE_Instance_get_setting_as_real8(instance, 'dt')

       time = 1

        t_cur = 0.0
        do while (t_cur + dt < t_max)
            ! O_I
            t_next = t_cur + dt


            ! SENDING DATA
            allocate(core_profiles_out%ids_properties%comment(1))
            core_profiles_out%ids_properties%comment(1) = 'Hello world!'
            core_profiles_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS
            allocate(core_profiles_out%code%name(1))
            core_profiles_out%code%name = 'HelloWorld Actor in Fortran'

            allocate(core_profiles_out%time(1))
            core_profiles_out%time(1) = t_next


            call ids_serialize(core_profiles_out, serialized_ids)
            call ids_deallocate(core_profiles_out)

            sitem = LIBMUSCLE_Data_create_byte_array(serialized_ids)
            smsg = LIBMUSCLE_Message_create(t_cur, sitem)

            if (t_next + dt <= t_max) then
                call LIBMUSCLE_Message_set_next_timestamp(smsg, t_next)
            end if
            call LIBMUSCLE_Instance_send(instance, 'core_profiles_out', smsg)
            call LIBMUSCLE_Message_free(smsg)
            call LIBMUSCLE_Data_free(sitem)
            deallocate(serialized_ids)


            ! > > > - - - RECEIVING DATA - - - < < <

            rmsg = LIBMUSCLE_Instance_receive(instance, 'distribution_sources_in')
            ritem = LIBMUSCLE_Message_get_data(rmsg);
    
            allocate(serialized_ids(LIBMUSCLE_DataConstRef_size(ritem)))
            call LIBMUSCLE_DataConstRef_as_byte_array(ritem, serialized_ids)
            call LIBMUSCLE_DataConstRef_free(ritem)

    
            ! deserialize and verify received IDS
            call ids_deserialize(serialized_ids, distribution_sources_in)
            deallocate(serialized_ids)

            if (.not. associated(distribution_sources_in%time)) then
                call LIBMUSCLE_Instance_error_shutdown(instance, "Received TIME is not associated")
                call exit(1)
            end if

            write(*,*) "TIME received ", distribution_sources_in%time
            write(10,*) distribution_sources_in%time

            time = distribution_sources_in%time(1)

            call ids_deallocate(distribution_sources_in)
                
            if (LIBMUSCLE_Message_has_next_timestamp(rmsg)) then
                t_max = LIBMUSCLE_Message_next_timestamp(rmsg)
            end if

            call LIBMUSCLE_Message_free(rmsg)

            ! a simulation would actually update something here, but we're just saying
            ! hi to connected actors and don't do anything else
            t_cur = t_cur + dt
        end do

    end do

    close(10)

end program helloworld
```

This Fortran program defines a macro model using the muscle library to communicate with other components. It sends and receives data in a loop, performing simple actions like incrementing a time step.

To compile this program, use the following command:

```{code-block} sh
gfortran -g -fPIC `pkg-config --cflags --libs al-fortran ymmsl libmuscle_fortran` -o macro.exe macro.f90
```

## Writing the yMMSL File

Next, let's write a yMMSL file that will be used to run the simulation:

```{code-block} yaml
:caption: test.ymmsl

model:
  name: helloworld
  components:
    macro: macro
    micro: micro
  conduits:
    macro.core_profiles_out: micro.core_profiles_in
    micro.distribution_sources_out: macro.distribution_sources_in

settings:
  t_max: 4.0
  dt: 1.0
  muscle_remote_log_level: DEBUG

implementations:
  macro:
    executable: /pfs/work/g2bpogo/iwrap-muscle/macro.exe
  micro:
    executable: python
    args: /pfs/work/g2bpogo/IWRAP_ACTORS/basic_actor/standalone.py

resources:
  macro:
    threads: 1
  micro:
    threads: 1
```

This yMMSL file defines the model name, components, conduits, settings, implementations, and resources required to run the simulation. It maps the macro and micro components to their respective executables and sets the simulation parameters.

## Running the Simulation

To execute the simulation, enter the following command:

```{code-block} sh
muscle_manager --start-all --log-level DEBUG test.ymmsl
```

Upon successful execution, a `test.out` file will be generated containing the simulation results.

In this chapter, we have meticulously outlined the process of creating and integrating iWrap-compatible MUSCLE3 actors into a multiscale simulation framework. By adhering to these steps, you can seamlessly generate actors from both Python and Fortran code, ensuring efficient communication and execution within the MUSCLE3 system. This method showcases the distinct approach of utilizing the MUSCLE3 workflow engine through yMMSL descriptions, which contrasts with the direct scripting in python employed in previous chapters.
