# Release notes

## Pre-release version

### New features
TBD

### YAML changes
* Code parameters format added
  * Field: `code_description/implementation/code_parameters/format`, optional
  * Available values: `legacy-xml` (default), `xml`, `namelist`, `yaml`, `json` 

### API version: 2.1
Minor version number changed due to:
* new field added to YAML


## Version 0.10.0
### New Features
* Wrapping Java code into a Python actor
* Unification of INIT/MAIN/FINALIZE parameters
* Introducing iWrap <--> plugins compatibility check mechanism
* New method of discovering of platform config file, without usage of `imas-config`
* Fortran wrapper: Optimisation of string conversion Fortran <--> Py
* Tutorial for the beginners: refactoring, single access point to tutorial and manual
* Minor improvements and corrections

### YAML changes
* `code_description/settings/open_mp_switch` (`boolean`) renamed to `compiler_flags` (`string`)
* Unification of methods INIT/MAIN/FINALIZE (`code_description/implementation/subroutines/<method>`) parameters
  * Method name `..../subroutines/<method>` (`string`) moved to `.../subroutines/<method>/name` (`string`)
  * Every method has its own set of arguments: `.../subroutines/<method>/arguments` (`list of Arguments`)
  * Arguments that were common for INIT and MAIN (`code_description/arguments`) removed
  * Flag determining if the method uses code parameters `subroutines/<method>/need_code_parameters` (`boolean`) added

## Version 0.9.3
* Hotfix:  Strings in actor provenance info are no longer put into columns of length 80 characters

## Version 0.9.2
* Fix of an actor debug mechanism:
  * Corrections of Debug.ATTACH mode
  * Debug.STANDALONE: Preventing from cleaning up source files

## Version 0.9.1
* Hotfix: GUI crashes due to incorrect handling of tooltips

## Version 0.9.0
* Add tutorial for the beginners
* Add tooltips in GUI
* Corrections of IDS storage handling

## Version 0.8.1
* Hotfix: Using UAL_VERSION if AL_VERSION is not set

## Version 0.8.0
* Compatibility with Access-Layer 5.x 
* Adding custom actor logging method 
* Adding mechanism for backward compatibility of YAML description
* Adding specific checks for code description of a given actor type
* Allow usage of system variables to specify compiler 
* Improved handling of code parameters 
* Minor bugs corrections

## Version 0.7.0

### New Features
* improved packaging, versioning and install
* declaration of the data-dictionary compliancy
* record static metadata
* improved plugin discovery mechanism
* add the ability to generate actor skeletons

### YAML changes
* Added `data_dictionary_compliant`
  * Mandatory: `YES` 
  * Path: `code_description/implementation/data_dictionary_compliant`
  * Value: IMAS Data Dictionary compliant version (`IMAS_VERSION`)
  * Type: string
  * Example: `3.37.0`
  
## Version 0.6.0

### New Features
* Handling GET_STATUS of native code API
* Handling SET_STATUS of native code API
* Handling GET_TIMESTAMP of native code API
* Minor bug corrections

### YAML changes
* Added `GET_STATE`
  * Mandatory: `NO` (only if `SET_STATE` specified)
  * Path: `code_description/implementation/subroutines/get_state`
  * Value: The name of the `GET_STATE` method 
  * Type: string
  * Example: `get_code_state`
* Added `SET_STATE`
  * Mandatory: `NO` (only if `GET_STATE` specified)
  * Path: `code_description/implementation/subroutines/set_state`
  * Value: The name of the `SET_STATE` method 
  * Type: string
  * Example: `restore_code_state`
* Added `GET_TIMESTAMP`
  * Mandatory: `NO` 
  * Path: `code_description/implementation/subroutines/get_timestamp`
  * Value: The name of the `GET_TIMESTAMP` method 
  * Type: string
  * Example: `get_timestamp`

## Version 0.5.0
* provides unique temporary storage per actor, now located in sandbox dir
* resolves system variables for install dir
* redesign handling of XML parameters in standalone mode
* access to documentation through iwrap-doc command
* change "mpi_nodes" to "mpi_processes" in runtime settings
* other bug fixes and improvements

## Version 0.4.1
* Correcting actor install location
* Improved argument checking
* Remove dependency on xmllib
* Fix XML params initialization
* GUI improvements: add actor install path and better build progress window

## Version 0.4.0
* A number of YAML and (corresponding) GUI changes
* ‘Per platform’ iWrap configuration
* Handling relative paths, sandbox, batch jobs
* Improved generators management
* Improved IDS data types handling
* Memory management analysis and corrections (
* iWrap manual update
* Developer guide added

## Version 0.3.0
* Validation of actor/code description before actor generation plus corrections/improvements resulting from this feature
* Serialisation of actor description
* Setting installation path
* Minor improvements and bug fixes

## Version 0.2.0
* CPP wrapper implemented
* Handling MPI and standalone runs of native code
* Adding actor debug modes (STANDALONE and ATTACH)
* Choosing backend for 'IDS cache'
* Adding examples: CPP and MPI
* Integration tests

## Version 0.1.0
iWarp first prototype.
