#######################################################################################################################
Developers manual
#######################################################################################################################

Introduction
############

The main purpose of the iWrap is automatic generation of 'actors'. An actor is a component that 'wraps' provided physics models,
enhancing them with an extra functionality. The added features depend on the actor type and may include, e.g.:

* calling model subroutines using other programming language than those used for model implementation,
  to allow integration of physics codes within complex computing scenarios
* mechanisms for interoperation of various models within given workflow orchestration system in terms of exchange of model input/output data
* support for configuration of an actor execution (sequential run, parallel mode, usage of sandbox, etc etc)
* additional features that allows for easy debugging, logging etc, etc

To create an actor the iWrap utilizes a concept of code 'generators' that, called by iWrap 'engine' produce sources
constituting particular layers of the actor.

This manual shows the internals of the iWrap focusing on the actor generation mechanisms
and provides guidelines describing the way a new types of actor generators could be added.


