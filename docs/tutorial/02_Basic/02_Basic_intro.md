# Tutorial For Beginners

```{admonition} Note!
This part is divided into three chapters:
```

## 1_Actor_Fundamentals

This chapter introduces essential practices for adapting physics codes to IMAS and outlines the minimum standards for actor implementation.   
It details the process of describing codes in iWrap and generating basic Python actors.   
The chapter concludes by explaining the importation and sequential execution of these Python actors within a Python script.

***Keywords:*** `IMAS` adaptation, `MAIN` method,     code compilation,  code packaging, `iWrap` code description, `iWrap`  actor creation, `iWrap` actor usage, `Python` workflow


## 2_INIT_FINALIZE

Building upon the initial creation of actors using iWrap, this chapter guides readers through enhancing their code interfaces with optional initialization and finalization functions.  
It covers their description in iWrap and the integration of these advanced actor functionalities within a Python workflow.  

***Keywords:*** `INIT` method, `FINALIZE` method

##  3_Code_Parameters

In this chapter you will learn that in addition to IDS data, you can pass _code parameters_ for the static configuration of 
individual codes. After explaining how to describe such code parameters in XML/XSD, the section will show how to adapt the code description 
in iWrap and how to check and modify these parameters for a given actor in a Python workflow.  

***Keywords:*** `XML` code parameters, `XSD` code parameters, static configuration