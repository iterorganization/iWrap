mappings = [
    #add data_dictionary_compliant
    { # since 0.7.0
     'command':'add',
     'target':'code_description/implementation/data_dictionary_compliant',
     'value':'$SYS_VAR("IMAS_VERSION")',
     'condition':'$VALUE_OF("code_description/implementation/data_dictionary_compliant") is None' },

    #move function names into code_description/implementaton/subroutines/<subroutine>/name = <name>
    { # since 0.10.0
     'condition':'$TYPE_OF("code_description/implementation/subroutines/init") == str',
     'actions':[
        {'command':'move',
        'source':'code_description/implementation/subroutines/init',
        'target':'code_description/implementation/subroutines/init/name',
        },
        {'command':'add',
         'target':'code_description/implementation/subroutines/init/need_code_parameters',
         'value':True,
         'condition':'$VALUE_OF("code_description/implementation/code_parameters/parameters") is not None '
         },
     ]
    },
    { # since 0.10.0
     'condition':'$TYPE_OF("code_description/implementation/subroutines/main") == str',
     'actions':[
        {'command':'move',
         'source':'code_description/implementation/subroutines/main',
         'target':'code_description/implementation/subroutines/main/name',
        },
        #move arguments into code_description/implementaton/subroutines/main/arguments
        {'command':'move',
         'source':'code_description/arguments',
         'target':'code_description/implementation/subroutines/main/arguments',
         'condition':'$TYPE_OF("code_description/arguments") is not None'
         },
        {'command':'add',
         'target':'code_description/implementation/subroutines/main/need_code_parameters',
         'value':True,
         'condition':'$VALUE_OF("code_description/implementation/code_parameters/parameters") is not None '
         },
     ]
    },
    { # since 0.10.0
     'condition':'$TYPE_OF("code_description/implementation/subroutines/finalize") == str',
     'actions':[
        {'command':'move',
        'source':'code_description/implementation/subroutines/finalize',
        'target':'code_description/implementation/subroutines/finalize/name',
        }
     ]
    },

    #move deprecated code_description/settings/open_mp_switch
    { # since 0.10.0
     'command': 'move',
     'source': 'code_description/settings/open_mp_switch',
     'target': 'code_description/settings/compiler_flags',
     'condition': '$TYPE_OF("code_description/settings/open_mp_switch") is not None'},

    #set code_description/settings/compiler_flags to None if contains boolean value (due to probable usage of open_mp_switch as Boolean flag in past)
    { # since 0.10.0
     'command':'set',
     'target':'code_description/settings/compiler_flags',
     'value':None,
     'condition':'$TYPE_OF("code_description/settings/compiler_flags") is bool'}
]
