mappings = [
    #add data_dictionary_compliant
    {'command':'add',
     'target':'code_description/implementation/data_dictionary_compliant',
     'value':'$SYS_VAR("IMAS_VERSION")',
     'condition':'$VALUE_OF("code_description/implementation/data_dictionary_compliant") is None' },

    #move function names into code_description/implementaton/subroutines/<subroutine>/name = <name>
    {'command':'move',
     'source':'code_description/implementation/subroutines/main',
     'target':'code_description/implementation/subroutines/main/name',
     'condition': '$TYPE_OF("code_description/implementation/subroutines/main") == str'},
    {'command':'move',
     'source':'code_description/implementation/subroutines/init',
     'target':'code_description/implementation/subroutines/init/name',
     'condition': '$TYPE_OF("code_description/implementation/subroutines/init") == str'},
    {'command':'move',
     'source':'code_description/implementation/subroutines/finalize',
     'target':'code_description/implementation/subroutines/finalize/name',
     'condition': '$TYPE_OF("code_description/implementation/subroutines/finalize") == str'},

    #move arguments into code_description/implementaton/subroutines/main/arguments
    {'command':'move',
     'source':'code_description/arguments',
     'target':'code_description/implementation/subroutines/main/arguments',
     'condition':'$TYPE_OF("code_description/arguments") is not None'},

    #add code_description/implementaton/subroutines/<subroutine>/needs_code_parameters
    {'command':'add',
     'target':'code_description/implementation/subroutines/main/needs_code_parameters',
     'value':True,
     'condition':'$VALUE_OF("code_description/implementation/code_parameters/parameters") is not None'},
    {'command': 'add',
     'target': 'code_description/implementation/subroutines/init/needs_code_parameters',
     'value': True,
     'condition': '$VALUE_OF("code_description/implementation/code_parameters/parameters") is not None '
                  'and bool($VALUE_OF("code_description/implementation/subroutines/init"))'},

    #delete deprecated code_description/settings/open_mp_switch
    {'command': 'delete',
     'target': 'code_description/settings/open_mp_switch'},

    #set code_description/settings/compiler_flags to None if contains boolean value
    {'command':'set',
     'target':'code_description/settings/compiler_flags',
     'value':None,
     'condition':'$TYPE_OF("code_description/settings/compiler_flags") is bool'}

    ]
