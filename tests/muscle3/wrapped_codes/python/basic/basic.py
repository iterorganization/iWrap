code_state = 0


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                   GET CODE STATE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
def get_code_state():
    state_str =  code_state

    print( '=======================================' )
    print( 'Code lifecycle: GET CODE STATE called' )
    print( 'STATE is :', state_str )
    print( '=======================================' )

    return state_str


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                   SET CODE STATE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
def restore_code_state(state_str):
    global code_state

    code_state = float( state_str )
    print( '=======================================' )
    print( 'Code lifecycle: RESTORE STATE called' )
    print( 'STATE TO BE RESTORED :', code_state )
    print( '=======================================' )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                   GET TIMESTAMP
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
def get_timestamp():

    timestamp_out = code_state

    return timestamp_out


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                   INITIALISATION
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
def init_code ():

    print('=======================================')
    print('Code lifecycle: INITIALISATION called')
    print('=======================================')


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                   FINALISATION
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
def clean_up():

    print('=======================================')
    print('Code lifecycle: FINALISATION called')
    print('=======================================')



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                   MAIN
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
def code_step(core_profiles_in, distribution_sources_out):
    global code_state
    print('=======================================')
    print('START OF PHYSICS CODE')
    print('=======================================')
    print('Starting from: ', code_state)

    for i in range (1, 20):
        # COMPUTATIONS
        code_state = code_state + 1

    print('Counting to: ', code_state)

    print('=======================================')

    # MANDATORY FLAG (UNIFORM TIME HERE)
    distribution_sources_out.ids_properties.homogeneous_time = 1

    distribution_sources_out.code.name   = 'EXAMPLE: code_restart'
    distribution_sources_out.code.version   = '1.0'

    distribution_sources_out.time.resize(1)
    distribution_sources_out.time[0] = code_state

    # FINAL DISPLAY
    print('END OF PHYSICS CODE')
    print('=======================================')



