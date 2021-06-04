from iwrap.settings import project
from iwrap.settings.code_description import Argument
from iwrap.settings.language_specific.fortran_settings import FortranSpecificSettings

from iwrap.settings.project import ProjectSettings
from iwrap.settings.serialization import YAMLSerializer

project_settings = ProjectSettings.get_settings()

project_settings.actor_name = 'test actor'
project_settings.actor_type = 'Python actor'
project_settings.data_type = 'LEGACY_IDS'

code_description = project_settings.code_description

code_description.programming_language = 'Fortran'
code_description.code_name = 'demo_code'
code_description.data_type = 'IDS'

code_description.arguments = [
    Argument( {'name': 'equilibrium00', 'type': "equilibrium", 'intent': Argument.IN} ),
    Argument( {'name': 'equilibrium01', 'type': "equilibrium", 'intent': Argument.IN} ),
    Argument( {'name': 'equilibrium10', 'type': "equilibrium", 'intent': Argument.OUT} ),
    Argument( {'name': 'equilibrium11', 'type': "equilibrium", 'intent': Argument.OUT} ), ]

code_description.code_path = './lib/libdemo.a'
code_description.code_parameters.parameters = './code_paramneters/parameters.xml'
code_description.code_parameters.schema = './code_paramneters/parameters.xsd'

code_description.documentation = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ' \
                                 'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. '

language_specific = FortranSpecificSettings()

language_specific.compiler = "Intel"
language_specific.mpi = 'MPICH2'
language_specific.open_mp = False
language_specific.system_libraries = ['fftw3f', 'glib', 'mkl', ]
language_specific.custom_libraries = ['./lib/custom/libcustom1.a', './lib/custom/libcustom2.a']

code_description.language_specific = language_specific


#Saving project settings to YAML file

file = open( 'test.yaml', 'w' )
project.ProjectSettings.get_settings().save( YAMLSerializer( file ) )

#Loading project settings from file
file = open( 'test.yaml', 'r' )
project.ProjectSettings.get_settings().clear()
project.ProjectSettings.get_settings().load( YAMLSerializer( file ) )

#printing YAML content
file = open( 'test.yaml', 'r' )
print( file.read() )
