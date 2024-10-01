########################################################################################################################
#                                                  SCRIPT FUNCTIONS                                                    #
########################################################################################################################

##########################################################################
#                             PRINT HELP                                 #
##########################################################################
print_usage() {
    echo -e "Usage:"
    echo -e "\t set-env.sh <test_site> <compiler_vendor> <AL_version>"
    echo
    echo -e "Mandatory positional arguments:"
    echo -e "\t <test site> \t Specifies the site, where environment is configured . Available values: 'iter', 'gw'."
    echo -e "\t <compiler vendor> \t Compiler vendor. Available values: 'gcc', 'intel'."
    echo -e "\t <AL_version>   \t Defines IMAS Access Layer version. Available values: '4', '5'."
}


##########################################################################
#                         PRINT CONFIGURATION                            #
##########################################################################
print_tests_configuration() {
    echo -e "------------------------------------------------------------"
    echo -e "Environment configuration:"
    echo -e "\t Current site:     '${current_site}'"
    echo -e "\t Compiler vendor:  '${compiler_vendor}'"
    echo -e "\t AL version:       '${al_version}'"
    echo -e "------------------------------------------------------------"
}

########################################################################################################################
#                                                 SCRIPT STARTS HERE                                                   #
########################################################################################################################

# Check script arguments
if [ "$#" -ne 3 ]; then
  # incorrect number of script arguments
  print_usage
  return 1
fi


# Set variables
current_site=${1}
compiler_vendor=${2}
al_version=${3}


export IWRAP_HOME=$(realpath "$(dirname ${BASH_SOURCE})/..")

configuration_file=${IWRAP_HOME}/envs/${current_site,,}/${compiler_vendor,,}/al${al_version}/configure_env.sh

# Check if configuration file exists
echo  ${configuration_file}
if [ ! -e ${configuration_file} ]; then
    echo "Cannot find configuration file for site: '${current_site}', compiler '${compiler_vendor}' and AL '${al_version}'"
    return 1
fi

# Set up environment such that module files can be loaded
if test -f /etc/profile.d/modules.sh ;then
. /etc/profile.d/modules.sh
else
. /usr/share/Modules/init/sh
fi
module purge

# Read and set environment configuration
source ${configuration_file}
source ${IWRAP_HOME}/envs/common/01_set_iwrap_env.sh

print_tests_configuration
