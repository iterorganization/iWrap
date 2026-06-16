########################################################################################################################
#                                                  SCRIPT FUNCTIONS                                                    #
########################################################################################################################

##########################################################################
#                             PRINT HELP                                 #
##########################################################################
print_usage() {
    echo -e "Usage:"
    echo -e "\t set-env.sh <test_site> <compiler_vendor>"
    echo
    echo -e "Mandatory positional arguments:"
    echo -e "\t <test site> \t Specifies the site, where environment is configured . Available values: 'iter', 'gw'."
    echo -e "\t <compiler vendor> \t Compiler vendor. Available values: 'gcc', 'intel'."
}


##########################################################################
#                         PRINT CONFIGURATION                            #
##########################################################################
print_tests_configuration() {
    echo -e "------------------------------------------------------------"
    echo -e "Environment configuration:"
    echo -e "\t Current site:     '${current_site}'"
    echo -e "\t Compiler vendor:  '${compiler_vendor}'"
    echo -e "------------------------------------------------------------"
}

########################################################################################################################
#                                                 SCRIPT STARTS HERE                                                   #
########################################################################################################################

# Check script arguments
if [ "$#" -ne 2 ]; then
  # incorrect number of script arguments
  print_usage
  return 1
fi


# Set variables
current_site=${1}
compiler_vendor=${2}


export IWRAP_HOME=$(realpath "$(dirname ${BASH_SOURCE})/..")

configuration_file=${IWRAP_HOME}/envs/${current_site,,}/${compiler_vendor,,}/configure_env.sh

# Check if configuration file exists
echo  ${configuration_file}
if [ ! -e ${configuration_file} ]; then
    echo "Cannot find configuration file for site: '${current_site}', compiler '${compiler_vendor}'"
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
