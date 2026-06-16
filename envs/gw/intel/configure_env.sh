# Set up GW Intel environment — extends ITER Intel config with GW-specific module path
source /usr/share/Modules/init/sh
module use /gw/swimas/easybuild/etc/all
source ${IWRAP_HOME}/envs/iter/intel/configure_env.sh
