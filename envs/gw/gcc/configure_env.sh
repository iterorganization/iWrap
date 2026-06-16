# Set up GW GCC environment — extends ITER GCC config with GW-specific module path
source /usr/share/Modules/init/sh
module use /gw/swimas/easybuild/etc/all
source ${IWRAP_HOME}/envs/iter/gcc/configure_env.sh
