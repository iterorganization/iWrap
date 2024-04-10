#!/bin/sh --login

#module unload IMAS/3.39.0-4.11.7-foss-2020b
#module load IMAS/3.39.0-5.0.0-foss-2020b
#echo  "-----------------Loaded IMAS 3.39.0-5.0.0 -----------------"

iwrap --version
echo "-----------------iWrap version-----------------"

./docs/scripts/create_db
echo "-----------------IMAS DB created------------"
