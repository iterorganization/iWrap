module purge
module load cineca
module load itm-python/3.6


if (! $?PYTHONPATH) then  
    setenv PYTHONPATH ""
endif

setenv PYTHONPATH $PWD/abcd/actor:$PYTHONPATH
setenv PYTHONPATH $PWD/xyz/actor:$PYTHONPATH
setenv PYTHONPATH $PWD/../common/python:$PYTHONPATH