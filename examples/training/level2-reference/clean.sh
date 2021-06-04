find . -name "*~" -delete
find . -name "*.pyc" -delete
find . -name "__pycache__" -delete
find . -name "*.o" -delete
find . -name "*.mod" -delete
find . -name "*.exe" -delete
find . -name "*.so" -delete

rm -f ./tmp/ids_*
