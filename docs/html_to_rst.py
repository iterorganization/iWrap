import glob
import os
import pypandoc
from pypandoc.pandoc_download import download_pandoc

os.chdir("iwrap_confluence_documentation/")
download_pandoc(version='2.14.2')

for file in glob.glob("*.html"):
    rst = pypandoc.convert_file(f'{file}', 'rst',  extra_args=['-s'])
    rst_file = open(f"{file[:-4]}rst", "w")
    rst_file.write(rst)

