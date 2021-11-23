import glob
import os
import pypandoc
from pypandoc.pandoc_download import download_pandoc

os.chdir("resources/")
download_pandoc(version='2.14.2')

def clean_text(text):
    text = ":orphan:\n" + text
    text = text.replace("confluence-information-macro confluence-information-macro-information", '.. note::')
    text = text.replace("confluence-information-macro confluence-information-macro-warning", ".. warning::")
    text = text.replace("``", "  ")
    text = text.replace(u'\xa0', " ")
    text = text.replace('syntaxhighlighter-pre', '')
    return text


for file in glob.glob("*.html"):
    rst = pypandoc.convert_file(f'{file}', 'rst',  extra_args=['-s'])
    rst = clean_text(rst)
    rst_file = open(f"{file[:-4]}rst", "w")
    rst_file.write(rst)

