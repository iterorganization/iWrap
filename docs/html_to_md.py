import markdownify as md
import glob
import os
import re

def fix_headers(markdown):
    reg_list = re.findall(r'\s\d+[.]\s+\w+', markdown, re.MULTILINE)
    for reg in reg_list:
        markdown = markdown.replace(reg, '\n' + "".join(reg.split(' ')))

    x = []
    for line in markdown.splitlines():
        if line.startswith('* '):
            line = line + '\n'
        x.append(line)
    markdown = '\n'.join(x)
    return markdown

def simple_clean(markdown):
    x = markdown.find("/*<")
    if x != -1:
        y = markdown.find(">*/") + len(">*/")
        markdown = markdown.replace(markdown[x:y], '')

    markdown = markdown.replace('.html', '.md')
    markdown = markdown.replace('\_', '_')
    markdown = markdown.replace('``', '')

    return markdown


os.chdir("iwrap_confluence_documentation/")

for file in glob.glob("*.html"):
    html = open(file, "r").read()
    markdown = md.markdownify(html)
    markdown = simple_clean(markdown)
    markdown = fix_headers(markdown)
    if file == "index.html":
        markdown = markdown.replace("Space Details:", "iWrap documentation")
    md_file = open(f"{file[:-4]}md", "w")
    md_file.write(markdown)

