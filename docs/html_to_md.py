import markdownify as md
import glob
import os
import re

def fix_headers(markdown):
    reg_list = re.findall(r'^\d+[.]\s{1,}\w+$', markdown, re.MULTILINE)
    reg_list2 = re.findall(r'\s\d+[.]\s+\w+', markdown, re.MULTILINE)

    # print(reg_list)
    print(reg_list2)
    for reg in reg_list:
        markdown = markdown.replace(reg, '\n' + "".join(reg.split(' ')))

    return markdown

def simple_clean(markdown):
    x = markdown.find("/*<")
    if x != -1:
        y = markdown.find(">*/") + len(">*/")
        markdown = markdown.replace(markdown[x:y], '')

    markdown = markdown.replace('.html', '.md')

    return markdown


os.chdir("iwrap_documentation/")

for file in glob.glob("*.html"):
    html = open(file, "r").read()
    html = html.replace('&#xa0;', '')
    # print(html)
    # print(html.find('&#xa0;'))
    # input()
    markdown = md.markdownify(html)
    markdown = simple_clean(markdown)
    markdown = fix_headers(markdown)
    md_file = open(f"{file[:-4]}md", "w")
    md_file.write(markdown)


