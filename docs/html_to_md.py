import markdownify as md
import glob
import os


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
    markdown = md.markdownify(html)
    markdown = simple_clean(markdown)
    md_file = open(f"{file[:-4]}md", "w")
    md_file.write(markdown)


