import os
from typing import Dict

import jinja2


def process_template_dir(
        template_pkg: str,
        template_dir: str,
        destination_dir: str,
        dictionary: Dict,
        output_stream = None) -> None:
    """Function processes all templates within chosen directory of selected package using data provided as dictionary

    Args:
        template_pkg (:obj:`Str`): Package containing directory with templates
        template_dir (`str`): Template directory within `template_pkg`
        destination_dir (`str`): Directory where all generated files will be put
        dictionary (Dict[str, Any]): Dictionary containing all data needed to 'feed' templates
    """
    # TODO: Add excluded files /dirs list (?)
    jinja_env = jinja2.Environment(
        loader=jinja2.PackageLoader(template_pkg, template_dir),
        autoescape=jinja2.select_autoescape(['html', 'xml']),
        trim_blocks=True,
        lstrip_blocks = True
    )
    templates = jinja_env.list_templates()

    for template_file in templates:
        if output_stream is not None:
            output_stream.write(f'Processing: {template_dir}/{template_file} \n' )
        file_path = destination_dir + '/' + template_dir + '/' + template_file
        file_dir_path = os.path.dirname(file_path)
        # create dir if not exists
        if not os.path.isdir(file_dir_path):
            os.makedirs(file_dir_path)

        # remove postfix '.jinja2' from REAL template files
        if file_path.endswith('.jinja2'):
            file_path = file_path.replace('.jinja2', '')

        # TODO Skip regular ('no templates) files

        template = jinja_env.get_template(template_file)
        template.stream(dictionary).dump(file_path)
