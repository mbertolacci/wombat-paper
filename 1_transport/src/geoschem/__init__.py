import glob
import logging
import os
import pathlib
import shutil

import jinja2


def fill_template(arguments, input_path, output_path):
    with open(input_path, 'r') as input_file, \
         open(output_path, 'w') as output_file:
        output_file.write(jinja2.Template(
            input_file.read()
        ).render(**arguments))


def fill_run_template(template, output, template_arguments, symlinks, name):
    def log_debug(fmt, *args, **kwargs):
        logging.debug(
            '[{}] {}'.format(name, fmt),
            *args,
            **kwargs
        )

    if os.path.exists(output):
        raise RuntimeError('Directory already exists: {}'.format(output))

    log_debug('Copying template to output destination')
    pathlib.Path(os.path.dirname(output)).mkdir(parents=True, exist_ok=True)
    shutil.copytree(
        template,
        output
    )

    log_debug('Filling templates')
    for template_path in glob.glob(os.path.join(template, '*.template')):
        from_filename = os.path.basename(template_path)
        fill_template(
            template_arguments,
            os.path.join(template, from_filename),
            os.path.join(output, from_filename[0:-9])
        )
        os.remove(os.path.join(output, from_filename))

    log_debug('Creating symlinks')
    for from_path, to_path in symlinks.items():
        os.symlink(
            os.path.relpath(from_path, output),
            os.path.join(output, to_path)
        )
