import argparse
import datetime
import logging
import os
import subprocess

from geoschem import fill_run_template

logging.basicConfig(
    format='[%(levelname)s] [%(asctime)s] %(message)s',
    level=logging.INFO
)

parser = argparse.ArgumentParser(description='''
Create the base run directory.
''')


def arg(name, type=str, required=True, *args, **kwargs):
    parser.add_argument(name, type=type, required=required, *args, **kwargs)


arg('--code-directory', help='Code directory')
arg('--ext-data', help='ExtData directory')
arg('--ct2019-b4', help='CT2019 B4 emissions')
arg('--odiac2018', help='ODIAC2018 emissions')
arg('--restart-file', help='Restart file')
arg('--obspack-data', help='ObsPack directory')
arg('--template', help='Template directory')
arg('--start-date', help='Start date in ISO8601 format, inclusive')
arg('--end-date', help='Start date in ISO8601 format, exclusive')
arg('--threads', type=int, help='Number of threads')
arg('--output', help='Output directory')

args = parser.parse_args()

start_date = datetime.datetime.strptime(args.start_date, '%Y-%m-%d').date()
end_date = datetime.datetime.strptime(args.end_date, '%Y-%m-%d').date()

fill_run_template(
    args.template,
    args.output,
    template_arguments={
        'ext_data': os.path.relpath(args.ext_data, args.output),
        'ct2019_b4_file': os.path.relpath(args.ct2019_b4, args.output),
        'odiac2018_file': os.path.relpath(args.odiac2018, args.output),
        'obspack_data': os.path.relpath(args.obspack_data, args.output),
        'start_date': start_date,
        'end_date': end_date,
        'threads': args.threads,
    },
    symlinks={
        args.code_directory: 'CodeDir',
        args.restart_file: os.path.basename(args.restart_file),
    },
    name=os.path.basename(args.output)
)

logging.info('Compiling code')
compile_env = os.environ.copy()
compile_env.update({
    'FC': 'gfortran',
    'CC': 'gcc',
    'CXX': 'g++',
    'GC_BIN': os.path.join(os.getcwd(), '.conda_env', 'bin'),
    'GC_INCLUDE': os.path.join(os.getcwd(), '.conda_env', 'include'),
    'GC_LIB': os.path.join(os.getcwd(), '.conda_env', 'lib'),
})
subprocess.run(
    ['make', '-j4', 'superclean'],
    cwd=args.output,
    env=compile_env,
    check=True
)
subprocess.run(
    ['make', '-j4', 'mpbuild'],
    cwd=args.output,
    env=compile_env,
    check=True
)

logging.info('Done')
