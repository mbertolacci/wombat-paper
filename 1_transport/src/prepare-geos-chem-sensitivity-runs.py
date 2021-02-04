import argparse
import datetime
import logging
import os

from dateutil.relativedelta import relativedelta
import pandas

from geoschem import fill_run_template


def fill_template(arguments, input_path, output_path):
    with open(input_path, 'r') as input_file, \
         open(output_path, 'w') as output_file:
        output_file.write(jinja2.Template(
            input_file.read()
        ).render(**arguments))


logging.basicConfig(
    format='[%(levelname)s] [%(asctime)s] %(message)s',
    level=logging.INFO
)

parser = argparse.ArgumentParser(description='''
Create the sensitivity run directories.
''')


def arg(name, type=str, required=True, *args, **kwargs):
    parser.add_argument(name, type=type, required=required, *args, **kwargs)


arg('--code-directory', help='Code directory')
arg('--ext-data', help='ExtData directory')
arg('--ct2019-b4', help='CT2019 B4 emissions')
arg('--odiac2018', help='ODIAC2018 emissions')
arg('--transcom-mask', help='TransCom mask')
arg('--base-run', help='Base run directory')
arg('--obspack-data', help='ObsPack directory')
arg('--template', help='Template directory')
arg('--start-date', help='Start date in ISO8601 format, inclusive')
arg('--end-date', help='Start date in ISO8601 format, exclusive')
arg('--region', type=int, help='Region numbers', nargs='+')
arg('--duration', type=int, help='Duration in months')
arg('--threads', type=int, help='Number of threads')
arg('--output', help='Output directory')

args = parser.parse_args()

month_starts_dt = pandas \
    .date_range(args.start_date, args.end_date, freq='MS') \
    .to_pydatetime() \
    .tolist()[0:-1]
month_starts = [x.date() for x in month_starts_dt]
end_month = month_starts[-1]

for month_start in month_starts:
    for region in args.region:
        path = os.path.join(
            str(month_start.year),
            'run.v12.3.2.R{0:02d}M{1:02d}'.format(region, month_start.month)
        )
        output_path = os.path.join(args.output, path)
        scaling_factors = [
            1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
        ]
        scaling_factors[month_start.month - 1] = 2.0
        restart_file_name = 'GEOSChem.Restart.{}_0000z.nc4'.format(
            month_start.strftime('%Y%m%d')
        )
        fill_run_template(
            args.template,
            output_path,
            template_arguments={
                'path': path,
                'ext_data': os.path.relpath(args.ext_data, output_path),
                'ct2019_b4_file': os.path.relpath(args.ct2019_b4, output_path),
                'odiac2018_file': os.path.relpath(args.odiac2018, output_path),
                'transcom_mask_file': os.path.relpath(
                    args.transcom_mask,
                    output_path
                ),
                'obspack_data': os.path.relpath(
                    args.obspack_data,
                    output_path
                ),
                'start_date': month_start,
                'end_date': datetime.date.fromisoformat(args.end_date),
                'daily_months': [
                    x.date()
                    for x in pandas \
                        .date_range(
                            month_start + relativedelta(months=args.duration),
                            end_month + relativedelta(months=1),
                            freq='MS'
                        ) \
                        .to_pydatetime() \
                        .tolist()[0:-1]
                ],
                'scaling_factors': scaling_factors,
                'region': region,
                'threads': args.threads,
                'slurm_job_name': 'gc.sensitivity.{0}.R{1:02d}M{2:02d}'.format(
                    month_start.year,
                    region,
                    month_start.month
                )
            },
            symlinks={
                args.code_directory: 'CodeDir',
                os.path.join(args.base_run, 'geos.mp'): 'geos.mp',
                os.path.join(args.base_run, restart_file_name):
                    restart_file_name,
            },
            name=path
        )


logging.info('Done')
