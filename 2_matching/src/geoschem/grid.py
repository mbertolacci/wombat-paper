import numpy as np

# NOTE(mgnb): see
# http://wiki.seas.harvard.edu/geos-chem/index.php/GEOS-Chem_horizontal_grids
GEOS_CHEM_GRID = {
    'longitude': {
        'centres': np.arange(-180, 177.51, 2.5),
        'widths': np.repeat(2.5, 144)
    },
    'latitude': {
        'centres': np.concatenate([
            np.array([-89.5]),
            np.arange(-88, 88.1, 2),
            np.array([89.5])
        ]),
        'widths': np.concatenate([
            np.array([1]),
            np.repeat(2, 89),
            np.array([1])
        ]),
    }
}

__all__ = ['GEOS_CHEM_GRID']
