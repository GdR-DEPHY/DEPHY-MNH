# dephy-mnh

Authors: F. Couvreux (fleur.couvreux@meteo.fr) &
         N. Villefranque (najda.villefranque@lmd.ipsl.fr)  
Creation: 29/11/2023  
Refactoring: 07/07/2026  

# Description

dephymnh provides commands to handle mesonh configuration files and output
files in the Dephy framework

such as:
- ```dmnh_namelist_create``` to create namelists from dephy-scm netCDF files
- ```dmnh_output_convert_000``` to convert MNH output to dephy output format
- ```dmnh_output_coarse_grain``` to horizontally average MNH 3D output files

All available commands are prefixed with dmnh_

# Contents:

- COPYING           GNU GPLv3
- TODO              ongoing work
- grilles/          available vertical grids
- src/              dephymnh package
- bin/              dephymnh shell scripts
- misc/             miscellaneous files

# Install dephymnh:

    python -m venv ${HOME}/.local/pyenvs/dephy-mnh
    source ${HOME}/.local/pyenvs/dephy-mnh/bin/activate
    pip install -e .
