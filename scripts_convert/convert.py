# Copyright (C) 2023 Météo-France
# Copyright (C) 2023 Centre National de la Recherche Scientifique
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

import netCDF4 as nc
import sys, os
import argparse

from Config import Config
from Utils import arg_error, log
from Utils import ERROR, WARNING, INFO, DEBUG
from Dephy import FC_filename, listCases, Case

##############################
# PARSE COMMAND LINE ARGUMENTS

## initialize parser object
parser = argparse.ArgumentParser()
  
## add mandatory arguments
parser.add_argument("-c", help="Case name", metavar="casename", required=True)

## add optional arguments
def add_opt_arg(m, h, var, d, a=None): parser.add_argument(m, help=h, metavar=var, default=d, action=a)
def add_opt_swt(m, h): parser.add_argument(m, help=h, action="store_true")
add_opt_arg("-s", "Subcase name",           "subcasename", "REF")
add_opt_arg("-m", "Simulation mode",        "sim_mode",    "LES")
add_opt_arg("-i", "Input files directory",  "input_dir",   "./")
add_opt_arg("-g", "Grids (zhat) directory", "grids_dir",   "./")
add_opt_arg("-o", "Output directory",       "output_dir",  "./")
add_opt_arg("-v", "Verbosity level [0-3]",  "verbosity",   0)
add_opt_swt("-z", "Use zorog from case def") # switch

## parse command line arguments
args = parser.parse_args()

## set local variables according to parsed command line argument
casename    = args.c
subcasename = args.s
sim_mode    = args.m
input_dir   = args.i
grids_dir   = args.g
output_dir  = args.o
verbosity   = int(args.v)
read_zorog  = args.z

## check arguments validity
if not casename in listCases: 
  arg_error("case %s is not available"%casename)
if not os.path.isdir(input_dir):
  arg_error("input dir %s does not exist"%input_dir)
if not os.path.isdir(grids_dir):
  arg_error("grids dir %s does not exist"%grids_dir)
if not os.path.isdir(output_dir): 
  log(WARNING, "will create output directory %s"%output_dir, verbosity)
  os.system("mkdir -p %s"%output_dir)
if not sim_mode in ["LES", "CRM", "SCM"]:
  arg_error("invalid simulation mode %s"%sim_mode)

inp_file_FC = FC_filename(input_dir, casename, subcasename)

if not os.path.isfile(inp_file_FC): 
  arg_error("input FC file %s does not exist"%inp_file_FC)

###

log(INFO, "####### SETUP SUMMARY #######" , verbosity)
log(INFO, "casename    : %s"%casename   , verbosity)
log(INFO, "subcasename : %s"%subcasename, verbosity)
log(INFO, "sim_mode    : %s"%sim_mode   , verbosity)
log(INFO, "input_dir   : %s"%input_dir  , verbosity)
log(INFO, "output_dir  : %s"%output_dir , verbosity)
log(INFO, "verbosity   : %i"%verbosity  , verbosity)

##################
# INITIALIZE OBJECTS AND READ GLOBAL ATTRIBUTES

log(INFO, "\n######### INIT CASE #########",  verbosity)
cas = Case(casename, subcasename)
log(INFO, cas, verbosity)

log(INFO, "\n######## READ FC FILE #######",  verbosity)
log(INFO, "FC filename: %s"%inp_file_FC,      verbosity)

## GLOBAL

ds = nc.Dataset(inp_file_FC, "r")
listVars = ds.variables
log(DEBUG, listVars, verbosity)
listAttributes = ds.ncattrs()
log(DEBUG, listAttributes, verbosity)
attributes = {}
for attr in listAttributes:
  attributes[attr] = getattr(ds, attr)
  log(DEBUG, "Attribute '%s' = %s"%(attr, attributes[attr]), verbosity)

### set start_date, end_date and durations
cas.set_times(attributes)

log(INFO, "start_date: %s UTC    "%cas.start_date, verbosity)
log(INFO, "end_date:   %s UTC    "%cas.end_date,   verbosity)
log(INFO, "duration:   %s == %s seconds"%(cas.duration, cas.duration_secs),   verbosity)

### set case init and forcing types
cas.set_init_and_forcing_types(attributes)
# var_t["ini"]="theta|thetal|ta"
# var_q["adv"]="rv|rt|qv"
# ...
log(DEBUG, "init and forcings\n\tT:" + str(cas.var_t) + "\n\tq:"+str(cas.var_q), verbosity)

##################
# READ VARIABLES THAT DESCRIBE THE CASE

## longitude latitude
cas.set_lonlat(ds)

## grille verticale 
cas.set_vertical_grid(grids_dir)
log(INFO, "vertical grid: "+str(cas.zgrid), verbosity)

## initial profiles and forcings
cas.read_initial_profiles_and_forcings(attributes, ds, verbosity, read_zorog=read_zorog)
log(INFO, "forcings and initial profiles have been read", verbosity)

####################
# WRITE DATA INTO NAMELISTS

## initialize namelist PRE_IDEA
preid = Config(casename, "PRE", sim_mode)

preid.freeformat_zhat(cas)
preid.freeformat_rsou(cas)
preid.freeformat_zfrc(cas)

log(DEBUG, "free format variables\n"+ preid.config["freeformat"]["ZHAT"]+"\n"+\
preid.config["freeformat"]["RSOU"]+"\n"+ preid.config["freeformat"]["ZFRC"], verbosity)

preid.set_domain_grid(cas)
preid.set_ini_filenames(cas)
preid.set_lonlat(cas)
preid.set_luser(cas)
preid.set_surface_forcings(cas)

preid.write("test_PRE_IDEA_%s.nam"%cas.shortname)
exit()

########

## initialize namelist EXSEG
exseg = Config(casename, "EXS", sim_mode)

config = Config("ARMCU", "EXS", "LES")
config.write('default_LES_namelist.nam')

config.set_warm_microphysics()
config.activate_radiation()

config.modify("NAM_PARAM_ECRAD", "NSWSOLVER", "1")
config.write('cumulus_with_radiation_LES_namelist.nam')

exseg1 = config.duplicate_config(name="EXSEG01")
exseg1.modify("NAM_DYN", "XSEGLEN", "%i"%(4*3600))
exseg1.write('exseg01_cumulus_with_radiation_LES_namelist.nam')

exseg2 = config.duplicate_config(name="EXSEG02")
exseg2.modify("NAM_DYN", "XSEGLEN", "%i"%(1*3600))
exseg2.write('exseg02_cumulus_with_radiation_LES_namelist.nam')

exit()

## Scénario du script de conversion au format commun

# Créer une namelist par défaut en mode LES|CRM|SCM

# Lire les variables du fichier format commun
# Modifier la namelist en fonction du format commun
# # en fonction du type de cas (moistshcv|dcv|stable|dryshcv)
# # en fonction des types de forçages (flag_t/flag_q, )
# => on obtient preidea et exseg mère

# Lire le fichier de config spécifique du cas
# contenant les temps clés et durée du spinup
# En déduire le nombre de EXSEG à créer,
# Copier autant de fois la config exseg mère
# Modifier les configs filles en fonction
#   fichier init, restart, durée ...
#   flux sur le segment simulé...
#   fréquence des xout ...
