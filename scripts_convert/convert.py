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

import numpy as np
import netCDF4 as nc
import sys, os
from datetime import timedelta, datetime
import argparse

from Config import Config
from Utils import arg_error, log, parse_time, T_to_theta
from Utils import ERROR, WARNING, INFO, DEBUG
from Dephy import FC_filename, listCases, Case

##############################
# PARSE COMMAND LINE ARGUMENTS

## initialize parser object
parser = argparse.ArgumentParser()
  
## add mandatory arguments
parser.add_argument("-c", help="Case name", metavar="casename", required=True)

## add optional arguments
def add_opt_arg(m, h, var, d): parser.add_argument(m, help=h, metavar=var, default=d)
add_opt_arg("-s", "Subcase name",          "subcasename", "REF")
add_opt_arg("-m", "Simulation mode",       "sim_mode",    "LES")
add_opt_arg("-i", "Input files directory", "input_dir",   "./")
add_opt_arg("-o", "Output directory",      "output_dir",  "./")
add_opt_arg("-v", "Verbosity level",       "verbosity",   0)

## parse command line arguments
args = parser.parse_args()

## set local variables according to parsed command line argument
casename    = args.c
subcasename = args.s
sim_mode    = args.m
input_dir   = args.i
output_dir  = args.o
verbosity   = int(args.v)

## check arguments validity
if not casename in listCases: 
  arg_error("case %s is not available"%casename)
if not os.path.isdir(input_dir):
  arg_error("input dir %s does not exist"%input_dir)
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
log(INFO, "> casename    : %s"%casename   , verbosity)
log(INFO, "> subcasename : %s"%subcasename, verbosity)
log(INFO, "> sim_mode    : %s"%sim_mode   , verbosity)
log(INFO, "> input_dir   : %s"%input_dir  , verbosity)
log(INFO, "> output_dir  : %s"%output_dir , verbosity)
log(INFO, "> verbosity   : %i"%verbosity  , verbosity)

##################
# START CONVERSION FROM FC TO MNH NAMELISTS

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

## DATES

y,m,d,hh,mm,ss = parse_time(attributes["start_date"])
start_date = datetime(y,m,d,hh,mm,ss)
y,m,d,hh,mm,ss = parse_time(attributes["end_date"])
end_date = datetime(y,m,d,hh,mm,ss)
duration = end_date-start_date
duration_secs = duration.total_seconds()

log(INFO, "> start_date: %s UTC    "%start_date, verbosity)
log(INFO, "> end_date:   %s UTC    "%end_date,   verbosity)
log(INFO, "> duration:   %s == %s seconds"%(duration, duration_secs),   verbosity)

## SCALAIRES

lat = ds.variables["lat"][0]
lon = ds.variables["lon"][0]

## CHAMPS INITIAUX T,q,ps,u,v

var_t = ds.variables[cas.flag_t][:]          # theta, thetal or ta
lev_t = ds.variables["lev_"+cas.flag_t][:]   # associated vertical grid
var_q = ds.variables[cas.flag_q][:]          # rv, rt or qv 
lev_q = ds.variables["lev_"+cas.flag_q][:]   # associated vertical grid

log(DEBUG, "> Original lev_t (flag %s) %s"%(cas.flag_t, lev_t), verbosity)
log(DEBUG, "> Original lev_q (flag %s) %s"%(cas.flag_q, lev_q), verbosity)
log(DEBUG, "> Original var_t (flag %s) %s"%(cas.flag_t, var_t), verbosity)
log(DEBUG, "> Original var_q (flag %s) %s"%(cas.flag_q, var_q), verbosity)

if cas.flag_t == "ta" :
  # convert T to theta
  press = ds.variables["pa"][:]
  var_t = T_to_theta(var_t, press)

if "q" in cas.flag_q: 
  # convert content to mixing ratio
  var_q = var_q/(1-var_q)

log(DEBUG, "> Converted var_t (flag %s) %s"%(cas.flag_t, var_t), verbosity)
log(DEBUG, "> Converted var_q (flag %s) %s"%(cas.flag_q, var_q), verbosity)

ps    = ds.variables['ps'][:]      # surface pressure
lev_u = ds.variables['lev_ua'][:]  # zonal      wind vertical grid
var_u = ds.variables['ua'][:]      # zonal      wind profile
lev_v = ds.variables['lev_va'][:]  # meridional wind vertical grid
var_v = ds.variables['va'][:]      # meridional wind profile
if (lev_v.size != lev_u.size):
  log(WARNING, 'ERROR!! DIFFERENT VERTICAL GRIDS FOR U & V to be dealt separately', verbosity)
  var_v = 0.*var_u+var_v[0][0]

## FORCAGES




exit()
########

default = Config("ARMCU", "EXSEG01", sim_mode)
default.write('default_namelist.nam')
#print(default)

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
# => on obtient preidea et exseg mère

# Lire le fichier de config spécifique du cas
# contenant les temps clés et durée du spinup
# En déduire le nombre de EXSEG à créer,
# Copier autant de fois la config exseg mère
# Modifier les configs filles en fonction
#   fichier init, restart, durée ...
#   flux sur le segment simulé...
#   fréquence des xout ...
