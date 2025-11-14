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
def add_opt_arg(m, h, var, d, a=None): 
    parser.add_argument(m, help=h, metavar=var, default=d, action=a)
def add_opt_swt(m, h): parser.add_argument(m, help=h, action="store_true")
add_opt_arg("-s", "Subcase name",           "subcasename", "REF")
add_opt_arg("-m", "Simulation mode",        "sim_mode",    "LES")
add_opt_arg("-i", "Input files directory",  "input_dir",   "./")
add_opt_arg("-g", "Grids (zhat) file",      "grid_file",   None)
add_opt_arg("-o", "Output directory",       "output_dir",  "./")
add_opt_arg("-v", "Verbosity level [0-3]",  "verbosity",   0)
add_opt_arg("-x", "Horizontal grid size",   "delta_x",     None)
add_opt_arg("-L", "Horizontal domain size", "ngrid_x",     None)
add_opt_arg("-P", "File to generate PPE",   "htexplo",     None)
add_opt_arg("-t", "Maximum seg length",     "max_seg",     4)
add_opt_arg("-a", "Adrien Marcel modifs",   "Adrien",      0)
add_opt_arg("-S", "SEAFLUX model",          "seaflux",     None)
add_opt_swt("-z", "Use zorog from case def") # switch
add_opt_swt("-e", "Deactivate EDKF")         # switch
add_opt_swt("-r", "ECMW instead of ECRA")    # switch
add_opt_swt("-I", "ICE3 instead of LIMA")    # switch
add_opt_swt("-M", "MOSAI instead of TSZ0")   # switch
add_opt_swt("-B", "3D budgets instead of 1D")# switch
add_opt_swt("-R", "deactivate rain")         # switch
add_opt_swt("-p", "modif precips Catherine") # switch

## parse command line arguments
args = parser.parse_args()

## set local variables according to parsed command line argument
casename    = args.c
subcasename = args.s
sim_mode    = args.m
input_dir   = args.i
grid_file   = args.g
output_dir  = args.o
delta_x     = args.x
ngrid_x     = args.L
htexplo     = args.P
max_seg_len = int(args.t)
read_zorog  = args.z
deac_edkf   = args.e
radi_ecmw   = args.r
adri_vers   = int(args.a)
acti_ice3   = args.I
acti_mosa   = args.M
acti_3Dbudg = args.B
deac_rain   = args.R
plui_cath   = args.p
seafux_mo   = args.S
verbosity   = int(args.v)

## check arguments validity
if not casename in listCases: 
  arg_error("case %s is not available"%casename)
if not os.path.isdir(input_dir):
  arg_error("input dir %s does not exist"%input_dir)
if grid_file is not None and not os.path.isfile(grid_file):
  arg_error("grid file %s does not exist"%grid_file)
if not os.path.isdir(output_dir): 
  log(WARNING, "will create output directory %s"%output_dir, verbosity)
  os.system("mkdir -p %s"%output_dir)
if not sim_mode in ["LES", "CRM", "SCM"]:
  arg_error("invalid simulation mode %s"%sim_mode)
if (adri_vers == 1 or adri_vers == 2) and acti_ice3 : 
  arg_error("AM versions 1 and 2 use LIMA microphysics\nSet AM version to 3 or 4 to use ICE3")

# force ICE3 with AM version 3 and 4
if (adri_vers == 3 or adri_vers == 4) : acti_ice3 = True

inp_file_FC = FC_filename(input_dir, casename, subcasename)

if not os.path.isfile(inp_file_FC): 
  arg_error("input FC file %s does not exist"%inp_file_FC)

###

log(INFO, "####### SETUP SUMMARY #######" , verbosity)
log(INFO, "casename           : %s"%casename   , verbosity)
log(INFO, "subcasename        : %s"%subcasename, verbosity)
log(INFO, "sim_mode           : %s"%sim_mode   , verbosity)
log(INFO, "input_dir          : %s"%input_dir  , verbosity)
log(INFO, "output_dir         : %s"%output_dir , verbosity)
log(INFO, "grid_file          : %s"%grid_file  , verbosity)
if delta_x is not None: log(INFO, "delta_x            : %s"%delta_x, verbosity)
if ngrid_x is not None: log(INFO, "ngrid_x            : %s"%ngrid_x, verbosity)
log(INFO, "htexplo PPE file   : %s"%htexplo    , verbosity)
log(INFO, "read_zorog?        : %i"%read_zorog , verbosity)
log(INFO, "deactivate EDKF?   : %i"%deac_edkf  , verbosity)
log(INFO, "ECMWF rad scheme?  : %i"%radi_ecmw  , verbosity)
log(INFO, "A. Marcel version? : %i"%adri_vers  , verbosity)
log(INFO, "ICE3 microphysics? : %i"%acti_ice3  , verbosity)
log(INFO, "A. Maison surface? : %i"%acti_mosa  , verbosity)
log(INFO, "3D budgets?        : %i"%acti_3Dbudg, verbosity)
log(INFO, "Rain deactivated?  : %i"%deac_rain  , verbosity)
log(INFO, "Precip for Cath?   : %i"%plui_cath  , verbosity)
log(INFO, "Seaflux model      : %s"%seafux_mo  , verbosity)
log(INFO, "verbosity          : %i"%verbosity  , verbosity)

##################
# INITIALIZE CASE 

log(INFO, "\n######### INIT CASE #########",  verbosity)
cas = Case(casename, subcasename)
log(INFO, cas, verbosity)

log(INFO, "\n######## READ FC FILE #######",  verbosity)
log(INFO, "FC filename: %s"%inp_file_FC,      verbosity)

## read global attributes
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
log(DEBUG, "init and forcings\n\tT:" + str(cas.name_var_t) + "\n\tq:"+str(cas.name_var_q), verbosity)

##################
# READ VARIABLES THAT DESCRIBE THE CASE

## longitude latitude
cas.set_lonlat(ds)

## initial profiles and forcings
# is the case pressure-defined or altitude-defined
if not("forc_zh" in attributes or "forc_pa" in attributes) or attributes["forc_zh"]:
  cas.def_lev = "z"
else :
  cas.def_lev = "P"
cas.read_initial_profiles_and_forcings(attributes, ds, verbosity, read_zorog=read_zorog)
log(INFO, "forcings and initial profiles have been read", verbosity)

cas.set_vertical_grid(grid_file, read_zorog=read_zorog)
log(INFO, "vertical grid: "+str(cas.zgrid), verbosity)

## output files 
## each seg lasts at most max_seg_len hours (default 4h ; option -t) 
print('max_seg_len',max_seg_len)
cas.setup_outputs(verbosity, max_seg_dur = max_seg_len)

####################
# SETUP AND WRITE DATA INTO NAMELISTS

## initialize namelist PRE_IDEA
preid = Config(casename, "PRE", sim_mode)
preid.set_domain_grid(cas)
preid.set_ini_filenames(cas)
preid.set_lonlat(cas)
preid.set_luser(cas)
preid.set_surface_forcings(cas)
preid.freeformat_zhat(cas)
preid.freeformat_rsou(cas)
preid.freeformat_zfrc(cas)

if delta_x is not None:
  preid.horizontal_resolution(delta_x)
if ngrid_x is not None:
  preid.horizontal_domain(ngrid_x)

if acti_mosa:
  if not "land" in cas.surface_forcing:
    arg_error("mosai surf can only be activated if surface is land")
  else: preid.set_mosai_surface()

preid.write("%s/conf_PRE_IDEA_%s_%s.nam"%(output_dir, 
    cas.name, sim_mode))

if htexplo is not None:
  import numpy as np
  save_preid = Config(casename, "PRE", sim_mode)
  save_preid.copy_config_from(preid)
  dat = np.genfromtxt(htexplo, dtype=str)
  param_names = dat[0][1:]
  for d in dat[1:]: # loop on PPE members 
    # reset preid from saved control
    preid.copy_config_from(save_preid)
    simu_name = d[0]
    for param, value in zip(param_names, d[1:]):
      preid.htexplo_set_parameter(param, value)
    preid.write("%s/conf_PRE_IDEA_%s_%s.nam_%s"%(output_dir, 
        cas.name, sim_mode, simu_name))

## initialize namelist EXSEG
exseg = Config(casename, "EXS", sim_mode)
exseg.set_name(cas)
exseg.set_ini_filenames(cas)
exseg.set_forcing_flags(cas)
exseg.set_buffer_layer(cas)
exseg.set_def_budget_zone(cas)
exseg.activate_budgets(acti_3Dbudg)

if ngrid_x is not None: # for budgets
  exseg.horizontal_domain(ngrid_x)

if "dryshcv" in cas.type: 
  exseg.set_adjust_microphysics()
elif "shcv" in cas.type:
  exseg.set_warm_microphysics()
else:
  exseg.set_cold_microphysics()

if adri_vers:
  if adri_vers == 1: exseg.set_adrien_version() # no accr
  if adri_vers == 2: exseg.set_adrien_version(accr="'PRFR'")
  if adri_vers == 3: exseg.set_adrien_version()
  if adri_vers == 4: exseg.set_adrien_version(accr="'PRFR'")

if acti_ice3:
  exseg.set_microphysics_scheme("ICE3")

if deac_edkf:
  exseg.deactivate_edkf()

if deac_rain:
  exseg.set_adjust_microphysics()
elif plui_cath:
  if not acti_ice3: log(ERROR, "error: Option -p should be used with -I to activate ICE3", verbosity)
  exseg.set_modifs_pluie()

if seafux_mo is not None:
  if not seafux_mo in ["ECUME", "ECUME6", "DIRECT"]:
    log(ERROR, "error: invalid seaflux model %s for option -S"%seafux_mo, verbosity)
  exseg.set_seaflux_model(seafux_mo)

if attributes["radiation"] == "on":
  exseg.activate_radiation(rad='ECMW' if radi_ecmw else 'ECRA')
else:
  exseg.deactivate_radiation()

if htexplo is not None:
  exseg.modify("NAM_LES", "XLES_TEMP_SAMPLING", "3600")
  exseg.modify("NAM_LES", "XLES_TEMP_MEAN_STEP", "3600")
  save_exseg = Config(casename, "EXS", sim_mode)
  save_exseg.copy_config_from(exseg)

for i in range(cas.nseg+1):
  exseg.set_outputs(cas, i)
  log(INFO, "iseg %i from hour %02i to %02i ; is hf ? %1i"%(i,
      exseg.seg_beg/3600, exseg.seg_end/3600, exseg.is_hf), verbosity)
  exseg.reset_seg_surface_forcings(cas, i)
  exseg.write("%s/conf_EXSEG%02i_%s_%s.nam"%(output_dir,
      i, cas.name, sim_mode))
  if sim_mode == "SCM" : break # only do 00 in SCM mode

# syntax in htexplo file
# first line = param names, 
# other lines = simu_name, param_values
# one line per PPE member
if htexplo is not None:
  import numpy as np
  dat = np.genfromtxt(htexplo, dtype=str)
  param_names = dat[0][1:]
  for d in dat[1:]: # loop on PPE members 
    # reset exseg from saved control
    exseg.copy_config_from(save_exseg)
    simu_name = d[0]
    for param, value in zip(param_names, d[1:]):
      exseg.htexplo_set_parameter(param, value)
    for i in range(cas.nseg+1):
      exseg.set_outputs(cas, i)
      exseg.reset_seg_surface_forcings(cas, i)
      exseg.write("%s/conf_EXSEG%02i_%s_%s.nam_%s"%(output_dir,
          i, cas.name, sim_mode, simu_name))
      if sim_mode == "SCM" : break # only do 00 in SCM mode

exit()
