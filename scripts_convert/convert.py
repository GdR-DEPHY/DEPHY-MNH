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
add_opt_arg("-g", "Grids (zhat) file",      "grid_file",   None)
add_opt_arg("-o", "Output directory",       "output_dir",  "./")
add_opt_arg("-v", "Verbosity level [0-3]",  "verbosity",   0)
add_opt_swt("-z", "Use zorog from case def") # switch
add_opt_swt("-e", "Deactivate EDKF") # switch
add_opt_swt("-r", "ECMW instead of ECRA") # switch

## parse command line arguments
args = parser.parse_args()

## set local variables according to parsed command line argument
casename    = args.c
subcasename = args.s
sim_mode    = args.m
input_dir   = args.i
grid_file   = args.g
output_dir  = args.o
verbosity   = int(args.v)
read_zorog  = args.z
deac_edkf   = args.e
radi_ecmw   = args.r

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

## output files ## each seg lasts 4h max
cas.setup_outputs(verbosity, max_seg_dur = 4)

####################
# WRITE DATA INTO NAMELISTS

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
preid.write("%s/conf_PRE_IDEA_%s_%s.nam"%(output_dir, 
    cas.shortname, sim_mode))

## initialize namelist EXSEG
exseg = Config(casename, "EXS", sim_mode)
exseg.set_name(cas)
exseg.set_ini_filenames(cas)
exseg.set_forcing_flags(cas)
exseg.set_buffer_layer(cas)
exseg.set_def_budget_zone(cas)

if "dryshcv" in cas.type: 
  exseg.set_adjust_microphysics()
elif "shcv" in cas.type:
  exseg.set_warm_microphysics()
else:
  exseg.set_cold_microphysics()

if deac_edkf:
  exseg.deactivate_edkf()

if attributes["radiation"] == "on":
  exseg.activate_radiation(rad='ECMW' if radi_ecmw else 'ECRA')
else:
  exseg.deactivate_radiation()

for i in range(cas.nseg+1):
  exseg.set_outputs(cas, i)
  log(INFO, "iseg %i from hour %02i to %02i ; is hf ? %1i"%(i,
      exseg.seg_beg/3600, exseg.seg_end/3600, exseg.is_hf), verbosity)
  exseg.reset_seg_surface_forcings(cas, i)
  exseg.write("%s/conf_EXSEG%02i_%s_%s.nam"%(output_dir,
      i, cas.shortname, sim_mode))
  if sim_mode == "SCM" : break # only do 00 in SCM mode

exit()
