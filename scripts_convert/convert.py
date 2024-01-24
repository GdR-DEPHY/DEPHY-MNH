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
import argparse

from Config import Config
from Utils import arg_error, log, T_to_theta
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

### set case init and forcing types
cas.set_init_and_forcing_types(attributes)
# var_t["ini"]="theta|thetal|ta"
# var_q["ini"]="rv|rt|qv"
# var_t["adv"]="theta|thetal|ta"
# var_q["adv"]="rv|rt|qv"
# ...
log(DEBUG, "init and forcings\n\tT:" + str(cas.var_t) + "\n\tq:"+str(cas.var_q), verbosity)

### set start_date, end_date and durations
cas.set_times(attributes)

log(INFO, "start_date: %s UTC    "%cas.start_date, verbosity)
log(INFO, "end_date:   %s UTC    "%cas.end_date,   verbosity)
log(INFO, "duration:   %s == %s seconds"%(cas.duration, cas.duration_secs),   verbosity)

##################
# READ VARIABLES THAT DESCRIBE THE CASE

## scalaires

lat = ds.variables["lat"][0]
lon = ds.variables["lon"][0]

## grille verticale 
cas.set_vertical_grid(grids_dir)
log(INFO, "vertical grid: "+str(cas.zgrid), verbosity)

## champs initiaux T,q,ps,u,v,zorog

name_var_t = cas.var_t["ini"]
name_var_q = cas.var_q["ini"]
name_var_u = cas.var_u["ini"]
name_var_v = cas.var_v["ini"]

### initialize with theta_dry or theta_liquid
mnh_init_keyword = "ZUVTHLMR" if name_var_t == "thetal" else "ZUVTHDMR"

### get initial profiles 
ps    = ds.variables['ps'][:]      # surface pressure
zs    = ds.variables['orog'][:] if read_zorog else ps*0
var_u = ds.variables[name_var_u][:]          # ua, zonal wind profile
lev_u = ds.variables['lev_'+name_var_u][:]   # associated vertical grid
var_v = ds.variables[name_var_v][:]          # va, merid. wind profile
lev_v = ds.variables['lev_'+name_var_v][:]   # associated vertical grid
var_t = ds.variables[name_var_t][:]          # theta, thetal or ta
lev_t = ds.variables["lev_"+name_var_t][:]   # associated vertical grid
var_q = ds.variables[name_var_q][:]          # rv, rt or qv 
lev_q = ds.variables["lev_"+name_var_q][:]   # associated vertical grid

nlev_init_uv = len(lev_u)
nlev_init_tq = len(lev_t)

log(DEBUG, "Original lev_t (%s) %s"%(name_var_t, lev_t), verbosity)
log(DEBUG, "Original lev_q (%s) %s"%(name_var_q, lev_q), verbosity)
log(DEBUG, "Original var_t (%s) %s"%(name_var_t, var_t), verbosity)
log(DEBUG, "Original var_q (%s) %s"%(name_var_q, var_q), verbosity)

### convert T to theta and q to mixing ratio if needed

if name_var_t == "ta" :
  # convert T to theta
  press = ds.variables["pa"][:]
  var_t = T_to_theta(var_t, press)

if "q" in name_var_q: 
  # convert content to mixing ratio
  var_q = var_q/(1-var_q)

log(DEBUG, "Converted var_t (%s) %s"%(name_var_t, var_t), verbosity)
log(DEBUG, "Converted var_q (%s) %s"%(name_var_q, var_q), verbosity)

if (lev_v.size != lev_u.size):
  log(WARNING, 'ERROR!! DIFFERENT VERTICAL GRIDS FOR U & V to be dealt separately', verbosity)
  var_v = 0.*var_u+var_v[0][0]

### store initialisation in the right format in str_init
str_init = "RSOU\n"
str_init += "%s %i\n"%(cas.start_date.strftime("%Y %m %d"), cas.start_seconds)
str_init += "%s   \n"%mnh_init_keyword # ZUVTHDMR or ZUVTHLMR
str_init += "%.2f \n"%zs[0]
str_init += "%.2f \n"%ps[0]
str_init += "%.2f \n"%var_t[0][0]
str_init += "%.8f \n"%var_q[0][0]
str_init += "%i   \n"%nlev_init_uv
for (z,u,v) in zip(lev_u, var_u[0], var_v[0]):
  str_init += "%14.1f %14.2f %14.2f\n"%(z,u,v)
str_init += "%i   \n"%nlev_init_tq
for (z,t,q) in zip(lev_t, var_t[0], var_q[0]):
  str_init += "%14.1f %14.2f %14.8f\n"%(z,t,q)

############################


## forçages advection, nudging
# vents : geo ou nudging sinon 0
# vertical : wa (vitesse vert) ou wap (omega) ou 0
# T,q : nudging ? sinon 0 ; adv ? sinon 0 

### get forcings

def getzvar(name_var):
    return ds.variables[name_var][:],      \
           ds.variables["lev_"+name_var]
def gettvar(name_var):
    return ds.variables[name_var][:],      \
           ds.variables["time_"+name_var]
def get2dvar(name_var):
    return ds.variables[name_var][:],      \
           ds.variables["lev_"+name_var], \
           ds.variables["time_"+name_var]

# horizontal winds : geostrophic or nudging ; tendencies = 0
## zonal
name_var_u = cas.var_u["frc"]
name_var_v = cas.var_v["frc"]
if name_var_u != "none": 
  var_u_frc, lev_u_frc, tim_u_frc = get2dvar(name_var_u)
  var_v_frc, lev_v_frc, tim_v_frc = get2dvar(name_var_v)
else:
  var_u_frc = None
  var_v_frc = None

# thermodynamics : nudging 
## T
if cas.var_t["nud"] != "none":
  name_var_t = cas.var_t["nud"]+"_nud"
  var_t_nud, lev_t_nud, tim_t_nud = get2dvar(name_var_t)
else:
  var_t_nud = None
## q
if cas.var_q["nud"] != "none":
  name_var_q = cas.var_q["nud"]+"_nud"
  var_q_nud, lev_q_nud, tim_q_nud = get2dvar(name_var_q)
else:
  var_q_nud = None

# thermodynamics : advection 
## T
if cas.var_t["adv"] != "none":
  name_var_t = "tn"+cas.var_t["adv"]+"_adv"
  var_t_adv, lev_t_adv, tim_t_adv = get2dvar(name_var_t)
else:
  var_t_adv = None
## q
if cas.var_q["adv"] != "none":
  name_var_q = "tn"+cas.var_q["adv"]+"_adv"
  var_q_adv, lev_q_adv, tim_q_adv = get2dvar(name_var_q)
else:
  var_q_adv = None

## radiation tendency is added to temperature advection
if attributes["radiation"] == "tend":
  name_var_t = "tn"+cas.var_t["adv"]+"_rad"
  var_t_adv += ds.variables[name_var_t][:]

# subsidence, either in w or omega
if attributes['forc_wa'] == 1:
  var_w_frc, lev_w_frc, time_w_frc = get2dvar("wa")
elif attributes['forc_wap'] == 1: 
  var_w_frc, lev_w_frc, time_w_frc = get2dvar("wap")
  var_w_frc /= -1.*288*ds.variables['ta'][:]*9.81
else:
  var_w_frc = None

# surface forcings
## prescribed SST or turbulent fluxes
if attributes['surface_forcing_temp'] == 'ts':
  var_sst, tim_sst = gettvar("ts_forc")
  var_sst = np.array([int(s*100)/100. for s in var_sst]
elif attributes['surface_forcing_temp'] == 'surface_flux':
  var_hfls, tim_hfls = gettvar('hfls')
  var_hfss, tim_hfss = gettvar('hfss')
elif attributes['surface_forcing_temp'] == 'none':
  var_z0h, tim_z0h = gettvar('z0h') # ?

## prescribed momentum flux or roughness length
if attributes['surface_forcing_wind'] == 'ustar':
  var_ustar, tim_ustar = gettvar('ustar')
elif attributes['surface_forcing_wind'] == 'z0':
  var_z0, tim_z0 = gettvar('z0')
  var_z0 = np.array([int(z0*1000)/1000. for z0 in var_z0])

############
# au final,
# pour PRE_IDEA, écrire pour chaque pas de temps it de forçage :
# date \n zs \n ps \n th_zs \n rv_zs \n nlev_frc \n 
# puis pour chaque niveau ik de nlev_frc forçages :
#('%.1f %.2f %.2f %.1f %.5f %.5f %.6f %.10f %.1f %.1f\n' % (lev_frc[ik],
#    ufrc[it][ik],vfrc[it][ik],thfrc[it][ik],qfrc[it][ik],wfrc[it][ik],
#    tadv[it][ik],qadv[it][ik],uadv[it][ik],vadv[it][ik]))
# sachant que uadv,vadv = 0, que ufrc,vfrc soit geo soit nudg
#
# pour EXSEG, écrire pour chaque pas de temps it de forçage :
# 
############################

### 


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
