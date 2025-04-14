# -*- coding: utf-8 -*-
"""
Created on Mon Jun 01 14:53:18 2020

@author: rodierq
@ to be run as python convertMNH550_to_DEPHY.py oldfile.nc newfile.nc
@modif:11/06: couvreuxf
@modif: 16/06: rodierq :: add sv1/2/3 (MEAN_SV), dealing with dry/wet case, correction missing attributes, add entrainement/detrainement
@modif: 11/07/22 fernandesr: pour utliser avec MNH 5-5-0*
         Key changes from 5-4-4 include variable dimensions, the way in which MNH saves variables and some variable names.
@modif: 10/04/2025 - villefranquen - reprise et nettoyage

"""
import netCDF4 as nc
import sys, os
import numpy as np
import datetime as dt
from datetime import datetime
from dephy_variables import Dict_attr
from mesonh2dephy_variables import Dict_new_varnames_all

list_bil = ["UU", "VV", "TH", "RV", "RC"]     # in MesoNH file
list_cs  = ["cart", "neb", "core", "cs1"]     # in MesoNH file
list_cs_name = ["cld", "core", "sam"]         # in var short name
list_cs_longname = ["cloud sampling ", "core sampling ", "tracer sampling "]
list_variables_convert = ["E0", "Q0"]

def print_help():
    print("usage: python %s input.nc output.nc"%(sys.argv[0]))

if (len(sys.argv) != 3) : print("error: missing arguments") ; print_help(); exit(1)

ncfile = sys.argv[1]
ncfileout = sys.argv[2]

dataIn = nc.Dataset(ncfile,'r')
dataOut = nc.Dataset(ncfileout,'w')

#Variable pour recuperer les dimensions (= doit toujours etre present)
varDate = dataIn.variables['time_les'][:]
varDateB = dataIn.variables['time_budget'][:]
varLevel = dataIn.variables['level_les'][:] #Suppression du halo sur la verticale
#varLevel = dataIn.variables['level_les'][0:95] #Suppression du halo sur la verticale

#### Gestion du Temps
timeInterval = varDate[1] - varDate[0]
initialTimeSince00h = varDate[1] - timeInterval #La 1ere entree de la variable time est deja avance d'un pas de temps
varTime = varDate.flatten() 
varTimeB = varDateB.flatten()
#varTime = np.linspace(timeInterval,timeInterval*varShape.shape[0],varShape.shape[0])

#Creation des Dimensions
dataOut.createDimension('levf',size=varLevel.size)
dataOut.createDimension('time',size=varTime.size)
dataOut.createDimension('time_budget',size=varTimeB.size)
dataOut.createDimension('S_N_direction',size=1)
dataOut.createDimension('W_E_direction',size=1)

#Variables correspondantes aux dimensions
levf = dataOut.createVariable('levf', np.float64, ('levf'))
levf[:] = varLevel[:]

time = dataOut.createVariable('time', np.float64, ('time'))
time[:] = varTime[:]

time_budget = dataOut.createVariable('time_budget', np.float64, ('time_budget'))
time_budget[:] = varTimeB[:]
#Attributs des variables dimensions
levelForAtt = dataIn.variables['level'] #pas de [:] sinon conversion en array et perte des attributs
levf.setncattr('long_name',levelForAtt.getncattr('long_name'))
levf.setncattr('units',levelForAtt.getncattr('units'))

timeForAtt = dataIn.variables['time_les'] ### Edit Roy
time.setncattr('long_name',timeForAtt.getncattr('long_name'))
time.setncattr('calendar',timeForAtt.getncattr('calendar'))
#Time Units : conservation de la chaine "seconds since date" + conversion du temps (seconds) en HH:MM:SS
timeFormatted = timeForAtt.getncattr('units')[:25] 
time.setncattr('units',timeFormatted)

def get_longname(new_var_name):
  if new_var_name in Dict_attr:
    longname = Dict_attr[new_var_name]
  else:
    for cs,nam in zip(list_cs_name, list_cs_longname):
      if cs in new_var_name:
        cart_name = new_var_name.split("_"+cs)[0]
        longname = nam+Dict_attr[cart_name]
  return longname

def create_var(new_var_name, old_var, vardims, data=None):
  vartype = old_var.dtype
  varunits = old_var.units
  longname = get_longname(new_var_name)
  new_var = dataOut.createVariable(new_var_name, vartype, vardims, fill_value=999)
  print(new_var_name, longname, new_var.shape, old_var.shape)
  if data is None : new_var[:] = old_var[:]
  else: new_var[:] = data[:]
  new_var.long_name = longname
  new_var.units = varunits #setncattr('units', old_var.getncattr('units'))
  return new_var

def convert(var, old_var, new_var):
  if(var == 'Q0'): #Flux de chaleur sensible surface m K s-1 ==> W/m2
    new_var[:] = new_var[:] * Dict_new_var['rho'][:,0,0,0] * 1004.9
    new_var.units = 'W m-2'
  
  if(var == 'E0'): #Flux de chaleur latente surface kg kg-1 ms-1 ==> W/m2
    new_var[:] = new_var[:] * Dict_new_var['rho'][:,0,0,0] * 2500000.0
    new_var.units = 'W m-2'

  return new_var

vardims4D = ('time', 'levf', 'S_N_direction','W_E_direction')
vardims1D = ('time',)
buddims4D = ('time_budget', 'levf', 'S_N_direction','W_E_direction')
buddims1D = ('time_budget',)

def extract_group(groupe, bilan=False):
  try:
    for var in dataIn[groupe].variables:
      if var in Dict_new_varnames.keys():
        old_var = dataIn[groupe].variables[var]
        vardims = vardims4D if len(old_var.shape)==2 else vardims1D
        if bilan: vardims=buddims4D
        new_var = create_var(Dict_new_varnames[var], old_var, vardims)
        if var in list_variables_convert:
          new_var = convert(var, old_var, new_var)
        Dict_new_var[Dict_new_varnames[var]] = new_var
  except (KeyError,IndexError):
    return

Dict_new_var = {}

# Copying existing variables 

for cs in list_cs:
  Dict_new_varnames = Dict_new_varnames_all[cs]
  for sgroup in ["Mean", "Resolved", "Subgrid", "Surface", "Radiation", "Miscellaneous"]:
    groupe="/LES_budgets/%s/Cartesian/Not_time_averaged/Not_normalized/%s"%(sgroup,cs)
    extract_group(groupe)

for bil in list_bil:
  Dict_new_varnames = Dict_new_varnames_all[bil]
  groupe="/Budgets/%s/"%bil
  extract_group(groupe, bilan=True)

# Processing variables
list_var_tot = ['wrt', 'wthl', 'uu', 'vv', 'ww', 'tke', 'uw', 'vw', 'thl2', 'rt2', 'wrv', 'wth', 'th2', 'rv2']
for var_tot in list_var_tot:
  try:
    old_var = Dict_new_var[var_tot+"_res"]
    dat_tot = Dict_new_var[var_tot+"_res"][:,:,:,:]+Dict_new_var[var_tot+"_sbg"][:,:,:,:]
    create_var(var_tot, old_var, vardims4D, data=dat_tot)
  except (KeyError, IndexError):
    continue

try:
    dat = Dict_new_var['theta'][:,:,:,:] * (Dict_new_var['pf'][:,:,:,:] / 100000.0)**0.286
    new_var = create_var("temp", Dict_new_var['theta'], vardims4D, data=dat)
except KeyError:
    print("warning: Missing key variable for computation of temperature")
    
dataOut.case = ncfileout
dataOut.version = "Created on " + str(datetime.now())
dataOut.format_version = "0"
dataOut.title = "Output from MesoNH v5-7-1"
dataOut.script = sys.argv[0]

dataOut.close()
dataIn.close()
