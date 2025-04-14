#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 01 14:53:18 2020

@author: rodierq
@ to be run as python convertMNH550_to_DEPHY_all.py oldfile.nc newfile.nc
@modif:11/06: couvreuxf
@modif: 16/06: rodierq :: add sv1/2/3 (MEAN_SV), dealing with dry/wet case, correction missing attributes, add entrainement/detrainement
@modif: 11/07/22 fernandesr: pour utliser avec MNH 5-5-0*
         Key changes from 5-4-4 include variable dimensions, the way in which MNH saves variables and some variable names.

"""
import netCDF4 as nc
import sys, os
import numpy as np
import datetime as dt
from datetime import datetime

ncfile = sys.argv[1]
dataIn = nc.Dataset(ncfile,'r')
ncfileout = sys.argv[2]
dataOut = nc.Dataset(ncfileout,'w')

#Variable pour recuperer les dimensions (= doit toujours etre present)
varDate = dataIn.variables['time_les'][:]
varLevel = dataIn.variables['level_les'][:] #Suppression du halo sur la verticale
#varLevel = dataIn.variables['level_les'][0:95] #Suppression du halo sur la verticale

#### Gestion du Temps
timeInterval = varDate[1] - varDate[0]
initialTimeSince00h = varDate[1] - timeInterval #La 1ere entree de la variable time est deja avance d'un pas de temps
varTime = varDate.flatten() 
#varTime = np.linspace(timeInterval,timeInterval*varShape.shape[0],varShape.shape[0])


#Creation des Dimensions
dataOut.createDimension('levf',size=varLevel.size)
dataOut.createDimension('time',size=varTime.size)
dataOut.createDimension('S_N_direction',size=1)
dataOut.createDimension('W_E_direction',size=1)

#Variables correspondantes aux dimensions
levf = dataOut.createVariable('levf', np.float64, ('levf'))
levf[:] = varLevel[:]

time = dataOut.createVariable('time', np.float64, ('time'))
time[:] = varTime[:]

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

############################# DEFINING VARIABLES IN FORMAT COMMUN ##########################################

#Redefinition des attributs long_name : Dict_attr[new_var_name] = long_name
#Remarque : l'ordre n'est pas important
Dict_attr = {}
Dict_attr["temp"] = "temperature"
Dict_attr["theta"] = "potential temperature"
Dict_attr["thetav"] = "virtual potential temperature"
Dict_attr["thl"] = "liquid potential temperature"
Dict_attr["qt"] = "total water content"
Dict_attr["pf"] = "pressure"
Dict_attr["rho"] = "density"
Dict_attr["ql"] = "liquid water content"
Dict_attr["qv"] = "specific humidity"
Dict_attr["rneb"] = "cloud fraction"
Dict_attr["qr"] = "rain water content"
Dict_attr["qi"] = "ice water content"
Dict_attr["qs"] = "snow water content"
Dict_attr["qg"] = "graupel water content"

Dict_attr["rv"] = "water vapor mixing ratio"
Dict_attr["rl"] = "liquid vater mixing ratio"
Dict_attr["rr"] = "rain water mixing ratio"
Dict_attr["ri"] = "ice water mixing ratio"
Dict_attr["rs"] = "snow water mixing ratio"
Dict_attr["rg"] = "graupel water mixing ratio"
Dict_attr["rt"] = "total water mixing ratio"
Dict_attr["rh"] = "Relative Humidity"

Dict_attr["ww_res"] = "resolved w-variance"
Dict_attr["ww_sbg"] = "subgrid w-variance"
Dict_attr["thl2_res"] = "resolved liquid potential temperature variance"
Dict_attr["thl2_sbg"] = "subgrid liquid potential temperature variance"
Dict_attr["th2_res"] = "resolved potential temperature variance"
Dict_attr["th2_sbg"] = "subgrid potential temperature variance"
Dict_attr["tke_res"] = "resolved turbulent kinetic energy"
Dict_attr["tke_sbg"] = "subgrid turbulent kinetic energy"
Dict_attr["shf"] = "sensible heat flux"
Dict_attr["lhf"] = "latent heat flux"
Dict_attr["hpbl"] = "boundary-layer height"
Dict_attr["wrt"] = "vertical moisture flux"
Dict_attr["wrv"] = "vertical moisture flux"
Dict_attr["wth"] = "vertical potential temperature flux"

Dict_attr["wthl"] = "vertical liquid potential temperature flux"
Dict_attr["wthv"] = "vertical virtual potential temperature flux"
Dict_attr["uu"] = "u-variance"
Dict_attr["vv"] = "v-variance"
Dict_attr["ww"] = "w-variance"
Dict_attr["thl2"] = "thl-variance"
Dict_attr["rt2"] = "rt-variance"
Dict_attr["th2"] = "th-variance"
Dict_attr["rv2"] = "rv-variance"
Dict_attr["uw"] = "zonal momentum flux"
Dict_attr["vw"] = "meridional momentum flux"
Dict_attr["u"] = "zonal component wind"
Dict_attr["t2m"] = "2m-temperature"
Dict_attr["t10m"] = "10m-temperature"
Dict_attr["q2m"] = "2m-specific humidity"
Dict_attr["u10m"] = "10m-zonal wind"
Dict_attr["v10m"] = "10m-meridional wind"
Dict_attr["ustar"] = "friction velocity"
Dict_attr["tke"] = "turbulent kinetic energy"
Dict_attr["alphau_shcon"] = "thermal fraction"
Dict_attr["qtu_shcon"] = "specific humidity in thermals"
Dict_attr["qlu_shcon"] = "liquid water content in thermals"
Dict_attr["alphau_shcon"] = "thermal fraction"
Dict_attr["mu_shcon"] = "thermal mass flux"
Dict_attr["v"] = "meridional component wind" 
Dict_attr["w"] = "vertical wind velocity"
Dict_attr["avg"] = "mean number of points"
Dict_attr["avg_cld"] = "mean number of points for cloud sampling"
Dict_attr["avg_cor"] = "mean number of points for core sampling"
Dict_attr["avg_cs1"] = "mean number of points for tracer CS1 sampling"
Dict_attr["avg_cs2"] = "mean number of points for tracer CS2 sampling"
Dict_attr["avg_cs3"] = "mean number of points for tracer CS3 sampling"
Dict_attr["uu_res"] = "resolved u-variance"
Dict_attr["vv_res"] = "resolved v-variance"
Dict_attr["uw_res"] = "resolved zonal momentum flux"
Dict_attr["vw_res"] = "resolved meridional momentum flux"
Dict_attr["rv2_res"] = "resolved vapour mixing ratio variance"
Dict_attr["wth_res"] = "resolved potential temperature flux"
Dict_attr["wrv_res"] = "resolved vapour mixing ratio flux"
Dict_attr["rv2_sbg"] = "subgrid vapour mixing ratio variance"
Dict_attr["wthv_res"] = "resolved virtual potential temperature flux"
Dict_attr["rt2_res"] = "resolved total mixing ratio variance"
Dict_attr["wthl_res"] = "resolved liquid potential temperature flux"
Dict_attr["wrt_res"] = "resolved total mixing ratio flux"
Dict_attr["rt2_sbg"] = "subgrid total mixing ratio variance"
Dict_attr["uu_sbg"] = "subgrid u-variance"
Dict_attr["vv_sbg"] = "subgrid v-variance"
Dict_attr["wth_sbg"] = "subgrid potential temperature flux"
Dict_attr["wrv_sbg"] = "subgrid vapour mixing ratio flux"
Dict_attr["wthl_sbg"] = "subgrid liquid potential temperature flux"
Dict_attr["wrt_sbg"] = "subgrid total mixing ratio flux"
Dict_attr["uw_sbg"] = "subgrid zonal momentum flux"
Dict_attr["vw_sbg"] = "subgrid meridional momentum flux"
Dict_attr["Q0"] = "surface sensible heat flux"
Dict_attr["E0"] = "surface latent heat flux"
Dict_attr["instprec"] = "surface instanteneous precipitation rate"
Dict_attr["accuprec"] = "surface accumulated precipitation rate"
Dict_attr["sv"] = "scalar"
Dict_attr["sv2"] = "scalar"
Dict_attr["sv3"] = "scalar"

## Additional variables modif Royston Fernandes
Dict_attr["wthv_sbg"] = "subgrid vertical flux of liquid potential temperature "
Dict_attr["wstar"] = "Convective velocity"
Dict_attr["lmo"] = "Monin Obhukov length"
Dict_attr["zcftot"] = "Total cloud cover"
Dict_attr["swu"] = "SW upward radiative flux"
Dict_attr["swd"] = "SW downward radiative flux"
Dict_attr["lwu"] = "LW upward radiative flux"
Dict_attr["lwd"] = "LW downward radiative flux"
Dict_attr["zcb"] = "Height of cloud base"
Dict_attr["lwp"] = "Liquid Water Path"
Dict_attr["sfce_rain"] = "Surface Rain Rate"
Dict_attr["acc_sf_rain"] = "Accumulated Surface Rain"
Dict_attr["rwp"] = "Rain Water Path"
Dict_attr["iwp"] = "Ice Water Path"
Dict_attr["lwpvar"] = "Variance of Liquid Water Path"
Dict_attr["swp"] = "Snow Water Path"
Dict_attr["gwp"] = "Graupel Water Path"
Dict_attr["zmaxcf"] = "Height of maximum of cloud fraction"

############################# COPYING EXISTING VARIABLES ##########################################


#Variables physiques
Dict_new_var = {}

######### MEAN PROFILES
Old_Mean_var = ['MEAN_TH','MEAN_THV','MEAN_THL','MEAN_RR','MEAN_PRE','MEAN_U','MEAN_V','MEAN_RHO','MEAN_RC',
           'MEAN_RV','MEAN_CF','MEAN_RT','MEAN_RI','MEAN_RS','MEAN_RG','MEAN_REHU']
New_Mean_var = ['theta','thetav','thl','rr','pf','u','v','rho','rl',
                'rv','rneb','rt','ri','rs','rg','rh']

for (old_var_name,new_var_name) in zip(Old_Mean_var,New_Mean_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cart/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
      #print(old_var)
  except KeyError:
      continue
  new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'), fill_value=999)
  new_var[:,:,:,:] = old_var[:,:] 
  new_var.setncattr('long_name', Dict_attr[new_var_name])
  new_var.setncattr('units', old_var.getncattr('units'))
  
  Dict_new_var[new_var_name] = new_var
######### RESOLVED FLUXES
Old_res_Flux_var = ['RES_W2','RES_U2','RES_V2','RES_WU','RES_WV','RES_THL2','RES_RT2','RES_WTHL','RES_WTHV','RES_KE','RES_WRT',
           'RES_RV2','RES_WTH','RES_WRV','RES_TH2']

New_res_Flux_var = ['ww_res','uu_res','vv_res','uw_res','vw_res','thl2_res','rt2_res','wthl_res','wthv_res','tke_res','wrt_res',
           'rv2_res','wth_res','wrv_res', 'th2_res']

for (old_var_name,new_var_name) in zip(Old_res_Flux_var,New_res_Flux_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Resolved/Cartesian/Not_time_averaged/Not_normalized/cart/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'), fill_value=999)
  new_var[:,:,:,:] = old_var[:,:]
  new_var.setncattr('long_name', Dict_attr[new_var_name])
  new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var

######### SUBGRID FLUXES
Old_sbg_Flux_var = ['SBG_W2','SBG_U2','SBG_V2', 'SBG_WU','SBG_WV', 'SBG_THL2','SBG_RT2','SBG_WTHL','SBG_WTHV',
                    'SBG_TKE', 'SBG_WRT' ]

New_sbg_Flux_var = ['ww_sbg','uu_sbg','vv_sbg', 'uw_sbg','vw_sbg', 'thl2_sbg','rt2_sbg','wthl_sbg','wthv_sbg',
                    'tke_sbg', 'wrt_sbg' ]

for (old_var_name,new_var_name) in zip(Old_sbg_Flux_var,New_sbg_Flux_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Subgrid/Cartesian/Not_time_averaged/Not_normalized/cart/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'), fill_value=999)
  new_var[:,:,:,:] = old_var[:,:]
  new_var.setncattr('long_name', Dict_attr[new_var_name])
  new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var

######### SURFACE FLUXES
Old_surface_Flux_var =['Q0','E0', 'Ustar', 'Wstar', 'L_MO','INST_PREC','ACCU_PREC']    
    
New_surface_Flux_var =['Q0','E0', 'ustar', 'wstar', 'lmo','sfce_rain','acc_sf_rain']                   

for (old_var_name,new_var_name) in zip(Old_surface_Flux_var,New_surface_Flux_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Surface/Cartesian/Not_time_averaged/Not_normalized/cart/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  new_var = dataOut.createVariable(new_var_name, np.float64, ('time'))
  new_var[:] = old_var[:]  
  new_var.setncattr('long_name', Dict_attr[new_var_name])
  new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var

  #Conversion des quelques flux de surfaces
  if(new_var_name == 'Q0'): #Flux de chaleur sensible surface m K s-1 ==> W/m2
    new_var[:] = -1.0 * new_var[:] * Dict_new_var['rho'][:,0,0,0] * 1004.9
    new_var.setncattr('units','W m-2')
    continue
  
  if(new_var_name == 'E0'): #Flux de chaleur latente surface kg kg-1 ms-1 ==> W/m2
    new_var[:] = -1.0 * new_var[:] * Dict_new_var['rho'][:,0,0,0] * 2500000.0
    new_var.setncattr('units','W m-2')
    continue

######### Miscellaneous variables
Old_Miscellaneous_Flux_var = ['AVG_PTS', 'BL_H', 'ZCF2TOT','ZCB','LWP','RWP','IWP','LWPVAR','SWP','GWP','ZMAXCF']
    
New_Miscellaneous_Flux_var = ['avg', 'hpbl', 'zcftot','zcb','lwp','rwp','iwp','lwpvar','swp','gwp','zmaxcf']

for (old_var_name,new_var_name) in zip(Old_Miscellaneous_Flux_var,New_Miscellaneous_Flux_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Miscellaneous/Cartesian/Not_time_averaged/Not_normalized/cart/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  if  (new_var_name == 'avg'):     
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'),
                                       fill_value=999)
    new_var[:,:,:,:] = old_var[:,:]
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
  else:
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time'))
    new_var[:] = old_var[:] 
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var

######### Cloud sampling variables
Old_Miscellaneous_Cloudsamp = ['AVG_PTS']
    
New_Miscellaneous_Cloudsamp = ['avg_cld']

for (old_var_name,new_var_name) in zip(Old_Miscellaneous_Cloudsamp,New_Miscellaneous_Cloudsamp):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Miscellaneous/Cartesian/Not_time_averaged/Not_normalized/neb/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  if  (new_var_name == 'avg_cld'):     
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'),
                                       fill_value=999)
    new_var[:,:,:,:] = old_var[:,:]
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
  else:
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time'))
    new_var[:] = old_var[:] 
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var

Old_CloudCoreSamp_var = ['MEAN_TH','MEAN_THV','MEAN_THL','MEAN_RR','MEAN_PRE','MEAN_U','MEAN_V','MEAN_RHO','MEAN_RC',
           'MEAN_RV','MEAN_RI','MEAN_RG','MEAN_RS','MEAN_W','MEAN_RT']
New_Cloud_var = ['theta_cld','thetav_cld','thl_cld','rr_cld','pf_cld','u_cld','v_cld',
           'rho_cld','rl_cld','rv_cld','ri_cld','rg_cld','rs_cld','w_cld','rt_cld']
suffix="_cld"
for element in New_Cloud_var:
  Dict_attr[element] = "cloud sampling " + Dict_attr[element[:len(element)-len(suffix)]]

for (old_var_name,new_var_name) in zip(Old_CloudCoreSamp_var,New_Cloud_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/neb/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
      #print(old_var)
  except KeyError:
      continue
  print('new_var_name',new_var_name)
  print(len(old_var))
  new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'), fill_value=999)
  new_var[:,:,:,:] = old_var[:,:] 
  new_var.setncattr('long_name', Dict_attr[new_var_name])
  new_var.setncattr('units', old_var.getncattr('units'))
  
  Dict_new_var[new_var_name] = new_var

######### Core sampling variables
Old_Miscellaneous_Coresamp = ['AVG_PTS']
    
New_Miscellaneous_Coresamp = ['avg_cor']

for (old_var_name,new_var_name) in zip(Old_Miscellaneous_Coresamp,New_Miscellaneous_Coresamp):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Miscellaneous/Cartesian/Not_time_averaged/Not_normalized/core/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  if  (new_var_name == 'avg_cor'):     
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'),
                                       fill_value=999)
    new_var[:,:,:,:] = old_var[:,:]
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
  else:
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time'))
    new_var[:] = old_var[:] 
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var
Old_CloudCoreSamp_var = ['MEAN_TH','MEAN_THV','MEAN_THL','MEAN_RR','MEAN_PRE','MEAN_U','MEAN_V','MEAN_RHO','MEAN_RC',
           'MEAN_RV','MEAN_RI','MEAN_RG','MEAN_RS','MEAN_W','MEAN_RT']
New_Core_var = ['theta_cor','thetav_cor','thl_cor','qr_cor','pf_cor','u_cor','v_cor',
           'rho_cor','rl_cor','rv_cor','ri_cor','rg_cor','rs_cor','w_cor','rt_cor']
suffix="_cor"
for element in New_Core_var:
  Dict_attr[element] = "core sampling " + Dict_attr[element[:len(element)-len(suffix)]]

for (old_var_name,new_var_name) in zip(Old_CloudCoreSamp_var,New_Core_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/core/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
      #print(old_var)
  except KeyError:
      continue
  new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'), fill_value=999)
  new_var[:,:,:,:] = old_var[:,:] 
  new_var.setncattr('long_name', Dict_attr[new_var_name])
  new_var.setncattr('units', old_var.getncattr('units'))
  
  Dict_new_var[new_var_name] = new_var
######### CS1 sampling variables
Old_Miscellaneous_Cs1samp = ['AVG_PTS']
    
New_Miscellaneous_Cs1samp = ['avg_cs1']

for (old_var_name,new_var_name) in zip(Old_Miscellaneous_Cs1samp,New_Miscellaneous_Cs1samp):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Miscellaneous/Cartesian/Not_time_averaged/Not_normalized/cs1/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  if  (new_var_name == 'avg_cs1'):     
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'),
                                       fill_value=999)
    new_var[:,:,:,:] = old_var[:,:]
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
  else:
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time'))
    new_var[:] = old_var[:] 
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var

Old_CloudCoreSamp_var = ['MEAN_TH','MEAN_THV','MEAN_THL','MEAN_RR','MEAN_PRE','MEAN_U','MEAN_V','MEAN_RHO','MEAN_RC',
           'MEAN_RV','MEAN_RI','MEAN_RG','MEAN_RS','MEAN_W','MEAN_RT']
New_Tracer_var = ['theta_sam','thetav_sam','thl_sam','rr_sam','pf_sam','u_sam','v_sam',
           'rho_sam','rl_sam','rv_sam','ri_sam','rg_sam','rs_sam','w_sam','rt_sam']
suffix="_sam"
for element in New_Tracer_var:
  Dict_attr[element] = "tracer sampling " + Dict_attr[element[:len(element)-len(suffix)]]

for (old_var_name,new_var_name) in zip(Old_CloudCoreSamp_var,New_Tracer_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Mean/Cartesian/Not_time_averaged/Not_normalized/cs1/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
      #print(old_var)
  except KeyError:
      continue
  new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'), fill_value=999)
  new_var[:,:,:,:] = old_var[:,:] 
  new_var.setncattr('long_name', Dict_attr[new_var_name])
  new_var.setncattr('units', old_var.getncattr('units'))
  
  Dict_new_var[new_var_name] = new_var
######### CS2 sampling variables
Old_Miscellaneous_Cs2samp = ['AVG_PTS']
    
New_Miscellaneous_Cs2samp = ['avg_cs2']

for (old_var_name,new_var_name) in zip(Old_Miscellaneous_Cs2samp,New_Miscellaneous_Cs2samp):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Miscellaneous/Cartesian/Not_time_averaged/Not_normalized/cs2/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  if  (new_var_name == 'avg_cs2'):     
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'),
                                       fill_value=999)
    new_var[:,:,:,:] = old_var[:,:]
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
  else:
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time'))
    new_var[:] = old_var[:] 
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var


######### CS2 sampling variables
Old_Miscellaneous_Cs3samp = ['AVG_PTS']
    
New_Miscellaneous_Cs3samp = ['avg_cs3']

for (old_var_name,new_var_name) in zip(Old_Miscellaneous_Cs3samp,New_Miscellaneous_Cs3samp):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Miscellaneous/Cartesian/Not_time_averaged/Not_normalized/cs3/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  if  (new_var_name == 'avg_cs3'):     
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'),
                                       fill_value=999)
    new_var[:,:,:,:] = old_var[:,:]
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
  else:
    new_var = dataOut.createVariable(new_var_name, np.float64, ('time'))
    new_var[:] = old_var[:] 
    new_var.setncattr('long_name', Dict_attr[new_var_name])
    new_var.setncattr('units', old_var.getncattr('units'))
    
  Dict_new_var[new_var_name] = new_var






######### RADIATIVE FLUXES 
Old_radiation_Flux_var =['SWU', 'SWD', 'LWU', 'LWD']

New_radiation_Flux_var =['swu', 'swd', 'lwu', 'lwd']

for (old_var_name,new_var_name) in zip(Old_radiation_Flux_var,New_radiation_Flux_var):
  #Gestion des variables inexistantes selon le fichier MNH a convertir
  try:
      old_var = dataIn['/LES_budgets/Radiation/Cartesian/Not_time_averaged/Not_normalized/cart/'+ str(old_var_name)] #Lecture de la variable dans fichier MNH
  except KeyError:
      continue
  new_var = dataOut.createVariable(new_var_name, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'), fill_value=999)
  print('size old_var',old_var_name,old_var.shape)
  new_var[:,:,:,:] = old_var[:,:]
  new_var.setncattr('long_name', Dict_attr[new_var_name])
  new_var.setncattr('units', old_var.getncattr('units'))


############################# PROCESSED VARIABLES ##########################################

######### RESOLVED + SUBGRID FLUXES 
new_res_sbg_name = ['wrt', 'wthl', 'uu', 'vv', 'ww', 'tke', 'uw', 'vw', 'thl2', 'rt2',
                    'wrv', 'wth', 'th2', 'rv2']
for var_tot in new_res_sbg_name:
  try: #First test if the resolved and subgrid variables exist
      Dict_new_var[var_tot + '_res'][:,:,:,:] + Dict_new_var[var_tot + '_sbg'][:,:,:,:]
  except KeyError:
      print("WARNING Missing key variable for computation of total (res+sbg) " + var_tot)
      continue
  new_var = dataOut.createVariable(var_tot, np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'), fill_value=999)
  new_var[:,:,:,:] = Dict_new_var[var_tot + '_res'][:,:,:,:] + Dict_new_var[var_tot + '_sbg'][:,:,:,:]
  new_var.setncattr('units', Dict_new_var[var_tot + '_res'].getncattr('units'))
  new_var.setncattr('long_name', Dict_new_var[var_tot + '_res'].getncattr('long_name')[9:])    

#Temperature
try:
    Dict_new_var['theta'][:,:,:,:] * (Dict_new_var['pf'][:,:,:,:] / 100000.0)**0.286
    temp = dataOut.createVariable('temp', np.float64, ('time', 'levf', 'S_N_direction','W_E_direction'))
    temp[:,:,:,:] = Dict_new_var['theta'][:,:,:,:] * (Dict_new_var['pf'][:,:,:,:] / 100000.0)**0.286
    temp.setncattr('units', 'K')
    temp.setncattr('long_name', 'temperature')
except KeyError:
    print("WARNING Missing key variable for computation of temperature")
    
############################# GLOBAL ATTRIBUTES ##########################################
dataOut.case = sys.argv[2]
dataOut.version = "Created on " + str(datetime.now())
dataOut.format_version = "0"
dataOut.title = "Output from MesoNH v5-7-0"
dataOut.script = "convertMNH570_to_DEPHY.py"

dataOut.close()
dataIn.close()
