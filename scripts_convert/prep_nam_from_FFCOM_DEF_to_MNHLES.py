#-*- coding:UTF-8 -*-
# utilisation du module netCDF4 pour la lecture et l'ecriture de fichiers netcdf3
"""
Created on August 2021 adapted from prep_nam_from_FFCOM_DEF_to_MNH1D.py
A lancer par python3  prep_nam_from_FFCOM_DEF_to_MNH1D.py  CAS SOUS_CAS

@author: Fleur Couvreux

Modification
   2023/11/27 pour nettoyer
"""
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

#Modification a apporter
# cas avec Rayonnement ex ARMCVP => choix des namelists adaptées
# voir avec Benoit si on peut tout faire avec Lima en utilisant des setup qui convergent vers ICE3 ou demande trop de temps de calcul?
# pour l'instant choix avec NI=NJ=512, res_Dx variable ~ 25m pour cvpp ou cvseche, ~ 200m pour transition cvp
# grille verticale homogène pour cvpp / cv_sèche / cvp
import matplotlib as mpl
# "backend" pour sortie fichier
mpl.use('agg')
import matplotlib.pyplot as plt
import numpy as np
import netCDF4 as nc
import sys, os
#import cdms2
#import cdtime

# pour enlever les dims = 1 dans un tableau
#new_array=np.squeeze(array)

casname=sys.argv[1]
subcasname=sys.argv[2]
NI=512
NJ=512
NAM2='TESTX'

zorog=0.
if casname =='ARMCU':
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=25. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname
    #provide name of the files for the simulations for EXSEG1.nam
    NAM1=casname
    # provide cloud scheme => possibility to add as a function of the CLOUD SCHEME Different namelists TBD
    CLOUDSC='ICE3'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01
    flag_t='theta'
    flag_q='rt'
elif casname =='BLLAST':
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=25. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname
    #provide name of the files for the simulations for EXSEG1.nam
    NAM1=casname[0:4]
    # provide cloud scheme => possibility to add as a function of the CLOUD SCHEME Different namelists TBD
    CLOUDSC='ICE3'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01
    flag_t='theta'
    flag_q='rv' 

elif casname =='AMMA':
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=200.
    DY=200.
    DZ=0. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname
    #provide name of the files for the simulations for EXSEG1.nam
    NAM1=casname+'D'
    # provide cloud scheme => possibility to add as a function of the CLOUD SCHEME Different namelists TBD
    CLOUDSC='ICE3'
    #provide size of the domain
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=16500.
    ktop=0.01
    flag_t='ta'
    flag_q='qv'    
    
elif casname =='LBA':
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=200.
    DY=200.
    DZ=0. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname
    #provide name of the files for the simulations for EXSEG1.nam
    NAM1=casnmae+'DD'
    # provide cloud scheme => possibility to add as a function of the CLOUD SCHEME Different namelists TBD
    CLOUDSC='ICE3'
    #provide size of the domain
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=16500.
    ktop=0.01
    flag_t='theta'
    flag_q='rv'    

elif casname =='SCMS':
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=25. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname
    #provide name of the files for the simulations for EXSEG1.nam
    NAM1=casname+'X'
    # provide cloud scheme => possibility to add as a function of the CLOUD SCHEME Different namelists TBD
    CLOUDSC='ICE3'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01
    flag_t='theta'
    flag_q='rv'
elif casname =='SANDU':
    case=casname[0:5]
    NAM1=casname[0:2]+subcase[0:3]
    print('cas =',case,'subcase=',subcase,'NAM1=',NAM1)
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=10. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=NAM1+'_1D'
    #provide name of the files for the simulations for EXSEG1.nam
    # provide cloud scheme => possibility to add as a function of the CLOUD SCHEME Different namelists TBD
    CLOUDSC='KHKO'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHLMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01
    flag_t='thetal'
    flag_q='qt'    
elif casname =='ASTEX':     
    NAM1=casname
    print('cas =',casname,'subcase=',subcasname,'NAM1=',NAM1)
    print('WARNING!!!! Verifier la grille verticale')
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    print('file=',file)
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=0. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname+subcasname+'_LES'
    #provide name of the files for the simulations for EXSEG1.nam
    # provide cloud scheme => possibility to add as a function of the CLOUD SCHEME Different namelists TBD
    CLOUDSC='KHKO'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHLMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01
    flag_t='thetal'
    flag_q='qt'
elif casname =='RICO':
    # PBM CAS SHORT DEFINI EN TEMP ALORS QUE DANS VANZANTEN C'est en THETA
    file='DEPHY-SCM/RICO/SHORT/RICO_SHORT_DEF_driver.nc'
    file='DEPHY-SCM/RICO/MESONH/RICO_MESONH_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=25. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI='RICO_1D'
    #provide name of the files for the simulations
    NAM1='RICON'
    CLOUDSC='ICE3'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01
    flag_t='theta'
    flag_q='rv'
elif casname =='BOMEX':
    # PBM CAS SHORT DEFINI EN TEMP ALORS QUE DANS VANZANTEN C'est en THETA
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=25. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname
    #provide name of the files for the simulations
    NAM1=casname
    CLOUDSC='ICE3'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHLMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01
    flag_t='thetal'
    flag_q='qt'
elif casname =='IHOP':
    # PBM CAS SHORT DEFINI EN TEMP ALORS QUE DANS VANZANTEN C'est en THETA
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=0. # dans un 1er temps meme grille verticale fine que LES
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname
    #provide name of the files for the simulations
    NAM1=casname+'X'
    CLOUDSC='ICE3'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01    
    flag_t='theta'
    flag_q='rv'
elif casname =='AYOTTE':
    shortname=casname[0:1]+subcasname[0:5]
    print('cas AYOTTE=',shortname)
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=25.
    DY=25.
    DZ=0.
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=shortname+'_1D'
    #provide name of the files for the simulations
    NAM1=shortname
    CLOUDSC='ICE3'
    #provide size of the domain
    NZ=160
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=3500.
    ktop=0.01    
    flag_t='theta'
    flag_q='rt'    
elif casname =='GABLS1':
    # PBM CAS SHORT DEFINI EN TEMP ALORS QUE DANS VANZANTEN C'est en THETA
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=10.
    DY=10.
    DZ=5 # dans un 1er temps meme grille verticale fine que LES
    DZmin=2
    DZmax=6
    zmax=250.
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI='GABLS1_1D'
    #provide name of the files for the simulations
    NAM1='GABL1'
    CLOUDSC='NONE'
    #provide size of the domain
    NZ=100
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=350.
    ktop=0.01
    flag_t='theta'
    flag_q='rt'
elif casname =='GABLS4':
    file='DEPHY-SCM/'+casname+'/'+subcasname+'/'+casname+'_'+subcasname+'_DEF_driver.nc'
    #provide horizontal & vertical resolution
    DX=1.
    DY=1.
    DZ=1 # dans un 1er temps meme grille verticale fine que LES
    DZmin=0.2
    DZmax=2
    zmax=50.
    #provide name of the files for the simulations for PRE_IDEA1.nam
    CINI=casname
    #provide name of the files for the simulations
    NAM1=casname[0:5]
    CLOUDSC='NONE'
    #provide size of the domain
    NZ=100
    keyword='ZUVTHDMR'
    #Provide bottom height of the dumping layer and its coefficient
    zbot=80.
    ktop=0.01
    flag_t='theta'
    flag_q='qv'    

f = nc.Dataset(file,'r')
liste_var=f.variables
print('liste des variables du fichier driver:')
for nomvar in liste_var:
    print(nomvar)

attributes = {}
for att in ['start_date','end_date','forcing_scale','adv_ta','adv_theta','adv_thetal','radiation','adv_qv','adv_qt','adv_rv','adv_rt','forc_wa','forc_wap','forc_geo','nudging_ua','nudging_va','nudging_ta','nudging_theta','nudging_thetal','nudging_qv','nudging_qt','nudging_rv','nudging_rt','surface_type','surface_forcing_temp','surface_forcing_wind','surface_forcing_moisture']:
  attributes[att] = 0
for att in f.ncattrs():
  attributes[att] = getattr(f,att)  
  print(att,attributes[att])

startDate = attributes['start_date']
print(startDate)
year = int(startDate[0:4])
print(year)
month = int(startDate[5:7])
print(month)
day = int(startDate[8:10])
print(day)
hour = int(startDate[11:13])
print(hour)
minute = int(startDate[14:16])
print(minute)
second = int(startDate[17:19])
print(second)
timedeb=hour*3600.+minute*60.+second
print('timedeb',timedeb)
endDate = attributes['end_date']
yearend = int(endDate[0:4])
monthend = int(endDate[5:7])
dayend = int(endDate[8:10])
hourend = int(endDate[11:13])
minuteend = int(endDate[14:16])
secondend = int(endDate[17:19])
timeend=hourend*3600.+minuteend*60.*secondend
print('timeend',timeend)
if (month==monthend) and (day==dayend):
    print('meme mois meme jour')
    duration=timeend-timedeb
if (month==monthend):
    print('meme mois jour different')
    duration=dayend*86400+timeend-(day*86400+timedeb)
else:
    print('ATTENTION PAS LE MEME MOIS DONC FAIRE UN CALCUL PLUS FIN DE DUREE')
tunits0 = 'seconds since ' + str(year) + '-' + str(month) + '-' + str(day) + ' ' + str(hour) + ':' + str(minute) + ':0.0'
print('duration',duration)

seconds = minute*60 + hour*3600


# champs qui ne varie pas en fonction du temps
lat = f.variables['lat'][0]
lon =f.variables['lon'][0]
if (flag_t=='theta'):
    print('lecture theta')
    theta=f.variables['theta'][:]
    lev_theta=f.variables['lev_theta'][:]
if (flag_t=='thetal'):
    print('lecture thetal')
    thetal=f.variables['thetal'][:]
    lev_thetal=f.variables['lev_thetal'][:]
    lev_theta=lev_thetal
if (flag_t=='ta'):
    print('lecture temp')
    temp=f.variables['ta'][:]
    press=f.variables['pa'][:]
    lev_temp=f.variables['lev_ta'][:]
    lev_theta=lev_temp
    print('pa=',press)
    print('ta=',temp)
    theta=0.*temp
    kappa=2./7.
    for ik in range(0,temp.shape[0]):
        theta[ik]=temp[ik]*(100000./press[ik])**(kappa)
    print('theta=',theta)
if (flag_q=='rt'):
    print('lecture rt')
    rt=f.variables['rt'][:]
    lev_rt=f.variables['lev_rt'][:]
if (flag_q=='qt'):
    print('lecture qt')
    qt=f.variables['qt'][:]
    lev_qt=f.variables['lev_qt'][:]    
    lev_rt=lev_qt
    rt=qt/(1-qt)
    if ((keyword=='ZUVTHDMR') and (flag_q=='rt')):
        rv=rt
        lev_rv=lev_rt
    if (lev_rt.size != lev_theta.size):
        print('ERROR!! DIFFERENT VERTICAL GRIDS FOR RV & TH to be dealt separately')
if (flag_q=='rv'):
    rv=f.variables['rv'][:]
    lev_rv=f.variables['lev_rv'][:]
if (flag_q=='qv'):
    qv=f.variables['qv'][:]
    lev_qv=f.variables['lev_qv'][:]
    rv=qv/(1-qv)
    lev_rv=lev_qv
lev_u=f.variables['lev_ua'][:]
u=f.variables['ua'][:]
lev_v=f.variables['lev_va'][:]
v=f.variables['va'][:]
print('u=',u)
print('v=',v)
if (lev_v.size != lev_u.size):
    print('ERROR!! DIFFERENT VERTICAL GRIDS FOR U & V to be dealt separately')
    print('POUR BOMEX & RICO v =constant sur verticale donc calculé sur niveauxu')
    vnew=0.*u+v[0][0]
    v=vnew
    print('taille vnew',v.shape,u.shape,v[0])
ps=f.variables['ps'][:]
#if (keyword=='ZUVTHLMR'):
#    pressure_f=f.variables['pressure_f'][:]
#    ql=f.variables['ql'][:]
#    thetal=theta-(100000./pressure_f)**0.286*2.5E6/1004.25*ql
print('lecture ug vg',attributes['forc_geo'])
if attributes['forc_geo']==1:
    time_ug=f.variables['time_ug'][:]
    lev_ug=f.variables['lev_ug'][:]
    ug=f.variables['ug'][:]
    time_vg=f.variables['time_vg'][:]
    lev_vg=f.variables['lev_vg'][:]
    vg=f.variables['vg'][:]
print('lecture u_nudg',attributes['nudging_ua'])   
if attributes['nudging_ua']>0:
    unudg=f.variables['ua_nud'][:]
print('lecture v_nudg',attributes['nudging_va'] )  
if attributes['nudging_va']>0:
    vnudg=f.variables['va_nud'][:]
print('lecture th_nudg',attributes['nudging_theta'])   
if attributes['nudging_theta']>0:
    thnudg=f.variables['theta_nud'][:]
print('lecture thl_nudg',attributes['nudging_theta'])   
if attributes['nudging_thetal']>0:
    thlnudg=f.variables['thetal_nud'][:]
if attributes['nudging_ta']>0:
    thlnudg=f.variables['ta_nud'][:]
print('lecture rv_nudg', attributes['nudging_rv']  ) 
if attributes['nudging_rv']>0:
    qnudg=f.variables['rv_nud'][:]    
if attributes['nudging_rt']>0:
    qnudg=f.variables['rt_nud'][:]    
if attributes['nudging_qv']>0:
    qnudg=f.variables['qv_nud'][:]    
if attributes['nudging_qt']>0:
    qnudg=f.variables['qt_nud'][:]    
if attributes['adv_theta']==1:
    time_theta_adv=f.variables['time_tntheta_adv'][:]
    lev_theta_adv=f.variables['lev_tntheta_adv'][:]
    theta_adv=f.variables['tntheta_adv'][:]
if attributes['adv_thetal']==1:
    time_thetal_adv=f.variables['time_tnthetal_adv'][:]
    lev_thetal_adv=f.variables['lev_tnthetal_adv'][:]
    thetal_adv=f.variables['tnthetal_adv'][:]    
print('lecture radiation',attributes['radiation'])
if attributes['radiation']=="tend":
    print('le terme de rayonnement est déjà pris en compte dans l advection')
    if attributes['adv_theta']==1: 
        theta_adv=theta_adv+f.variables['tntheta_rad'][:]
    if attributes['adv_thetal']==1: 
        thetal_adv=thetal_adv+f.variables['tnthetal_rad'][:]
    if attributes['adv_ta']==1: 
        ta_adv=ta_adv+f.variables['tnta_rad'][:]
print('lecture rv_adv', attributes['adv_rv'])   
if attributes['adv_rv']==1:
    time_rv_adv=f.variables['time_tnrv_adv'][:]
    lev_rv_adv=f.variables['lev_tnrv_adv'][:]
    rv_adv=f.variables['tnrv_adv'][:]
if attributes['adv_qv']==1:
    time_rv_adv=f.variables['time_tnqv_adv'][:]
    lev_rv_adv=f.variables['lev_tnqv_adv'][:]
    rv_adv=f.variables['tnqv_adv'][:]    
    print('lecture rv_adv')
if attributes['adv_qt']==1:
    time_rt_adv=f.variables['time_tnqt_adv'][:]
    lev_rt_adv=f.variables['lev_tnqt_adv'][:]
    rt_adv=f.variables['tnqt_adv'][:]    
if attributes['adv_rt']==1:
    time_rt_adv=f.variables['time_tnrt_adv'][:]
    lev_rt_adv=f.variables['lev_tnrt_adv'][:]
    rt_adv=f.variables['tnrt_adv'][:]
    print('lecture rt_adv')    
    if (keyword=='ZUVTHDMR'):
        rv_adv=rt_adv
        lev_rv_adv=lev_rt_adv
        time_rv_adv=time_rt_adv
print('lecture forc_wa', attributes['forc_wa'])   
if attributes['forc_wa']==1:
    w=f.variables['wa'][:]
    print('lecture wa')
print('lecture forc_wap', attributes['forc_wap'])   
if attributes['forc_wap']==1:
    omega=f.variables['wap'][:]
    print('lecture omega')
print('lecture surface_forc_temp', attributes['surface_forcing_temp'])   
print('lecture surface_forc_moist', attributes['surface_forcing_moisture'])   
print('lecture surface_forc wind', attributes['surface_forcing_wind'])   
if attributes['surface_forcing_temp'] == 'ts':
    sst = f.variables['ts_forc'][:]
    timesst = f.variables['time_ts_forc'][:]
    ntsst=sst.shape[0]
    for it in range(0,ntsst):
      sst[it] = int(sst[it]*100)/100.
    print('lecture sst',ntsst)
    print(' sst shape=',sst.shape,ntsst)
elif attributes['surface_forcing_temp'] == 'surface_flux':
  hfls = f.variables['hfls'][:]
  time_hfls = f.variables['time_hfls'][:]
  hfss = f.variables['hfss'][:]
  time_hfss = f.variables['time_hfss'][:]
  print('lecture flux')
if attributes['surface_forcing_wind'] == 'ustar':
  ustar = f.variables['ustar'][:]
  print('lecture ustar')
if attributes['surface_forcing_wind'] == 'z0':
  time_z0 = f.variables['time_z0'][:]
  z0 = f.variables['z0'][:]
  #conversion de z0 a au plus 3 chiffres derriere la , ie au mm
  for it in range(0,z0.shape[0]):
      z0[it] = int(z0[it]*1000)/1000.
  print('lecture z0',z0)  
if attributes['surface_forcing_temp'] == 'none':
  #time_z0 = f.variables['time_z0'][:]
  z0h = f.variables['z0h'][:]
#if attributes['surface_forcing_moisture'] == 'none':
  #time_z0 = f.variables['time_z0'][:]
#  z0q = f.variables['z0q'][:]
f.close()


#Ecriture du fichier PRE_IDEA1.nam
g = open('PRE_IDEA1.nam_'+casname,'w')

g.write('&NAM_CONFIO  LCDF4=T, LLFIOUT=F, LLFIREAD=F /\n')
g.write('&NAM_DIMn_PRE NIMAX='+str(NI)+', NJMAX='+str(NJ)+' /\n')
g.write('&NAM_CONF_PRE LCARTESIAN=.TRUE., NVERB=10,'+'\n')
g.write(' CIDEAL="RSOU",  CZS="FLAT", LFORCING=.TRUE.,'+'\n')
g.write(' LBOUSS=.FALSE., CEQNSYS="DUR", LPERTURB=.TRUE.,'+'\n')
g.write(' JPHEXT=1,NHALO=1'+' /\n')
g.write('&NAM_PERT_PRE CPERT_KIND="WH",XAMPLIWH=0.1 /\n')
g.write('&NAM_CONFn LUSERV=.TRUE.,\n')

if (keyword=='ZUVTHLMR'):
    g.write('LUSERC=.TRUE. /\n')
else :
    g.write('/\n')
print('lat=',lat,'lon=',lon)
g.write('&NAM_GRID_PRE'+'  XLAT0 = ' + str(int(lat*100)/100.) +',  XLON0 = ' + str(int(lon*100)/100.) +' /\n')

g.write('&NAM_GRIDH_PRE XDELTAX='+str(DX)+', XDELTAY='+str(DY)+' /\n')
if (DZ <=0):
    zhat=np.genfromtxt('grille_'+casname+'.txt',dtype=None,skip_header=1,usecols=0)
#    print('z=',zhat)
    NZ=zhat.shape[0]
g.write('&NAM_VER_GRID  LTHINSHELL=.TRUE., NKMAX='+str(NZ-1)+',\n')
if (DZ <=0):
    g.write(' YZGRID_TYPE="MANUAL"/\n')
else:
    if ((casname=='GABLS1') or (casname == 'GABLS4')):
        g.write(' YZGRID_TYPE="FUNCTN",\n')
        g.write(' ZDZGRD='+str(DZmin)+', ZDZTOP='+str(DZmax)+',\n')
        g.write(' ZZMAX_STRGRD='+str(zmax)+' , ZSTRGRD=0., ZSTRTOP=20. /\n')
    else:
        g.write(' YZGRID_TYPE="FUNCTN"/\n')
        g.write(' ZDZGRD='+str(DZ)+', ZDZTOP='+str(DZ)+',\n')
        g.write(' ZZMAX_STRGRD=1000. , ZSTRGRD=0., ZSTRTOP=0. /\n')
g.write('&NAM_LUNITn CINIFILE="'+CINI+'",\n')
g.write('            CINIFILEPGD="'+CINI+'_PGD" /\n')
g.write('&NAM_LBCn_PRE CLBCX=2*"CYCL", CLBCY=2*"CYCL" /\n') 
g.write('&NAM_CONFZ MPI_BUFFER_SIZE=400/\n')
g.write('&NAM_GRn_PRE CSURF="EXTE"/\n')

daymonth=[31,28,31,30,31,30,31,31,30,31,30,31]
day0=int(day)
month0=int(month)
print('day0=',day0,'month0=',month0,daymonth[month0-1])
timeref=timedeb
g.write('&NAM_PGD_SCHEMES\n')
#Cas OCEAN & TS ex RICO
if attributes['surface_type'] == 'ocean' and attributes['surface_forcing_temp'] == 'ts':
  g.write('  CSEA    = "SEAFLX"/\n')
  g.write('&NAM_COVER XUNIF_COVER(1)=1. /\n')
  g.write('&NAM_SEABATHY XUNIF_SEABATHY=5. /\n')
  g.write('&NAM_PREP_SEAFLUX XSST_UNIF = '+str(float(sst[0]))+' /\n')
  g.write('&NAM_DATA_SEAFLUX  LSST_DATA=T, \n')
  g.write('NTIME_SST= '+str(ntsst)+' ,\n')
  for it in range(0,ntsst):
    if (it >=1):
        timeref=timeref+int(timesst[it])-int(timesst[it-1])
    if (timeref >= 86400):
        timeref=timeref-86400
        day0=day0+1
        if (day0 >daymonth[month0-1]):
            day0=day0-daymonth[month0-1]
            month0=month0+1    
    g.write('XUNIF_SST('+str(it+1)+')='+str(float(sst[it])) +',\n')
    g.write('NYEAR_SST('+str(it+1)+')='+str(int(year)) +',\n')
    g.write('NMONTH_SST('+str(it+1)+')='+str(month0) +',\n')
    g.write('NDAY_SST('+str(it+1)+')='+str(day0) +',\n')
    g.write('XTIME_SST('+str(it+1)+')='+str(timeref) +',\n')
  g.write('/\n')
#CAS OCEAN & FLUX PRESCRIT ex BOMEX
if attributes['surface_type'] == 'ocean' and attributes['surface_forcing_temp'] == 'surface_flux':
  g.write('  CSEA    = "FLUX" /\n')
  g.write('&NAM_COVER XUNIF_COVER(1)=1. /\n')
#CAS CONTINENTAL & FLUX PRESCRIT ex ARM 
if attributes['surface_type'] == 'land' and attributes['surface_forcing_temp'] == 'surface_flux':
  g.write('  CSEA    = "FLUX"  /\n')
  g.write('&NAM_COVER XUNIF_COVER(1)=1. /\n')
#CAS CONTINENTAL & ZO ex GABLS1 
if (attributes['surface_type'] == 'land' or attributes['surface_type']=='landice') and attributes['surface_forcing_temp'] == 'ts':
  g.write('   CNATURE = "TSZ0"  /\n')
  g.write('&NAM_COVER XUNIF_COVER(6)=1. /\n')
  g.write('&NAM_FRAC LECOCLIMAP = T,XUNIF_NATURE = 1. /\n')
  g.write('&NAM_DATA_TSZ0  NTIME='+str(ntsst)+' \n')
  for it in range(0,ntsst-1):
      g.write('XUNIF_DTS('+str(it+1)+')='+str(float(sst[it+1]-sst[it])) +',\n')
      print('dt=',str(float(sst[it+1]-sst[it])))
  g.write('XUNIF_DTS('+str(ntsst)+')=0. /\n')
  g.write('&NAM_DATA_ISBA NTIME=1 ,XUNIF_Z0(1,1)='+str(z0[0])+',\n')
  g.write('XUNIF_Z0(1,2)='+str(z0[0])+',XUNIF_Z0(1,3)='+str(z0[0])+',\n')
  g.write('XUNIF_Z0(1,4)='+str(z0[0])+',XUNIF_Z0(1,5)='+str(z0[0])+',\n')
  g.write('XUNIF_Z0(1,6)='+str(z0[0])+',XUNIF_Z0(1,7)='+str(z0[0])+',\n')
  g.write('XUNIF_Z0(1,8)='+str(z0[0])+',XUNIF_Z0(1,9)='+str(z0[0])+',\n')
  g.write('XUNIF_Z0(1,10)='+str(z0[0])+',XUNIF_Z0(1,11)='+str(z0[0])+',\n')
  g.write('XUNIF_Z0(1,12)='+str(z0[0])+' /\n')
  g.write('&NAM_ISBA XUNIF_CLAY=1.,XUNIF_SAND=0.,XUNIF_RUNOFFB=0.5,\n')
  g.write('CISBA="2-L", CPHOTO="NON", NPATCH=1,NGROUND_LAYER=2 /\n')
  g.write('&NAM_PREP_SURF_ATM NYEAR='+str(year)+',NMONTH='+str(month0)+',\n')
  g.write('NDAY='+str(day0)+',XTIME='+str(timeref)+' /\n')
  g.write('&NAM_PREP_ISBA XHUG_SURF=0.,XHUG_ROOT = 0.,XHUG_DEEP = 0.,\n')
  g.write('XHUGI_SURF=1.,XHUGI_ROOT = 1.,XHUGI_DEEP = 1.,\n')
  g.write('XTG_SURF='+str(sst[0])+',XTG_ROOT='+str(sst[0])+', \n')
  g.write('XTG_DEEP='+str(sst[0])+',LISBA_CANOPY=.FALSE., \n')
  g.write('NYEAR='+str(year)+',NMONTH='+str(month0)+',NDAY='+str(day0)+', \n')
  g.write('XTIME='+str(timeref)+' /\n')
  g.write('&NAM_DEEPSOIL LPHYSDOMC = F,LDEEPSOIL = F /\n')



#if (str(attributes['zorog']) > 0.):
#    print('Attention on ne prend pas en compte l orographie dans simu MNH LES/1D car pbm sinon')
    #g.write('&NAM_ZS\n')
    #g.write('  XUNIF_ZS = ' + str(attributes['zorog']) + '/\n')

#cas ou grille verticale prescrite
#CHANGER POUR METTRE UNE CLE POUR GRILLE MANUELLE CAR PEUT ETRE INDEPENDANT DU CAS
if (DZ <=0):
    g.write(' ZHAT \n')
    for ik in range(0,NZ):
        g.write(str(zhat[ik])+'\n')
#ecriture du fichier initial
g.write('RSOU\n')
g.write(str(int(year)) + " "+ str(int(month)) + " " + str(int(day)) + " "+ str(int(timedeb))+'\n')
g.write(keyword+'\n')
#g.write('%.2f' % attributes['zorog']+'\n')
g.write('%.2f' % zorog +'\n')
g.write('%.2f' % ps[0]+'\n')
if (keyword=='ZUVTHDMR'):
    g.write('%.2f' % theta[0][0]+'\n')
elif (keyword=='ZUVTHLMR'):
    g.write('%.2f' % thetal[0][0]+'\n')
if (keyword=='ZUVTHDMR'):
    if ((flag_q=='rv') or (flag_q=='qv')):
        g.write('%.8f' % rv[0][0]+'\n')
    if ((flag_q=='rt') or (flag_q=='qt')):
        g.write('%.8f' % rt[0][0]+'\n')
elif (keyword=='ZUVTHLMR'):
    g.write('%.8f' % rt[0][0]+'\n')
nknew=lev_u.shape[0]
g.write('%d' % nknew+'\n')
for ik in range(0,nknew):
    g.write('%.1f %.2f %.2f' % (float(lev_u[ik]),float(u[0][ik]),float(v[0][ik]))+'\n')
  
nknew=lev_theta.shape[0]
g.write('%d' % nknew+'\n')
for ik in range(1,nknew):
    if (keyword=='ZUVTHDMR'):
        if ((flag_q=='rv') or (flag_q=='qv')):
            g.write('%.1f %.2f %.8f' % (lev_theta[ik],theta[0][ik],rv[0][ik])+'\n')
        elif ((flag_q=='rt') or (flag_q=='qt')):    
            g.write('%.1f %.2f %.8f' % (lev_theta[ik],theta[0][ik],rt[0][ik])+'\n')
    elif (flag_t=='thetal'):
        g.write('%.1f %.2f %.8f' % (lev_theta[ik],thetal[0][ik],rt[0][ik])+'\n')


#ecriture des forcages
g.write('ZFRC \n')
if attributes['adv_theta']==1:
    nt=time_theta_adv.shape[0]
    print('nt=',nt)
    g.write('%d' %  nt+'\n')
    nknew=lev_theta_adv.shape[0]
elif attributes['adv_thetal']==1:
    nt=time_thetal_adv.shape[0]
    print('nt=',nt)
    g.write('%d' %  nt+'\n')
    nknew=lev_thetal_adv.shape[0]
elif attributes['forc_geo']==1:
    nt=time_ug.shape[0]
    print('nt=',nt)
    g.write('%d' %  nt+'\n')
    nknew=lev_ug.shape[0]
# attention ne va marcher que pour 1 seul jour sinon besoin d'updater le fichier pour tenir compte de la date variable
# Attention pour l'instant pas de traitement particulier pour le trad
day0=int(day)
month0=int(month)
timeref=timedeb
utemp=np.zeros((nt,nknew))
vtemp=np.zeros((nt,nknew))
tadvtemp=np.zeros((nt,nknew))
qadvtemp=np.zeros((nt,nknew))
print('nknew=',nknew)
if (attributes['adv_theta']==1):
    tadvtemp=theta_adv
elif (attributes['adv_thetal']==1):
    tadvtemp=thetal_adv

else:
    tadvtemp=np.zeros((nt,nknew))
if ((attributes['adv_rv']==1) or (attributes['adv_qv']==1)):
    qadvtemp=rv_adv
elif ((attributes['adv_rt']==1) or (attributes['adv_qt']==1)):
    qadvtemp=rt_adv
else:
    qadvtemp=np.zeros((nt,nknew))    
if (attributes['forc_wa']==1):
    wtemp=w
    print('w_forc=',w)
elif (attributes['forc_wap']==1):
    wtemp=omega/(-1.*288*temp*9.81)
else:
    wtemp=tadvtemp*0.
if (attributes['forc_geo']==1):
    utemp=ug
    vtemp=vg
elif (attributes['nudging_ua']>0):
    utemp=unudg
    vtemp=vnudg
else:
    utemp=np.zeros((nt,nknew))
    vtemp=np.zeros((nt,nknew))
if (attributes['nudging_theta']>0):
    thtemp=thnudg
    qtemp=rvnudg
else:
    thtemp=np.zeros((nt,nknew))
    qtemp=np.zeros((nt,nknew))
for it in range(0,nt):
    if (it >=1):
        if (attributes['adv_theta']==1):
            timeref=timeref+time_theta_adv[it]-time_theta_adv[it-1]
        elif (attributes['adv_thetal']==1):
            timeref=timeref+time_thetal_adv[it]-time_thetal_adv[it-1]
        elif (attributes['forc_geo']==1):
            timeref=timeref+time_ug[it]-time_ug[it-1]
    if (timeref >= 86400):
        timeref=timeref-86400
        day0=day0+1
        if (day0 >daymonth[month0-1]):
            day0=day0-daymonth[month0-1]
            month0=month0+1
    print('timeref=',timeref,'day0=',day0,'month0=',month0)
    g.write(str(int(year)) + " "+ str(month0) + " " + str(day0) + " "+ str(timeref)+'\n')
    g.write('%.2f' % zorog+'\n')
    g.write('%.2f' % ps+'\n')
    if ((flag_t=='theta') or (flag_t=='ta')):
        g.write('%.2f' % theta[0][0]+'\n')
        if ((flag_q=='rv') or (flag_q=='qv')):
            g.write('%.8f' % rv[0][0]+'\n')
        if ((flag_q=='rt') or (flag_q=='qt')):
            g.write('%.8f' % rt[0][0]+'\n')
    if (flag_t=='thetal'):
        g.write('%.2f' % thetal[0][0]+'\n')
        g.write('%.8f' % rt[0][0]+'\n')
    g.write('%d' % nknew+'\n')
    for ik in range(0,nknew):
        if ((attributes['adv_theta']==1) and (attributes['forc_geo']==1)):
            if (ik==0):
                print('size temp',utemp.shape,tadvtemp.shape)
            if (time_ug.shape[0]==time_theta_adv.shape[0]):
                g.write('%.1f %.2f %.2f %.1f %.5f %.5f %.6f %.10f %.1f %.1f' % (lev_theta_adv[ik],utemp[it][ik],vtemp[it][ik],thtemp[it][ik],qtemp[it][ik], wtemp[it][ik],tadvtemp[it][ik],qadvtemp[it][ik],0.,0.)+'\n')
            else:
                g.write('%.1f %.2f %.2f %.1f %.5f %.5f %.6f %.10f %.1f %.1f' % (lev_theta_adv[ik],utemp[0][ik],vtemp[0][ik],thtemp[it][ik],qtemp[it][ik], wtemp[it][ik],tadvtemp[it][ik],qadvtemp[it][ik],0.,0.)+'\n')
        elif ((attributes['adv_thetal']==1) and (attributes['forc_geo']==1)):
            if (ik==0):
                print('size temp',utemp.shape,tadvtemp.shape)
            if (time_ug.shape[0]==time_thetal_adv.shape[0]):
                g.write('%.1f %.2f %.2f %.1f %.5f %.5f %.6f %.10f %.1f %.1f' % (lev_thetal_adv[ik],utemp[it][ik],vtemp[it][ik],thtemp[it][ik],qtemp[it][ik], wtemp[it][ik],tadvtemp[it][ik],qadvtemp[it][ik],0.,0.)+'\n')
            else:
                g.write('%.1f %.2f %.2f %.1f %.5f %.5f %.6f %.10f %.1f %.1f' % (lev_thetal_adv[ik],utemp[0][ik],vtemp[0][ik],thtemp[it][ik],qtemp[it][ik], wtemp[it][ik],tadvtemp[it][ik],qadvtemp[it][ik],0.,0.)+'\n')

        elif (attributes['forc_geo']==1):
            if (ik==0):
                print('size temp',utemp.shape,tadvtemp.shape)
            g.write('%.1f %.2f %.2f %.1f %.5f %.5f %.6f %.10f %.1f %.1f' % (lev_ug[ik],utemp[it][ik],vtemp[it][ik],thtemp[it][ik],qtemp[it][ik], wtemp[it][ik],tadvtemp[it][ik],qadvtemp[it][ik],0.,0.)+'\n')
        elif (attributes['adv_theta']==1):
            g.write('%.1f %.2f %.2f %.1f %.5f %.5f %.6f %.10f %.1f %.1f' % (lev_theta_adv[ik],utemp[it][ik],vtemp[it][ik],thtemp[it][ik],qtemp[it][ik], wtemp[it][ik],tadvtemp[it][ik],qadvtemp[it][ik],0.,0.)+'\n')
        elif (attributes['adv_thetal']==1):
            g.write('%.1f %.2f %.2f %.1f %.5f %.5f %.6f %.10f %.1f %.1f' % (lev_theta_adv[ik],utemp[it][ik],vtemp[it][ik],thtemp[it][ik],qtemp[it][ik], wtemp[it][ik],tadvtemp[it][ik],qadvtemp[it][ik],0.,0.)+'\n')

g.close()
g = open('EXSEG1.nam_'+casname,'w')
g.write('&NAM_CONFIO  LCDF4=T, LLFIOUT=F, LLFIREAD=F /\n')
g.write('&NAM_LUNITn CINIFILE="'+CINI+'",\n')
g.write('		 CINIFILEPGD="'+CINI+'_PGD" /\n')
g.write('&NAM_CONFn LUSERV=T/\n')
g.write('&NAM_DYNn XTSTEP=1.,CPRESOPT="ZRESI" /\n')
# besoin de modifier pour avoir la bonne durée
g.write('&NAM_DYN XSEGLEN='+str(duration)+',\n'	 	)
if attributes['forc_geo']==1:
    g.write(' LCORIO=T,\n'	 	)
else:
    g.write(' LCORIO=F,\n'	 	)
g.write('             LNUMDIFU=F,\n'   	)
g.write('             LNUMDIFTH = F,\n')
# ca aussi ca doit pouvoir etre paramétré => pas vraiment l'info dans les cas 1D
g.write('         XALKTOP='+str(ktop)+', XALZBOT='+str(zbot)+' /\n')
g.write('&NAM_ADVn  CUVW_ADV_SCHEME = "CEN4TH",CTEMP_SCHEME="RKC4",\n')
g.write('           CMET_ADV_SCHEME = "PPM_01", CSV_ADV_SCHEME = "PPM_01",/\n')
print('RAD',attributes['radiation'])
if ((attributes['radiation']=='off') or (attributes['radiation']=='tend')):
    g.write('&NAM_PARAMn  CTURB="TKEL", CRAD="NONE",\n')
elif (attributes['radiation']=='on'):
    g.write('&NAM_PARAMn  CTURB="TKEL", CRAD="ECMW",\n')
g.write('CCLOUD="'+CLOUDSC+'", CSCONV="NONE",CDCONV="NONE"  /\n')
g.write('&NAM_LBCn    CLBCX = 2*"CYCL", CLBCY = 2*"CYCL"/\n'		)
if (attributes['radiation']=='on'):
    g.write('&NAM_PARAM_RADn XDTRAD = 150.,XDTRAD_CLONLY = 150.,\n') 
    g.write('CLW="MORC",CAER="SURF",CEFRADL="C2R2",COPWLW="SMSH",\n')
    g.write('COPWSW="FOUQ",LCLEAR_SKY = F,NRAD_COLNBR=1000,XFUDG=1. /\n')
g.write('&NAM_TURBn XIMPL=0., CTURBLEN="DEAR", CTURBDIM="3DIM",\n')
g.write('           LTURB_FLX=T, LTURB_DIAG=T, LSUBG_COND=F,\n')
g.write('           XKEMIN=1E-10,\n')
g.write('           LSIGMAS=F, LSIG_CONV=F, LRMC01=T /\n')
if (CLOUDSC=='KHKO'):
    g.write('&NAM_PARAM_C2R2 HPARAM_CCN="CPB", HINI_CCN="CCN",\n')
    g.write('XCHEN=0.173E+09, XKHEN=1.403, XMUHEN=0.834,\n')		    
    g.write('XBETAHEN=25.499, LRAIN= F,LSEDC= F/\n')
g.write('&NAM_CONF CCONF="START", CEQNSYS ="DUR", LFLAT=T,\n')
g.write('          NMODEL=1, NVERB=6, CEXP="'+NAM1+'", CSEG="'+NAM2+'", LFORCING=T/\n')
#sorties 3D toutes les h
g.write('&NAM_BACKUP XBAK_TIME_FREQ(1) = 3600.0/\n')
g.write('&NAM_CONFZ MPI_BUFFER_SIZE=400/\n')
# a modifier pour tenir compte des clés
g.write('&NAM_FRC \n')
if attributes['forc_geo']==1:
    g.write('         LGEOST_UV_FRC=.TRUE.,\n')
else:
    g.write('         LGEOST_UV_FRC=.FALSE.,\n')
if (attributes['adv_theta']==1) or (attributes['adv_rv']==1):
    g.write('         LTEND_THRV_FRC=.TRUE.,\n')
else:
    g.write('         LTEND_THRV_FRC=.FALSE.,\n')
if (attributes['forc_wa']==1) or (attributes['forc_wap']==1):
    g.write('         LVERT_MOTION_FRC=.TRUE.,\n')
else:
    g.write('         LVERT_MOTION_FRC=.FALSE.,\n')    
g.write('         LGEOST_TH_FRC=.FALSE.,\n')
if (attributes['nudging_ua']>0) or (attributes['nudging_va']>0):
    g.write('         LRELAX_UV_FRC=.TRUE.,XRELAX_TIME_FRC='+str(attributes["nudging_ua"])+',\n')
else:
    g.write('         LRELAX_UV_FRC=.FALSE.,\n')
if (attributes['nudging_theta']>0) or (attributes['nudging_rv']>0):
    g.write('         LRELAX_THRV_FRC=.TRUE.,XRELAX_TIME_FRC='+str(attributes["nudging_theta"])+',\n')
else:
    g.write('         LRELAX_THRV_FRC=.FALSE.,\n'    )
g.write('/\n')
g.write('&NAM_LES LLES_MEAN=.TRUE., LLES_SUBGRID=.TRUE.,LLES_RESOLVED=.FALSE.,\n')
g.write('         XLES_TEMP_SAMPLING=300.,XLES_TEMP_MEAN_START=1.,\n' )
g.write('         XLES_TEMP_MEAN_END='+str(duration)+', XLES_TEMP_MEAN_STEP=3600.  /\n')

if attributes['surface_forcing_temp'] == 'surface_flux':
    g.write('&NAM_IDEAL_FLUX\n' )
    nt=time_hfss.shape[0]
    g.write('NFORCF = '+str(nt)+',\n')
    for it in range(0,nt):
        g.write('XTIMEF('+str(it+1)+') = '+str(time_hfss[it])+',\n')
    for it in range(0,nt):
        g.write('XSFTH('+str(it+1)+') = '+str(float(hfss[it]))+',\n')
    g.write('CSFTQ="W/m2",\n')
    for it in range(0,nt):
        g.write('XSFTQ('+str(it+1)+') = '+str(float(hfls[it]))+',\n')
    for it in range(0,nt):
        g.write('XSFCO2('+str(it+1)+')=0.\n')
    if attributes['surface_forcing_wind'] == 'z0': 
        g.write('CUSTARTYPE = "Z0   ",\n')
        g.write('XZ0='+str(int(z0[0]*1000)/1000.)+',\n')
    if attributes['surface_forcing_wind'] == 'ustar':
        g.write('CUSTARTYPE = "USTAR",\n')
        for it in range(0,nt):
            g.write('USTAR('+str(it+1)+')= '+str(float(ustar[it]))+',\n')  
   # g.write('XALB   = 0.,\n')
   # g.write('XEMIS  = 1.,\n')
   # for it in range(0,nt):
   #     g.write('XTSRAD('+str(it+1)+') = '+str(float(ts[it]))+',\n')
    g.write('/\n')
g.close()  
