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

from Utils import parse_time, log
from Utils import T_to_theta, P_to_z, interp_plev_to_zlev, interp_zlev_to_plev, H_atm, p0
from Utils import bilin_interp
from Utils import ERROR, WARNING, INFO, DEBUG
from cases_output import CasesOutputs
from datetime import timedelta, datetime
import numpy as np
import os

# missing cases in prep_nam_from_FFCOM_DEF_to_MNHLES
# KB2006, DYNAMO, MAGIC, FIRE, ISDAC, MPACE

# To add a case to the database, add its name to one of the lists:
listCaseMoistShCv = ["ARMCU", "BOMEX", "SANDU", "RICO", "SCMS", "FIRE"] # moist shallow conv
listCaseDCv       = ["LBA", "AMMA", "KB2006", "EUROCS"]         # deep conv
listCaseStable    = ["GABLS1", "GABLS4"]                        # stable
listCaseDryShCv   = ["AYOTTE", "IHOP", "BLLAST", "MOSAI"]       # dry shallow conv

# this is the list of all defined cases
listCases = listCaseMoistShCv+listCaseDryShCv+listCaseStable+listCaseDCv

# should this be defined in mesonh rather than here?
cas_type = {"moistshcv" : {"dx" : 25,  "dy" : 25,  "dz" : 25,   "zbot": 3500}, 
            "dcv"       : {"dx" : 200, "dy" : 200, "dz" : None, "zbot": 16500},
            "stable"    : {"dx" : 5,   "dy" : 5,   "dz" : None, "zbot": None},
            "dryshcv"   : {"dx" : 25,  "dy" : 25,  "dz" : 25,   "zbot": 3000},
            }

def FC_filename(dir, cas, subcas):
  fil="/".join([dir, cas, subcas, cas+"_"+subcas+"_DEF_driver.nc"])
  return fil

class Case:
  def __init__(self, casename, subcasename):
    """ Initialize case specific config
    .casename 
    .subcasename
    .name
    .shortname
    .type ["moistshcv", "dryshcv", "stable", "dcv"]
    .name_var_t, name_var_q, name_var_u, name_var_v, name_var_w = {}
    """
    self.casename    = casename
    self.subcasename = subcasename
    self.name = casename+"_"+subcasename
    # si sous cas n'est pas REF (sauf SANDU) ou MESONH, combine cas / sous-cas
    if subcasename != "MESONH" and subcasename != "REF" or casename == "SANDU":
      # si le sous cas est long ; 3 premières lettres du cas + 2 premières du sous cas
      if len(subcasename)>5: short = casename[:3]+subcasename[:2]
      # sinon ; x premières lettres du cas + sous cas tel que = 5 caractères au total
      else:                  short = casename[:5-len(subcasename)]+subcasename
    # si sous cas = REF ou MESONH, shortname = nom du cas
    else: 
      short   = casename[:5]
    
    # traitement spécifique pour GABLS
    if "GABLS" in casename :   # replace S by number of GABLSx case
      short = "GABL%s"%casename[-1]
 
    # ajout de caractères si < 5 caractères
    if len(short)<5:           # pad case shortname with _ at the end
        short += "_"*(5-len(short))

    self.shortname = short

    if   casename in listCaseMoistShCv : self.type = "moistshcv"
    elif casename in listCaseDCv       : self.type = "dcv" 
    elif casename in listCaseStable    : self.type = "stable"
    elif casename in listCaseDryShCv   : self.type = "dryshcv"

    # name of variable use for initialisation, advection and nudging
    # e.g. for temperature : name_var_t["ini"] = "ta" name_var_t["adv"] = "theta"
    self.name_var_t={} # temperature: ta | theta | thetal
    self.name_var_q={} # humidity: rv | qv | rt | qt
    self.name_var_u={} # zonal wind : ug | ua_nud
    self.name_var_v={} # meridional wind : vg | va_nud
    self.name_var_w={} # vertical wind : wa|wap|none

    # default grid as a function of case type 
    self.dx = cas_type[self.type]["dx"]
    self.dy = cas_type[self.type]["dy"]
    self.dz = cas_type[self.type]["dz"]
    self.zbot = cas_type[self.type]["zbot"]

    # grilles stretchées pour GABLS
    # non en fait, grilles fixes à 5m et 1m respectivement
    if casename == "GABLS1": 
        self.dz = 5
        #self.dzmin = 2    ; self.dzmax = 6
        #self.zzmax = 250. ; self.zztop = 20
        self.zbot = 350.
    elif casename == "GABLS4":
        self.dz = 1
        #self.dzmin = 0.2  ; self.dzmax = 2
        #self.zzmax = 50.  ; self.zztop = 20
        self.zbot = 80.

    self.dzmin = self.dz ; self.dzmax = self.dz 
    self.zzmax = 1000    ; self.zztop = 0

  def setup_outputs(self, verbosity, max_seg_dur=24):
    ## max_seg_dur :: maximum duration of one segment
    caseoutput = CasesOutputs[self.casename]
    self.hourhf = caseoutput["hourhf"]
    begin_seg = [0, caseoutput["spinup"]]
    for h in self.hourhf:
      if h == begin_seg[-1]: begin_seg += [h+1]
      elif h > begin_seg[-1] and h+1 <= self.duration_hour:
        begin_seg += [h, h+1]
    if begin_seg[-1] == self.duration_hour: begin_seg = begin_seg[:-1]
    more_segs = []
    for i,h in enumerate(begin_seg):
      if h < caseoutput["spinup"] : continue
      if i+1 < len(begin_seg): # not the anti last 
        hnext = begin_seg[i+1]
      else:
        hnext = self.duration_hour
      if hnext-h > max_seg_dur:
        log(WARNING, "seg %i (%i-%i) lasts more than max seg dur (%i)"%(i, h,
            hnext, max_seg_dur), verbosity)
        more_segs += [i for i in np.arange(h+max_seg_dur,hnext,max_seg_dur)]
        log(WARNING, "will be cut into more segs", verbosity)
    begin_seg = np.unique(begin_seg+more_segs)
    begin_seg.sort()
    self.nseg = len(begin_seg)
    self.begin_seg = begin_seg

  def set_init_and_forcing_types(self, attributes):
    """ set the type of variables that define initial profiles, advection and
    nudging from global attributes read in common format file """

    for key in "ini","adv","nudging":
      self.name_var_t[key] = "none"
      if attributes[key+"_ta"] > 0      : self.name_var_t[key] = "ta"
      if attributes[key+"_thetal"] > 0  : self.name_var_t[key] = "thetal"
      if attributes[key+"_theta"] > 0   : self.name_var_t[key] = "theta"

      self.name_var_q[key] = "none"
      if attributes[key+"_qv"] > 0      : self.name_var_q[key] = "qv"
      if attributes[key+"_qt"] > 0      : self.name_var_q[key] = "qt"
      if attributes[key+"_rt"] > 0      : self.name_var_q[key] = "rt"
      if attributes[key+"_rv"] > 0      : self.name_var_q[key] = "rv"

    # ini wind is always ua,va ; no wind advection ; 
    # forcing can be either geostrophic or nudging or none
    self.name_var_u["ini"]   = "ua"
    self.name_var_v["ini"]   = "va"
    if attributes["forc_geo"]: 
      self.name_var_u["frc"] = "ug"
      self.name_var_v["frc"] = "vg"
    elif attributes["nudging_ua"]:
      if attributes["nudging_va"]==0: 
          print("error: nudging u but not v ?"); exit()
      self.name_var_u["frc"] = "ua_nud"
      self.name_var_v["frc"] = "va_nud"
      self.xrelax_time_frc = attributes["nudging_ua"]
    else:
      self.name_var_u["frc"] = "none"
      self.name_var_v["frc"] = "none"

    # oceants or oceanflux or landflux or landz0
    self.surface_forcing = attributes["surface_type"]+attributes["surface_forcing_temp"]

  def set_lonlat(self, ds):
    self.lat = ds.variables["lat"][0]
    self.lon = ds.variables["lon"][0]

  def set_times(self, attributes):
    y,m,d,hh,mm,ss = parse_time(attributes["start_date"])
    self.start_date = datetime(y,m,d,hh,mm,ss)
    self.start_seconds = 3600*hh+60*mm+ss
    y,m,d,hh,mm,ss = parse_time(attributes["end_date"])
    self.end_date = datetime(y,m,d,hh,mm,ss)
    self.duration = self.end_date-self.start_date
    self.duration_secs = self.duration.total_seconds()
    self.duration_hour = self.duration_secs/3600.

  def set_vertical_grid(self, inp_grid_file, read_zorog=0):
    if self.type == "dcv" or (inp_grid_file is not None):
      if os.path.isfile(inp_grid_file):
        self.zgrid = np.genfromtxt(inp_grid_file, dtype=None,skip_header=0,usecols=0)
        self.nz    = len(self.zgrid)
      else: print("error: vertical grid file %s not found for case %s/%s"%(
          inp_grid_file, self.casename, self.subcasename)); exit()
      if read_zorog: self.zgrid += self.zs
    else: self.zgrid = None

  def read_initial_profiles_and_forcings(self, attributes, ds, verbosity, read_zorog=0):
    # local helper function to read 2D, z-dependent and t-dependent variables
    def getzvar(name_var): 
      lev = "lev_"+name_var                 # specific vertical grid 
      if not lev in ds.variables: lev="lev" # or standard vert grid
      return ds.variables[name_var][:], ds.variables[lev][:]
    def gettvar(name_var): 
      tim = "time_"+name_var                 # specific time grid
      if not tim in ds.variables: tim="time" # or standard
      return ds.variables[name_var][:], ds.variables[tim][:]
    def get2dvar(name_var):
      lev = "lev_"+name_var
      if not lev in ds.variables: lev="lev"
      tim = "time_"+name_var 
      if not tim in ds.variables: tim="time"
      return ds.variables[name_var][:], ds.variables[lev][:], ds.variables[tim][:]

    ## champs initiaux T,q,ps,u,v,zorog
    name_var_t = self.name_var_t["ini"]
    name_var_q = self.name_var_q["ini"]
    name_var_u = self.name_var_u["ini"]
    name_var_v = self.name_var_v["ini"]
    
    ### initialize with theta_dry or theta_liquid
    mnh_init_keyword = "ZUVTHLMR" if name_var_t == "thetal" else "ZUVTHDMR"
    
    ### init: read profiles 
    ps    = ds.variables['ps'][:]      # surface pressure
    zs    = ds.variables['orog'][:] if read_zorog else ps*0
    if len(zs)>1: zs=zs[0]
    var_u, lev_u = getzvar(name_var_u)           # ua: zonal wind profile
    var_v, lev_v = getzvar(name_var_v)           # va: merid wind profile
    var_t, lev_t = getzvar(name_var_t)           # theta, thetal or ta
    var_q, lev_q = getzvar(name_var_q)           # rv, rt or qv

    if (lev_v.size != lev_u.size):
      log(WARNING, 'ERROR!! DIFFERENT VERTICAL GRIDS FOR U & V to be dealt \
              separately', verbosity)
      var_v = 0.*var_u+var_v[0][0]

    if len(lev_t) != len(lev_q):
      log(ERROR, "T and q not initialized on the same grid", verbosity)
    
    nlev_init_uv = len(lev_u)
    nlev_init_tq = len(lev_t)
    #print(nlev_init_uv, var_u.shape, nlev_init_tq, var_t.shape)
    
    log(DEBUG, "Original lev_t (%s) %s"%(self.def_lev, lev_t), verbosity)
    log(DEBUG, "Original lev_u (%s) %s"%(self.def_lev, lev_u), verbosity)
    log(DEBUG, "Original var_t (%s) %s"%(name_var_t, var_t), verbosity)
    log(DEBUG, "Original var_q (%s) %s"%(name_var_q, var_q), verbosity)

    ### init: convert Pressure to height if needed

    if self.def_lev == "P" :
      lev_pre_t = lev_t[:]*1
      lev_pre_u = lev_u[:]*1
      lev_t = P_to_z(name_var_t, var_t, name_var_q, var_q, lev_pre_t, zs)
      if np.max(lev_pre_u) <= np.max(lev_pre_t) and np.min(lev_pre_u) >= np.min(lev_pre_t):
        # interpolate from thermo profiles
        lev_u = np.array([interp_plev_to_zlev(lev_t, lev_pre_t, p) for p in lev_pre_u])
      else:
        # use basic exponential function and atmospheric length scale
        lev_u = -H_atm*np.log(-lev_pre_u/p0)

      #import matplotlib.pyplot as plt
      #plt.plot(lev_pre_t, lev_t, ls="", marker="o")
      #plt.plot(lev_pre_u, lev_u, ls="", marker=".")
      #plt.show()
      #exit()
    
    ### init: convert T to theta and q to mixing ratio if needed
    
    if name_var_t == "ta" :
      # convert T to theta
      if self.def_lev == "P":
        press=ds.variables["lev"][:]
      else:
        press = ds.variables["pa"][:]
      var_t = T_to_theta(var_t, press)
    
    if "q" in name_var_q: 
      # convert content to mixing ratio
      var_q = var_q/(1-var_q)
    
    log(DEBUG, "Converted lev_t (%s) %s"%("z", lev_t), verbosity)
    log(DEBUG, "Converted lev_u (%s) %s"%("z", lev_u), verbosity)
    log(DEBUG, "Converted var_t (%s) %s"%(name_var_t, var_t), verbosity)
    log(DEBUG, "Converted var_q (%s) %s"%(name_var_q, var_q), verbosity)
    
    ### get forcings
    
    # horizontal winds : frc = geostrophic or nudging ; tendencies = 0
    ## zonal
    name_var_u = self.name_var_u["frc"]
    name_var_v = self.name_var_v["frc"]
    if name_var_u != "none": 
      var_u_frc, lev_u_frc, tim_u_frc = get2dvar(name_var_u)
      var_v_frc, lev_v_frc, tim_v_frc = get2dvar(name_var_v)
    else:
      var_u_frc, lev_u_frc, tim_u_frc = None, None, None
      var_v_frc, lev_v_frc, tim_v_frc = None, None, None
    
    # thermodynamics : frc = nudging | none ; tend = adv | rad | none
    ## T
    if self.name_var_t["nudging"] != "none":
      self.xrelax_time_frc = attributes["nudging_"+self.name_var_t["nudging"]]
      name_var_t = self.name_var_t["nudging"]+"_nud"
      var_t_frc, lev_t_frc, tim_t_frc = get2dvar(name_var_t)
    else:
      var_t_frc, lev_t_frc, tim_t_frc = None, None, None
    if name_var_t == "ta_nud" :
      log(ERROR, "Don't know how to convert T nudge to theta", verbosity)
    ## q
    if self.name_var_q["nudging"] != "none":
      name_var_q = self.name_var_q["nudging"]+"_nud"
      var_q_frc, lev_q_frc, tim_q_frc = get2dvar(name_var_q)
      if "q" in name_var_q: 
        # convert content to mixing ratio
        var_q_frc = var_q_frc/(1-var_q_frc)
    else:
      var_q_frc, lev_q_frc, tim_q_frc = None, None, None
    
    # thermodynamics : advection 
    ## T
    if self.name_var_t["adv"] != "none":
      name_var_t = "tn"+self.name_var_t["adv"]+"_adv"
      var_t_ten, lev_t_ten, tim_t_ten = get2dvar(name_var_t)
    else:
      var_t_ten, lev_t_ten, tim_t_ten = None, None, None
    if name_var_t == "tnta_adv":
      # log(ERROR, "Don't know how to convert T adv to theta", verbosity)
      ##### ONGOING WORK !!!!! #####
      # need pressure to convert T to theta
      # on the same grid as t adv forcings
      if self.def_lev == "P": 
        # already defined on pressure levels, conversion is immediate
        var_t_ten = T_to_theta(var_t_ten, lev_t_ten)
      else:
        # defined on altitude levels, use initial pressure profile
        # to convert ; first need to interpolate press_ini on lev_t_ten
        altit_ini = lev_t
        press_ini = ds.variables["pa"][:].squeeze()
        pre_t_ten = np.array([interp_zlev_to_plev(press_ini, altit_ini, l) for l in lev_t_ten])
        var_t_ten = T_to_theta(var_t_ten, pre_t_ten)
    ## q
    if self.name_var_q["adv"] != "none":
      name_var_q = "tn"+self.name_var_q["adv"]+"_adv"
      var_q_ten, lev_q_ten, tim_q_ten = get2dvar(name_var_q)
      if "q" in name_var_q: 
        # convert content to mixing ratio
        var_q_ten = var_q_ten/(1-var_q_ten)
    else:
      var_q_ten, lev_q_ten, tim_q_ten = None, None, None
    
    ## radiation tendency 
    if attributes["radiation"] == "tend":
      if self.name_var_t["adv"]!="none":
        name_var_t = "tn"+self.name_var_t["adv"]+"_rad"
        var_t_ten += ds.variables[name_var_t][:]
      else:
        name_var_t = "tn"+self.name_var_t["ini"]+"_rad"
        #var_t_ten = ds.variables[name_var_t][:]
        var_t_ten, lev_t_ten, tim_t_ten = get2dvar(name_var_t)
        self.name_var_t["adv"] = self.name_var_t["ini"]
    
    # subsidence, either in w or omega
    if attributes['forc_wa'] == 1:
      self.name_var_w["forc"] = "wa"
      var_w_frc, lev_w_frc, tim_w_frc = get2dvar("wa")
    elif attributes['forc_wap'] == 1: 
      self.name_var_w["forc"] = "wap"
      var_w_frc, lev_w_frc, tim_w_frc = get2dvar("wap")
      var_w_frc /= -1.*288*ds.variables['ta'][:]*9.81
    else:
      self.name_var_w["forc"] = "none"
      var_w_frc, lev_w_frc, tim_w_frc = None, None, None
    
    # surface forcings
    var_ts   = None; var_hfls = None; var_hfss = None; var_z0h  = None
    var_ustar= None; var_z0   = None
    tim_rad_ts = None

    ## prescribed surface temperature or turbulent fluxes
    if attributes['surface_forcing_temp'] == 'ts':
      var_ts, tim_sst = gettvar("ts_forc")
      var_ts = np.array([int(s*100)/100. for s in var_ts])
      tim_forc_ts = tim_sst
    elif attributes['surface_forcing_temp'] == 'surface_flux':
      var_hfls, tim_hfls = gettvar('hfls')
      var_hfss, tim_hfss = gettvar('hfss')
      tim_forc_ts = tim_hfls
      # prescribed fluxes + Ts for radiation scheme
      att = 'surface_radiation_temp'
      if att in attributes and attributes[att] == "ts" :
        var_ts, tim_sst = gettvar("ts_forc")
        var_ts = np.array([int(s*100)/100. for s in var_ts])
        tim_rad_ts = tim_sst
    elif attributes['surface_forcing_temp'] == 'none':
      #var_z0h, tim_z0h = gettvar('z0h') # ? this is never used ??
      #tim_forc_ts = tim_z0h
      tim_forc_ts = []
    else: log(ERROR, "No surface condition for temperature?", verbosity)
    
    ## prescribed momentum flux or roughness length
    if attributes['surface_forcing_wind'] == 'ustar':
      var_ustar, tim_ustar = gettvar('ustar')
      tim_forc_uv = tim_ustar
    elif attributes['surface_forcing_wind'] == 'z0':
      var_z0, tim_z0 = gettvar('z0')
      var_z0 = np.array([int(z0*1000)/1000. for z0 in var_z0])
      tim_forc_uv = tim_z0
    else:
      tim_forc_uv = None
      log(WARNING, "No surface condition for momentum?", verbosity)
    
    # make lev, tim grid that includes all forcings lev/tim
    # for each type of forcing and variable, bilinear interpolation on new grid
    tim_forcings = []
    lev_forcings = []
    if self.name_var_t["adv"]!="none": tim_forcings += list(tim_t_ten); lev_forcings+=list(lev_t_ten)
    if self.name_var_q["adv"]!="none": tim_forcings += list(tim_q_ten); lev_forcings+=list(lev_q_ten)
    if attributes["forc_geo"]: tim_forcings += list(tim_u_frc); lev_forcings+=list(lev_u_frc)
    tim_forcings = np.unique(tim_forcings)
    lev_forcings = np.unique(lev_forcings)
    tim_forcings.sort()
    lev_forcings.sort()
    ntime_forcings = len(tim_forcings)
    nlevs_forcings = len(lev_forcings)

    if self.def_lev == "P" :
      ## lev_forcings have been sorted in increasing order so need to reverse them if pressure defined
      lev_pre_forcings = lev_forcings[::-1]*1
      if np.max(lev_pre_forcings) <= np.max(lev_pre_t) and np.min(lev_pre_forcings) >= np.min(lev_pre_t):
        # interpolate from thermo profiles
        lev_forcings = np.array([interp_plev_to_zlev(lev_t, lev_pre_t, p) for p in lev_pre_forcings])
      else:
        # use basic exponential function and atmospheric length scale
        lev_forcings = -H_atm*np.log(-lev_pre_forcings/p0)

      #import matplotlib.pyplot as plt
      #plt.plot(lev_pre_forcings, lev_forcings, ls="", marker="o", label="forcings")
      #plt.plot(lev_pre_t, lev_t, ls="", marker=".", label="t")
      #plt.plot(lev_pre_u, lev_u, ls="", marker=".", label="u")
      #plt.legend()
      #plt.show()
    
    # set case variables
    ## init
    self.mnh_init_keyword = mnh_init_keyword
    self.nlev_init_uv     = nlev_init_uv
    self.nlev_init_tq     = nlev_init_tq
    self.zs    = zs    ; self.ps    = ps 
    self.lev_u = lev_u ; self.var_u = var_u ; self.var_v = var_v
    self.lev_t = lev_t ; self.var_t = var_t ; self.var_q = var_q
    # forcings atm
    self.tim_forcings = tim_forcings
    self.lev_forcings = lev_forcings
    self.ntime_forcings = ntime_forcings
    self.nlevs_forcings = nlevs_forcings
    self.var_u_frc = self.set_none_to_zero_and_interp_to_grid(var_u_frc,
            tim_u_frc, lev_u_frc, verbosity)
    self.var_v_frc = self.set_none_to_zero_and_interp_to_grid(var_v_frc,
            tim_v_frc, lev_v_frc, verbosity)
    self.var_w_frc = self.set_none_to_zero_and_interp_to_grid(var_w_frc,
            tim_w_frc, lev_w_frc, verbosity)
    self.var_t_frc = self.set_none_to_zero_and_interp_to_grid(var_t_frc,
            tim_t_frc, lev_t_frc, verbosity)
    self.var_q_frc = self.set_none_to_zero_and_interp_to_grid(var_q_frc,
            tim_q_frc, lev_q_frc, verbosity)
    self.var_t_ten = self.set_none_to_zero_and_interp_to_grid(var_t_ten,
            tim_t_ten, lev_t_ten, verbosity)
    self.var_q_ten = self.set_none_to_zero_and_interp_to_grid(var_q_ten,
            tim_q_ten, lev_q_ten, verbosity)
    # forcings surf
    self.tim_forc_ts = tim_forc_ts
    self.tim_rad_ts = tim_rad_ts
    self.tim_forc_uv = tim_forc_uv
    self.var_ts    = var_ts
    self.var_hfls  = var_hfls
    self.var_hfss  = var_hfss
    self.var_z0h   = var_z0h
    self.var_z0    = var_z0
    self.var_ustar = var_ustar

  def set_none_to_zero_and_interp_to_grid(self, var, tgrid, zgrid, verbosity):
    if var is None: var = np.zeros((self.ntime_forcings, self.nlevs_forcings))
    if var.shape != (self.ntime_forcings, self.nlevs_forcings):
      # if not the right grid, will be interpolated to the right grid
      log(DEBUG, "in set_none_to_zero", verbosity)
      log(DEBUG, "var shape "+str(var.shape), verbosity)
      log(DEBUG, "var t,z grids"+str(tgrid)+str(zgrid), verbosity)
      log(DEBUG, "frc shape %i %i"%(self.ntime_forcings, self.nlevs_forcings), verbosity)
      log(DEBUG, "frc t,z grids"+str(self.tim_forcings)+str(self.lev_forcings), verbosity)
      var = bilin_interp(var, tgrid, zgrid, self.tim_forcings, self.lev_forcings)
    return var

  def __str__(self):
    return "<Case %s/%s (%s) of type %s>"%(
            self.casename, self.subcasename, self.shortname, self.type)
