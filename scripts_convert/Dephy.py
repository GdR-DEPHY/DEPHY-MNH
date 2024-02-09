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

from Utils import parse_time, log, T_to_theta
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
listCaseDryShCv   = ["AYOTTE", "IHOP", "BLLAST"]                # dry shallow conv

# this is the list of all defined cases
listCases = listCaseMoistShCv+listCaseDryShCv+listCaseStable+listCaseDCv

# should this be defined in mesonh rather than here?
cas_type = {"moistshcv" : {"dx" : 25,  "dy" : 25,  "dz" : 25,   "zbot": 3500}, 
            "dcv"       : {"dx" : 200, "dy" : 200, "dz" : None, "zbot": 16500},
            "stable"    : {"dx" : 5,   "dy" : 5,   "dz" : None, "zbot": None},
            "dryshcv"   : {"dx" : 25,  "dy" : 25,  "dz" : 25,   "zbot": 3500},
            }

def FC_filename(dir, cas, subcas):
  fil="/".join([dir, cas, subcas, cas+"_"+subcas+"_DEF_driver.nc"])
  return fil

class Case:
  def __init__(self, casename, subcasename):
    """ Initialize case specific config
    .casename 
    .subcasename
    .shortname
    .type ["moistshcv", "dryshcv", "stable", "dcv"]
    .name_var_t, name_var_q, name_var_u, name_var_v, name_var_w = {}
    """
    self.casename    = casename
    self.subcasename = subcasename
    if subcasename != "MESONH" and subcasename != "REF" or casename == "SANDU":
      if len(subcasename)>5: self.shortname = casename[:3]+subcasename[:2]
      else: self.shortname   = casename[:5-len(subcasename)]+subcasename
    else: 
      self.shortname   = casename[:5]
    if "GABLS" in casename :
      self.shortname = "GABL%s"%casename[-1]
    if len(self.shortname)<5: 
        toto=self.shortname+"000" 
        self.shortname = toto[:5] 

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
    
    if casename == "GABLS1": 
        self.dzmin = 2    ; self.dzmax = 6
        self.zzmax = 250. ; self.zztop = 20
        self.zbot = 350.
    elif casename == "GABLS4":
        self.dzmin = 0.2  ; self.dzmax = 2
        self.zzmax = 50.  ; self.zztop = 20
        self.zbot = 80.
    else: 
        self.dzmin = self.dz ; self.dzmax = self.dz 
        self.zzmax = 1000    ; self.zztop = 0
        self.zbot = cas_type[self.type]["zbot"]

  def setup_outputs(self, max_seg_dur=24):
    ## max_seg_dur :: maximum duration of one segment
    caseoutput = CasesOutputs[self.casename]
    self.hourhf = caseoutput["hourhf"]
    begin_seg = [0, caseoutput["spinup"]]
    for h in self.hourhf:
      if h == begin_seg[-1]: begin_seg += [h+1]
      elif h > begin_seg[-1] and h+1 <= self.duration_hour:
        begin_seg += [h, h+1]
    more_segs = []
    for i,h in enumerate(begin_seg):
      if h < caseoutput["spinup"] : continue
      if i+1 < len(begin_seg): # not the anti last 
        hnext = begin_seg[i+1]
      else:
        hnext = self.duration_hour
      if hnext-h > max_seg_dur:
        print(i,h,hnext, "more than", max_seg_dur)
        more_segs += [i for i in np.arange(h+max_seg_dur,hnext,max_seg_dur)]
        print(more_segs)
    begin_seg = np.unique(begin_seg+more_segs)
    begin_seg.sort()
    print(begin_seg)
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

  def set_vertical_grid(self, inp_dir):
    if self.type == "dcv":
      fil="%s/grille_dcv.txt"%(inp_dir)
      if os.path.isfile(fil):
        self.zgrid = np.genfromtxt(fil, dtype=None,skip_header=0,usecols=0)
        self.nz    = len(self.zgrid)
      else: print("error: vertical grid file %s not found for case %s/%s"%(
          fil, self.casename, self.subcasename)); exit()
    else: self.zgrid = None

  def read_initial_profiles_and_forcings(self, attributes, ds, verbosity, read_zorog=0):
    # local helper function to read 2D, z-dependent and t-dependent variables
    def getzvar(name_var): 
      lev = "lev_"+name_var
      if not lev in ds.variables: lev="lev"
      return ds.variables[name_var][:], ds.variables[lev]
    def gettvar(name_var): 
      tim = "time_"+name_var 
      if not tim in ds.variables: tim="time"
      return ds.variables[name_var][:], ds.variables[tim]
    def get2dvar(name_var):
      lev = "lev_"+name_var
      if not lev in ds.variables: lev="lev"
      tim = "time_"+name_var 
      if not tim in ds.variables: tim="time"
      return ds.variables[name_var][:], ds.variables[lev], ds.variables[tim]

    ## champs initiaux T,q,ps,u,v,zorog
    name_var_t = self.name_var_t["ini"]
    name_var_q = self.name_var_q["ini"]
    name_var_u = self.name_var_u["ini"]
    name_var_v = self.name_var_v["ini"]
    
    ### initialize with theta_dry or theta_liquid
    mnh_init_keyword = "ZUVTHLMR" if name_var_t == "thetal" else "ZUVTHDMR"
    
    ### get initial profiles 
    ps    = ds.variables['ps'][:]      # surface pressure
    zs    = ds.variables['orog'][:] if read_zorog else ps*0
    var_u, lev_u = getzvar(name_var_u)           # ua: zonal wind profile
    var_v, lev_v = getzvar(name_var_v)           # va: merid wind profile
    var_t, lev_t = getzvar(name_var_t)           # theta, thetal or ta
    var_q, lev_q = getzvar(name_var_q)           # rv, rt or qv 
    print('lev_u',lev_u)
    if lev_u[0] >= 900.:
        mnh_init_keyword= "PUVTHLMR"
    
    nlev_init_uv = len(lev_u)
    nlev_init_tq = len(lev_t)
    
    log(DEBUG, "Original lev_t (%s) %s"%(name_var_t, lev_t), verbosity)
    log(DEBUG, "Original lev_q (%s) %s"%(name_var_q, lev_q), verbosity)
    log(DEBUG, "Original var_t (%s) %s"%(name_var_t, var_t), verbosity)
    log(DEBUG, "Original var_q (%s) %s"%(name_var_q, var_q), verbosity)
    
    ### convert T to theta and q to mixing ratio if needed
    
    if name_var_t == "ta" :
      # convert T to theta
      if lev_u[0] >= 900.:
        press=ds.variables["lev"][:]
      else:
        press = ds.variables["pa"][:]
      var_t = T_to_theta(var_t, press)
    
    if "q" in name_var_q: 
      # convert content to mixing ratio
      var_q = var_q/(1-var_q)
    
    log(DEBUG, "Converted var_t (%s) %s"%(name_var_t, var_t), verbosity)
    log(DEBUG, "Converted var_q (%s) %s"%(name_var_q, var_q), verbosity)
    
    if (lev_v.size != lev_u.size):
      log(WARNING, 'ERROR!! DIFFERENT VERTICAL GRIDS FOR U & V to be dealt \
              separately', verbosity)
      var_v = 0.*var_u+var_v[0][0]
    if len(lev_t) != len(lev_q):
      log(ERROR, "T and q not initialized on the same grid", verbosity)
    
    ### get forcings
    
    # horizontal winds : frc = geostrophic or nudging ; tendencies = 0
    ## zonal
    name_var_u = self.name_var_u["frc"]
    name_var_v = self.name_var_v["frc"]
    if name_var_u != "none": 
      var_u_frc, lev_u_frc, tim_u_frc = get2dvar(name_var_u)
      var_v_frc, lev_v_frc, tim_v_frc = get2dvar(name_var_v)
    else:
      var_u_frc = None
      var_v_frc = None
    
    # thermodynamics : frc = nudging | none ; tend = adv | rad | none
    ## T
    if self.name_var_t["nudging"] != "none":
      self.xrelax_time_frc = attributes["nudging_"+self.name_var_t["nudging"]]
      name_var_t = self.name_var_t["nudging"]+"_nud"
      var_t_frc, lev_t_frc, tim_t_frc = get2dvar(name_var_t)
    else:
      var_t_frc = None
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
      var_q_frc = None
    
    # thermodynamics : advection 
    ## T
    if self.name_var_t["adv"] != "none":
      name_var_t = "tn"+self.name_var_t["adv"]+"_adv"
      var_t_ten, lev_t_ten, tim_t_ten = get2dvar(name_var_t)
    else:
      var_t_ten = None
    if name_var_t == "tnta_adv":
      log(ERROR, "Don't know how to convert T adv to theta", verbosity)
    ## q
    if self.name_var_q["adv"] != "none":
      name_var_q = "tn"+self.name_var_q["adv"]+"_adv"
      var_q_ten, lev_q_ten, tim_q_ten = get2dvar(name_var_q)
      if "q" in name_var_q: 
        # convert content to mixing ratio
        var_q_ten = var_q_ten/(1-var_q_ten)
    else:
      var_q_ten = None
    
    ## radiation tendency 
    if attributes["radiation"] == "tend":
      if self.name_var_t["adv"]!="none":
        name_var_t = "tn"+self.name_var_t["adv"]+"_rad"
        var_t_ten += ds.variables[name_var_t][:]
      else:
        name_var_t = "tn"+self.name_var_t["ini"]+"_rad"
        var_t_ten = ds.variables[name_var_t][:]
    
    # subsidence, either in w or omega
    if attributes['forc_wa'] == 1:
      self.name_var_w["forc"] = "wa"
      var_w_frc, lev_w_frc, time_w_frc = get2dvar("wa")
    elif attributes['forc_wap'] == 1: 
      self.name_var_w["forc"] = "wap"
      var_w_frc, lev_w_frc, time_w_frc = get2dvar("wap")
      var_w_frc /= -1.*288*ds.variables['ta'][:]*9.81
    else:
      self.name_var_w["forc"] = "none"
      var_w_frc = None
    
    # surface forcings
    var_ts   = None; var_hfls = None; var_hfss = None; var_z0h  = None
    var_ustar= None; var_z0   = None

    ## prescribed SST or turbulent fluxes
    if attributes['surface_forcing_temp'] == 'ts':
      var_ts, tim_sst = gettvar("ts_forc")
      var_ts = np.array([int(s*100)/100. for s in var_ts])
      tim_forc_ts = tim_sst
    elif attributes['surface_forcing_temp'] == 'surface_flux':
      var_hfls, tim_hfls = gettvar('hfls')
      var_hfss, tim_hfss = gettvar('hfss')
      tim_forc_ts = tim_hfls
    elif attributes['surface_forcing_temp'] == 'none':
      var_z0h, tim_z0h = gettvar('z0h') # ? this is never used ??
      tim_forc_ts = tim_z0h
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
    
    ## chose forcing time x lev grid (priority to advection)
    tim_forcings = tim_t_ten if self.name_var_t["adv"]!="none" else \
            tim_q_ten if self.name_var_q["adv"]!="none" else \
            tim_u_frc if attributes["forc_geo"] else None
    lev_forcings = lev_t_ten if self.name_var_t["adv"]!="none" else \
            tim_q_ten if self.name_var_q["adv"]!="none" else \
            lev_u_frc if attributes["forc_geo"] else None
    ntime_forcings = len(tim_forcings)
    nlevs_forcings = len(lev_forcings)

    # set case variables
    ## init
    self.mnh_init_keyword = mnh_init_keyword
    self.nlev_init_uv     = nlev_init_uv
    self.nlev_init_tq     = nlev_init_tq
    self.zs    = zs    ; self.ps    = ps 
    self.lev_u = lev_u ; self.lev_t = lev_t
    self.var_u = var_u ; self.var_v = var_v
    self.var_t = var_t ; self.var_q = var_q
    # forcings atm
    self.tim_forcings = tim_forcings
    self.lev_forcings = lev_forcings
    self.ntime_forcings = ntime_forcings
    self.nlevs_forcings = nlevs_forcings
    self.var_u_frc = self.set_none_to_zero(var_u_frc, verbosity)
    self.var_v_frc = self.set_none_to_zero(var_v_frc, verbosity)
    self.var_w_frc = self.set_none_to_zero(var_w_frc, verbosity)
    self.var_t_frc = self.set_none_to_zero(var_t_frc, verbosity)
    self.var_q_frc = self.set_none_to_zero(var_q_frc, verbosity)
    self.var_t_ten = self.set_none_to_zero(var_t_ten, verbosity)
    self.var_q_ten = self.set_none_to_zero(var_q_ten, verbosity)
    # forcings surf
    self.tim_forc_ts = tim_forc_ts
    self.tim_forc_uv = tim_forc_uv
    self.var_ts    = var_ts
    self.var_hfls  = var_hfls
    self.var_hfss  = var_hfss
    self.var_z0h   = var_z0h
    self.var_z0   = var_z0
    self.var_ustar   = var_ustar

  def set_none_to_zero(self,var, verbosity): 
    if var is None: var = np.zeros((self.ntime_forcings, self.nlevs_forcings))
    if var.shape != (self.ntime_forcings, self.nlevs_forcings):
      # if not the right grid, will be broadcasted to the right grid
      log(DEBUG, "var shape "+str(var.shape)+"\n shape frc %i\
              %i"%(self.ntime_forcings, self.nlevs_forcings), verbosity)
      var = np.broadcast_to(var, (self.ntime_forcings, self.nlevs_forcings))
    return var

  def __str__(self):
    return "<Case %s/%s (%s) of type %s>"%(
            self.casename, self.subcasename, self.shortname, self.type)
