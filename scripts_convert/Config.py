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

"""
Created on November 2023 
@author: F. Couvreux & N. Villefranque
"""


import os
import logging
logging.basicConfig(format='%(asctime)s - %(name)20s - %(levelname)s - %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

import time
import copy
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta

from mesonh import default_preidea, default_exseg
from mesonh import preidea_LES, exseg_LES
from mesonh import preidea_CRM, exseg_CRM
from mesonh import preidea_SCM, exseg_SCM
from Utils import str_time, time_to_secs

#------------------------------------------#
#------------ HELPER FUNCTIONS ------------#
#------------------------------------------#
def copy_config(copy_to, copy_from, namelist, key):
  """ either full copy or only one namelist or only one key of one namelist 
  # copy_to   intent inout
  # copy_from intent in
  # namelist  intent in
  # key       intent in
  """
  if namelist=="all": # copy all
    copy_to = copy.deepcopy(copy_from)
  else:
    if key=="all":
      copy_to[namelist] = copy_from[namelist].copy()
    else:
      copy_to[namelist][key] = copy_from[namelist][key]
  return copy_to

def replace_and_delete_config(copy_to, copy_from, namelist, key):
  """ either full config or only one namelist or only one key of one namelist 
  # copy_to   intent inout
  # copy_from intent in
  # namelist  intent in
  # key       intent in
  """
  if namelist=="all": # copy all
    for nam in copy_from:
      for k in copy_from[nam]:
        copy_to[nam][k] = copy_from[nam][k]
    list_del = []
    for nam in copy_to:
      if nam not in copy_from:
        list_del+=[nam]
    for nam in list_del:
      #print("delete nam", nam, "from config")
      del(copy_to[nam])
  else:
    if key=="all":
      for k in copy_from[namelist]:
        copy_to[namelist][k] = copy_from[namelist][k]
      list_del = []
      for k in copy_to[namelist]:
        if k not in copy_from[namelist]: 
          list_del += [k]
      for k in list_del: 
        #print("delete k", k, "from namelist", namelist)
        del(copy_to[namelist][k])
    else:
      copy_to[namelist][key] = copy_from[namelist][key]
  return copy_to

def replace_config(copy_to, copy_from, namelist, key):
  """ either full config or only one namelist or only one key of one namelist 
  # copy_to   intent inout
  # copy_from intent in
  # namelist  intent in
  # key       intent in
  """
  if namelist=="all": # copy all
    for nam in copy_from:
      for k in copy_from[nam]:
        copy_to[nam][k] = copy_from[nam][k]
  else:
    if key=="all":
      for k in copy_from[namelist]:
        copy_to[namelist][k] = copy_from[namelist][k]
    else:
      copy_to[namelist][key] = copy_from[namelist][key]
  return copy_to

def write_namelist(to_write, outfile):
  """ write config to namelist file 
  input: to_write is dict of dicts config 
         outfile  is str name of file to write
  """
  str=""
  for k in to_write:
    if "freeformat" in k: continue
    str+="&%s\n"%k
    for kk in to_write[k]:
      str+="  {:s} = {:s},\n".format(kk, to_write[k][kk])
    str+="/ \n\n"
  if "freeformat" in to_write:
    for k in to_write["freeformat"]:
      str+=to_write["freeformat"][k]
      str+="\n"
  with open(outfile, "w") as f:
    f.write(str)

#------------------------------------------#
#----------------- CLASS ------------------#
#------------------------------------------#

class Config:
  
  def __init__(self, case, fic, mode="LES"):
    """ construct Config object 
    ## inputs:
    # case = ARMCU, RICO, AYOTTE, ...
    # fic  = PREIDEA, EXSEG01, ...
    # mode = None (default MNH), LES, CRM, SCM
    #
    ## fields of Config object:
    # .case = case
    # .name = fic
    # .mode = mode [None|LES|CRM|SCM]
    # .is_exseg = 0 if preidea, 1 if exseg
    # .config = preidea or exseg dicts of dicts of the form:
    # {"NAM_name":{"key1":"val1", "key2":"val2}, "NAM_other":{"key":"val"}}
    """
    self.case = case # ARMCU...
    self.name = fic  # PREIDEA OR EXSEG ...
    self.mode = mode # None|LES|CRM|SCM

    if "PRE" in fic:
      self.is_exseg=0
      self.config = copy.deepcopy(default_preidea)
    elif "EXS" in fic:
      self.is_exseg=1
      self.config = copy.deepcopy(default_exseg)
    else: print("error: unknown file format %s"%fic); exit()

    if mode is None: return # default Meso-NH config
    elif mode == "LES" :    # adapted to LES
      config_ = exseg_LES if self.is_exseg else preidea_LES
    elif mode == "CRM" : 
      config_ = exseg_CRM if self.is_exseg else preidea_CRM
    elif mode == "SCM" :
      config_ = exseg_SCM if self.is_exseg else preidea_SCM
    else : print("error: unknown constructor mode %s"%mode); exit()

    self.config = replace_config(self.config, config_, "all", "all")

  def __str__(self):
    """ print namelist to screen """
    str="############\n %s \n############\n"%(self.name)
    for k in self.config:
        str+="===\nNAMELIST : %s\n>> KEYS : %s\n"%(k, self.config[k])
    return str

  def write(self, outfile):
    write_namelist(self.config, outfile)

  def duplicate_config(self, name=None):
    new_conf = Config(self.case, self.name if name is None else name, self.mode)
    new_conf.config = copy_config(new_conf.config, self.config, "all", "all")
    return new_conf

  def copy_config_from(self, copy_from, namelist="all", key="all"):
    self.config = copy_config(self.config, copy_from.config, namelist, key)

  def modify(self, namelist, key, value):
    if namelist not in self.config:
        print("cannot modify namelist %s"%namelist); exit()
    self.config[namelist][key] = value

  def freeformat_zhat(self, cas):
    if cas.zgrid is None : str_zhat=""
    else:
      str_zhat = "ZHAT\n"
      for z in cas.zgrid: str_zhat+="%i\n"%z 
    self.config["freeformat"]["ZHAT"] = str_zhat

  def freeformat_rsou(self, cas):
    str_init = "RSOU\n"
    str_init += str_time(cas.start_date)
    str_init += "%s   \n"%cas.mnh_init_keyword
    str_init += "%.2f \n"%cas.zs[0]
    str_init += "%.2f \n"%cas.ps[0]
    str_init += "%.2f \n"%cas.var_t[0][0]
    str_init += "%.8f \n"%cas.var_q[0][0]
    str_init += "%i   \n"%cas.nlev_init_uv
    for (z,u,v) in zip(cas.lev_u, cas.var_u[0], cas.var_v[0]):
      str_init += "%14.1f %14.2f %14.2f\n"%(z,u,v)
    str_init += "%i   \n"%cas.nlev_init_tq
    for (z,t,q) in zip(cas.lev_t, cas.var_t[0], cas.var_q[0]):
      str_init += "%14.1f %14.2f %14.8f\n"%(z,t,q)
    self.config["freeformat"]["RSOU"] = str_init

  def freeformat_zfrc(self, cas):
    str_zfrc = "ZFRC\n"
    str_zfrc += "%i\n"%cas.ntime_forcings
    for it in range(cas.ntime_forcings):
      date = cas.start_date + timedelta(seconds = int(cas.tim_forcings[it]))
      str_zfrc += str_time(date)
      str_zfrc += "%.2f \n"%cas.zs[0]
      str_zfrc += "%.2f \n"%cas.ps[0]
      str_zfrc += "%.2f \n"%cas.var_t[0][0]
      str_zfrc += "%.8f \n"%cas.var_q[0][0]
      str_zfrc += "%i   \n"%cas.nlevs_forcings
      for ik in range(cas.nlevs_forcings):
        str_zfrc += "%14.1f %14.2f %14.2f %14.2f %14.5f %14.5f %14.10f %14.10f %4.1f %4.1f\n"%(
        cas.lev_forcings[ik], cas.var_u_frc[it,ik], cas.var_v_frc[it,ik],
        cas.var_t_frc[it,ik], cas.var_q_frc[it,ik], cas.var_w_frc[it,ik],
        cas.var_t_ten[it,ik], cas.var_q_ten[it,ik], 0., 0.)
    self.config["freeformat"]["ZFRC"] = str_zfrc

  def set_domain_grid(self, cas):
    if self.mode == "LES":
      self.modify("NAM_GRIDH_PRE", "XDELTAX", "%f"%cas.dx)
      self.modify("NAM_GRIDH_PRE", "XDELTAY", "%f"%cas.dy)
    self.modify("NAM_VER_GRID", "LTHINSHELL", ".TRUE.")
    if cas.zgrid is not None: # manual z grid, ZHAT will be dumped in PRE_IDEA
      self.modify("NAM_VER_GRID", "NKMAX", "%i"%cas.nz)
      self.modify("NAM_VER_GRID", "YZGRID_TYPE", "'MANUAL'")
    else:  # automatically stretched (or constant) z grid
      self.modify("NAM_VER_GRID", "YZGRID_TYPE", "'FUNCTN'")
      self.modify("NAM_VER_GRID", "ZDZGRD", "%f"%cas.dzmin)
      self.modify("NAM_VER_GRID", "ZDZTOP", "%f"%cas.dzmax)
      self.modify("NAM_VER_GRID", "ZSTRTOP", "%f"%cas.zztop)
      self.modify("NAM_VER_GRID", "ZZMAX_STRGRD", "%f"%cas.zzmax)
      self.modify("NAM_VER_GRID", "ZSTRGRD", "0")

  def set_ini_filenames(self, cas):
    inifile = "init_"+cas.shortname
    self.modify("NAM_LUNITn", "CINIFILE", "'%s'"%inifile)
    self.modify("NAM_LUNITn", "CINIFILEPGD", "'%s_PGD'"%inifile)

  def set_lonlat(self, cas):
    self.modify("NAM_GRID_PRE", "XLAT0", str(int(cas.lat*100)/100.))
    self.modify("NAM_GRID_PRE", "XLON0", str(int(cas.lon*100)/100.))

  def set_luser(self, cas):
    self.modify("NAM_CONFn", "LUSERV", ".TRUE.")
    if cas.mnh_init_keyword == "ZUVTHLMR":
      self.modify("NAM_CONFn", "LUSERC", ".TRUE.")

  def set_surface_forcings(self, cas):
    sf = cas.surface_forcing
    nts = len(cas.tim_forc_ts)
    # continental with prescribed flux ==== ocean.f90 in mesonh!
    if "ocean" in sf or sf == "landsurface_flux":
      self.modify("NAM_PGD_SCHEMES", "CSEA", "'SEAFLX'" if "ts" in sf else "'FLUX'")
      self.modify("NAM_COVER", "XUNIF_COVER(1)", "1.")
      self.modify("NAM_COVER", "XUNIF_COVER(6)", "0.")
      if "ts" in sf:
        self.modify("NAM_SEABATHY", "XUNIF_SEABATHY", "5")
        self.modify("NAM_PREP_SEAFLUX", "XSST_UNIF", "%f"%cas.var_ts[0])
        self.modify("NAM_DATA_SEAFLUX", "LSST_DATA", ".TRUE.")
        self.modify("NAM_DATA_SEAFLUX", "NTIME_SST", "%i"%nts)
        for it in range(nts):
          date = cas.start_date + timedelta(seconds = int(cas.tim_forc_ts[it]))
          date_secs = time_to_secs(date)
          self.modify("NAM_DATA_SEAFLUX", "NYEAR_SST(%i)"%(it+1), date.strftime("%Y"))
          self.modify("NAM_DATA_SEAFLUX", "NMONTH_SST(%i)"%(it+1), date.strftime("%m"))
          self.modify("NAM_DATA_SEAFLUX", "NDAY_SST(%i)"%(it+1), date.strftime("%d"))
          self.modify("NAM_DATA_SEAFLUX", "XTIME_SST(%i)"%(it+1), "%f"%date_secs)
          self.modify("NAM_DATA_SEAFLUX", "XUNIF_SST(%i)"%(it+1), "%f"%cas.var_ts[it])
    elif "land" in sf:
      self.modify("NAM_PGD_SCHEMES", "CNATURE", "'TSZ0'")
      self.modify("NAM_COVER", "XUNIF_COVER(1)", "0.")
      self.modify("NAM_COVER", "XUNIF_COVER(6)", "1.")
      self.modify("NAM_FRAC", "LECOCLIMAP", ".TRUE.") 
      self.modify("NAM_FRAC", "XUNIF_NATURE", "1.")
      self.modify("NAM_DATA_TSZ0", "NTIME", "%i"%nts)
      for it in range(nts-1):
        dts = cas.var_ts[it+1] - cas.var_ts[it]
        self.modify("NAM_DATA_TSZ0", "XUNIF_DTS(%i)"%(it+1), "%f"%dts)
      self.modify("NAM_DATA_TSZ0", "XUNIF_DTS(%i)"%(nts), "0.")
      self.modify("NAM_DATA_ISBA", "NTIME", "1")
      for i in range(1,13):
        self.modify("NAM_DATA_ISBA", "XUNIF_Z0(1,%i)"%i, "%f"%cas.var_z0[0])
      self.modify("NAM_PREP_SURF_ATM", "NYEAR", cas.start_date.strftime("%Y"))
      self.modify("NAM_PREP_SURF_ATM", "NMONTH", cas.start_date.strftime("%m"))
      self.modify("NAM_PREP_SURF_ATM", "NDAY", cas.start_date.strftime("%d"))
      self.modify("NAM_PREP_SURF_ATM", "XTIME", "%f"%time_to_secs(cas.start_date))
      self.modify("NAM_PREP_ISBA", "XTG_SURF", "%f"%cas.var_ts[0])
      self.modify("NAM_PREP_ISBA", "XTG_ROOT", "%f"%cas.var_ts[0])
      self.modify("NAM_PREP_ISBA", "XTG_DEEP", "%f"%cas.var_ts[0])
      self.modify("NAM_PREP_ISBA", "NYEAR", cas.start_date.strftime("%Y"))
      self.modify("NAM_PREP_ISBA", "NMONTH", cas.start_date.strftime("%m"))
      self.modify("NAM_PREP_ISBA", "NDAY", cas.start_date.strftime("%d"))
      self.modify("NAM_PREP_ISBA", "XTIME", "%f"%time_to_secs(cas.start_date))

  def set_forcing_flags(self, cas):
    self.modify("NAM_FRC", "LGEOST_TH_FRC", ".FALSE.")
    if cas.name_var_u["frc"] == "ug" : 
      self.modify("NAM_DYN", "LCORIO", ".TRUE.")
      self.modify("NAM_FRC", "LGEOST_UV_FRC", ".TRUE.")
      self.modify("NAM_FRC", "LRELAX_UV_MEAN_FRC", ".FALSE.")
    else:
      self.modify("NAM_DYN", "LCORIO", ".FALSE.")
      self.modify("NAM_FRC", "LGEOST_TH_FRC", ".FALSE.")
      if cas.name_var_u["frc"] == "ua_nud": 
        self.modify("NAM_FRC", "LRELAX_UV_MEAN_FRC", ".TRUE.")
        self.modify("NAM_FRC", "XRELAX_TIME_FRC", "%f"%cas.xrelax_time_frc)
    if cas.name_var_t["adv"] == "none" :
      self.modify("NAM_FRC", "LTEND_THRV_FRC", ".FALSE.")
    else:
      self.modify("NAM_FRC", "LTEND_THRV_FRC", ".TRUE.")
    if cas.name_var_w["forc"] == "none" :
      self.modify("NAM_FRC", "LVERT_MOTION_FRC", ".FALSE.")
    else:
      self.modify("NAM_FRC", "LVERT_MOTION_FRC", ".TRUE.")
    if cas.name_var_t["nudging"] == "none" :
      self.modify("NAM_FRC", "LRELAX_THRV_FRC", ".FALSE.")
    else:
      self.modify("NAM_FRC", "LRELAX_THRV_FRC", ".TRUE.")
      self.modify("NAM_FRC", "XRELAX_TIME_FRC", "%f"%cas.xrelax_time_frc)

  def set_buffer_layer(self, cas):
    self.modify("NAM_DYN", "XALZBOT", "%f"%cas.zbot)

  def set_outputs(self, cas, iseg):
    # list of hours to begin segments
    lseg = cas.begin_seg 
    # convention defined in Dephy.py setup_outputs
    # lseg[0] = 0 ; lseg[1] = spinup ; lseg[2] = ...
    # si iseg == 0 : on fait toute la simul d'un coup
    # si iseg > 0 :
    #   lseg[iseg-1] = à quelle heure on commence
    #   lseg[iseg] = à quelle heure on s'arrête

    if iseg == 0:          duration = cas.duration_hour
    elif iseg < len(lseg): duration = lseg[iseg]-lseg[iseg-1]
    else : duration = cas.duration_hour - lseg[iseg-1]
    spinup_secs = lseg[1]*3600.
    seg_dur = duration*3600.

    if iseg in [0,1]:   # 0 = mother config, the whole simulation ; 1 = spinup
      bak_fir = spinup_secs ; bak_frq = 3600.
      out_fir = spinup_secs ; out_frq = 1800.
      self.is_hf = 0
    else: # >= 2
      if iseg == 2 or iseg-2 in cas.hourhf: nbak_in_prev = 1
      else: nbak_in_prev = lseg[iseg-1]-lseg[iseg-2]
      prev_name = "%s.1.%s.%03i"%(cas.shortname, "SEG%02i"%(iseg-1),
              nbak_in_prev)
      self.modify("NAM_CONF",   "CCONF", "'RESTA'")
      self.modify("NAM_LUNITn", "CINIFILE", "'%s'"%prev_name)
      if lseg[iseg-1] in cas.hourhf: # heure d'intérêt
        bak_fir = 3600. ; bak_frq = 3600.
        out_fir = 60.   ; out_frq = 60.
        self.is_hf = 1
      else: # entre deux heures d'intérêt ou à la fin
        bak_fir = 3600. ; bak_frq = 3600.
        out_fir = 1800. ; out_frq = 1800.
        self.is_hf = 0

    self.iseg    = iseg
    self.seg_beg = 0 if iseg==0 else lseg[iseg-1]*3600.
    self.seg_dur = seg_dur
    self.seg_end = self.seg_dur + self.seg_beg

    self.modify("NAM_CONF",   "CSEG", "'SEG%02i'"%iseg)
    self.modify("NAM_DYN",    "XSEGLEN", "%f"%seg_dur)
    self.modify("NAM_LES",    "XLES_TEMP_MEAN_END", "%f"%seg_dur)
    self.modify("NAM_OUTPUT", "XOUT_TIME_FREQ(1)",       "%f"%out_frq)
    self.modify("NAM_OUTPUT", "XOUT_TIME_FREQ_FIRST(1)", "%f"%out_fir)
    self.modify("NAM_BACKUP", "XBAK_TIME_FREQ(1)",       "%f"%bak_frq)
    self.modify("NAM_BACKUP", "XBAK_TIME_FREQ_FIRST(1)", "%f"%bak_fir)

  def reset_seg_surface_forcings(self, cas, iseg):
    default = Config("def", "EXSEG", self.mode)
    self.config = replace_and_delete_config(self.config, default.config,
            "NAM_IDEAL_FLUX", "all")
    all_tim_forc = cas.tim_forc_ts[:]
    ntf = len(all_tim_forc)
    if cas.var_ustar is not None and cas.tim_forc_uv[:] != all_tim_forc:
      print(cas.tim_forc_uv[:], all_tim_forc)
      print(cas.var_ustar, cas.var_z0)
      print("problemo ???"); exit()
    
    # select only times that are needed for the segment
    list_ = None
    for i,t in enumerate(all_tim_forc):
      if t > self.seg_beg:
        if list_ is None: list_ = [i-1]
        list_ += [i]
        if t >= self.seg_end : break

    # forcing time relative to the beginning of the segment
    rel_times = [t - self.seg_beg for t in all_tim_forc[list_]]
    nf = len(rel_times)
    
    # extract forcing for the segment, interpolate if needed
    def forc(var):
      forc = []
      if rel_times[0] == 0: forc = [var[list_[0]]]
      else:
        t0 = rel_times[0]  ; t1 = rel_times[1]  ; t = 0.
        v0 = var[list_[0]] ; v1 = var[list_[1]] 
        forc = [(v1-v0)/(t1-t0)*(t-t0) + v0]
      forc += [v for v in var[list_[1]:list_[-1]+1]]
      return forc

    var = rel_times ; key = "XTIMEF"
    for i,f in enumerate(var):
      if i == 0: self.modify("NAM_IDEAL_FLUX", key+"(1)", "0.")
      else: self.modify("NAM_IDEAL_FLUX", key+"(%i)"%(i+1), "%f"%f)

    list_vars = [cas.var_hfls, cas.var_hfss, cas.var_ts, cas.var_z0,
            cas.var_ustar, [0.]]
    list_keys = ["XSFTQ", "XSFTH", "XTSRAD", "XZ0", "USTAR", "XSFCO2"]
    for var,key in zip(list_vars, list_keys):
      if var is not None :
        if len(var) != ntf: var = [var[0]]*ntf
        if key == "XZ0":   self.modify("NAM_IDEAL_FLUX", "CUSTARTYPE", "Z0")
        if key == "USTAR": self.modify("NAM_IDEAL_FLUX", "CUSTARTYPE", "USTAR")
        if key == "XTSRAD": self.modify("NAM_IDEAL_FLUX", "NFORCT", "%i"%nf)
        for i,f in enumerate(forc(var)):
          print(i,f)
          self.modify("NAM_IDEAL_FLUX", key+"(%i)"%(i+1), "%f"%f)

    self.modify("NAM_IDEAL_FLUX", "NFORCF", "%i"%nf)

  def set_name(self,cas):
    self.modify("NAM_CONF",   "CEXP", "'%s'"%cas.shortname)

  def set_warm_microphysics(self):
    self.modify("NAM_PARAM_LIMA", "NMOM_I", "0")
    self.modify("NAM_PARAM_LIMA", "NMOM_S", "0")
    self.modify("NAM_PARAM_LIMA", "NMOM_G", "0")

  def deactivate_radiation(self):
    self.modify("NAM_PARAMn", "CRAD", "'NONE'")
  def activate_radiation(self):
    self.modify("NAM_PARAMn", "CRAD", "'ECRA'")

  def deactivate_budgets(self):
    self.modify("NAM_BUDGET", "CBUTYPE", "'NONE'")
  def activate_budgets(self):
    self.modify("NAM_BUDGET", "CBUTYPE", "'CART'")
