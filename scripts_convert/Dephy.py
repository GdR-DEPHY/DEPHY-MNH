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

from Utils import parse_time
from datetime import timedelta, datetime
import os

# missing cases in prep_nam_from_FFCOM_DEF_to_MNHLES
# KB2006, DYNAMO, MAGIC, FIRE, ISDAC, MPACE

# To add a case to the database, add its name to one of the lists:
listCaseMoistShCv = ["ARMCU", "BOMEX", "SANDU", "RICO", "SCMS"] # moist shallow conv
listCaseDCv       = ["LBA", "AMMA", "KB2006", "EUROCS"]         # deep conv
listCaseStable    = ["GABLS1", "GABLS4"]                        # stable
listCaseDryShCv   = ["AYOTTE", "IHOP", "BLLAST"]                # dry shallow conv

# this is the list of all defined cases
listCases = listCaseMoistShCv+listCaseDryShCv+listCaseStable+listCaseDCv

def FC_filename(dir, cas, subcas):
  fil="/".join([dir, cas, subcas, cas+"_"+subcas+"_DEF_driver.nc"])
  if cas=="RICO":
    fil=dir+'/RICO/MESONH/RICO_MESONH_DEF_driver.nc'
  return fil

class Case:

  def __init__(self, casename, subcasename):
    """ Initialize case specific config
    .casename 
    .subcasename
    .shortname
    .type ["moistshcv", "dryshcv", "stable", "dcv"]
    .var_t, var_q, var_u, var_v = {}
    """
    self.casename    = casename
    self.subcasename = subcasename
    if subcasename != "REF" or casename == "SANDU":
      self.shortname   = casename[:5-len(subcasename)]+subcasename
    else: 
      self.shortname   = casename[:5]

    if   casename in listCaseMoistShCv : self.type = "moistshcv"
    elif casename in listCaseDCv       : self.type = "dcv" 
    elif casename in listCaseStable    : self.type = "stable"
    elif casename in listCaseDryShCv   : self.type = "dryshcv"

    # name of variable use for initialisation, advection and nudging
    # e.g. for temperature : var_t["ini"] = "ta" var_t["adv"] = "theta"
    self.var_t={} # temperature: ta | theta | thetal
    self.var_q={} # humidity: rv | qv | rt | qt
    self.var_u={} # zonal wind : ug | ua_nud
    self.var_v={} # meridional wind : vg | va_nud

  def set_init_and_forcing_types(self, FC_attributes):
    """ set the type of variables that define initial profiles, advection and
    nudging from global attributes read in common format file """

    for key in "ini","adv","nudging":
      if FC_attributes[key+"_ta"] > 0        : self.var_t[key] = "ta"
      elif FC_attributes[key+"_theta"] > 0   : self.var_t[key] = "theta"
      elif FC_attributes[key+"_thetal"] > 0  : self.var_t[key] = "thetal"
      else : self.var_t[key] = "none"
      if FC_attributes[key+"_qv"] > 0        : self.var_q[key] = "qv"
      elif FC_attributes[key+"_qt"] > 0      : self.var_q[key] = "qt"
      elif FC_attributes[key+"_rv"] > 0      : self.var_q[key] = "rv" 
      elif FC_attributes[key+"_rt"] > 0      : self.var_q[key] = "rt"
      else : self.var_q[key] = "none"

    # ini wind is always ua,va ; no wind advection ; 
    # forcing can be either geostrophic or nudging or none
    self.var_u["ini"]   = "ua"
    self.var_v["ini"]   = "va"
    if FC_attributes["forc_geo"]: 
      self.var_u["frc"] = "ug"
      self.var_v["frc"] = "vg"
    elif FC_attributes["nudging_ua"]:
      if FC_attributes["nudging_va"]==0: 
          print("error: nudging u but not v ?"); exit()
      self.var_u["frc"] = "ua_nud"
      self.var_v["frc"] = "va_nud"
    else:
      self.var_u["frc"] = "none"
      self.var_v["frc"] = "none"

  def set_times(self, FC_attributes):
    y,m,d,hh,mm,ss = parse_time(FC_attributes["start_date"])
    self.start_date = datetime(y,m,d,hh,mm,ss)
    self.start_seconds = 3600*hh+60*mm+ss
    y,m,d,hh,mm,ss = parse_time(FC_attributes["end_date"])
    self.end_date = datetime(y,m,d,hh,mm,ss)
    self.duration = self.end_date-self.start_date
    self.duration_secs = self.duration.total_seconds()

  def set_vertical_grid(self, inp_dir):
    if self.type == "dcv":
      fil="%s/grille_%s.txt"%(inp_dir, self.casename)
      if os.path.isfile(fil):
        import numpy as np
        self.zgrid = np.genfromtxt(fil, dtype=None,skip_header=0,usecols=0)
        self.nz    = len(self.zgrid)
      else: print("error: vertical grid file %s not found for case %s/%s"%(
          fil, self.casename, self.subcasename)); exit()
    else: self.zgrid = None ; self.nz = 0

  def __str__(self):
    return "<Case %s/%s (%s) of type %s>"%(
            self.casename, self.subcasename, self.shortname, self.type)
