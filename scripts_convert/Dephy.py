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

  def set_flags_and_keyword(self, FC_attributes):
    """ set the type of variables that define initial profiles and advections
    from global attributes read in common format file """
    if FC_attributes["adv_ta"] == 1       : self.flag_t = "ta"
    elif FC_attributes["adv_theta"] == 1  : self.flag_t = "theta"
    elif FC_attributes["adv_thetal"] == 1 : self.flag_t = "thetal"
    else : self.flag_t = "none"
    if FC_attributes["adv_qv"] == 1       : self.flag_q = "qv"
    elif FC_attributes["adv_qt"] == 1     : self.flag_q = "qt"
    elif FC_attributes["adv_rv"] == 1     : self.flag_q = "rv" 
    elif FC_attributes["adv_rt"] == 1     : self.flag_q = "rt"
    else : self.flag_q = "none"
    if self.flag_t == "thetal" : self.keyword = "ZUVTHLMR"
    else: self.keyword = "ZUVTHDMR"

  def set_times(self, FC_attributes):
    y,m,d,hh,mm,ss = parse_time(FC_attributes["start_date"])
    self.start_date = datetime(y,m,d,hh,mm,ss)
    y,m,d,hh,mm,ss = parse_time(FC_attributes["end_date"])
    self.end_date = datetime(y,m,d,hh,mm,ss)
    self.duration = self.end_date-self.start_date
    self.duration_secs = self.duration.total_seconds()

  def set_vertical_grid(self, inp_dir):
    if self.type == "dcv":
      fil="%s/grille_%s.txt"%(inp_dir, self.casename)
      if os.path.isfile(fil):
        self.zgrid = np.genfromtxt(fil, dtype=None,skip_header=1,usecols=0)
        self.nz    = len(self.zgrid)
      else: print("error: vertical grid file %s not found for case %s/%s"%(
          fil, self.casename, self.subcasename)); exit()
    else: self.zgrid = None ; self.nz = 0

  def __str__(self):
    return "<Case %s/%s (%s) of type %s>"%(
            self.casename, self.subcasename, self.shortname, self.type)
