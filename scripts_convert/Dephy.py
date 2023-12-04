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

# missing cases in prep_nam_from_FFCOM_DEF_to_MNHLES
# KB2006, DYNAMO, MAGIC, FIRE, ISDAC, MPACE
listCases = [
  "ARMCU", "AMMA", "AYOTTE", "BOMEX", "GABLS1", "IHOP", "SANDU", "BLLAST",
  "GABLS4", "LBA", "RICO", "SCMS"
]

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

    if   casename == "ARMCU"  : self.set_("moistshcv", "ZUVTHDMR", "rt", "theta" )
    elif casename == "BOMEX"  : self.set_("moistshcv", "ZUVTHLMR", "qt", "thetal")
    elif casename == "SANDU"  : self.set_("moistshcv", "ZUVTHLMR", "qt", "thetal")
    elif casename == "RICO"   : self.set_("moistshcv", "ZUVTHDMR", "rv", "theta" )
    elif casename == "SCMS"   : self.set_("moistshcv", "ZUVTHDMR", "rv", "theta" )
    elif casename == "LBA"    : self.set_("dcv",       "ZUVTHDMR", "rv", "theta" )
    elif casename == "AMMA"   : self.set_("dcv",       "ZUVTHDMR", "qv", "ta"    )
    elif casename == "GABLS1" : self.set_("stable",    "ZUVTHDMR", "rt", "theta" )
    elif casename == "GABLS4" : self.set_("stable",    "ZUVTHDMR", "qv", "theta" )
    elif casename == "AYOTTE" : self.set_("dryshcv",   "ZUVTHDMR", "rt", "theta" )
    elif casename == "IHOP"   : self.set_("dryshcv",   "ZUVTHDMR", "rv", "theta" )
    elif casename == "BLLAST" : self.set_("dryshcv",   "ZUVTHDMR", "rv", "theta" )

  # helper function to condensate writing
  def set_(self, type, keyword, flag_q, flag_t):
    self.type = type
    self.keyword = keyword
    self.flag_q  = flag_q
    self.flag_t  = flag_t

  def __str__(self):
    return "<Case %s/%s (%s) of type %s : init %s with %s and %s>"%(
            self.casename, self.subcasename, self.shortname,
            self.type, self.keyword, self.flag_q, self.flag_t)
