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

# verbosity levels
ERROR=0
WARNING=1
INFO=2
DEBUG=3

def log(lev, msg, verb):
  if verb>=lev: print("%s %s"%(">"*(lev-1), msg))

def error(where,msg):
  print("> error in %s: %s"%(where,msg))
  exit(1)
def arg_error(msg):
  error("command line argument", msg)

def parse_time(date):
  year = int(date[0:4])
  month = int(date[5:7])
  day = int(date[8:10])
  hour = int(date[11:13])
  minute = int(date[14:16])
  second = int(date[17:19])
  return year, month, day, hour, minute, second

def T_to_theta(t,p):
  kappa=2./7.
  prref=100000.
  return  t*(prref/p)**(kappa)
