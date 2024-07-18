# Copyright (C) 2024 Météo-France
# Copyright (C) 2024 Centre National de la Recherche Scientifique
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

# thermo constants
# Gravity acceleration (m s-2)
g=9.80665
# Boltzman constant (J K-1)
boltzman=1.380658e-23
# Avogadro number (mol-1)
avogadro=6.0221367e+23
# Gaz constant (J mol-1 K-1)
R=avogadro*boltzman
# Dry air molar mass (g mol-1)
Md=28.9644
# Water vapor molar mass (g mol-1)
Mv=18.0153
# Dry air gaz constant (J kg-1 K-1)
Rd=1000.*R/Md
# Water vapor gaz constant (J kg-1 K-1)
Rv=1000.*R/Mv
# Reference pressure
p0 = 100000.
# Dry air specific capacity at constant pressure (J kg-1 K-1)
Cpd=3.5*Rd
# Water vapor specific capacity at constant pressure (J kg-1 K-1)
Cpv=4.*Rv
# Latent heat of liquid water vaporization (J kg-1) at 0°C
Lv=2.5008e6
# Latent heat of solid water sublimation (J kg-1) at 0°C
Ls=2.8345e6
# Latent heat of solid water fusion (J kg-1) at 0°C
Lf=Ls-Lv
# Derived constant
kappa=Rd/Cpd
# atmo scale for basic pressure profile (m)
H_atm = 8000

def log(lev, msg, verb):
  if verb>=lev: print("%s %s"%(">"*(lev), msg))
  if lev==0:exit(1)

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

def time_to_secs(date):
  return 3600*date.hour + 60*date.minute + date.second
def str_time(date):
  """ format YYYY MM DD seconds """
  ts = time_to_secs(date)
  return "%s %i\n"%(date.strftime("%Y %m %d"), ts)

def T_to_theta(t,p):
  prref=p0
  return  t*(prref/p)**(kappa)

def theta_to_T(theta,p):
  prref=p0
  return  theta*(p/prref)**(kappa)

def P_to_z(name_var_t, var_t, name_var_q, var_q, var_p, zs):
    import numpy as np
    """Compute the altitude of a given set of pressure levels
    (func copied and adapted from dephy-scm/dephycf/thermo.py)
    """

    if name_var_t != "ta":
        ta = theta_to_T(var_t, var_p).squeeze()
    else: ta=1*var_t.squeeze()

    if not "q" in name_var_q:
      # convert from mixing ratio to specific humidity
      q = (var_q/(1+var_q)).squeeze()
    else: q = 1*var_q.squeeze()

    _,nlev = var_t.shape
    R = Rd+q*(Rv-Rd)

    z = ta*0. ; z[0] = zs
    for ilev in range(1,nlev):
        dz = (R[ilev-1]*ta[ilev-1]+R[ilev]*ta[ilev])/(2.0*g)*(np.log(var_p[ilev-1])-np.log(var_p[ilev]))
        z[ilev] = z[ilev-1] + dz
    return z

def interp_plev_to_zlev(zz, pp, plev):
    """Compute the altitude of a given pressure level (interpolation)
    (func copied and adapted from dephy-scm/dephycf/thermo.py)
    """

    nlev, = zz.shape
    zlev = 0
    # increasing altitude and decreasing pressure order
    if pp[0] < pp[1]: p = pp[::-1]
    else: p = pp
    if zz[0] > zz[1]: z = zz[::-1]
    else: z = zz

    for ilev in range(0,nlev-1):
      if p[ilev] >= plev and p[ilev+1] <= plev:
        zlev = z[ilev] + (z[ilev+1]-z[ilev])/(p[ilev+1]-p[ilev])*(plev-p[ilev])
    return zlev

def interp_zlev_to_plev(pp, zz, zlev):
    """Compute the pressure of a given altitude level (interpolation)
    inputs:
       pp = vector of pressures
       zz = vector of altitudes corresponding to p
     zlev = altitude at which the pressure is computed
    """
    nlev, = pp.shape
    # increasing altitude and decreasing pressure order
    if pp[0] < pp[1]: p = pp[::-1]
    else: p = pp
    if zz[0] > zz[1]: z = zz[::-1]
    else: z = zz
    if zlev < z[0] or zlev > z[-1]:
      import numpy as np
      #import matplotlib.pyplot as plt
      # p = p0 exp(-z/H)
      # ln(p/p0) = -z/H
      # H = -z/ln(p/p0)
      H = -(z-z[0])/np.log(p/p[0])
      #plt.plot(H,z); plt.show(); exit()
      H = np.mean(H[-10:])
      plev = p[0]*np.exp(-(zlev-z[0])/H) #float("nan")
      #plt.plot(p,z,label="p,z")
      #plt.plot(p[0]*np.exp(-(z-z[0])/H),z, label="exp")
      #plt.legend()
      #plt.show()
    for ilev in range(0,nlev-1):
      if z[ilev] <= zlev and z[ilev+1] > zlev: # zlev is between ilev and ilev+1
        plev = p[ilev] + (p[ilev+1]-p[ilev])/(z[ilev+1]-z[ilev])*(zlev-z[ilev])
        break
    print(z[ilev], z[ilev+1], zlev, plev)
    return plev

def bilin_interp(var, t, z, newt, newz):
    import numpy as np
    from scipy.interpolate import RegularGridInterpolator
    nt = len(t); nz = len(z)
    nnt = len(newt); nnz = len(newz)
    nvar = np.zeros((nnt, nnz))
    interp = RegularGridInterpolator((t, z), var, bounds_error=False, fill_value=0.)
    for i, tt in enumerate(newt):
        for j, zz in enumerate(newz):
            nvar[i,j] = interp([tt,zz])
    return nvar
