# init dictionary to empty
Dict_new_varnames_all = {}

# helper function
def make_dic(cs):
  # fills dictionary
  # from Old_varnames and New_varnames lists
  dic={}
  for ov, nv in zip(Old_varnames, New_varnames):
    dic[ov] = nv
  Dict_new_varnames_all[cs] = dic

# cart (entire domain, no conditional sampling)
Old_varnames = ['MEAN_TH', 'MEAN_THV', 'MEAN_THL', 'MEAN_RR', 'MEAN_PRE', 'MEAN_U', 'MEAN_V', 'MEAN_RHO', 'MEAN_RC', 'MEAN_RV', 'MEAN_CF', 'MEAN_RT', 'MEAN_RI', 'MEAN_RS', 'MEAN_RG']
Old_varnames+= ['RES_W2', 'RES_U2', 'RES_V2', 'RES_WU', 'RES_WV', 'RES_THL2', 'RES_RT2', 'RES_WTHL', 'RES_WTHV', 'RES_KE', 'RES_WRT', 'RES_RV2', 'RES_WTH', 'RES_WRV', 'RES_TH2']
Old_varnames+= ['SBG_W2', 'SBG_U2', 'SBG_V2', 'SBG_WU', 'SBG_WV', 'SBG_THL2', 'SBG_RT2', 'SBG_WTHL', 'SBG_WTHV', 'SBG_TKE', 'SBG_WRT' ]
Old_varnames+= ['Q0','E0', 'Ustar', 'Wstar', 'L_MO','INST_PREC','ACCU_PREC']
Old_varnames+= ['AVG_PTS', 'BL_H', 'ZCF2TOT', 'ZCB', 'LWP', 'RWP', 'IWP', 'LWPVAR', 'SWP', 'GWP', 'ZMAXCF']
Old_varnames+= ['SWU', 'SWD', 'LWU', 'LWD']

New_varnames = ['theta', 'thetav', 'thl', 'rr', 'pf', 'u', 'v', 'rho', 'rl', 'rv', 'rneb', 'rt', 'ri', 'rs', 'rg']
New_varnames+= ['ww_res', 'uu_res', 'vv_res', 'uw_res', 'vw_res', 'thl2_res', 'rt2_res', 'wthl_res', 'wthv_res', 'tke_res', 'wrt_res', 'rv2_res', 'wth_res', 'wrv_res', 'th2_res']
New_varnames+= ['ww_sbg','uu_sbg','vv_sbg', 'uw_sbg','vw_sbg', 'thl2_sbg','rt2_sbg','wthl_sbg','wthv_sbg', 'tke_sbg', 'wrt_sbg']
New_varnames+= ['Q0','E0', 'ustar', 'wstar', 'lmo','sfce_rain','acc_sf_rain']
New_varnames+= ['avg', 'hpbl', 'zcftot', 'zcb', 'lwp', 'rwp', 'iwp', 'lwpvar', 'swp', 'gwp', 'zmaxcf']
New_varnames+= ['swu', 'swd', 'lwu', 'lwd']

make_dic("cart")

# neb, core and cs1: fractions + atmosphere mean variables
Old_varnames = ['AVG_PTS']
Old_varnames+= ['MEAN_TH','MEAN_THV','MEAN_THL','MEAN_RR','MEAN_PRE','MEAN_U','MEAN_V','MEAN_RHO','MEAN_RC', 'MEAN_RV','MEAN_RI','MEAN_RG','MEAN_RS','MEAN_W','MEAN_RT']

New_basename = ['avg']
New_basename+= ['theta','thetav','thl','rr','pf','u','v', 'rho','rl','rv','ri','rg','rs','w','rt']

New_varnames = [var+"_cld" for var in New_basename]
make_dic("neb")

New_varnames = [var+"_core" for var in New_basename]
make_dic("core")

New_varnames = [var+"_sam" for var in New_basename]
make_dic("cs1")

# cs2, cs3: fractions only
Old_varnames = ['AVG_PTS']
New_basename = ['avg']

New_varnames = [var+"_cs2" for var in New_basename]
make_dic("cs2")

New_varnames = [var+"_cs3" for var in New_basename]
make_dic("cs3")


# Bilans

# u, v
Old_varnames =['AVEF', 'ASSE', 'INIF', 'ENDF','FRC','ADV','PRES','VTURB','HTURB']

New_varnames =['U_budget_avg', 'U_budget_asse', 'U_budget_ini', 'U_budget_end','U_budget_frc','U_budget_adv','U_budget_pres','U_budget_vturb','U_budget_hturb']
make_dic("UU")

New_varnames =['V_budget_avg', 'V_budget_asse', 'V_budget_ini', 'V_budget_end','V_budget_frc','V_budget_adv','V_budget_pres','V_budget_vturb','V_budget_hturb']
make_dic("VV")

Old_varnames =['RAD', 'MAFL', 'CORR', 'GMLT', 'DRYG', 'SFR', 'NECON', 'DEPI', 'ADJU', 'HTURB', 'NETUR', 'DISSH', 'ADV', 'NEGA', 'NEADV', 'AVEF', 'ENDF', 'INIF', 'ASSE', 'VTURB', 'FRC', 'REVA', 'BERFI', 'IMLT', 'RIM', 'WETG', 'CFRZ', 'ACC', 'HIN', 'HON', 'DEPG', 'DEPS'] 
Old_varnames+=['WETH', 'HMLT', 'CEDS', 'HINC', 'HIND', 'HONC', 'HONR'] # lima only
New_varnames =['TH_budget_rad', 'TH_budget_mafl', 'TH_budget_corr', 'TH_budget_gmlt', 'TH_budget_dryg', 'TH_budget_sfr', 'TH_budget_necon', 'TH_budget_depi', 'TH_budget_adju', 'TH_budget_hturb', 'TH_budget_netur', 'TH_budget_dissh', 'TH_budget_adv', 'TH_budget_nega', 'TH_budget_neadv', 'TH_budget_avg', 'TH_budget_end', 'TH_budget_ini', 'TH_budget_asse', 'TH_budget_vturb', 'TH_budget_frc', 'TH_budget_reva', 'TH_budget_berfi', 'TH_budget_imlt', 'TH_budget_rim', 'TH_budget_wetg', 'TH_budget_cfrz', 'TH_budget_acc', 'TH_budget_hin', 'TH_budget_hon', 'TH_budget_depg', 'TH_budget_deps']
New_varnames+=['TH_budget_weth', 'TH_budget_hmlt', 'TH_budget_ceds', 'TH_budget_hinc', 'TH_budget_hind', 'TH_budget_honc', 'TH_budget_honr']
make_dic("TH")

Old_varnames = ['MAFL', 'AVEF', 'ASSE', 'INIF', 'ENDF', 'FRC', 'ADV', 'VTURB', 'HTURB', 'DEPI', 'DEPG', 'DEPS', 'REVA', 'CORR', 'NEADV', 'NEGA', 'NECON', 'NETUR', 'HIN', 'ADJU']
Old_varnames+=['CORR2', 'CEDS'] # lima only
New_varnames = ['Rv_budget_mafl', 'Rv_budget_avg', 'Rv_budget_asse', 'Rv_budget_ini', 'Rv_budget_end', 'Rv_budget_frc', 'Rv_budget_adv', 'Rv_budget_vturb', 'Rv_budget_hturb', 'Rv_budget_depi', 'Rv_budget_depg', 'Rv_budget_deps', 'Rv_budget_reva', 'Rv_budget_corr', 'Rv_budget_neadv', 'Rv_budget_nega', 'Rv_budget_necon', 'Rv_budget_netur', 'Rv_budget_hin', 'Rv_budget_adju']
New_varnames+=['Rv_budget_corr2', 'Rv_budget_ceds']
make_dic("RV")

Old_varnames =['AVEF', 'ASSE', 'INIF', 'ENDF', 'FRC', 'ADV', 'HTURB', 'VTURB', 'DRYG', 'DEPI', 'WETG', 'IMLT', 'ACCR', 'CORR', 'NEADV', 'NEGA', 'NECON', 'NETUR', 'HON', 'ADJU', 'AUTO', 'SEDI', 'RIM', 'CMEL', 'BERFI']
Old_varnames+=['DEPO', 'CORR2', 'HONC', 'CEDS', 'R2C1'] # lima only
New_varnames =['Rc_budget_avg', 'Rc_budget_asse', 'Rc_budget_ini', 'Rc_budget_end', 'Rc_budget_frc', 'Rc_budget_adv', 'Rc_budget_hturb', 'Rc_budget_vturb', 'Rc_budget_dryg', 'Rc_budget_depi', 'Rc_budget_wetg', 'Rc_budget_imlt', 'Rc_budget_accr', 'Rc_budget_corr', 'Rc_budget_neadv', 'Rc_budget_nega', 'Rc_budget_necon', 'Rc_budget_netur', 'Rc_budget_hon', 'Rc_budget_adju', 'Rc_budget_auto', 'Rc_budget_sedi', 'Rc_budget_rim', 'Rc_budget_cmel', 'Rc_budget_berfi']
New_varnames+=['Rc_budget_depo', 'Rc_budget_corr2', 'Rc_budget_honc', 'Rc_budget_ceds', 'Rc_budget_r2c1']
make_dic("RC")
