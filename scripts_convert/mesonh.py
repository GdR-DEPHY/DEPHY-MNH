# specific adaptations of default exseg to LES mode
preidea_LES = {
  "NAM_DIMn_PRE" : { "NIMAX" : "1024", 
                     "NJMAX" : "1024" },
  "NAM_CONF_PRE" : { "NVERB"    : "10",
                     "CIDEAL"   : "'RSOU'",
                     "LFORCING" : ".TRUE.",
                     "LPERTURB" : ".TRUE.", },
  "NAM_PERT_PRE" : { "CPERT_KIND" : "'WH'", },
}

exseg_LES = {
  "NAM_DYN"    : { "LNUMDIFU"   : ".FALSE.", },
  "NAM_DYNn"   : { "XTSTEP"     : "1.",
                   "CPRESOPT"   : "'ZRESI'", },
  "NAM_PARAMn" : { "CTURB"      : "'TKEL'",
                   "CCLOUD"     : "'LIMA'", },
  "NAM_TURBn"  : { "XIMPL"      : "0.",
                   "XKEMIN"     : "1E-10",
                   "CTURBLEN"   : "'DEAR'",
                   "CTURBDIM"   : "'3DIM'",
                   "LTURB_FLX"  : ".TRUE.",
                   "LTURB_DIAG" : ".TRUE.", 
                   "LSIGMAS"    : ".FALSE.",
                   "LRMC01"     :".TRUE.", },
  "NAM_PARAM_LIMA" : 
                 { "NMOM_C"     : "1",
                   "NMOM_R"     : "1",
                   "NMOM_I"     : "1",
                   "NMOD_CCN"   : "0",
                   "LACTI"      : ".FALSE.",
                   "NMOD_IFN"   : "0",
                   "LKESSLERAC" : ".TRUE.", },
  "NAM_PARAM_C2R2" :
                 { "HPARAM_CCN" : "'CPB'",
                   "HINI_CCN"   : "'CCN'",
                   "XCHEN"      : "0.173E+09",
                   "XKHEN"      : "1.403",
                   "XMUHEN"     : "0.834",
                   "XBETAHEN"   : "25.499",
                   "LRAIN"      : ".FALSE.",
                   "LSEDC"      : ".FALSE.", },
  "NAM_LES" :   { "LLES_MEAN"            : ".TRUE.",
                  "LLES_SUBGRID"         : ".TRUE.",
                  "LLES_RESOLVED"        : ".TRUE.",
                  "LLES_NEB_MASK"        : ".TRUE.", 
                  "LLES_CORE_MASK"       : ".TRUE.", 
                  "LLES_CS_MASK"         : ".TRUE.",
                  "XLES_TEMP_SAMPLING"   : "300.",
                  "XLES_TEMP_MEAN_START" : "0.",
                  "XLES_TEMP_MEAN_STEP"  : "3600." },
  "NAM_CONDSAMP": { "LCONDSAMP"          : ".TRUE.", 
                    "NCONDSAMP"          : "3"},
  # "NAM_BUDGET" :
#        CBUTYPE='CART',
#        NBUIL=1, NBUIH=512,
#        NBUJL=1, NBUJH=512,
#        NBUKL=1, NBUKH=118,
#        LBU_KCP=.FALSE.,
#        LBU_JCP=.TRUE.,
#        LBU_ICP=.TRUE.,
#        XBUWRI=1800.,
#        XBULEN=1800. /

#&NAM_BU_RTH LBU_RTH=.TRUE., CBULIST_RTH(1)='FRC', CBULIST_RTH(2)='ADV', 
#            CBULIST_RTH(3)='VTURB+HTURB',
#            CBULIST_RTH(4)='ADJU', 
#            CBULIST_RTH(5)='REVA+HENU+HON+SFR+DEPS+DEPG+IMLT+BERFI+RIM+ACC+CFRZ+WETG+DRYG+GMLT+DEPI+CORR+NECON+NEADV+NETUR+NEGA',
#            CBULIST_RTH(6)='INIF', CBULIST_RTH(7)='ENDF', CBULIST_RTH(8)='AVEF'/
#
#&NAM_BU_RRV LBU_RRV=.TRUE., CBULIST_RRV(1)='FRC', CBULIST_RRV(2)='ADV', CBULIST_RRV(3)='VTURB+HTURB',
#            CBULIST_RRV(4)='ADJU', 
#            CBULIST_RRV(5)='REVA+DEPS+DEPG+DEPI+HENU+CORR+NECON+NEADV+NETUR+NEGA', 
#            CBULIST_RRV(6)='INIF', CBULIST_RRV(7)='ENDF', CBULIST_RRV(8)='AVEF'/
#&NAM_BU_RU LBU_RU=.TRUE., CBULIST_RU(1)='FRC', CBULIST_RU(2)='COR', 
#            CBULIST_RU(3)='ADV', CBULIST_RU(4)='VTURB+HTURB',
#            CBULIST_RU(5)='PRES', 
#            CBULIST_RU(6)='INIF', CBULIST_RU(7)='ENDF', CBULIST_RU(8)='AVEF'/
#&NAM_BU_RV LBU_RV=.TRUE., CBULIST_RV(1)='FRC', CBULIST_RV(2)='COR', 
#            CBULIST_RV(3)='ADV', CBULIST_RV(4)='VTURB+HTURB',
#            CBULIST_RV(5)='PRES', 
#            CBULIST_RV(6)='INIF', CBULIST_RV(7)='ENDF', CBULIST_RV(8)='AVEF'/
#  "NAM_OUTPUT"     :
#                { "COUT_VAR(1,1)='UT'", COUT_VAR(1,2) = 'VT',
#            COUT_VAR(1,3) = 'WT', COUT_VAR(1,4) = 'THT',
#            COUT_VAR(1,5) = 'RVT', COUT_VAR(1,6)='RCT',
#            COUT_VAR(1,7)='RRT', COUT_VAR(1,8)='TKET',
#            COUT_VAR(1,9)='PABST',
#            COUT_VAR(1,10)='RIT', COUT_VAR(1,11)='RST',
#            COUT_VAR(1,12)='RGT',
#            XOUT_TIME_FREQ(1) = 3600.,
#            XOUT_TIME_FREQ_FIRST(1) = 3600. },
  }

# specific adaptations of default exseg to CRM mode
preidea_CRM = {}
exseg_CRM = {
  "NAM_PARAMn" : {
    "CTURB"             : "'TKEL'",
    "CSCONV"            : "'EDKF'",
  }
}

# specific adaptations of default exseg to SCM mode
preidea_SCM = {}
exseg_SCM = {}

######################################
## DEFAULT CONFIGS IN MESO-NH 5.6.2 ##
######################################

MODD_CONFIO__ = { # I/O file type
    "LCDF4"           : ".TRUE.",
    "LLFIOUT"         : ".FALSE.",
    "LLFIREAD"        : ".FALSE.",
}

MODD_CONFZ__  = { # parallelism
  "MPI_BUFFER_SIZE"   : "400",
}

MODD_BACKUP__ = { # backup files for restarts
  "XBAK_TIME_FREQ(1)" : "3600.0",
}

SURF_IDEAL_FLUX__ = { # surfex default namelist
  "NFORCF"            : "2",
  "NFORCT"            : "2",
  "XTIMEF"            : "0",
  "XTIMET"            : "0",
  "XSFTH"             : "0.",
  "CSFTQ"             : "'kg/m2/s'",
  "XSFTQ"             : "0.",
  "CUSTARTYPE"        : "Z0",
  "XUSTAR"            : "0.",
  "XZ0"               : "0.01",
  "XTSRAD"            : "273.15K",
}

MODD_CONF__   = { # general config
  "CCONF"             :"'START'",
  "LTHINSHELL"        : ".FALSE.",
  "L2D"               : ".FALSE.",
  "L1D"               : ".FALSE.",
  "LFLAT"             : ".FALSE.",
  "NMODEL"            : "1",
  "CEQNSYS"           : "'DUR'",
  "NVERB"             : "5",
  "CEXP"              : "'EXP01'",
  "CSEG"              : "'SEG01'",
  "LFORCING"          : ".FALSE.",
  "L2D_ADV_FRC"       : ".FALSE.",
  "L2D_REL_FRC"       : ".FALSE. ",
  "XRELAX_HEIGHT_BOT" : "0.",
  "XRELAX_HEIGHT_TOP" : "30000.",
  "XRELAX_TIME"       : "864000.",
  "LPACK"             : ".TRUE.",
  "NHALO"             : "1",
  "CSPLIT"            :"'BSPLITTING'",
  "NZ_PROC"           : "0",
  "NZ_SPLITTING"      : "10",
  "LLG"               : ".FALSE.",
  "LINIT_LG"          : ".FALSE.",
  "CINIT_LG"          : "'FMOUT'",
  "LNOMIXLG"          : ".FALSE.",
  "LCHECK"            : ".FALSE.",
}

MODD_CONFn__  = { # config for model n (nesting)
  "LUSERV"            : ".TRUE.",
  "LUSERC"            : ".FALSE.",
  "LUSERR"            : ".FALSE.",
  "LUSERI"            : ".FALSE.",
  "LUSERS"            : ".FALSE.",
  "LUSERG"            : ".FALSE.",
  "LUSERH"            : ".FALSE.",
  "LOCEAN"            : ".FALSE.",
  "LUSECI"            : ".FALSE.",
}

MODD_FRC__    = {   # forcings
  "LGEOST_UV_FRC"     : ".FALSE.",
  "LGEOST_TH_FRC"     : ".FALSE.",
  "LTEND_THRV_FRC"    : ".FALSE.",
  "LTEND_UV_FRC"      : ".FALSE.", 
  "LVERT_MOTION_FRC"  : ".FALSE.",
  "LRELAX_THRV_FRC"   : ".FALSE.",
  "LRELAX_UV_FRC"     : ".FALSE.",
  "LRELAX_UVMEAN_FRC" : ".FALSE.",
  "XRELAX_TIME_FRC"   : "10800.",
  "XRELAX_HEIGHT_FRC" : "0.",
  "CRELAX_HEIGHT_TYPE": "'FIXE'",
  "LTRANS"            : ".FALSE.",
  "XUTRANS"           : "0.0",
  "XVTRANS"           : "0.0",
  "LPGROUND_FRC"      : ".FALSE.",
  "LDEEPOC"           : ".FALSE.",
  "XCENTX_OC"         : "16000.",
  "XCENTY_OC"         : "16000.",
  "XRADX_OC"          :  "8000.",  
  "XRADY_OC"          :  "8000.",
}

MODD_LBCn__   = {  # boundary conditions for model n
  "CLBCX(1)"          :"'CYCL'",
  "CLBCX(2)"          :"'CYCL'",
  "CLBCY(1)"          :"'CYCL'",
  "CLBCY(2)"          :"'CYCL'",
  "NLBLX(:)"          : "1",
  "NLBLY(:)"          : "1",
  "XCPHASE"           : "20.",
  "XCPHASE_PBL"       : "0.",
  "XPOND"             : "1.0",
}

MODD_DYN__    = { # general dynamics
  "XSEGLEN"           : "43200.",
  "XASSELIN"          : "0.2",
  "XASSELIN_SV"       : "0.02",
  "LCORIO"            : ".TRUE.",
  "LNUMDIFU"          : ".TRUE.",
  "LNUMDIFTH"         : ".FALSE.",
  "LNUMDIFSV"         : ".FALSE.",
  "XALZBOT"           : "4000.",
  "XALKTOP"           : "0.01",
  "XALKGRD"           : "0.01",
  "XALZBAS"           : "0.01",
}

MODD_DYNn__   = { # dynamics for model n (nesting)
  "XTSTEP"            : "60.",
  "CPRESOPT"          : "'CRESI'",
  "NITR"              : "4",
  "LITRADJ"           : ".TRUE.",
  "LRES"              : ".FALSE.",
  "XRES"              : "1.E-07",
  "XRELAX"            : "1.",
  "LVE_RELAX"         : ".FALSE.",
  "LVE_RELAX_GRD"     : ".FALSE.",
  "XT4DIFU"           : "1800.",
  "XT4DIFTH"          : "1800.",
  "XT4DIFSV"          : "1800.",
}

MODD_ADVn__   = { # advection for model n
  "CUVW_ADV_SCHEME"   : "'CEN4TH'",
  "CMET_ADV_SCHEME"   : "'PPM_01'",
  "CSV_ADV_SCHEME"    : "'PPM_01'",
  "CTEMP_SCHEME"      : "'RKC4'",
  "NWENO_ORDER"       : "3",
  "NSPLIT"            : "1",
  "LSPLIT_CFL"        : ".TRUE.",
  "LSPLIT_WENO"       : ".TRUE.",
  "XSPLIT_CFL"        : "0.8",
  "LCFL_WRIT"         : ".FALSE.",
}

MODD_PARAMn__ = { # activate params for model n
  "CTURB"             : "'NONE'",
  "CRAD"              : "'NONE'",
  "CCLOUD"            : "'NONE'",
  "CDCONV"            : "'NONE'",
  "CSCONV"            : "'NONE'",
  "CELEC"             : "'NONE'",
  "CACTCCN"           : "'NONE'",
}

MODD_TURBn__  = { # config turbulence
  "XIMPL"             : "1.",
  "XKEMIN"            : "0.01",
  "XCEDIS"            : "0.84",
  "XCADAP"            : "0.5",
  "CTURBLEN"          : "'BL89'",
  "CTURBDIM"          : "'1DIM'",
  "LTURB_FLX"         :".FALSE.",
  "LTURB_DIAG"        :".FALSE.",
  "LSUBG_COND"        :".FALSE.",
  "CSUBG_AUCV"        :"'NONE'", 
  "CSUBG_AUCV_RI"     :"'NONE'",
  "LSIGMAS"           :".TRUE.",
  "LSIG_CONV"         :".FALSE.",
  "LRMC01"            :".FALSE.",
  "CTOM"              :"'NONE'",
  "VSIGQSAT"          : "0.02",
  "CCONDENS"          :"'CB02'",
  "CLAMBDA3"          :"'CB'",
  "CSUBG_MF_PDF"      :"'TRIANGLE'",
  "LLEONARD"          :".FALSE.",
  "XCOEFHGRADTHL"     : "1.0",
  "XCOEFHGRADRM"      : "1.0",
  "XALTHGRAD"         : "2000.0",
  "XCLDTHOLD"         : "-1.0",
}

MODD_RADn__   = { # config radiation
  "XDTRAD"            : "XTSTEP",
  "XDTRAD_CLONLY"     : "XTSTEP",
  "LCLEAR_SKY"        :".FALSE.",
  "NRAD_COLNBR"       : "1000",
  "NRAD_DIAG"         : "0",
  "CLW"               :"'RRTM'",
  "CAER"              :"'SURF'",
  "CAOP"              :"'CLIM'",
  "CEFRADL"           :"'MART'",
  "CEFRADI"           :"'LIOU'",
  "COPWSW"            : "'FOUQ'",
  "COPISW"            : "'EBCU'",
  "COPWLW"            : "'SMSH'",
  "COPILW"            : "'EBCU'",
  "XFUDG"             : "1.",
  "LAERO_FT"          :".FALSE.",
  "LFIX_DAT"          :".FALSE.",
}

MODD_PARAM_ECRAD__ = { # config radiation scheme
  "NSWSOLVER"         : "0",
  "NLWSOLVER"         : "0",
  "LSPEC_ALB"         : ".FALSE.",
  "LSPEC_EMISS"       : ".FALSE.",
  "SURF_TYPE"         : "'SNOW'",
  "NREG"              : "3",
  "NLWSCATTERING"     : "2",
  "NAERMACC"          : "0",
  "NOVLP"             : "1",
  "NLIQOPT"           : "3",
  "NICEOPT"           : "3",
  "NRADLP"            : "1",
  "NRADIP"            : "1",
  "XCLOUD_FRAC_STD"   : "1.0",
}

MODD_PARAM_LIMA__ = {  # config microphysics scheme
   "LPTSPLIT"         : ".TRUE.",
   "L_LFEEDBACKT"     : ".TRUE.",
   "L_NMAXITER"       : "5",
   "L_XMRSTEP"        : "0.005",
   "L_XTSTEP_TS"      : "20.",
   "YNUC"             : "1.0",
   "YALPHAC"          : "3.0",
   "YNUR"             : "2.0",
   "YALPHAR"          : "1.0",
   "LACTI"            : ".TRUE.",
   "OSEDC"            : ".TRUE.",
   "OACTIT"           : ".FALSE.",
   "LADJ"             : ".TRUE.",
   "LSPRO"            : ".FALSE.",
   "LKHKO"            : ".FALSE.",
   "ODEPOC"           : ".TRUE.",
   "LBOUND"           : ".FALSE.",
   "OACTTKE"          : ".TRUE.",
   "LKESSLERAC"       : ".FALSE.",
   "NMOM_C"           : "2",
   "NMOM_R"           : "2",
   "OVDEPOC"          : "0.02",
   "CINI_CCN"         : "'AER'",
   "CTYPE_CCN(:)"     : "'M'",
   "YAERDIFF"         : "0.0",
   "YAERHEIGHT"       : "2000.",
   "YFSOLUB_CCN"      : "1.0",
   "YACTEMP_CCN"      : "280.",
   "NMOD_CCN"         : "1",
   "LSCAV"            : ".FALSE.",
   "LAERO_MASS"       : ".FALSE.",
   "LCCN_HOM"         : ".TRUE.",
   "CCCN_MODES"       : "'COPT'",
   "XCCN_CONC(:)"     : "300.",
   "LHHONI"           : ".FALSE.",
   "LNUCL"            : ".TRUE.",
   "LSEDI"            : ".TRUE.",
   "YSNOW_T"          : ".FALSE.",
   "LMURAKAMI"        : ".TRUE.",
   "CPRISTINE_ICE_LIMA": "'PLAT'",
   "CHEVRIMED_ICE_LIMA": "'GRAU'",
   "XFACTNUC_DEP"     : "1.0",  
   "XFACTNUC_CON"     : "1.0",
   "NMOM_I"           : "2",
   "NMOM_S"           : "1",
   "NMOM_G"           : "1",
   "NMOM_H"           : "0",
   "NMOD_IFN"         : "1",
   "NIND_SPECIE"      : "1",
   "LMEYERS"          : ".FALSE.",
   "LIFN_HOM"         : ".TRUE.",
   "CIFN_SPECIES"     : "'PHILLIPS'",
   "CINT_MIXING"      : "'DM2'",
   "XIFN_CONC(:)"     : "100.",
   "NMOD_IMM"         : "0",
   "NPHILLIPS"        : "8",
   "LCIBU"            : ".FALSE.",
   "XNDEBRIS_CIBU"    : "50.0",
   "LRDSF"            : ".FALSE.",
}

MODD_PARAM_C2R2__ = {
    "HPARAM_CCN"      : "'XXX'",
    "HINI_CCN"        : "'XXX'",
    "XCHEN"           : "0.0",
    "XKHEN"           : "0.0",
    "XMUHEN"          : "0.0",
    "XBETAHEN"        : "0.0",
    "LRAIN"           : ".TRUE.",
    "LSEDC"           : ".TRUE.",
}

MODD_BUDGET__  = { # compute budgets
  "CBUTYPE"           : "'NONE'",
  "NBUMOD"            : "1",
  "XBULEN"            : MODD_DYN__["XSEGLEN"],
  "XBUWRI"            : MODD_DYN__["XSEGLEN"],
  "NBUKL"             : "1",
  "NBUKH"             : "0",        
  "LBU_KCP"           : ".TRUE.",
  "NBUIL"             : "1",
  "NBUIH"             : "0",      
  "NBUJL"             : "1",
  "NBUJH"             : "0",          
  "LBU_ICP"           : ".TRUE.",
  "LBU_JCP"           : ".TRUE.",
  "NBUMASK"           : "1",
}

MODD_LES__     = { # compute horizontal statistics 
  "LLES_MEAN"           : ".FALSE.",
  "LLES_RESOLVED"       : ".FALSE.",
  "LLES_SUBGRID"        : ".FALSE.",
  "LLES_UPDRAFT"        : ".FALSE.",
  "LLES_DOWNDRAFT"      : ".FALSE.",
  "LLES_SPECTRA"        : ".FALSE.",
  "CLES_NORM_TYPE"      : "'NONE'",
  "CBL_HEIGHT_DEF"      : "'KE'",
  "XLES_TEMP_MEAN_STEP" : "3600.",
  "LLES_CART_MASK"      : ".FALSE.",
  "LLES_NEB_MASK"       : ".FALSE.",
  "LLES_CORE_MASK"      : ".FALSE.",
  "LLES_MY_MASK"        : ".FALSE.",
  "LLES_CS_MASK"        : ".FALSE.",
}

MODD_CONDSAMP__ = { # conditional sampling
  "LCONDSAMP"         : ".FALSE.",
  "NCONDSAMP"         : "3",
  "XRADIO(:)"         : "900.",
  "XSCAL(:)"          : "1.",
  "XHEIGHT_BASE"      : "100.",
  "XDEPTH_BASE"       : "100.",
  "XHEIGHT_TOP"       : "100.",
  "XDEPTH_TOP"        : "100.",
  "NFINDTOP"          : "0",
  "XTHVP"             : "0.25",
  "LTPLUS"            : ".TRUE.",
}

MODD_DIMn_PRE__ = { # pre_idea dimensions
  "NIMAX" : "10", 
  "NJMAX" : "10" 
}

MODD_CONF_PRE__ = { # pre_idea config
  "LCARTESIAN" : ".TRUE.", 
  "LPACK"      : ".TRUE.", 
  "CEQNSYS"    : "'DUR'",
  "NVERB"      : "5",
  "CIDEAL"     : "'CSTN'",
  "CZS"        : "'FLAT'", 
  "LBOUSS"     : ".FALSE.", 
  "LPERTURB"   : ".FALSE.",
  "LFORCING"   : ".FALSE.",
  "LSHIFT"     : ".FALSE.",
  "L2D_ADV_FRC": ".FALSE.",
  "L2D_REL_FRC": ".FALSE.",
  "NHALO"      : "1",
  "JPHEXT"     : "1",
  "LOCEAN"     : ".FALSE.",
}

MODD_PERT_PRE__ = {
  "CPERT_KIND" : "'TH'",
  "XAMPLITH"   : "1.5",
  "XAMPLIRV"   : "0.0",
  "XAMPLIUV"   : "1.0834",
  "XAMPLIWH"   : "0.1",
  "NKWH"       : "2",
  "LSET_RHU"   : ".TRUE.",
  "XCENTERZ"   : "2000.",
  "XRADX"      : "10000.",
  "XRADY"      : "10000.",
  "XRADZ"      : "2000.",
  "LWH_LBXU"   : ".FALSE.",
  "LWH_LBYV"   : ".FALSE.",
}

default_preidea = {
  "NAM_CONFIO": MODD_CONFIO__,
  "NAM_CONFZ": MODD_CONFZ__,
  "NAM_DIMn_PRE": MODD_DIMn_PRE__,
  "NAM_CONF_PRE": MODD_CONF_PRE__,
  "NAM_PERT_PRE": MODD_PERT_PRE__,
  "NAM_CONFn": MODD_CONFn__,
}

default_exseg = {
  "NAM_CONFIO": MODD_CONFIO__,
  "NAM_CONFZ": MODD_CONFZ__,
  "NAM_BACKUP": MODD_BACKUP__,
  "NAM_IDEAL_FLUX": SURF_IDEAL_FLUX__,
  "NAM_CONF": MODD_CONF__,
  "NAM_CONFn": MODD_CONFn__,
  "NAM_FRC" : MODD_FRC__,
  "NAM_LBCn" : MODD_LBCn__,
  "NAM_DYN" : MODD_DYN__,
  "NAM_DYNn" : MODD_DYNn__,
  "NAM_ADVn" : MODD_ADVn__,
  "NAM_PARAMn" : MODD_PARAMn__,
  "NAM_TURBn" : MODD_TURBn__,
  "NAM_RADn" : MODD_RADn__,
  "NAM_PARAM_ECRAD" : MODD_PARAM_ECRAD__,
  "NAM_PARAM_LIMA" : MODD_PARAM_LIMA__,
  "NAM_PARAM_C2R2" : MODD_PARAM_C2R2__,
  "NAM_BUDGET" : MODD_BUDGET__,
  "NAM_LES" : MODD_LES__,
  "NAM_CONDSAMP" : MODD_CONDSAMP__,
}
