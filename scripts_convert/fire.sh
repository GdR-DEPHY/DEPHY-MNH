out=../output_namelists/
cmd="python convert.py -c FIRE -i ../../dephy-scm -o ../output_namelists/"

$cmd -m SCM  -g ../grilles/grille_AROME_CL_25m.txt # edkf AROME 25m
mv $out/conf_PRE_IDEA_FIRE_edkf_SCM.nam $out/conf_PRE_IDEA_FIRE_edkf_25m_SCM.nam
mv $out/conf_EXSEG00_FIRE_edkf_SCM.nam $out/conf_EXSEG00_FIRE_edkf_25m_SCM.nam 
$cmd -m SCM  -g ../grilles/grille_AROME_CL.txt     # edkf AROME 5m
$cmd -m SCM  -g ../grilles/grille_AROME_CL.txt -e  # noedkf AROME 5m

$cmd -m CRM  -g ../grilles/grille_AROME_CL_25m.txt # edkf AROME 25m
mv $out/conf_PRE_IDEA_FIRE_edkf_CRM.nam $out/conf_PRE_IDEA_FIRE_edkf_25m_CRM.nam
mv $out/conf_EXSEG00_FIRE_edkf_CRM.nam $out/conf_EXSEG00_FIRE_edkf_25m_CRM.nam 
$cmd -m CRM  -g ../grilles/grille_AROME_CL.txt     # edkf AROME 5m
$cmd -m CRM  -g ../grilles/grille_AROME_CL.txt -e  # noedkf AROME 5m
