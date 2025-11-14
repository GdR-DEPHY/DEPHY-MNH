cd scripts_convert

listcas="ARMCU RICO BOMEX FIRE AMMA EUROCS KB2006 LBA IHOP AYOTTE GABLS1 GABLS4 RCEMIP DYNAMO"

for cas in $listcas ; do
  echo $cas
  bash test.sh "$cas"
done
