for a in $(ls ./*.xsd)
do
echo -----------------------------
echo $a
/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/XsdToPas $a -t -s -p -c CopyrightInfo.inc -o ../
#echo 1

done