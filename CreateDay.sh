#!/bin/bash
read -p 'Daynumber: ' day

cp -R Template $day
cd $day
mv AdvCodeTemplate.lpi AdvCode$day.lpi
mv AdvCodeTemplate.lpr AdvCode$day.lpr
mv uAdvCodeTemplate.lfm uAdvCode$day.lfm
mv uAdvCodeTemplate.pas uAdvCode$day.pas
mv AdvCodeTemplate.ico AdvCode$day.ico

# Support for !(*.ico)
shopt -s extglob
sed -i '' -e "s/AdvCodeTemplate/AdvCode$day/g" !(*.ico)

cd ..
echo AdvCode$day >> .gitignore
