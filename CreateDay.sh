#!/bin/bash
cp -R Template $1
cd $1
mv AdvCodeTemplate.lpi AdvCode$1.lpi
mv AdvCodeTemplate.lpr AdvCode$1.lpr
mv uAdvCodeTemplate.lfm uAdvCode$1.lfm
mv uAdvCodeTemplate.pas uAdvCode$1.pas
mv AdvCodeTemplate.ico AdvCode$1.ico

# Support for !(*.ico)
shopt -s extglob
sed -i '' -e "s/AdvCodeTemplate/AdvCode$1/g" !(*.ico)

cd ..
echo AdvCode$1 >> .gitignore
