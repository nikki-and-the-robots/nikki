#!/bin/bash

# to abort in case of errors
function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo This is just for testing. No real deployment yet!
echo \(libraries are not deployed.\)

if [ -d nikki ]
then
    echo removing nikki
    rm -rf nikki
fi

mkdir nikki
echo copying executables...
cp dist/build/nikki/nikki nikki/
cp dist/build/core/core nikki/
echo copying data...
cp -r ../data nikki/
touch nikki/yes_nikki_is_deployed
