#!/bin/bash
# Author: Wei Wang (ww2315)
# Date: Dec. 18, 2012

# Setup test environment
make
cp ./masl tests/
cd tests

# 1. Translate all masl source files to java files
echo "1. Translate all masl source files to java files"
rm -f translate.txt
fails1=()
for f in `ls *.masl`
do	
	echo "Translating $f"
	./masl -t $f >> translate.txt 2>&1
	name=$(echo $f | sed 's/\.masl$//g')
	if [ -f "$name.java" ]
	then
		echo ""
	else
		fails1+=($f)
	fi
done
fails1Count=${#fails1[@]}
if [ $fails1Count != 0 ]
then
	echo "Cannot translate files below:"
	for var in "${fails1[@]}"
		do
		echo "${var}"
		done
else
	echo "All masl files are translated to java files"
fi

# 2. Compile all masl source files to class files
echo "2. Compile all masl source files to class files"
fails2=()
rm -f compile.txt
for f in `ls *.masl`
do	
	echo "Compiling $f"
	./masl -c $f >> compile.txt 2>&1
	name=$(echo $f | sed 's/\.masl$//g')
	if [ -f "$name.class" ]
	then
		echo ""
	else
		fails2+=($f)
	fi
done
fails2Count=${#fails2[@]}
if [ $fails2Count != 0 ]
then
	echo "Cannot compile files below:"
	for var in "${fails2[@]}"
		do
		echo "${var}"
		done
else
	echo "All masl files are compiled to class files"
fi

# 3. Compare the output of compiled programs and expected output
echo "3. Compare the output of compiled programs and expected output"
fails3=()
for f in `ls *.masl`
do	
	name=$(echo $f | sed 's/\.masl$//g')
	echo "Start: $name"
	java $name > $name.output.txt
	dif=`diff $name.out $name.output.txt`
	if [ -z "$dif" ]
	then
		echo ""
	else
		fails3+=($f)
	fi
	echo "End: $name"
done
fails3Count=${#fails3[@]}
if [ $fails3Count != 0 ]
then
	echo "The output of following files doesn't match expected output"
	for var in "${fails3[@]}"
		do
		echo "${var}"
		done
else
	echo "All output of masl programs matches expected output"
fi
