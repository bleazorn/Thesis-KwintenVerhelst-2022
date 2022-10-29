#!/bin/bash
varRacket=Milestone3/main.rkt
varBuilder=$1
varEmulator=$2

while getopts e:v: flag
do
	case "${flag}" in
		e) varRacket=Milestone${OPTARG}/main.rkt;;
		v) varVerbose="-V"
	esac
done

if [ -z "$varBuilder" ]
then	
	echo "Builder is empty"
else
	if [ -z "$varEmulator" ]
	then
		echo "Emulator argument is empty"
	else
		echo "Builder: " $varBuilder 
		echo "Emulato: " $varEmulator
	fi
fi

file=runProgram.sh
cat <<< "#!/bin/bash" > $file
cat <<< "varRacket=$varRacket" >> $file
cat <<< "varBuilder=$varBuilder" >> $file
cat <<< "varEmulator=$varEmulator" >> $file

cat << 'EOF' >>$file
varCurLoc=${PWD}
echo $varCurLoc

if [ -z "$1" ]
then
	varFile="test.txt"
else
	varFile=$1
fi

tmp=$(echo $varFile| cut -d'.' -f 1)
varELF=$tmp.elf

echo $varRacket
echo "Compile from file"
racket $varRacket $varFile

echo $varBuilder
echo "Make elf"
make -C $varBuilder $varCurLoc/$varELF

echo $varEmulator
echo "Run emulator"
~/$varEmulator $varCurLoc/$varELF -V 

EOF

file=verboseRunProgram.sh
cat <<< "#!/bin/bash" > $file
cat <<< "varRacket=$varRacket" >> $file
cat <<< "varBuilder=$varBuilder" >> $file
cat <<< "varEmulator=$varEmulator" >> $file

cat << 'EOF' >>$file
varCurLoc=${PWD}
echo $varCurLoc

if [ -z "$1" ]
then
	varFile="test.txt"
else
	varFile=$1
fi

tmp=$(echo $varFile| cut -d'.' -f 1)
varELF=$tmp.elf

echo $varRacket
echo "Compile from file"
racket $varRacket $varFile

echo $varBuilder
echo "Make elf"
make -C $varBuilder $varCurLoc/$varELF

echo $varEmulator
echo "Run emulator"
~/$varEmulator $varCurLoc/$varELF

EOF

file=compiler.sh
cat <<< "#!/bin/bash" > $file
cat <<< "varRacket=$varRacket" >> $file
cat << 'EOF' >>$file

varCurLoc=${PWD}
echo $varCurLoc

if [ -z "$1" ]
then
	varFile="test.txt"
else
	varFile=$1
fi

tmp=$(echo $varFile| cut -d'.' -f 1)
varELF=$tmp.elf

echo $varRacket
echo "Compile from file"
racket $varRacket $varFile
EOF

