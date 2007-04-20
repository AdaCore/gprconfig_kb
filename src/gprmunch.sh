#!/bin/sh

if [ $# != 1 ]; then
  echo "Bad number of arguments";
  exit 2
fi

exch_file=$1

cp $exch_file $exch_file.saved

# Save stdin and use exchange file as input.
exec >&3
exec < $exch_file

# Save and set IFS to new line.
OLD_IFS=$IFS
IFS="
"

# Parse exchange file.
section='Unknown'
dep_files=""
bindsec='Unknown'
nm="nm-not-defined"
cc="cc-not-defined"
verbose=""

while read line; do
   case $line in
      "[MAIN BASE NAME]")  section="base name" ;;
      "[COMPILER PATH]") section="discard" ;;
      "[COMPILER OPTIONS]") section="discard" ;;
      "[DEPENDENCY FILES]") section="dependency" ;;
      "[BINDING OPTIONS]") section="options" ;;
      "[VERBOSE]") verbose=y; section="Unknown" ;;
      \[*)  echo "Unknown section ($line)"; exit 1 ;;
      *) case $section in
        "discard") ;;
        "Unknown") echo "Malformed exchange file"; exit 1 ;;
        "base name") basename=$line ;;
        "dependency") dep_files="$dep_files $line" ;;
        "options")
           case $line in
              --nm=*) nm=`echo $line | sed -e "s/^--nm=//"` ;;
              --cc=*) cc=`echo $line | sed -e "s/^--cc=//"` ;;
              *) echo "Unknown binder option ($line)" ;;
           esac ;;
        *) echo "Internal error (section $section) unhandled"; exit 1 ;;
        esac
   esac
done

# Restore IFS and stdin.
IFS=$OLD_IFS
exec 3>&1
exec 3>&-

# Convert dependancy files to object files.
object_files=`echo $dep_files | sed -e 's/\\.d\$/.o/'`

# Do the real work.
$nm $object_files | munch > cpp__$basename.c
$cc -c cpp__$basename.c

# Generate the exchange file.
cat > $1 <<EOF
[GENERATED SOURCE FILES]
cpp__$basename.c
[GENERATED OBJECT FILE]
cpp__$basename.o
EOF
