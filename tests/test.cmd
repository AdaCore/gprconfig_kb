# Not a real GNAT compiler, so the GNAT_Runtime variable will be wrong in the output

test_config () {
  expected="$1"
  shift
  switches="$@"
  pwd=`pwd`
  ../gprconfig -o output.gpr -batch $switches | sed -e '/Creating configuration file/,$d'
  sed -e "s,$pwd,.," output.gpr > output2.gpr
  diff $expected output2.gpr >/dev/null
  if test $? != 0 ; then
     echo "-------  $expected failed"
     diff $expected output2.gpr
  fi
  rm output.gpr output2.gpr
}


pwd=`pwd`

## Testing lynxos
test_config test1.out -config "GNAT,$pwd/bin/,5.04,Ada,x86-lynxos"

## Testing windows
test_config test2.out -config "GNAT,$pwd/bin/,5.04,Ada,pentium-mingw32"

## Testing tru64
test_config test3.out -config "GNAT,$pwd/bin/,5.04,Ada,alphaev56-dec-osf5.1"

## Testing vms
test_config test4.out -config "GNAT,$pwd/bin/,5.04,Ada,alpha-openvms"

## Testing linux
test_config test5.out -config "GNAT,$pwd/bin/,5.04,Ada,i686-pc-linux-gnu"
test_config test5.out -config "GNAT,$pwd/bin/,5.04,Ada,i585-suse-linux"



