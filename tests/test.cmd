# Not a real GNAT compiler, so the GNAT_Runtime variable will be wrong in the output

test_config () {
  expected="$1"
  shift
  switches="$@"
  ../gprconfig -o output.gpr -batch $switches
  diff $expected output.gpr >/dev/null
  if test $? != 0 ; then
     echo "-------  $expected failed"
     diff $expected output.gpr
  fi
  rm output.gpr
}


## Testing lynxos
test_config test1.out -config 'GNAT,/usr/foo/bin/,5.04,Ada,x86-lynxos'

## Testing windows
test_config test2.out -config 'GNAT,/usr/foo/bin/,5.04,Ada,pentium-mingw32'

## Testing tru64
test_config test3.out -config 'GNAT,/usr/foo/bin/,5.04,Ada,alphaev56-dec-osf5.1'

## Testing vms
test_config test4.out -config 'GNAT,/usr/foo/bin/,5.04,Ada,alpha-openvms'

## Testing linux
test_config test5.out -config 'GNAT,/usr/foo/bin/,5.04,Ada,i686-pc-linux-gnu'
test_config test5.out -config 'GNAT,/usr/foo/bin/,5.04,Ada,i585-suse-linux'



