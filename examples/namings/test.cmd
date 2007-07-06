gprbuild  main.adb -XC1=case1 -XC2=case1 -q
main-case1-case1
gprbuild  main.adb -XC1=case2 -XC2=case1 -q
main-case2-case1
gprbuild  main.adb -XC1=case2 -XC2=case2 -q
main-case2-case2
gprclean -Pnamings -XC1=case1 -XC2=case1 -q
gprclean -Pnamings -XC1=case2 -XC2=case1 -q
gprclean -Pnamings -XC1=case2 -XC2=case2 -q
