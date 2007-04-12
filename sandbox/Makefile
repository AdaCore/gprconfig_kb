GPRBUILD=gprbuild

ada_c_old_win32:
	$(GPRBUILD) --config=configs/ada_c_old.gpr -P../examples/first_steps/c_main
	$(GPRBUILD) --config=configs/ada_c_old.gpr -P../examples/first_steps/ada_main

vxworks6_ppc_kernel_gnat:
	$(GPRBUILD) --config=configs/vx6_ppc_kernel.cgpr -P../examples/first_steps/c_main
	$(GPRBUILD) --config=configs/vx6_ppc_kernel.cgpr -P../examples/first_steps/ada_main

vx_ccppc:
	$(GPRBUILD) --config=configs/vx_ccppc.cgpr -Pc_hello/c_hello


vxworks6_ppc_kernel_adacpp:
	$(GPRBUILD) --config=configs/vx6_ppc_kernel_adacpp.cgpr -P../examples/ada_cpp/animals

vxworks6_ppc_kernel_cpp:
	$(GPRBUILD) --config=configs/vx6_ppc_kernel_adacpp.cgpr -Pcpp_hello/cpp_hello
