.DEFAULT_GOAL := example.elf

.PHONY = generate clean

#Variables
#arguments
MVersion=6t
MEmulator="off"
MVerbose="on"
CFLAGS=""

#local variables
varCurLoc=${PWD}
varCompile=Milestone$(MVersion)/main.rkt

COLLECTS=$(shell racket -e "(require setup/dirs) (printf (path->string (find-collects-dir)))")

define VERS
$(shell test '$(1)' -lt 5 2>/dev/null && echo yes || echo no)
endef

ifeq ($(call VERS, $(MVersion)),yes)
	varBuilder=Builders/elf-risc-v-builder/
else
	varBuilder=Builders/elf-cheri-risc-v-builder/
endif
varRiscBuilder=Builders/elf-risc-v-builder/
varCheriBuilder=Builders/elf-cheri-risc-v-builder/
varLinBuilder=Builders/elf-cheri-risc-v-builder-linear-cap/


varBuilder=$(varLinBuilder)
MEmulator="T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64"

compiler:
	@echo "Creating bytecode files"
	raco make $(varCompile)
	@echo "Creating compiler for latest version (Milestone"$(MVersion)")"
	raco exe --collects-path $(COLLECTS) ++lang "at-exp racket" -o compiler $(varCompile)

#Code
%.S: %.txt
	@echo "Only Compile"
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) $< 

ifeq ($(MEmulator), "off")
%.elf: %.txt
	@echo "No emulator"
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) $(CFLAGS) $<
	@echo "Make elf for " $(varCurLoc)/$(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/$@
else
ifeq ($(MVerbose), "off")
%.elf: %.txt
	@echo "non verbose"
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) $(CFLAGS) $<
	@echo "\nMake elf for " $(varCurLoc)/$(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/$@
	@echo "Run program: " $(varCurLoc)/$(varCurLoc)
	~/$(MEmulator) $(varCurLoc)/$@ -V
else
%.elf: %.txt
	@echo "Verbose"
	@echo $(CFLAGS)
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) $(CFLAGS) $<
	@echo "Make elf for " $(varCurLoc)/$(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/$@
	@echo "Run program: " $(varCurLoc)/$(varCurLoc)
	~/$(MEmulator) $(varCurLoc)/$@
endif
endif

exam:
	MEmulator="T/sail-cheri-riscv/c_emulator/cheri_riscv_sim_RV64"
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-cheri-riscv/c_emulator/cheri_riscv_sim_RV64 $(varCurLoc)/example.elf

examp:
	MEmulator="T/sail-cheri-riscv/c_emulator/cheri_riscv_sim_RV64"
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) example.txt
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-cheri-riscv/c_emulator/cheri_riscv_sim_RV64 $(varCurLoc)/example.elf

examl:
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 $(varCurLoc)/example.elf

exampl:
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) -o example.S example.txt
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 $(varCurLoc)/example.elf

examsl:
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) -s -o example.S example.txt
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 $(varCurLoc)/example.elf

examel:
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) -e -o example.S example.txt
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 $(varCurLoc)/example.elf

examcl:
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) -c -o example.S example.txt
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 $(varCurLoc)/example.elf

examtl:
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) -t -o example.S example.txt
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 $(varCurLoc)/example.elf

test:
	raco test Milestone$(MVersion)/*.rkt
combined:
	/home/kwinten/cheri/output/sdk/bin/clang --target=riscv64-unknown-elf -march=rv64imxcheri -mabi=l64pc128 -mno-relax    -c -o /media/kwinten/Data1/Thesis/New/Compiler/main.o /media/kwinten/Data1/Thesis/New/Compiler/main.S
	/home/kwinten/cheri/output/sdk/bin/clang --target=riscv64-unknown-elf -march=rv64imxcheri -mabi=l64pc128 -mno-relax    -c -o /media/kwinten/Data1/Thesis/New/Compiler/example.o /media/kwinten/Data1/Thesis/New/Compiler/example.S
	/home/kwinten/cheri/output/sdk/bin/clang --target=riscv64-unknown-elf -march=rv64imxcheri -mabi=l64pc128 -mno-relax    -c -o /media/kwinten/Data1/Thesis/New/Compiler/ap.o /media/kwinten/Data1/Thesis/New/Compiler/ap.S
	/home/kwinten/cheri/output/sdk/bin/clang --target=riscv64-unknown-elf -march=rv64imxcheri -mabi=l64pc128 -mno-relax    -c -o /media/kwinten/Data1/Thesis/New/Compiler/Builders/elf-cheri-risc-v-builder-linear-cap/sail_boot.o /media/kwinten/Data1/Thesis/New/Compiler/Builders/elf-cheri-risc-v-builder-linear-cap/sail_boot.S
	/home/kwinten/cheri/output/sdk/bin/clang --target=riscv64-unknown-elf-64 -march=rv64gcxcheri -mno-relax -T /media/kwinten/Data1/Thesis/New/Compiler/Builders/elf-cheri-risc-v-builder-linear-cap/sail.ld -nostdlib -o /media/kwinten/Data1/Thesis/New/Compiler/main.elf /media/kwinten/Data1/Thesis/New/Compiler/main.o /media/kwinten/Data1/Thesis/New/Compiler/example.o /media/kwinten/Data1/Thesis/New/Compiler/ap.o /media/kwinten/Data1/Thesis/New/Compiler/Builders/elf-cheri-risc-v-builder-linear-cap/sail_boot.o
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 /media/kwinten/Data1/Thesis/New/Compiler/main.elf
generate:
	raco make tester.rkt
	raco make Milestone6t/main.rkt
cleanbin:
	rm -rvf compiled/ 
	rm -rvf Milestone2/compiled/ 
	rm -rvf Milestone3/compiled/ 
	rm -rvf Milestone4/compiled/ 
	rm -rvf Milestone4c/compiled/ 
	rm -rvf Milestone5/compiled/ 
	rm -rvf Milestone6/compiled/ 
	rm -rvf Milestone6f/compiled/ 
	rm -rvf Milestone6s/compiled/ 
	rm -rvf Milestone6t/compiled/ 

cleanS:
	rm -fv *.S
cleanE:
	rm -fv *.elf
cleanCompiler:
	rm -fv compiler

cleanFiles: cleanS cleanE cleanCompiler

cleanTest:
	rm -fv tmpTest.txt
	rm -fv tmpTest.S
	rm -fv tmpTest.elf

clean: cleanFiles cleanbin

