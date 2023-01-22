.DEFAULT_GOAL := example.elf

.PHONY = generate clean

#Variables
#arguments
MVersion=6s
MEmulator="off"
MVerbose="on"

#local variables
varCurLoc=${PWD}
varCompile=Milestone$(MVersion)/main.rkt

define VERS
$(shell test '$(1)' -lt 5 2>/dev/null && echo yes || echo no)
endef

ifeq ($(call VERS, $(MVersion)),yes)
	varBuilder=Builders/elf-risc-v-builder/
else
	varBuilder=Builders/elf-cheri-risc-v-builder/
endif

varLinBuilder=Builders/elf-cheri-risc-v-builder-linear-cap/
varBuilder=$(varLinBuilder)

#Code
%.S: %.txt
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) $< 

ifeq ($(MEmulator), "off")
%.elf: %.txt
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) $<
	@echo "Make elf for " $(varCurLoc)/$(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/$@
else
ifeq ($(MVerbose), "off")
%.elf: %.txt
	@echo
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) $<
	@echo "\nMake elf for " $(varCurLoc)/$(varCurLoc)
	make -C $(varBuilder) $(varCurLoc)/$@
	@echo "Run program: " $(varCurLoc)/$(varCurLoc)
	~/$(MEmulator) $(varCurLoc)/$@ -V
else
%.elf: %.txt
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) $<
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
	make -C $(varLinBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 $(varCurLoc)/example.elf

exampl:
	@echo "Compile program in: " $(varCurLoc)/$(varCurLoc)
	racket $(varCompile) example.txt
	@echo "\nMake elf for " $(varCurLoc)
	make -C $(varLinBuilder) $(varCurLoc)/example.elf
	@echo "Run program: " $(varCurLoc)
	~/T/sail-borrowed-cap/sail-cheri-riscv/ocaml_emulator/cheri_riscv_ocaml_sim_RV64 $(varCurLoc)/example.elf

test:
	raco test Milestone$(MVersion)/*.rkt

generate:
	raco make tester.rkt
	raco make Milestone2/main.rkt
	raco make Milestone3/main.rkt
	raco make Milestone4/main.rkt
	raco make Milestone4c/main.rkt
	raco make Milestone5/main.rkt
	raco make Milestone6/main.rkt
	raco make Milestone6f/main.rkt
	raco make Milestone6s/main.rkt
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

cleanS:
	rm -v *.S
cleanE:
	rm -v *.elf

cleanFiles: cleanS cleanE

cleanTest:
	rm -v tmpTest.txt
	rm -v tmpTest.S
	rm -v tmpTest.elf

clean: cleanFiles cleanbin

