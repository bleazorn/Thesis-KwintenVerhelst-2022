.DEFAULT_GOAL := example.elf

.PHONY = generate clean

#Variables
#arguments
MVersion=4c
MEmulator="off"
MVerbose="off"

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


generate:
	raco make tester.rkt
	raco make Milestone2/main.rkt
	raco make Milestone3/main.rkt
	raco make Milestone4/main.rkt
cleanbin:
	rm -rvf compiled/ 
	rm -rvf Milestone2/compiled/ 
	rm -rvf Milestone3/compiled/ 
	rm -rvf Milestone4/compiled/ 

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

