##
# CHERI RISC-V Nanopass Elf builder
#
# @file
# @version 0.1

.DEFAULT_GOAL := example.elf

CHERI_DIR = $(HOME)/cheri
CHERI_BIN = $(CHERI_DIR)/output/sdk/bin

CC = $(CHERI_BIN)/clang --target=riscv64-unknown-elf -march=rv64imxcheri -mabi=l64pc128 -mno-relax
AS = $(CHERI_BIN)/clang --target=riscv64-unknown-elf -march=rv64imxcheri -mabi=l64pc128 -mno-relax -c

ELF_FLAGS = -T sail.ld -ffreestanding -nostdlib

BOOT_OBJECT = sail_boot.o

%.elf : %.o $(BOOT_OBJECT)
	$(CC) $(ELF_FLAGS) -o $@ $^

clean :
	rm -f *.o *.elf

# end
