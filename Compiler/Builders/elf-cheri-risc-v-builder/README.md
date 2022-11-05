# Sail ELF builder

This tool contains five files.

## `sail_boot.S`

Initializes the following things:
- registers to null
- PMP
- Address translation
- Trap delegation
- Simple trap handler

At the end, it puts the machine in user mode and jumps to the `main` symbol.
`main` is assumed to be defined in another file, so the linker can fill in the address.

## `example.S`

Very simple example as sanity check.
Contains the `main` label which `sail_boot.S` will jump to.
When run it should return zero.

## `sail.ld`

Linker script, see https://blog.thea.codes/the-most-thoroughly-commented-linker-script/ for a good guide.

## `encoding.h`

Header file with a long list of defined constants for RISC-V.

## ```Makefile```

Automates the building of the ELF files.

Simply running `make` will compile `example.elf`.
To compile and link a file named `test.S`, run `make test.elf` which will produce `test.elf`.
