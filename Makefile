
all:
	rm tooly.nes
	rm ./nes-js/roms/tooly.nes
	asm6f tooly.asm tooly.nes
	cp tooly.nes ./nes-js/roms/tooly.nes
