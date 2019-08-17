all: clean tooly

tooly:
	asm6f tooly.asm tooly.nes
	cp tooly.nes ./nes-js/roms/tooly.nes

clean:
	rm tooly.nes
	rm ./nes-js/roms/tooly.nes
