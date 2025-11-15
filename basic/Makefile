
build:
	dosbox -noconsole -exit -fastlaunch -c "mount c ." -c "c:" -c "cycles=max" -c "ASM.BAT" >nul 2>&1
	srec_cat INT.HEX -Intel BAS.HEX -Intel -o basic.bin -binary

disasm:
	z88dk-dis -mz80 -o 0x0000 basic.bin > dis.asm