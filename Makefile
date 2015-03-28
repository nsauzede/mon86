# Makefile for mymon 512 byte contest

# depends on gcc, nasm (version 2.05.01) and qemu (for make check/checkr)

# 'make all' should build rom and disk sector entries
# 'make check' will test disk sector with qemu
# 'make checkr' will test rom with qemu

# you can also inspect all 20 bit memory space by moving the cursor left/right (h,l)
# through the dump address, and decrement/increment the current byte (j,k)

# note that the cursor wraps around from the left to the data area (on the right),
# where you can also alter the memory contents with same keys (j,k,h,l)

# beware ! you may damage your system by tinkering with the memory contents !
# YOU HAVE BEEN WARNED !!

# some funny things to try :

# when booted into mymon (whatever rom or floppy version),
# one can reboot and single step every single instructions 
# of the BIOS like this : (type the following key sequences, without the ' ')
# j h j h h j j j j j j j j j j j j j j j j
# => now, the dump address should show F000:FFFF (BIOS entry point)
# then type :
# x <space>
# => now, you should see the registers animating,
# as your BIOS executes instrution per instruction.. enjoy! :o)

# you can also write a little hex program and execute it :
# => modify the dump address to : (see above for keys)
# 0001:0000
# => this points to writeable area of RAM (at address=1MByte)
# modify the four first data bytes to : (beware, data bytes go from right to left)
# .. .. FF FC E9 40 <=== righmost data bytes ; here start ASCII equivalent => @éüÿ...
# => that would disassemble to :
# 40     inc ax
# E9FCFF jmp $-4
# (infinite loop that increments ax)
# => finally, executes :
# x <space>
# => now you should see the infinite loop animating, and ax increments forever.. enjoy! :o)

# (C) Nicolas Sauzede 2009 (nsauzede@laposte.net)

ROM=	mon.rom
SEC=	mon.sec

TARGET= mkrom sum $(ROM) $(SEC)

QEMU=	qemu
#QEMU+=	-M isapc
QEMU+=	-m 3
NASM=	nasm

CFLAGS=-Wall -Werror
CFLAGS+=-g -O0 -fno-omit-frame-pointer

ifdef T
QEMU+=-S -s
endif

ifdef D
AFLAGS+=-DDEBUG
endif

ifdef C
AFLAGS+=-DCUSTOM_ADDR
endif

all: $(TARGET)

a:CFLAGS+=-m32
a:LDFLAGS+=-m32

%.sec:	%.asm
	$(NASM) -o $@ -DSECTOR $(AFLAGS) $<

%.rom:	%.asm
	$(NASM) -o $@ -DROM $(AFLAGS) $<
	mv -f $@ temp.rom
	./mkrom temp.rom $@
	./sum $@

%.opt:	%.rom
	./mkrom $< $@

check:	$(SEC)
	$(QEMU) -fda $< -boot a

checkr:	mkrom sum $(ROM)
	$(QEMU) -option-rom $(ROM) /dev/zero

clean:
	$(RM) $(TARGET) *.sec *.rom *.opt
