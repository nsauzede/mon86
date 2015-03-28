; MyMon : low level 8086 monitor / debugger
; (C) Nicolas Sauzede 2009 (nsauzede@laposte.net)

; compile with gcc, nasm version 2.05.01; test with qemu (see Makefile)

; should theoretically run on any x86 cpu family, including original 8086/8088.
; very rudimentary, but fits a single floppy (or ROM extension) 512-byte sector.
; dumps all registers content plus stack/code bytes (no disassembly though).
; dumps memory at specified address (can be interactively modified).

; the rationale behind not using any single service from BIOS, beyond the
; technical challenge, is because it hence allows to debug the BIOS itself,
; without any reentrancy issue.

; the following is in fact build-conditional : (see USE_PROMPT define)
; at boot, ROM version prints a prompt and waits for keypress..
; press 'scroll lock' to activate the monitor, within a given timeout.
; the prompt is displayed in red at the left top of the screen
; with subsequent dots printing during timeout..
; after timeout elapses, ROM version will continue normal boot process.
; sector version will automatically activate the monitor

; when the monitor is entered, the cpu has been reset to the BIOS entry point
; (0xffff:0x0000) such that the entire startup sequence can be traced.

; controls : (some must be enabled via USE_XX defines)
;  's'   step next insn (execute exactly one)
;  space animate insns (toggle autostep)
;  <any> fast animate (while in autostep mode)
;  'h'   move cursor to the left
;  'l'   move cursor to the left
;  'j'   increment byte under cursor
;  'k'   decrement byte under cursor
;  'u'   animate until the dump address is reached
;  'x'   goto dump address and execute instruction (single step)
;  esc   proceed execution (stop debugging and leave monitor)

; keys have been chosen to work (hopefully) with any keyboard layouts

%define SPECIFIC_KB ; use kb that generate only one scancode per nonextended press/release

;%define USE_PROMPT ; prompts at boot and wait for 'scroll lock'
%define TIMEOUT 5 ; this is is the timeout to enter the monitor (in seconds)
%define USE_ANIMATE		; +8 adds the 'space' key to 'animate'
;%define USE_FAST ; adds the 'z' key to make 'animate' faster +12
%define USE_DUMP ; adds the 'h'/'j'/'k' keys to modify dump addr +45
;%define USE_UNTIL ; adds the 'u' key to continue until the chosen address is reached (breakpoint like)
; seems to hang after F000:E0B7,
; code was 30382C90
; at F000:FF53 (from ax)
; => reason : at boot, BIOS overwrite interrupt handlers, comprising INT1 ! arggh
%define USE_EXECUTE 		; +14 adds the 'x' key to transfer dump address into cs:ip and single step instruction

%if 1
;%define AUTO_REBOOT ; auto reboot at start
%else
;%define USE_REBOOT ; adds the 'r' key to reboot
%endif

%define DUMP_MEM			; dump memory at custom location
%define SHOW_CURSOR			; +14 shows dump cursor
%define CURSOR_LE			; cursor begins on the right of the address
%define USE_DATACURSOR		; +23 cursor walks data too
%define USE_BIDIRCURSOR		; +6 cursor goes both sides

%define OPTIM_DISP			; -8
%define OPTIM_REGS			; -5
%define OPTIM_BYTES			; -7

; debug options (dangerous)
;%define DEBUG ; don't use this, will crash
;%define GREYSCALE ; just to simulate output on greyscale monitors

%warning "Options report :"
%ifdef SPECIFIC_KB
%warning .SPECIFIC_KB
%endif
%ifdef USE_PROMPT
%warning .USE_PROMPT
%warning .TIMEOUT=TIMEOUT
%endif
%ifdef USE_ANIMATE
%warning .USE_ANIMATE
%endif
%ifdef USE_FAST
%warning .USE_FAST
%endif
%ifdef USE_DUMP
%warning .USE_DUMP
%endif
%ifdef USE_UNTIL
%warning .USE_UNTIL
%endif
%ifdef USE_EXECUTE
%warning .USE_EXECUTE
%endif
%ifdef AUTO_REBOOT
%warning .AUTO_REBOOT
%endif
%ifdef USE_REBOOT
%warning .USE_REBOOT
%endif
%ifdef DUMP_MEM
%warning .DUMP_MEM
%endif
%ifdef SHOW_CURSOR
%warning .SHOW_CURSOR
%endif
%ifdef OPTIM_DISP
%warning .OPTIM_DISP
%endif
%ifdef OPTIM_REGS
%warning .OPTIM_REGS
%endif
%ifdef OPTIM_BYTES
%warning .OPTIM_BYTES
%endif
%ifdef DEBUG
%warning .DEBUG
%endif
%ifdef GREYSCALE
%warning .GREYSCALE
%endif

; this is where we put some persistent data (within segment 0)
%define DATA 0x7e00
%define STATUS DATA+1 ; 1 byte
 %define INSTEP   1 ; currently animating
%ifdef USE_FAST
 %define STEPFAST 2 ; use fast animate speed
%endif
%ifdef USE_DUMP
 %define DMINUS 4 ; dec dump digit
 %define DPLUS 8 ; inc dump digit
%define BASE_POS DATA+0 ; 1 byte
%endif
%ifdef USE_UNTIL
%if 0
 %define UNTIL 16 ; until (breakpoint like)
%else
 %define UNTIL 1 ; until (breakpoint like)
%endif
%endif
%define BASE_OFFSET DATA+2 ; 1 word
%define BASE_SEGMENT DATA+4 ; 1 word

%ifdef ROM
ORG 0
%else
ORG 0x7C00
%endif

BITS 16
CPU 8086		; this is a REAL challenge !!!! believe me ;-)

; some AT kb, generate only one scancode per press/release events
; NOTE : tested this only with qemu
%ifdef SPECIFIC_KB
; 01 esc
; 1C enter
; 1F s
; 2e c
; 39 space
%define _RELEASED 0x80
%define ESC 0x01
%define RESC (_RELEASED | ESC)
%define ENTER 0x1C
%define RENTER (_RELEASED | ENTER)
%define SPACE 0x39
%define RSPACE (_RELEASED | SPACE)
%define SCROLL 0x46
%define RSCROLL (_RELEASED | SCROLL)
%define KEY_A 0x10
%define RKEY_A (_RELEASED | KEY_A)
%define KEY_S 0x1F
%define RKEY_S (_RELEASED | KEY_S)
%define KEY_G 0x22
%define RKEY_G (_RELEASED | KEY_G)
%define KEY_H 0x23
%define RKEY_H (_RELEASED | KEY_H)
%define KEY_J 0x24
%define RKEY_J (_RELEASED | KEY_J)
%define KEY_K 0x25
%define RKEY_K (_RELEASED | KEY_K)
%define KEY_L 0x26
%define RKEY_L (_RELEASED | KEY_L)
%define KEY_U 0x16
%define RKEY_U (_RELEASED | KEY_U)
%ifdef USE_FAST
%define KEY_Z 0x11
%define RKEY_Z (_RELEASED | KEY_Z)
%endif
%define LSHIFT 0x2A
%define RLSHIFT (_RELEASED | LSHIFT)
%define KEY_X 0x2D
%define RKEY_X (_RELEASED | KEY_X)
%define RSHIFT 0x36
%define RRSHIFT (_RELEASED | RSHIFT)
%ifdef USE_DUMP
%define BACKSPACE 0x0E
%define RBACKSPACE (_RELEASED | BACKSPACE)
%define PLUS 0x4E
%define RPLUS (_RELEASED | PLUS)
%define MINUS 0x4A
%define RMINUS (_RELEASED | MINUS)
%endif
%ifdef USE_REBOOT
%define KEY_R 0x13
%define RKEY_R (_RELEASED | KEY_R)
%endif
%else
%error "kb not implemented"
%define EXTENDED 0xE0
%endif

%define K_ACTIVATE RSCROLL
%define K_STEP KEY_S
%define K_ANIMATE RLSHIFT
%ifdef CURSOR_LE
%define K_CURSORP KEY_H
%define K_CURSORM KEY_L
%else
%define K_CURSORP KEY_L
%define K_CURSORM KEY_H
%endif
%define K_INC KEY_K
%define K_DEC KEY_J
%define K_LEAVE RESC
%define K_UNTIL RKEY_U

; this provides roughly 1 second timebase
; dependent on default timer freq
%define HZ 0x18

%define DEFAULT_ATTR 0x0C
;%define DEFAULT_ATTR 0xB8
%define SCOLON ((DEFAULT_ATTR << 8) | ':')
%define SSP ((DEFAULT_ATTR << 8) | ' ')
%define SSTEP ((DEFAULT_ATTR << 8) | 'S')
%define SDOT ((DEFAULT_ATTR << 8) | '.')

; boot stack contents :

; @ffda
; sec : 0001 0000 0000 0000 0000 0000 0000 0000
;       ffff 0000 7c00 0000 0040 0000 9fc0 fff6
;       ab78 0000 0000

; @ffee
;       [ret far]   ??  ds<.
; rom : b70a f000 0003 d000 fdba 0000 e000 e2a3
;       0000

;---------start of code---------------------------------

start:

; assume ds=ss=0

%ifdef ROM
db 0x55,0xaa,0x00
%endif

; execution entry point

entry:

%ifdef GREYSCALE
; greyscale (debug)
mov ax,0x1200
mov bl,0x33
int 0x10
mov ah,0xf
int 0x10
mov ah,0
int 0x10
%endif

%ifdef AUTO_REBOOT
%ifdef SECTOR ; for sector, we do this unconditionaly because once a drive is booted
; we can't skip and continue boot process.
; this one works, reboot and trace first BIOS POST insn
xor ax,ax
dec ax
push ax ; we push ffff
inc ax
push ax ; we push 0
; assume ax=0
%endif
%endif

%ifdef USE_PROMPT
xor di,di
mov si,sgreet
%define SGREET_LEN (sgreet1-sgreet)
mov cx,SGREET_LEN
push cs ; 9*2=18
pop ds

%ifdef OPTIM_DISP
.looprint:
call dprint ; 6+10=16
%else
mov dx,0xB800
mov es,dx
mov ah,DEFAULT_ATTR
.looprint:
lodsb
stosw
%endif

loop .looprint
; assume cx=0

; disable all intr except timer
call diskbint

; assume cx=0
mov bh,TIMEOUT
; poll kb
.pollkb:
mov cl,HZ
.resume:
call getchwto
cmp al,K_ACTIVATE
je .endpoll
.nactivate:
mov ax,SDOT
stosw
;loop .pollkb
dec bh
jnz .pollkb
.timeout:
.endpoll:

; restore old intr mask
call restorekbint

jcxz .skip ; cx==0 means timeout

%endif

.activate:

;%ifdef ROM
xor ax,ax
;%endif

; assume ax=0
; assume ds=cs
; assume es=b800
; assume ss=0
mov es,ax
; assume es=0

; init static data
; assume ax=0
%ifdef AUTO_REBOOT
%ifdef ROM
; in case of ROM we only reboot when activated
; this one works, reboot and trace first BIOS POST insn
dec ax
push ax ; we push ffff
inc ax
push ax ; we push 0
%endif
%endif

mov di,DATA
;mov al,2
stosw
;xor ax,ax
stosw
;mov ax,0xffff
;dec ax
stosw

; hook int1 (single step)
mov di,1*4
%ifdef SECTOR
mov ax,traphandler
%else
; assume ax=0
mov al,traphandler ; this optim is because the offset is <= 0xFF
%endif
stosw
mov ax,cs
stosw

%ifdef AUTO_REBOOT
%if 0 ; warm boot
%if 0
mov ax,0x1234
mov di,0x40
mov es,di
mov di,0x72
stosw
%else
mov ax,0x40
mov ds,ax
mov word[0x72],0x1234
%endif
%endif
mov ax,0x102 ; force TF=1
push ax
popf ; next instruction will be traced..
%else

.inf0:
int 1

%ifdef DEBUG ; warning will crash
.debug0:
mov ax,0xdead
push ax
mov bx,0xbeef
push bx
mov cx,0xcafe
push cx
mov dx,0xdeca
push dx
mov bp,0xfeed
push bp
mov si,0x5151
push si
mov ds,si
mov di,0xd1d1
push di
mov es,di
pop ax
pop ax
pop ax
pop ax
pop ax
pop ax
pop ax
.debug1:
%assign debugpayload (.debug1 - .debug0)
%endif

%endif ; AUTO_REBOOT


.skip:

%ifdef ROM
retf
%else
.inf:
;hlt
jmp short .inf0
%endif

; 0         1         2         3         4         5         6         7
; 01234567890123456789012345678901234567890123456789012345678901234567890123456789
; ax=aaaa bx=bbbb cx=cccc dx=dddd bp=bbbb ss=ssss sp=ssss 0123456789ABCDEF01234567
; ds=dddd si=ssss es=eeee di=dddd fl=ffff cs=cccc ip=iiii 0123456789ABCDEF01234567
; ds=dddd si=ssss es=eeee di=ddddODITSZAPCcs=cccc ip=iiii 0123456789ABCDEF01234567

shello:
;   0         1         2         3         4         5         6         7
;   01234567890123456789012345678901234567890123456789012345678901234567890123456789
;db 'ax=aaaa bx=bbbb cx=cccc dx=dddd bp=bbbb ss=ssss sp=';ssss                         '
db 'axbxcxdx'
db 'bpsssp'
; should be 6 bytes
sgreet:
;db 'enter?'
db 'M?'
sgreet1:
; o0o1s0s1
; 00010203

; 0123456789abcdef01
; sasasasa:aoaoaoaoa
%ifdef CURSOR_LE
db 0xf,0xb,0x5,0x1	; LE
%else
db 0x1,0x5,0xb,0xf	; BE
%endif
db 'dssiesdiflcsip'

; disable all intrs except timer; enable ints
; ah=old intr mask, al=0xfe
diskbint:
in al,0x21
xchg ah,al
mov al,0xfe
out 0x21,al
sti
ret

; restore old intr mask from ah
; al=old intr mask, ah=orig_al
restorekbint:
xchg al,ah
out 0x21,al
ret

; get kb char with timeout
; cx=timeout (1=~instantaneous) ; returns al=scancode or 0 if timeout
; cx is modified according to elapsed timeout
getchwto:
.kbread:
in al,0x64
and al,0x1
; no key assumes al=0
jz .ngotk
.gotk:
in al,0x60
;jmp short .done
ret
.ngotk:
.resume:
hlt
;dec ah
;jnz .kbread
loop .kbread
.timeout:
.done:
ret

%define REGS_START 0
%define NWORDS 6
%define NHELLO 17

%define REGS_SSTART (bp-16)
%define NREGS 4

; clobbers ax, es, si, di; set es=0xB800, ah=DEFAULT_ATTR
dprint:
mov ax,0xB800
mov es,ax
mov ah,DEFAULT_ATTR
lodsb
stosw
ret

; clobbers cx, ax, si, di, DF
dprintregs:
mov cx,NREGS
.lo:
lodsw
call dprintw
lea di,[di+4*2]
loop .lo
ret

; print cl bytes
; clobbers cx, ax, si, di, DF
dprintbytes:
xor ch,ch
.lo0:
std
lodsw
cld
call dprintw
loop .lo0
ret

; ax=word to print
; assume es=0xb800, cld ; direct video print at es:di ; di incremented
; clobbers ax, ah=DEFAULT_ATTR
dprintw:
;push ax
push bx
push cx
mov bx,ax
; ch=digit loops, cl=4 bit rotate to extract digits
mov cx,0x0404
mov ah,DEFAULT_ATTR
.loop:
rol bx,cl
mov al,bl
and al,0xf
cmp al,10
jb .ngrt
add al,'A'-'0'-10
.ngrt:
add al,'0'
stosw
dec ch
jnz .loop
pop cx
pop bx
;pop ax
ret

; this is the major routine
traphandler:
; at sp : ip cs flags
cli
push bp
mov bp,sp ; bp=orig_sp-8
push di
push es
push si
push ds
push dx
push cx
push bx
push ax

; from now on, we have :
; bp+6 flags
; bp+4 cs
; bp+2 ip
; bp+0 bp
; bp-2 di
; bp-4 es
; bp-6 si
; bp-8 ds
; bp-10 dx
; bp-12 cx
; bp-14 bx
; bp-16 ax

%ifdef USE_UNTIL
xor cx,cx
mov ds,cx
; assume ds=0, cx=0
mov si,DATA
lodsw ; status:pos
; let's keep ax as status:pos
test ah,UNTIL
je .nountil
lodsw
cmp ax,[bp+2] ; offset
jne .untilskip
lodsw
cmp ax,[bp+4] ; segment
jne .untilskip
and byte [STATUS],~UNTIL
jmp short .nountil
.untilskip:
;jmp .skip
.nountil:
%endif

call diskbint
push ax ; save old intr mask

mov si,shello
mov cx,NHELLO ; will print 'xx=yyyy ' as often
xor di,di
push cs
pop ds

.looprint:
call dprint
lodsb
stosw
mov al,'='
stosw
lea di,[di+4*2]
mov al,' '
stosw
loop .looprint
; assume cx=0

;                                                         111122223333444455556666
; 0         1         2         3         4         5         6         7
; 01234567890123456789012345678901234567890123456789012345678901234567890123456789
; ax=aaaa bx=bbbb cx=cccc dx=dddd bp=bbbb ss=ssss sp=ssss 0123456789ABCDEF01234567
; ds=dddd si=ssss es=eeee di=dddd fl=ffff cs=cccc ip=iiii 0123456789ABCDEF01234567
; ds=dddd si=ssss es=eeee di=ddddODITSZAPCcs=cccc ip=iiii 0123456789ABCDEF01234567

mov ax,ss
mov ds,ax
mov di,(REGS_START+43)*2; 3 ss
call dprintw
mov di,(REGS_START+51)*2; 3 sp
lea ax,[bp+8]
call dprintw

lea si,[REGS_SSTART]

mov di,(3)*2; 3 ax
call dprintregs
mov di,(80+3)*2; 3 ds
call dprintregs

mov di,(REGS_START+35)*2; 3 bp
lodsw
call dprintw

mov di,(REGS_START+80+51)*2; 3 ip
lodsw
mov bx,ax
call dprintw
mov di,(REGS_START+80+43)*2; 3 cs
lodsw
push ax
call dprintw
mov di,(REGS_START+80+35)*2; 3 flags
lodsw
call dprintw

lea si,[si+10]
mov di,(REGS_START+56)*2; 3 [ss:sp]
mov cl,NWORDS
call dprintbytes
lea si,[bx+10]
mov di,(REGS_START+80+56)*2; 3 [cs:ip]
pop ds
mov cl,NWORDS
call dprintbytes
; assume cx=0

mov ds,cx
; assume ds=0, cx=0

.redump:

mov si,DATA

lodsw ; status
mov dx,ax ; let's keep dx as status:pos

%define NDUMP 22

lodsw ; offset
mov bx,ax

lodsw ; segment

push ax

%ifdef USE_DUMP
test dh,DPLUS|DMINUS
jz .nomod
mov cx,dx
%ifdef USE_DATACURSOR
cmp cl,3
jbe .modaddr
xor si,si
mov ds,ax
;jmp short .mod
.modaddr:
.mod:
%endif
xor ch,ch
;add si,cx
xchg bx,cx
lea si,[si+bx-4]
xchg bx,cx
mov cl,[si]
test dh,DPLUS
jz .noplus
inc cx
inc cx
.noplus:
dec cx
mov [si],cl
.nomod:
and dh,~(DPLUS|DMINUS)
%endif

%ifdef DUMP_MEM
pop ds

mov di,(80+80)*2
push di

; assume bx=off
; assume ax=seg

call dprintw
mov al,':'
stosw

mov ax,bx
call dprintw
mov al,' '
stosw

lea si,[bx+NDUMP-2]
mov cl,NDUMP/2
call dprintbytes
; assume cx=0

mov al,' '
stosw

mov si,bx
mov cx,NDUMP
.lo5:
call dprint
loop .lo5
; assume cx=0
%endif

;mov di,(80+80)*2
pop di

%ifdef SHOW_CURSOR
%if 1 ; non seq cursor
mov bx,sgreet1
mov al,dl
%ifdef USE_DATACURSOR
cmp al,3
jbe .curs_addr
add al,al
add al,al
%ifdef CURSOR_LE ; LE
sub al,(32+NDUMP*4)+1
neg al
%else ; BE
add al,(1*4)+1
%endif
jmp short .curs_data
.curs_addr:
%endif
push cs
pop ds
xlat
.curs_data:
%if 1
xchg bl,al	; allows to force al to a fixed value (variable with sgreet1 offset), allows to remove further al init
%else
mov bl,al
%endif
%else ; seq cursor
mov bl,dl
add bl,bl
add bl,bl
inc bl
%endif
%ifndef ROM ; in ROM, we assume that high byte of sgreet1 is 0
xor bh,bh
%endif
%if 0 ; +4
;mov byte [es:di+bx],2
mov byte [es:di+bx],al ; only if above xchg bl,al
%else ; +5
;mov al,2	; can be removed if above xchg bl,al
lea di,[di+bx]
;add di,bx
stosb
%endif
%endif

mov ds,cx
; assume ds=0, cx=0

;.reloop:
%if 0
xor bl,bl
test dh,INSTEP
jz .nfast
inc bx
%ifdef USE_FAST
test dh,STEPFAST
jnz .nfast
%endif
;inc bx
.nfast:
%else
inc cx
%ifdef USE_ANIMATE
;mov bl,2 ; slow - kb works
;inc cx
;add cl,2
inc cx
%else
;mov bl,1 ; fast - may pose key press miss issues
%endif
%endif
; poll kb
;.pollkb:
;mov ah,bl
;mov ah,cl
call getchwto
cmp al,K_LEAVE
je .proceed
cmp al,K_STEP
je .dostep
%ifdef USE_ANIMATE
;cmp al,LSHIFT
cmp al,SPACE
jne .nanimate
xor dh,INSTEP
.nanimate:
%endif ; USE_ANIMATE
%ifdef USE_FAST
cmp al,RKEY_Z
jne .nfast1
xor dh,STEPFAST
.nfast1:
%endif ; USE_FAST
%ifdef USE_REBOOT
cmp al,RKEY_R
je .reboot
%endif
%ifdef USE_EXECUTE
cmp al,RKEY_X
jne .nexecute
%ifdef USE_EXECUTE
.execute:
; assume ds=0
mov si,BASE_OFFSET
push ss
pop es
lea di,[bp+2]
;lodsw ; offset
;stosw
movsw
;lodsw ; segment
;stosw
movsw
;jmp short .dostep
.nexecute:
%endif

%endif
%ifdef USE_DUMP
cmp al,K_CURSORP
jne .nbackspace0
inc dl
.nbackspace0:
%ifdef USE_BIDIRCURSOR
cmp al,K_CURSORM
jne .ncursorm
dec dl
.ncursorm:
.nbackspace:
%endif
%ifdef USE_DATACURSOR
cmp dl,25
jbe .noreset
xor dl,dl
.noreset:
%else
and dl,3
%endif
cmp al,K_INC
jne .nplus
or dh,DPLUS
.nplus:
cmp al,K_DEC
jne .nminus
or dh,DMINUS
.nminus:
%endif
%ifdef USE_UNTIL
cmp al,K_UNTIL
jne .nuntil
or dh,UNTIL
.nuntil:
%endif
.apply:
mov word [DATA],dx
test dh,INSTEP
;jz .reloop
jnz .nredump
jmp .redump
.nredump:

.dostep:
; this forces TF for interrupted code (ie: step next insn)
or byte [bp+7],0x1 ; modify TF directly in high byte
jmp short .finish

.proceed:
; this clears TF for interrupted code (ie: don't step insn)
and byte [bp+7],~0x1 ; modify TF directly in high byte

.finish:
pop ax ; restore old intr mask
call restorekbint

.skip:

pop ax
pop bx
pop cx
pop dx
pop ds
pop si
pop es
pop di
pop bp
iret

;------------end of code---------------------------------------------------

end0:

%assign sleft 0
%assign usedlen 0
%assign spayload 0

%assign payload0 (end0 - start)

%ifdef SECTOR
%assign spayload (payload0 + 2)
%assign sleft (512 - spayload)
%if (sleft > 0)
times sleft db 0
%endif
db 0x55,0xAA ; obligatory magic value for boot sector
%endif

%ifdef ROM
%assign spayload (payload0 + 1)
%assign sleft (512 - spayload)
%assign spad (512 - (spayload % 512))
%if (spad > 0)
;times spad db 0
%endif
;db 0x66 ; dummy checksum - will be patched later
%endif

end:

%assign usedlen (end - start)

%warning "========= CODE FOOTPRINT STATISTICS ===================="
%warning ========= total used size=usedlen
%warning ========= sector payload size=spayload
%if sleft > 0
%warning ========= sector payload left=sleft
%endif
%if sleft == 0
%warning "========= sector payload is full ! that's gnarly dude"
%endif
%if sleft < 0
%assign ssleft -sleft
%warning ========= sector payload excess=ssleft
%ifdef SECTOR
%warning "!!!!!!!!!!!!!!! CODE TOO BIG TO FIT BOOT_SECTOR !! WON T RUN !!!!!!!!!!!!!!!!!!!!"
%endif
%endif

%ifdef DEBUG
%warning "!!!!!!!!!!!!!!!!!WARNING !!!! DEBUG MODE ACTIVATED !! WILL CRASH !!!!!!!!!!!!!!!!!!!"
%assign spayloadnd (spayload - debugpayload)
%warning "========= nondebug payload size=spayloadnd"
%endif
