# mon86
 MyMon : low level 8086 monitor / debugger
 (C) Nicolas Sauzede 2009 (nsauzede@laposte.net)

 compile with gcc, nasm version 2.05.01; test with qemu (see Makefile)

 should theoretically run on any x86 cpu family, including original 8086/8088.
 very rudimentary, but fits a single floppy (or ROM extension) 512-byte sector.
 dumps all registers content plus stack/code bytes (no disassembly though).
 dumps memory at specified address (can be interactively modified).

 the rationale behind not using any single service from BIOS, beyond the
 technical challenge, is because it hence allows to debug the BIOS itself,
 without any reentrancy issue.

 the following is in fact build-conditional : (see USE_PROMPT define)
 at boot, ROM version prints a prompt and waits for keypress..
 press 'scroll lock' to activate the monitor, within a given timeout.
 the prompt is displayed in red at the left top of the screen
 with subsequent dots printing during timeout..
 after timeout elapses, ROM version will continue normal boot process.
 sector version will automatically activate the monitor

 when the monitor is entered, the cpu has been reset to the BIOS entry point
 (0xffff:0x0000) such that the entire startup sequence can be traced.

 controls : (some must be enabled via USE_XX defines)
  's'   step next insn (execute exactly one)
  space animate insns (toggle autostep)
  <any> fast animate (while in autostep mode)
  'h'   move cursor to the left
  'l'   move cursor to the left
  'j'   increment byte under cursor
  'k'   decrement byte under cursor
  'u'   animate until the dump address is reached
  'x'   goto dump address and execute instruction (single step)
  esc   proceed execution (stop debugging and leave monitor)

 keys have been chosen to work (hopefully) with any keyboard layouts
