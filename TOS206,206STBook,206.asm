ROM_TOS206  EQU 1								; regular TOS 2.06 ROM
ROM_TOS306  EQU 0								; regular TOS 2.06 ROM
ROM_STBOOK  EQU 0                               ; ROM for the ST Book (variation of TOS 2.06)

osroml:     bra.s     reseth                    ; os_entry - BRA to reset handler
IF ROM_TOS306
            DC.W      $0306                     ; os_version - TOS version number
ELSE
			DC.W      $0206                     ; os_version - TOS version number
ENDIF
            DC.L      reseth                    ; os_reseth -> reset handler
            DC.L      osroml                    ; os_beg -> base of OS
            DC.L      endos                     ; os_end -> end BIOS/GEMDOS/VDI ram usage
            DC.L      reseth                    ; os_exec/os_rsv1 << unused, reserved >>
            DC.L      gem_mupb                  ; os_magic -> GEM memory usage param. block
IF ROM_STBOOK
            DC.L      $03101992                 ; os_date - Date of system build ($YYYYMMDD)
ELIF ROM_TOS206
			DC.L      $11141991                 ; os_date - Date of system build ($YYYYMMDD)
ELIF ROM_TOS306
			DC.L      $09241991                 ; os_date - Date of system build ($YYYYMMDD)
ENDIF
            DC.W      $0000                     ; os_conf - OS configuration bits
IF ROM_STBOOK
			DC.W      $186a                     ; os_dosdate - DOS-format date of system build
ELIF ROM_TOS206
			DC.W      $176e                     ; os_dosdate - DOS-format date of system build
ELIF ROM_TOS306
			DC.W      $1738                     ; os_dosdate - DOS-format date of system build
ENDIF
            DC.L      ospool_base               ; p_root -> base of OS pool
            DC.L      kb_shift                  ; pkbshift -> keyboard shift state variable
            DC.L      gemdos_pid                ; p_run -> GEMDOS PID of current process
            DC.L      $000000                   ; p_rsv2 << unused, reserved >>

*+
* [ROM based system]
* reseth - System reset handler
*
*  Gains control of the system upon power-up reset,
*  or when the RESET button is pressed,
*  or after a really messy system crash.
*
*-
reseth:     move      #$2700,sr                 ; super mode, no interrupts
IF ROM_TOS306
			move.w    #$100,(fifo).w
			move.w    #0,(fifo).w
ENDIF
            reset                               ; reset hardware

*+
*  [ROM based system]
*  Check for a diagnostic cartridge;
*  if one is inserted, load a return address
*  into A6 and jump into the cat's entry point.
*
*-
            cmpi.l    #$fa52235f,(cartbase).l   ; is the magic number there?
            bne.s     reset1                    ; (no)
            lea       reset1(pc),a6             ; a6 -> return address
            jmp       (cartbase+4).l            ; execute diagnostic cartridge

*+
*  [ROM based system]
*  Is this is a warm reset, setup the memory
*  controller configuration register so that
*  the reset-bailout vector has something to
*  stand on ....
*
*-
reset1:

IF ROM_TOS306
			move.l    #$808,d0
			movec     d0,cacr

			moveq     #0,d0						; set vector base to address 0
			movec     d0,vbr
			pmove     ($e35fe4).l,tc			; 0L - disable PMMU
			pmove     ($e35fe4).l,tt0			; 0L
			pmove     ($e35fe4).l,tt1			; 0L
			frestore  ($e35fe4).l				; 0L - clear FPU

			btst      #0,(scu_gp1).w			; memconfig valid?
			beq.s     reset3					; (no)
ENDIF

			lea       ret_1(pc),a6              ; load return addr
            bra       val_memval                ; check memory configuration validity
ret_1:      bne.s     reset3                    ; (invalid -- don't set anything up)
            move.b    (memcntlr).w,(memconf).w  ; initialize memory controller
reset2:

IF ROM_STBOOK
			move.w    (STConfig).w,d0           ; Configuration on ST Book
            cmp.b     #$fc,d0                   ; power pressed while the ST Book is closed?
            beq.s     reset2b                   ; yes => do not execute reset vector
            move.w    (tt_mcu+4).l,d0           ; ST Book: ???
            and.b     #6,d0                     ; check bit 1 & 2
            bne.s     reset2c
reset2b:    clr.l     (resvalid).l
reset2c:
ENDIF

*+
*  [ROM based system]
*  RESET bailout vector check.
*  Check to make sure we have a clean, well-bred
*  bailout vector. The high byte must be zero,
*  it must be even, and cannot be entirely zero.
*
*-
            cmpi.l    #$31415926,(resvalid).w   ; is the resvalid the magic number?
            bne.s     reset3                    ; (no)
            move.l    (resvector).w,d0          ; d0 = reset bailout vector
IF !ROM_TOS306
            tst.b     (resvector).w             ; bits 24..31 must be zero
            bne.s     reset3                    ; (they aren't, so punt)
ENDIF
            btst      #0,d0                     ; the vector must be even
            bne.s     reset3                    ; (it isn't, so punt)
            movea.l   d0,a0                     ; a0 -> reset handler
            lea       reset2(pc),a6             ; a6 -> return address
            jmp       (a0)                      ; execute reset bailout

*+
*  Initialize PSG output ports.
*  Make port A and B output-only,
*  initialize floppy select lines (so
*  that none are selected)
*-
reset3:     lea       (psgsel).w,a0             ; a0 -> giselect, giwrite-2
            move.b    #7,(a0)                   ; set porta & portb to output
            move.b    #$c0,2(a0)
            move.b    #14,(a0)                  ; deselect disks
            move.b    #7,2(a0)

*+
*  Determine 50hz or 60hz.
*  The hardware RESETs to 60hz. Check a bit in the
*  ROM configuration byte to see if we have to twiddle
*  the hardware into 50hz mode.
*
*-
IF ROM_TOS306
			move.b    #1,(v_shf_mod).w          ; Switch to 640x200x2
ELSE
            btst      #0,(osroml+29).l          ; check bit: configured for 50hz?
            beq.s     notpal                    ; (nope -- we're good ol' NTSC)
            lea       ret_1b(pc),a6
            bra       waitvbl                   ; a short delay for PAL
ret_1b:     move.b    #2,(synmod).w             ; yes -- twiddle to 50hz
notpal:
IF ROM_STBOOK
		 	move.b    #2,(v_shf_mod).w          ; Switch to 640x400x1
            move.b    #$80,(lcdPowerControl).w  ; LCD display on
            tst.w     (tt_mcu).l                ; ST Book: ???
ENDIF
ENDIF

*+
*  Initialize palette registers to
*  their default values
*
*-
            lea       (palette).w,a1            ; a1 -> hardware reg
            move.w    #15,d0                    ; setup 16 colors
            lea       colors(pc),a0             ; a0 -> table of default colors
sysic1:     move.w    (a0)+,(a1)+               ; copy palette assignment
            dbra      d0,sysic1                 ; (loop for more colors)

*+
*  On a ROM system, put the screen (temporarily)
*  at $10000, so icon-drawing routines won't
*  blow any any system variables.
*-
            move.b    #1,(v_bas_h).w            ; set high ptr
            clr.b     (v_bas_m).w               ; set low ptr

*+
*  [ROM based system]
*  Determine how much memory there is, and initialize
*  the memory controller configuration register
*
*-
IF ROM_TOS306
			btst      #0,(scu_gp1).w			; memconfig valid?
			beq.s     checkmem					; (no)
ENDIF
            move.b    (memcntlr).w,d6           ; d6 = memory controller configuration
IF ROM_TOS306
            move.b    d6,(memconf).w            ; setup memory controller configuration register
ENDIF
            move.l    (phystop).w,d5            ; d5 -> (possible) top of physical mem
            lea       ret_2(pc),a6              ; load return address
            bra       val_memval                ; check if the memory configuration is valid
ret_2:      beq       reset4

IF ROM_STBOOK
*--- init vars + hardware:
            move.b    #%1010,d6                 ; setup controller for 2Mb/2Mb - only valid configuration on a ST Book
            move.b    d6,(memconf).w            ; setup memory controller

*--- write test-pattern to determine memory configuration:
            move.l    #$400000,d5               ; d5 -> physical top of memory (4Mb)
            move.l    #$6161964,d0              ; 16th June 1964...
            move.l    #$3251987,d1              ; 25th March 1987...
            move.l    d0,($300010).l
            move.l    d1,($300014).l
            cmp.l     ($300010).l,d0            ; test if 4MB is actually installed
            bne.s     ret_2a
            cmp.l     ($300014).l,d1
            beq.s     ret_2b
ret_2a:     move.l    #$100000,d5               ; d5 -> physical top of memory (1Mb)

ret_2b:     lea       ($8000).l,sp

ELIF ROM_TOS206
* First we try to configure the memory controller

            clr.w     d6
            move.b    #$a,(memconf).w			; default: setup controller for 2Mb/2Mb

            movea.w   #$8,a0
            lea       ($200008).l,a1			; + 2Mb
            clr.w     d0
chkpatloop: move.w    d0,(a0)+					; fill 512-8 bytes with a test pattern
            move.w    d0,(a1)+
            add.w     #$fa54,d0
            cmpa.w    #$200,a0
            bne.s     chkpatloop

            move.b    #90,(v_bas_l).w			; wrote low byte of video address
            tst.b     (v_bas_m).w				; touch the middle byte (this should reset the low byte)
            move.b    (v_bas_l).w,d0
            cmp.b     #90,d0					; low byte not reset?
            bne.s     chkmem1
            clr.b     (v_bas_l).w				; try a different low byte value
            tst.w     (palette).w				; touch the color palette
            tst.b     (v_bas_l).w				; low byte changed?
            bne.s     chkmem1
            move.l    #$40000,d7				; 256Kb offset
            bra.s     chkmem1b
chkmem1:    move.l    #$200,d7					; 512 byte offset
chkmem1b:   move.l    #$200000,d1				; 2Mb = maximum size per bank

chkmemloop: lsr.w     #2,d6						; shift memory configuration down by a bank (bank 1 is in bits 0..1, bank 0 is in bits 2..3)

            movea.l   d7,a0						; + 512/256Kb bytes
            addq.l    #8,a0
            lea       chkmem3(pc),a4
            bra       memchk
chkmem3:    beq.s     chkmem7					; bank is not working =>

            movea.l   d7,a0
            adda.l    d7,a0						; + 1024/512Kb byte
            addq.l    #8,a0
            lea       chkmem4(pc),a4
            bra       memchk
chkmem4:    beq.s     chkmem6					; bank has 512Kb of memory =>

            movea.w   #$8,a0					; + 0 bytes
            lea       chkmem5(pc),a4
            bra       memchk
chkmem5:    bne.s     chkmem7					; bank is empty =>

            addq.w    #4,d6						; 4+4 = 1000 2Mb bank size
chkmem6:    addq.w    #4,d6						; 4   = 0100 512Kb bank size
chkmem7:    sub.l     #$200000,d1				; - 2Mb
            beq.s     chkmemloop
            move.b    d6,(memconf).w			; set memory configuration

ELIF ROM_TOS306
checkmem:
			move.w    #5,d6
			move.b    #$a,(memconf).w
			moveq     #0,d0
			move.l    d0,($1008).w
			move.l    d0,($100c).w
			move.l    #$6161964,d0
			move.l    d0,($0008).w
			cmp.l     ($1008).w,d0
			bne.s     checkmem1
			move.l    #$4251987,d0
			move.l    d0,($000c).w
			cmp.l     ($100c).w,d0
			beq.s     checkmem2
checkmem1:	move.w    #$a,d6
checkmem2:	move.b    d6,(memconf).w
ENDIF

IF ROM_TOS206 || ROM_TOS306
* memory is configured, set the stack pointer
            lea       ($8000).l,sp

* test the memory in 128Mb blocks to find out the maximum installed
* physical memory. A failure (bus error or a memory checksum error)
* terminates the search.
            movea.l   (busexception).w,a4
            lea       chkmemx(pc),a0			; if a bus error occurs, we are done, too
            move.l    a0,(busexception).w
            move.w    #$fb55,d3
            move.l    #$20000,d7				; 128Mb is the page size for testing
            movea.l   d7,a0						; 128Mb is the minimum physical memory
chkmem8:    movea.l   a0,a1						; test just below the current top of memory
            move.w    d0,d2
            moveq     #$2a,d1					; test 43 words
chkmem9:    move.w    d2,-(a1)					; write test pattern
            add.w     d3,d2
            dbra      d1,chkmem9

            movea.l   a0,a1
            moveq     #$2a,d1					; check 43 words
chkmem10:   cmp.w     -(a1),d0					; compare the test pattern
            bne.s     chkmemx					; test failed => we are done
            clr.w     (a1)						; erase memory after testing
            add.w     d3,d0
            dbra      d1,chkmem10

            adda.l    d7,a0						; advance to the next page
            bra.s     chkmem8

chkmemx:    suba.l    d7,a0						; the last page didn't work
            move.l    a0,d5						; end of the physical memory
            move.l    a4,(busexception).w
ENDIF

*+
*  [ROM based system]
*  Clear memory from $400 to 'd5' (phystop)
*-
            movea.w   #etv_timer,a0             ; start at the beginning
            move.l    d5,d4                     ; where to end
            moveq     #0,d0
clm_1:      move.l    d0,(a0)+                  ; initialize all the memory
            move.l    d0,(a0)+
            move.l    d0,(a0)+
            move.l    d0,(a0)+
            cmpa.l    d4,a0                     ; done?
            bne.s     clm_1                     ; (loop for more bytes)

*+
*  [ROM based system]
*  Indicate that memory has successfully
*  been sized and tested.  Set two variables
*  to magic values ...
*
*-
            move.b    d6,(memcntlr).w           ; save configuration byte
            move.l    d5,(phystop).w            ; save physical top-of-memory
            move.l    #$752019f3,(memvalid).w   ; indicate memory was configured
            move.l    #$237698aa,(memval2).w    ; ditto (paranoia variable)
            move.l    #$5555aaaa,(memval3).w    ; ditto #2 (paranoia variable)
IF ROM_TOS306
			move.l    #fchkmemex,(busexception).w
			move.w    #0,(tt_mcu+224).l
fchkmemex:

* test the memory in 128Mb blocks to find out the maximum installed
* physical fast mem. A failure (bus error or a memory checksum error)
* terminates the search.
			move.l    #fchkmemx,(busexception).w	; if a bus error occurs, we are done, too
            move.w    #$fb55,d3
			moveq     #0,d0
            move.l    #$20000,d7				; 128Mb is the page size for testing
            movea.l   #$1020000,a0				; start address of fast mem area + 128Mb
fchkmem8:   movea.l   a0,a1						; test just below the current top of memory
            move.w    d0,d2
            moveq     #$2a,d1					; test 43 words
fchkmem9:   move.w    d2,-(a1)					; write test pattern
            add.w     d3,d2
            dbra      d1,fchkmem9

            movea.l   a0,a1
            moveq     #$2a,d1					; check 43 words
fchkmem10:  cmp.w     -(a1),d0					; compare the test pattern
            bne.s     fchkmemx					; test failed => we are done
            clr.w     (a1)						; erase memory after testing
            add.w     d3,d0
            dbra      d1,fchkmem10

            adda.l    d7,a0						; advance to the next page
            bra.s     fchkmem8

fchkmemx:   suba.l    d7,a0						; the last page didn't work
			cmpa.l    #$1000000,a0				; it failed right at the beginning of the fast mem?
			bne.s     fchkmemno					; (no)
			suba.l    a0,a0						; then we have no fast mem
fchkmemno:  move.l    a0,d5						; end of the fast mem
			move.l    d5,(ramtop).w				; top of fast mem
			move.l    #$1357bd13,(ramvalid).w	; ramtop is valid (ramvalid == RAMMAGIC)
			bset      #0,(scu_gp1).w			; memconfig is valid
ELSE
            clr.l     (ramtop).l                ; no FASTRAM available
            move.l    #$1357bd13,(ramvalid).w   ; ramtop is valid (ramvalid == RAMMAGIC)
ENDIF

reset4:     movea.l   #_supstk+2048,sp          ; setup supervisor stack

IF ROM_STBOOK
*+
*  Initialize interrupt vectors
*
*  The exception vectors are pointed to a cold boot (_coldboot)
*  during startup.
*
*  Trap 2 and Divide-by-zero are pointed at an rte
*
*  The HBLANK, VBLANK, line 1001 [someday: line 1111), trap 13, trap 14,
*  and "extended" trap vectors are initialized appropriately.
*
*-
            lea       _rte(pc),a3               ; a3 -> handy RTE
            lea       _rts(pc),a4               ; a4 -> handy RTS

*--- setup 64 vectors:
            lea       (_coldboot).l,a1          ; a1 -> during boot all exception trigger a coldboot, which erases all memory and resets
            lea       (busexception).w,a0       ; a0 -> interrupt RAM
            move.w    #$3f,d0                   ; d0 = count
sei1:       move.l    a1,(a0)+                  ; write vector
            dbra      d0,sei1                   ; (loop to write more vectors)
            move.l    a3,(divzeroexception).w   ; divide-by-zero vector -> rte

            move.l    a3,(Level7IRQ).w          ; level #7 interrupt -> rte (power exception in a ST Book)
            moveq     #6,d0
            lea       (Level1IRQ_TTVME).w,a1
sei2:       move.l    #_rte,(a1)+               ; level #1 ... level #6 to RTE
            dbra      d0,sei2

*--- install OS interrupt vectors:
            move.l    #vbl,(Level4IRQ_VBL).w    ; vblank handler
            move.l    #hbl,(Level2IRQ_HBL).w    ; hblank handler
            move.l    a3,(trap_aesvdi).w        ; (empty) trap#2 handler
            move.l    #trp13h,(trap_bios).w     ; trap #13 handler
            move.l    #trp14h,(trap_xbios).w    ; trap #14 handler
            move.l    #line1010,(lineAexception).w ;line 1010 handler
            move.l    a4,(etv_timer).w          ; default timer-tick vector -> rts
            move.l    #_critich,(etv_critic).w  ; default critical error handler
            move.l    a4,(etv_term).w           ; default terminal vector -> rts

*+
*  Setup the vblank deferred vector list.
*  (This data structor is ugly,
*   but we seem to be stuck with it).
*
*-
            lea       (_vbl_list).w,a0          ; a0 -> default list of vbl locs
            move.l    a0,(_vblqueue).w          ; install ptr to them
            move.w    #7,d0                     ; clear vbl vectors
avbl:       clr.l     (a0)+                     ; one at a time
            dbra      d0,avbl

            lea       (tconstat).l,a0
            movea.w   #xconstat,a1
            moveq     #31,d0
tconl:      move.l    (a0)+,(a1)+
            dbra      d0,tconl

            movea.l   (busexception).w,a0
            movea.l   sp,a1
            move.l    #vmeinit,(busexception).w
            move.b    #$40,(vme_mask).w         ; Enable IRQ6 from VMEBUS/MFP
            move.b    #$14,(sys_mask).w         ; VSYNC & HSYNC enable in the VME Bus System Control Unit
vmeinit:    move.l    a0,(busexception).w
            movea.l   a1,sp
ENDIF

            clr.b     ($aa6).l
IF ROM_STBOOK
            sf        (STEFlag).l               ; ST Book is not an Atari STE
ELSE
			movea.l   sp,a6
			move.l    #mwinitex,(busexception).w
			clr.w     (sndmactl).w
			st        ($a02).l                  ; microwire interface available
			lea       mwinitdata(pc),a0
			move.w    (a0)+,(mwmask).w
			bra.s     mwinit3

mwinitdata: DC.W      $0ffe                     ; LMC 1992 mask
            DC.W      $09d1                     ; 10-011-101000-1 = LCM - Master Volume - 0 db volume (max) - end-bit
            DC.W      $0aa9                     ; 10-101-010100-1 = LCM - Left channel volume - 0 db volume (max) - end-bit
            DC.W      $0a29                     ; 10-100-010100-1 = LCM - Right channel volume - 0 db volume (max) - end-bit
            DC.W      $090d                     ; 10-010-000110-1 = LCM - Trebble control - 0 db (linear) - end-bit
            DC.W      $088d                     ; 10-001-000110-1 = LCM - Bass control - 0 db (linear) - end-bit
            DC.W      $0803                     ; 10-000-000001-1 = LCM - Mixer - DMA + YM2149 - end-bit
            DC.W      $0000                     ; 0 = end of list

mwinit1:	move.w    d0,(mwdata).w
mwinit2:	tst.w     (mwdata).w
			bne.s     mwinit2
mwinit3:	move.w    (a0)+,d0
			bne.s     mwinit1
mwinitex:	movea.l   a6,sp

IF ROM_TOS206
			move.b    #90,(v_bas_l).w           ; write low byte of video address
			tst.b     (v_bas_m).w               ; access the medium byte
			move.b    (v_bas_l).w,d0            ; (which should reset the low byte on an STE!)
			cmp.b     #90,d0                    ; reset?
			bne.s     nostedetect               ; yes => STE detected
			clr.b     (v_bas_l).w               ; clear the low byte again
			tst.w     (palette).w               ; access the color palette
			tst.b     (v_bas_l).w
nostedetect:sne       (STEFlag).l               ; <>0 => no STE hardware available
ELSE
			sf        (STEFlag).l				; no STE hardware available
ENDIF
ENDIF

*+
*  Clear OS bss (from 'endosbss' to 'ostext')
*-
            movea.l   #ostext,a1                ; a1 -> end
            movea.l   #endosbss,a0              ; a0 -> start

*--- common code to clear memory:
            moveq     #0,d0                     ; quick zero
clrm_1:     move.w    d0,(a0)+                  ; clobber a word
            cmpa.l    a0,a1                     ; at end?
            bne.s     clrm_1                    ; (no -- loop for more words)

IF ROM_TOS306
			bsr		  setupPMMU					; setup PMMU translation table
ENDIF
*+
*  Setup display base,
*  clear display memory.
*
*-
            movea.l   (phystop).w,a0            ; video_base = phystop - 0x8000
IF !ROM_TOS306
            suba.l    #$8000,a0					; screen size = 32kb
            move.w    #$7ff,d1                  ; d1 = # 16-byte chunks to zero
ELSE
			suba.l    #$25900,a0				; screen size = 154kb
			move.w    #$258f,d1					; d1 = # 16-byte chunks to zero
ENDIF
            move.l    a0,(_v_bas_adr).w
            move.b    (_v_bas_adr+1).w,(v_bas_h).w ;load high addr
            move.b    (_v_bas_adr+2).w,(v_bas_m).w ;load low (really, medium) addr
            moveq     #0,d0                     ; quick zero
clrm_2:     move.l    d0,(a0)+                  ; zero a longword
            move.l    d0,(a0)+                  ; zero a longword
            move.l    d0,(a0)+                  ; zero a longword
            move.l    d0,(a0)+                  ; zero a longword
            dbra      d1,clrm_2                 ; (loop for more longwords)

*+
*  Initialize all kinds of OS variables
*
*-

*--- OS parameters:
            movea.l   osroml+20(pc),a0          ; get pointer to magic
            cmpi.l    #$87654321,(a0)           ; is the magic there?
            beq.s     usem
            lea       (osroml+8).l,a0           ; yes -- use numbers there
usem:       move.l    4(a0),(end_os).w          ; init end-of-OS pointer
            move.l    8(a0),(exec_os).w         ; init default-shell pointer

*--- Disk vectors:
            move.l    #_dskinit,(hdv_init).w    ; initialization
            move.l    #_floprw,(hdv_rw).w       ; read/write absolute sectors
            move.l    #_getbpb,(hdv_bpb).w      ; media change inquiry
            move.l    #_mediach,(hdv_mediach).w ; get BIOS parameter block
            move.l    #_boot,(hdv_boot).w       ; boot-from-device

            move.l    #_lstostat,(prv_lsto).w
            move.l    #_lstout,(prv_lst).w
            move.l    #_auxostat,(prv_auxo).w
            move.l    #_auxout,(prv_aux).w
            move.l    #_scrdmp,(scr_dump).w

*--- Randoms:
            move.l    (_v_bas_adr).w,(_memtop).w ;_memtop = _v_bas_ad
            move.l    (end_os).w,(_membot).w    ; set bottom of memory (for DOS)
            move.w    #8,(nvbls).w              ; default number of vbl queue entries
            st        (_fverify).w              ; enable write-verify
            move.w    #3,(seekrate).w           ; set default seek-rate
            move.l    #_diskbuf,(_dskbufp).w    ; set pointer to disk buffer
            move.w    #-1,(_prtcnt).w           ; initialize print-count
            move.l    #osroml,(_sysbase).w      ; -> base of OS
            move.l    #savend,(savptr).w        ; register-save pointer for traps 13&14
            move.l    #_rts,(swv_vec).w         ; ignore monitor changes for now
            clr.l     (_drvbits).w              ; remove all drives
            move.l    #keybellsnd,(bell_hook).w
            move.l    #keyclicksnd,(kcl_hook).w

            bsr       sysbase2ram               ; copy sysbase into RAM and patch os_conf to contain os_dosdate

*--- Cookies:
            lea       (cookieBuffer).l,a0
            move.l    a0,(_p_cookies).w
            move.l    #'_CPU',(a0)+
            moveq     #0,d1                     ; 68000 CPU
            movea.w   #illegalexception,a2
            movea.l   (a2),a3
            movea.l   sp,a1
            move.l    #cookieCPU,(a2)
            move      ccr,d0                    ; first exists in the 68010
            moveq     #10,d1                    ; 68010 CPU
            extb.l    d0                        ; first exists in the 68020
            moveq     #20,d1                    ; 68020 CPU
            movec     cacr,d0
            bset      #9,d0
            movec     d0,cacr
            movec     cacr,d0
            bclr      #9,d0
            beq.s     cookieCPU
            moveq     #30,d1                    ; 68030 CPU
            movec     d0,cacr
cookieCPU:  movea.l   a1,sp
            move.l    a3,(a2)
            move.l    d1,(a0)+
            sne       (_longframe+1).w          ; 68010+ have a longer interrupt stack frame
IF ROM_STBOOK
            move.l    #'_VDO',(a0)+             ; setup VDO cookie: Video hardware
            move.l    #$10001,(a0)+             ; 1,1 => Atari STE, ST-Book
            move.l    #'_MCH',(a0)+             ; setup MCH cookie: Machine type
            move.l    #$10001,(a0)+             ; 1,1 => Atari STE, ST-Book

            move.b    #$7f,d0
            tst.b     (STEFlag).l
            bne.s     cookieSTE
            move.l    #'_SWI',(a0)+             ; setup SWI cookie: DIP configuration switches
            moveq     #0,d0
            move.w    (STConfig).w,d0
            lsr.w     #8,d0
            move.l    d0,(a0)+                  ; all DIP switches as a bit mask 0..7
cookieSTE:  moveq     #3,d1                     ; bit 0: PSG, bit 1: 8-bit DMA
            move.l    #'_SND',(a0)+             ; setup SND cookie: Sound hardware
            btst      #7,d0                     ; DIP switch 7 on?
            bne.s     cookieSND                 ; (punt)
            bclr      #1,d1                     ; no 8-bit DMA sound
cookieSND:  move.l    d1,(a0)+
            btst      #6,d0                     ; DIP switch 6 on?
            bne.s     cookieFDC                 ; (punt - no HD floppy)
            move.b    #8,(dsb0).l               ; select HD density for drive A
            move.l    #'_FDC',(a0)+             ; setup FDC cookie: Floppy disk controller
            move.l    #$1415443,(a0)+           ; 'FDC' | (1 << 24)
cookieFDC:  move.l    #'_FPU',(a0)+             ; Setup FPU cookie: Type of the FPU
            moveq     #0,d7                     ; 0 = no FPU
            suba.w    #$24,sp
            move.l    (lineFexception).w,(sp)
            move.l    (coprocexception).w,4(sp)
            move.l    #cookieFPU,(lineFexception).w
            move.l    #cookieFPU,(coprocexception).w
            lea       8(sp),a1
            movea.w   #ffcp_unorderedcond,a2
            move.l    #cookieFPU2,d0
            move.l    (a2),(a1)+
            move.l    d0,(a2)+
            move.l    (a2),(a1)+
            move.l    d0,(a2)+
            move.l    (a2),(a1)+
            move.l    d0,(a2)+
            move.l    (a2),(a1)+
            move.l    d0,(a2)+
            move.l    (a2),(a1)+
            move.l    d0,(a2)+
            move.l    (a2),(a1)+
            move.l    d0,(a2)+
            move.l    (a2),(a1)+
            move.l    d0,(a2)+
            clr.l     -(sp)
            movea.l   sp,a2
            frestore  (sp)
cookieFPU2: move.l    #$20000,d7                ; 0x20000 = 68881 or 68882 as co-processor (Exact type unknown)
cookieFPU:  movea.l   a2,sp
            addq.w    #4,sp
            move.l    (sp)+,(lineFexception).w
            move.l    (sp)+,(coprocexception).w
            move.l    (sp)+,(ffcp_unorderedcond).w
            move.l    (sp)+,(ffcp_inexactresult).w
            move.l    (sp)+,(ffcp_divzero).w
            move.l    (sp)+,(ffcp_underflow).w
            move.l    (sp)+,(ffcp_operanderror).w
            move.l    (sp)+,(ffcp_inexactresult).w
            move.l    (sp)+,(ffcp_divzero).w
            move.l    d7,(a0)+
            movea.l   (busexception).w,a1
            movea.l   sp,a2
            move.l    #cookieFPU3,(busexception).w
            move.w    (FPStat).w,d0
            bset      #0,-3(a0)                 ; SFP004 present
cookieFPU3: move.l    a1,(busexception).w
            movea.l   a2,sp
ELSE

IF ROM_TOS206
			tst.b     (STEFlag).l               ; no STE hardware available?
			beq.s     cookieMCH                 ; (correct)
			move.l    #'_VDO',(a0)+
			clr.l     (a0)+                     ; 0,0 = Atari ST (260 ST, 520 ST, 1040 ST, Mega ST, ...)
			move.l    #'_MCH',(a0)+
			clr.l     (a0)+                     ; 0,0 = Atari ST
			bra.s     cookieSWI

cookieVDO:	move.l    #'_VDO',(a0)+
			move.l    #$10000,(a0)+             ; 1,0 = STE Shifter

			move.l    #$10000,d0                ; x = $00 = regular STE
			movea.l   (busexception).w,a1
			movea.l   sp,a2
			move.l    #cookieMCH1,(busexception).w
			tst.b     (scu_gp1).w
			move.w    #$10,d0                   ; x = $10 = Mega STE (with SCSI)
			bra.s     cookieMCH2
cookieMCH1:	clr.w     d0
			movea.l   a2,sp
			move.l    #cookieMCH2,(busexception).w
			tst.b     (ide_stat2).l
			move.w    #8,d0                     ; x = $08 = STE with IDE (unknown machine)
cookieMCH2:	move.l    a1,(busexception).w
			movea.l   a2,sp
			move.l    #'_MCH',(a0)+
			move.l    d0,(a0)+                  ; 1,x = STE (520, 1040, 2080, 4160, Mega STE, ST Book)

ELIF ROM_TOS306
			move.l    #'_VDO',(a0)+
			move.l    #$20000,(a0)+				; 2,0 = TT Shifter
			move.l    #'_MCH',(a0)+
			move.l    #$20000,(a0)+				; 2,0 = TT
ENDIF

cookieSWI:	move.b    #$7f,d0
			tst.b     (STEFlag).l               ; no STE hardware available?
			bne.s     cookieSND2                ; (correct)
			move.l    #'_SWI',(a0)+             ; setup SWI cookie: DIP configuration switches
			moveq     #0,d0
			move.w    (STConfig).w,d0
			lsr.w     #8,d0
			move.l    d0,(a0)+

cookieSND2:	moveq     #3,d1                     ; bit 0: PSG, bit 1: 8-bit DMA
			move.l    #'_SND',(a0)+             ; setup SND cookie: Sound hardware
			btst      #7,d0                     ; DIP switch 7 on?
			bne.s     cookieSND3                ; (punt)
			bclr      #1,d1                     ; no 8-bit DMA sound
cookieSND3:	move.l    d1,(a0)+

			btst      #6,d0                     ; DIP switch 6 on?
			bne.s     cookieFDC2                ; (punt - no HD floppy)
            move.b    #8,(dsb0).l               ; select HD density for drive A
			move.l    #'_FDC',(a0)+             ; setup FDC cookie: Floppy disk controller
			move.l    #$1415443,(a0)+			; 'FDC' | (1 << 24)

cookieFDC2:	
			move.l    #'_FPU',(a0)+             ; Setup FPU cookie: Type of the FPU
			movea.l   (lineFexception).w,a1
			movea.l   (coprocexception).w,a2
			movea.l   sp,a3
			move.l    #cookieFPUex,(lineFexception).w
			move.l    #cookieFPUex,(coprocexception).w
			fmove.l   d0,fp0
			move.l    #$20000,(a0)+             ; 6888x present
			bra.s     cookieFPUe2
cookieFPUex:clr.l     (a0)+
cookieFPUe2:move.l    a1,(lineFexception).w
			move.l    a2,(coprocexception).w
			movea.l   a3,sp
			movea.l   (busexception).w,a1
			movea.l   sp,a2
			move.l    #cookieFPUe3,(busexception).w
			move.w    (FPStat).w,d0
			bset      #0,-3(a0)                 ; SFP004 present
cookieFPUe3:move.l    a1,(busexception).w
			movea.l   a2,sp

IF ROM_TOS306
			tst.l     (ramtop).w				; fast mem installed?
			beq.s     nfrbcookie				; (no)
			move.l    #'_FRB',(a0)+				; allocate FRB cookie for DMA buffer
			move.l    (_membot).w,d0
			move.l    d0,(a0)+					; buffer adress
			add.l     #$10000,d0				; 64kb size
			move.l    d0,(_membot).w			; adjust memory base
			move.l    d0,(end_os).w
nfrbcookie:
ENDIF

ENDIF
            clr.l     (a0)+                     ; terminate cookie list
            move.l    #$10,(a0)+                ; 16 available slots in the cookie store

IF !ROM_STBOOK
*+
*  Initialize interrupt vectors
*
*  The exception vectors are pointed to a cold boot (_coldboot)
*  during startup.
*
*  Trap 2 and Divide-by-zero are pointed at an rte
*
*  The HBLANK, VBLANK, line 1001 [someday: line 1111), trap 13, trap 14,
*  and "extended" trap vectors are initialized appropriately.
*
*-
            lea       _rte(pc),a3               ; a3 -> handy RTE
            lea       _rts(pc),a4               ; a4 -> handy RTS

*--- setup 62 vectors:
			lea       _term(pc),a1				; _term -> draw number of bombs
IF !ROM_TOS306
			adda.l    #$2000000,a1				; exception number in top 8 bits of the address
ENDIF
			lea       (busexception).w,a0		; a0 -> interrupt RAM
			move.w    #$3d,d0					; d0 = count
sei1:	    move.l    a1,(a0)+					; write vector
IF !ROM_TOS306
			adda.l    #$1000000,a1				; increment the exception number
ENDIF
			dbra      d0,sei1					; (loop to write more vectors)
            move.l    a3,(divzeroexception).w   ; divide-by-zero vector -> rte

            move.l    a3,(Level7IRQ).w          ; level #7 interrupt -> rte (power exception in a ST Book)
            moveq     #6,d0
            lea       (Level1IRQ_TTVME).w,a1
sei2:       move.l    #_rte,(a1)+               ; level #1 ... level #6 to RTE
            dbra      d0,sei2

*--- install OS interrupt vectors:
            move.l    #vbl,(Level4IRQ_VBL).w    ; vblank handler
            move.l    #hbl,(Level2IRQ_HBL).w    ; hblank handler
            move.l    a3,(trap_aesvdi).w        ; (empty) trap#2 handler
            move.l    #trp13h,(trap_bios).w     ; trap #13 handler
            move.l    #trp14h,(trap_xbios).w    ; trap #14 handler
            move.l    #line1010,(lineAexception).w ;line 1010 handler
            move.l    a4,(etv_timer).w          ; default timer-tick vector -> rts
            move.l    #_critich,(etv_critic).w  ; default critical error handler
            move.l    a4,(etv_term).w           ; default terminal vector -> rts

*+
*  Setup the vblank deferred vector list.
*  (This data structor is ugly,
*   but we seem to be stuck with it).
*
*-
            lea       (_vbl_list).w,a0          ; a0 -> default list of vbl locs
            move.l    a0,(_vblqueue).w          ; install ptr to them
            move.w    #7,d0                     ; clear vbl vectors
avbl:       clr.l     (a0)+                     ; one at a time
            dbra      d0,avbl

            lea       (tconstat).l,a0
            movea.w   #xconstat,a1
            moveq     #31,d0
tconl:      move.l    (a0)+,(a1)+
            dbra      d0,tconl

            movea.l   (busexception).w,a0
            movea.l   sp,a1
            move.l    #vmeinit,(busexception).w
            move.b    #$40,(vme_mask).w         ; Enable IRQ6 from VMEBUS/MFP
            move.b    #$14,(sys_mask).w         ; VSYNC & HSYNC enable in the VME Bus System Control Unit
vmeinit:    move.l    a0,(busexception).w
            movea.l   a1,sp
ENDIF

*+
*  "The other half" of the BIOS handles character I/O,
*  call its initialization hoook.
*  (It can "never fail". This will get interesting
*  if we ever to a detachable keyboard ....)
*-
            bsr       initmfp

            move.w    #$400,d0                  ; Delay Mode, /50 Prescale, data = 0 (about 10us delay)
            bsr       mfpdelay
            move.l    #setikbd,-(sp)
            move.w    #1,-(sp)
            jsr       (ikbdws).l                ; reset ikbd
            addq.l    #6,sp
            move.w    #$700,d0                  ; Delay Mode, /200 Prescale, data = 0 (about 40us delay)
            move.w    #14,d1                    ; 15 * 40us = 600us delay
ikbddelay:  bsr       mfpdelay
            dbra      d1,ikbddelay

*+
*  Fire up %%2 cartridges
*
*-
            moveq     #2,d0                     ; bit# = 2
            bsr       cartscan                  ; execute cartridge aps
IF ROM_STBOOK
            bsr       cartscan_STBOOK_EXTROM

            move.b    #$80,(lcdPowerControl).w  ; LCD display on
            move.b    #$80,(lcdPowerControlShadow).w ; LCD display shadow register
            moveq     #2,d1                     ; Switch to 640x400x1
            lea       setvb1(pc),a6
            bra       waitvbl
setvb1:     move.b    d1,(v_shf_mod).w          ; set rez hardware register

ELIF ROM_TOS206
			moveq     #0,d1						; Switch to 320x200x4
			btst      #7,($fffffa01).w			; monochrome monitor connected?
			bne.s     nomonomon					; (no)
			moveq     #2,d1                     ; Switch to 640x400x1
nomonomon:
            lea       setvb1(pc),a6
            bra       waitvbl
setvb1:     move.b    d1,(v_shf_mod).w          ; set rez hardware register

ELIF ROM_TOS306
			moveq     #4,d1						; Switch to 640x480x4
			btst      #7,($fffffa01).w			; monochrome monitor connected?
			bne.s     nomonomon					; (no)
			moveq     #6,d1                     ; Switch to 1280x960x1
nomonomon:
			move.b    d1,(shift_tt).w
ENDIF
            move.b    d1,(sshiftmd).w           ; set rez shadow

            bsr       blittest
            jsr       ($e072aa).l				; linaA blitter/no-blitter table init
            jsr       (esc_init).l              ; clear screen, initialize cursor
            move.l    #reseth,(swv_vec).w       ; RESET system on monitor change
            move.w    #1,(vblsem).w             ; enable vblank processing

*+
*  [1] Fire up %%0 cartridges
*  [2] Enable interrupts
*  [3] Fire up %%1 cartridges
*
*-
            clr.w     d0                        ; magic bit# = 0
            bsr       cartscan                  ; execute cartridge aps
IF ROM_STBOOK
            bsr       cartscan_STBOOK_EXTROM
ENDIF
            move      #$2300,sr                 ; Enable interrupts, go to IPL 3

            moveq     #1,d0                     ; magic bit# = 1
            bsr       cartscan                  ; execute cartridge aps
IF ROM_STBOOK
            bsr       cartscan_STBOOK_EXTROM

            move.l    (_hz_200).w,d0
            addq.l    #3,d0
resDelayL:  cmp.l     (_hz_200).w,d0            ; a short delay of 15-20ms
            bhi.s     resDelayL
            clr.b     (kb_shift).l              ; reset keyboard shift state
ENDIF

            move.l    #privinstr_exception,(privexception).w ;Emulate the MOVE SR so it works in usermode on a 68020+
            bra       resetcont

*+
* Flush instruction and data cache (68020+)
*-
flushCaches:move      sr,-(sp)
            ori       #$700,sr
            movec     cacr,d0
            or.l      #$808,d0                  ; CI (Clear Instruction Cache), DI (Clear Data Cache)
            movec     d0,cacr
            move      (sp)+,sr
            rts

*+
* Emulate the MOVE SR,<ea> so it works in usermode on 68010+
*-
privinstr_exception:movem.l d0-d2,-(sp)
            move.l    a1,-(sp)
            move.l    a0,-(sp)
            movea.l   $16(sp),a0                ; a0 -> pc
            move.w    (a0),d0                   ; d0 -> current opcode
            move.w    d0,d1                     ; d1 -> save current opcode
            and.w     #$ffc0,d0
            cmp.w     #$40c0,d0                 ; move SR,<ea>?
            bne       privinstr_exception_term  ; (no -- punt)

            move.l    #$30004e71,(endosbss).w   ; move.w d0,d0, nop
            move.l    #'NqNu',(endosbss+2*2).w  ; nop, rts
            move.w    d1,d0
            and.w     #7,d0                     ; <ea> register (bit 0..2)
            lsl.w     #8,d0
            lsl.w     #1,d0                     ; move into bit 9..11 of the destination <ea>
            or.w      d0,(endosbss).w
            move.w    d1,d0
            and.w     #$38,d0                   ; <ea> mode (bit 3..5)
            lsl.w     #3,d0                     ; move into bit 6..8 of the destination <ea>
            or.w      d0,(endosbss).w           ; generate a MOVE D0,<ea> from the MOVE SR,<ea> opcode

            moveq     #2,d2                     ; opcode size = 2
            cmp.w     #$180,d0                  ; <ea> == d8(An,Xn)?
            beq       privinstr_exception_term  ; not supported -> _term
            tst.w     d0                        ; <ea> == Dn?
            beq.s     privinstr_exception_dn
            cmp.w     #$140,d0                  ; <ea> == d16(An)?
            beq.s     privinstr_exception_d16
            cmp.w     #$1c0,d0                  ; <ea> == (xxx).w or (xxx).l?
            bne.s     privinstr_exception_other

* <ea> = (xxx).l or (xxx).w
            and.w     #7,d1                     ; register == 0 => (xxx).w
            beq.s     privinstr_exception_abs16
            addq.w    #2,d2                     ; it is (xxx).l -> opcode size += 2
            move.w    4(a0),(endosbss+2*2).w    ; move lower word of the absolute address
privinstr_exception_abs16:addq.w #2,d2          ; opcode size += 2
            move.w    2(a0),(endosbss+2).w      ; move upper word of the absolute address
            bra.s     privinstr_exception_ea    ; -> regular destination ea

* <ea> = d16(An)
privinstr_exception_d16:addq.w #2,d2            ; opcode size += 2
            move.w    2(a0),(endosbss+2).w      ; move d16 offset
privinstr_exception_other:and.w #7,d1           ; d1 = An from the destination <ea>
            cmp.w     #7,d1                     ; relative to A7?
            bne.s     privinstr_exception_ea    ; no -> regular destination ea

* <ea> = d16(A7) or d8(A7,Dn)
* Special case because A7 has to be the USP, instead of the current SSP (in A7)
            move      usp,a1                    ; have to use the USP, otherwise we wouldn't have gotten the exception
            andi.w    #$f3ff,(endosbss).w       ; convert A7-relative into A1-relative destination <ea>
            add.l     d2,$16(sp)                ; pc += opcode size
IF ROM_TOS306
			bsr       flushCaches
ENDIF
            move.w    $14(sp),d0                ; d0 = SR
            jsr       (endosbss).w              ; execute: MOVE D0,d(A1,Dn); NOP; RTS or MOVE D0,d(A1); NOP; RTS
            move      a1,usp                    ; restore USP, but shouldn't have changed anyway
            movea.l   (sp)+,a0                  ; restore registers
            movea.l   (sp)+,a1
            movem.l   (sp)+,d0-d2
            rte                                 ; continue execution

* <ea> = Dn
privinstr_exception_dn:add.l d2,$16(sp)         ; pc += opcode size
            ori.w     #$10,(endosbss).w         ; source ea = (A0)
IF ROM_TOS306
			bsr       flushCaches
ENDIF
            lea       20(sp),a0                 ; point to SR register to SSP
            movem.l   8(sp),d0-d2               ; restore d0-d2
            jsr       (endosbss).w              ; execute: MOVE (A0),Dn; NOP; NOP; RTS
            movea.l   (sp)+,a0                  ; restore registers
            movea.l   (sp)+,a1
            adda.w    #$c,sp                    ; skip d0-d2, because they have already been restored
            rte                                 ; continue execution

privinstr_exception_ea:add.l d2,$16(sp)         ; pc += opcode size
IF ROM_TOS306
			bsr       flushCaches
ENDIF
            movea.l   (sp)+,a0
            movea.l   (sp)+,a1
            move.w    12(sp),d0                 ; d0 = SR
            jsr       (endosbss).w              ; execute: MOVE D0,<ea>; ...; RTS
            movem.l   (sp)+,d0-d2               ; restore registers
            rte                                 ; continue execution

privinstr_exception_term:movea.l (sp)+,a0       ; restore registers
            movea.l   (sp)+,a1
            movem.l   (sp)+,d0-d2
            jmp       (_term).l                 ; illegal instruction => _term

*+
* Load shell (if _cmdload is nonzero)
* or execute GEM in ROM
*-
resetcont:  jsr       (osi).l                   ; initialize DOS

*--- set the current system time and date
            move.w    (osroml+30).l,(systemDate).l ;use BIOS time as current time
            jsr       (readCurrentTime).l       ; set current time to RTC time
            beq.s     notimechip                ; (no RTC)
            bsr       readIKBDTime              ; read time from the keyboard controller
            swap      d0
            tst.b     d0
            beq.s     notimechip                ; (not valid either)
            move.w    d0,(systemDate).l         ; update system time
            swap      d0
            move.w    d0,(systemTime).l

notimechip: clr.b     (tacr).w
            bclr      #5,(iera).w               ; disable Timer A interrupts

IF ROM_TOS306
			move.l    #$3111,d0
			movec     d0,cacr
ENDIF

            movea.l   #atari_image,a0           ; draw Atari Logo on the screen
            movea.l   (_v_bas_adr).w,a1
            move.b    (sshiftmd).w,d0
            cmp.b     #2,d0                     ; 640 x 400 (bw)
            beq.s     drawLogobw                ; (yes)
            cmp.b     #6,d0                     ; 1280 x 960 (bw)
            beq.s     drawLogobw                ; (yes)
IF ROM_TOS306
			adda.w    #4*320,a1
ELSE
            adda.w    #4*160,a1
ENDIF
            move.w    #86-1,d0                  ; 86 lines
drawLogoc1: moveq     #6-1,d1                   ; 6 words per raster line
drawLogoc2: move.w    (a0)+,d2                  ; draw 4 plane Atari Logo
            move.w    d2,(a1)+
            move.w    d2,(a1)+
            move.w    d2,(a1)+
            move.w    d2,(a1)+
            dbra      d1,drawLogoc2
IF ROM_TOS306
			adda.w    #320-6*8,a1
ELSE
            adda.w    #160-6*8,a1
ENDIF
            dbra      d0,drawLogoc1
            bra.s     drawLogo3b
drawLogobw:
IF ROM_TOS306
			adda.w    #4*160,a1
ELSE
			adda.w    #4*80,a1
ENDIF
            move.w    #86-1,d0                  ; 86 lines
drawLogobw2:moveq     #12-1,d1                  ; 12 bytes per raster line
drawLogobw3:move.b    (a0)+,(a1)+               ; draw 1 plane Atari Logo
            dbra      d1,drawLogobw3
IF ROM_TOS306
			adda.w    #160-12,a1
ELSE
            adda.w    #80-12,a1
ENDIF
            dbra      d0,drawLogobw2

drawLogo3b: moveq     #32+7,d7                   ; y = 7
            tst.b     (sshiftmd).w
            bne.s     drawLogo4
            moveq     #32+12,d7                 ; y = 12
drawLogo4:  move.l    #$30002,d6
            move.w    #$1b,-(sp)
            move.l    d6,-(sp)
            trap      #$d                       ; Bconout(2, ESC)
            move.w    #$59,4(sp)
            move.l    d6,(sp)
            trap      #$d                       ; Bconout(2, 'Y')
            move.w    d7,4(sp)
            move.l    d6,(sp)
            trap      #$d                       ; Bconout(2, ' '+0)
            move.w    #$20,4(sp)
            move.l    d6,(sp)
            trap      #$d                       ; Bconout(2, ' '+y)
            addq.w    #6,sp

IF !ROM_TOS306
            cmpi.l    #$3e80,(_hz_200).l        ; system running for >80s?
            bcc       ptch_term                 ; (then no ROM CRC check)
ENDIF

*--- Check the ROM CRC
checkROM:
IF ROM_STBOOK
			move.l    #$3fffe,d7                ; offset to ROM CRC (last 2 bytes)
            move.w    #0,d6                     ; number of banks = 1
ELIF ROM_TOS206
			move.l    #$1fffe,d7                ; offset to ROM CRC (last 2 bytes)
			move.w    #1,d6                     ; number of banks = 2
ELIF ROM_TOS306
			move.l    #$1fffe,d7                ; offset to ROM CRC (last 2 bytes)
			move.w    #3,d6                     ; number of banks = 4
ENDIF
            movea.l   #osroml,a5                ; ROM base address
checkROMl:
IF ROM_STBOOK
	  		move.w    #1,-(sp)                  ; checksum over every byte
ELIF ROM_TOS206
			move.w    #2,-(sp)                  ; checksum over every other byte
ELIF ROM_TOS306
			move.w    #4,-(sp)                  ; checksum over every 4th byte
ENDIF
            move.l    d7,-(sp)                  ; number of bytes
            move.l    a5,-(sp)                  ; buffer address
            bsr       calccrc
            adda.w    #10,sp
            movea.l   a5,a0
IF ROM_STBOOK
            adda.l    d7,a0
ELIF ROM_TOS206
			adda.l    #$3fffc,a0
ELIF ROM_TOS306
			adda.l    #$7fff8,a0
ENDIF
            move.b    (a0),d1                   ; high byte of CRC
            lsl.w     #8,d1
IF ROM_STBOOK
            move.b    1(a0),d1                  ; low byte of CRC
ELIF ROM_TOS206
			move.b    2(a0),d1                  ; low byte of CRC
ELIF ROM_TOS306
			move.b    4(a0),d1                  ; low byte of CRC
ENDIF
            cmp.w     d1,d0                     ; CRC identical?
            bne.s     checkROMbad               ; (no)
            addq.l    #1,a5                     ; next bank
            dbra      d6,checkROMl
            bra.s     ptch_term

badCRCtext: DC.B      'WARNING: BAD ROM CRC IN CHIP ',0
badCRCtextB:DC.B      '.\r\n',0

checkROMbad:move.l    a5,d5
            pea       (badCRCtext).l
            move.w    #9,-(sp)
            trap      #1
            move.b    #'E',d0                   ; 'E' - even
            btst      #0,d5
            beq.s     checkROMbad2
            move.b    #'O',d0                   ; 'O' - odd
checkROMbad2:move.w   d0,2(sp)
            move.w    #2,(sp)
            trap      #1
IF ROM_TOS306
            move.b    #'E',d0                   ; 'E' - even
            btst      #1,d5
            beq.s     checkROMbad3
            move.b    #'O',d0                   ; 'O' - odd
checkROMbad3:move.w   d0,2(sp)
            move.w    #2,(sp)
            trap      #1
ENDIF
            move.l    #badCRCtextB,2(sp)
            move.w    #9,(sp)
            trap      #1
            addq.w    #6,sp
            addq.l    #1,a5
            dbra      d6,checkROMl

* if no monochrome is active, holding down the alternate key forces
* to 320x200x4 instead of 640x480x4 for ST compatibility
IF ROM_TOS306
			cmpi.b    #6,(sshiftmd).w			; TT high?
			beq.s     ttlowreschk				; (yes)
			move.l    #$bffff,-(sp)
			trap      #$d						; Kbshift()
			addq.w    #4,sp
			btst      #3,d0						; alternate pressed?
			beq.s     ttlowreschk				; (no)
			clr.w     -(sp)
			pea       ($ffffffff).w
			pea       ($ffffffff).w
			move.w    #5,-(sp)
			trap      #$e						; switch to low-res
			adda.w    #$c,sp
			move.l    #$808,d0
			movec     d0,cacr
ttlowreschk:
ENDIF

* During boot till this point any exception triggers a _coldboot, which
* erases the first MB and resets. From now own we point the exceptions
* to _term, which draws the bombs and terminates the currently running app
ptch_term:

IF ROM_STBOOK
			move.l    #$1000000,d1              ; d1 -> exception number
            lea       _term(pc),a0              ; new exception vector
            adda.l    d1,a0                     ; add the exception number into the upper 8 bits (2 = bus error)
            adda.l    d1,a0
            lea       (busexception).w,a1       ; start with the bus error exception
            move.w    #$3f,d0
            move.l    #_coldboot,d2             ; old exception vector
ptch_term1: cmp.l     (a1)+,d2                  ; is it pointing to _coldboot?
            bne.s     ptch_term2                ; no -> ignore It
            move.l    a0,$fffc(a1)              ; point it to _term
ptch_term2: adda.l    d1,a0                     ; increment the exception number in the upper 8 bits
            dbra      d0,ptch_term1             ; next vector ->
ENDIF

            bsr       _dskboot                  ; attempt to boot from disk
            bsr       mtest                     ; memory test and attempt to boot from DMA
            bsr       runresapps                ; execute resident applications in memory

            tst.w     (_cmdload).l              ; load shell from disk?
            beq.s     st_1                      ; (no -- execute GEM in ROM)
            bsr       _auto
            move.l    #osroml,(_sysbase).l      ; -> base of OS
            pea       nullenv(pc)               ; null environment string
            pea       nullenv(pc)               ; null argument string
            pea       cmdname(pc)               ; push shell filename
            clr.w     -(sp)                     ; load-and-go flavor of execute
            bra.s     st_x                      ; exec shell ("never return")

*--- bring up GEM:
st_1:       bsr       _auto                     ; do auto-exec
IF ROM_STBOOK
            bsr       _auto_ROMDISK
ENDIF
            move.l    #osroml,(_sysbase).l      ; -> base of OS

*--- kludge up an enviroment string
            lea       orig_env(pc),a0           ; kludge up an environment string, a0 -> original environment string
            movea.l   #the_env,a1               ; a1 -> place to put it
st_2:       cmpi.b    #'#',(a0)                 ; look for drive# character
            bne.s     st_3                      ; (not it)
            movea.l   a1,a2                     ; a2 -> place to put drive#
st_3:       move.b    (a0)+,(a1)+               ; copy a byte
            bpl.s     st_2                      ; copy while not end-of-string
            move.b    (_bootdev).l,d0           ; compute drive#, and shove it
            add.b     #$41,d0                   ; into the env string at the
            move.b    d0,(a2)                   ; appropriate spot

*--- kludge up an enviroment string:
            pea       (the_env).l               ; push address of environment string
            pea       (nullenv).l               ; no arguments
            pea       nullenv(pc)               ; null shell name (in ROM, after all)
            move.w    #5,-(sp)                  ; createPSP flavor of exec
            move.w    #$4b,-(sp)                ; exec function#
            trap      #1                        ; get pointer to PSP
            adda.w    #14,sp                    ; (clean up cruft)
            movea.l   d0,a0                     ; a0 -> PSP
            move.l    (exec_os).l,8(a0)         ; stuff saddr of GEM in PSP
            pea       (the_env).l               ; our environment string
            move.l    a0,-(sp)                  ; push addr of PSP
            pea       nullenv(pc)               ; null filename
            move.w    #4,-(sp)                  ; just-go
st_x:       move.w    #$4b,-(sp)                ; function = exec
            trap      #1                        ; do it
            adda.w    #14,sp                    ; cleanup stack

*+
* When startup fails (or if the exec returns,
* which "cannot happen") fake a system reset:
*-
            jmp       (reseth).l                ; back to the beginning...

*+
* Default enviroment string
* Cannot be more than 20 chars long without modifying
* the declaration for the_env,
* Any char >= $80 terminates the string (and is included in it)
* The last '#' character is replaced by the boot drive's name (A, B, ...)
*-
orig_env:   DC.B      'PATH=',0                 ; default pathname
            DC.B      '#:\',0                   ; is the boot device
            DC.B      $00                       ; terminate env string
            DC.B      $ff                       ; end of the env string (for our copy)

cmdname:    DC.B      'COMMAND.PRG',0           ; shell name
gemname:    DC.B      'GEM.PRG',0               ; desktop name
            DC.B      0
            DC.B      0

setikbd:    DC.B      $80,$01

*+
* _dskboot - boot (or return diagnostics)
* Passed:       nothing
* Returns:      D0.W = error number (if nonzero)
*-
_dskboot:   moveq     #3,d0                     ; %%3 ap cart
            bsr       cartscan
IF ROM_STBOOK
            bsr       cartscan_STBOOK_EXTROM
ENDIF
            movea.l   (hdv_boot).w,a0           ; go through boot vector
            jsr       (a0)
            tst.w     d0                        ; any errors?
            bne.s     dskb1                     ; (yes -- punt)
            movea.l   (_dskbufp).w,a0           ; a0 -> disk buffer
            jsr       (a0)                      ; execute boot sector (it might return)
dskb1:      rts                                 ; return status

mtest:      move.l    #80*200,d7                ; 80s
            cmp.l     (_hz_200).l,d7            ; is the system running for > 80s?
            bcs       dmaBoot                   ; (no memory test)
            movea.w   #0,a5                     ; done status = false
mtestloop:  cmpa.w    #0,a5                     ; memtest done?
            bne       mtest4                    ; (yes)
            bsr       memtest
            movea.w   d0,a5                     ; memtest done?
            beq       mtest5                    ; (no)
            move.w    #$1b,-(sp)
            move.l    #$30002,-(sp)
            trap      #$d                       ; Bconout(2, ESC)
            move.w    #'p',4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, 'p') Reverse video
            move.w    #$1b,4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, ESC)
            move.w    #'w',4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, 'w') Discard end of line
            move.l    #200,d5                   ; 200 = 1s
            move.w    #79,d4                    ; 79
            tst.b     (sshiftmd).l
            bne.s     mtest2
            move.l    #2*200,d5                 ; 2s
            moveq     #39,d4                    ; 39
mtest2:     move.l    d5,d6
            move.l    d4,d3
mtestsp:    move.w    #' ',4(sp)                ; d3 + 1 spaces
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, ' ')
            dbra      d4,mtestsp
            move.w    #$d,4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, 13) CR
            subq.l    #1,d3
mtestsp2:   move.w    #' ',4(sp)                ; d3 spaces
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, ' ')
            dbra      d3,mtestsp2
            move.w    #$1b,4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, ESC)
            move.w    #'q',4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, 'q') Normal video
            addq.l    #6,sp
mtest4:     cmp.l     (_hz_200).l,d6
            bhi.s     mtest5
            move.w    #$1b,-(sp)
            move.l    #$30002,-(sp)
            trap      #$d                       ; Bconout(2, ESC)
            move.w    #'K',4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, 'K') Clear to eol
            move.w    #8,4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, 8) Backspace
            addq.w    #6,sp
            add.l     d5,d6
mtest5:     cmpa.w    #0,a5                     ; memtest done?
            beq.s     mtest6                    ; (no)
            cmp.l     (_hz_200).l,d7
            bls.s     mtest8
mtest6:
IF ROM_STBOOK
		 	move.w    (tt_mcu+4).l,d0
            not.w     d0
            and.w     #$c,d0                    ; check bit 2 & 3
            bne.s     mtest7
ENDIF
            move.l    #$10002,-(sp)
            trap      #$d                       ; Bconstat(2) - key pressed?
            addq.l    #4,sp
            tst.l     d0
            beq       mtestloop                 ; (no - continue)
            move.l    #$20002,-(sp)
            trap      #$d                       ; Bconin(2) - read key
            addq.l    #4,sp
mtest7:     cmpa.w    #0,a5                     ; memtest done?
            bne.s     mtest8                    ; (yes)
            bsr       memtestabort
            move.l    d7,(_hz_200).l
            bra.s     dmaBoot
mtest8:     move.l    d7,(_hz_200).l
            move.w    #$d,-(sp)
            move.l    #$30002,-(sp)
            trap      #$d                       ; Bconout(2, 13) CR
            move.w    #$1b,4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, ESC)
            move.w    #'K',4(sp)
            move.l    #$30002,(sp)
            trap      #$d                       ; Bconout(2, 'K') Clear to eol
            addq.l    #6,sp

*--- boot from DMA device
dmaBoot:
IF ROM_STBOOK
			clr.w     (the_env).l               ; index to the dmaDevList
ELIF ROM_TOS206
			moveq	  #$10,d4
ELIF ROM_TOS306
			jsr       (_scsisubb).l

			moveq     #8,d4
			move.b    (scu_gp1).w,d0
			and.w     #$f8,d0
			move.w    d0,(defaultBootDevice).w
			bne.s     dmaBoot2
			pea       (defaultBootDevice).w
			move.w    #2,-(sp)
			clr.l     -(sp)
			jsr       (_NVMaccess).l
			adda.w    #$a,sp
			tst.w     d0
			beq.s     dmaBoot2
ENDIF
            clr.w     (defaultBootDevice).w
dmaBoot2:   move.w    #1,d1                     ; d1 -> 2 tries per device
dmaBoot3:   move.w    d1,-(sp)
IF ROM_STBOOK
            move.w    (the_env).l,d4
            move.b    dmaDevList(pc,d4.w),d4
ENDIF
            move.w    d4,-(sp)                  ; pdev
            move.l    (_dskbufp).w,-(sp)        ; buf = _dskbufp
            move.w    #1,-(sp)                  ; count = 1
            clr.l     -(sp)                     ; sectnum = 0
            jsr       (_dmaread).l              ; read first sector of this device
            adda.w    #12,sp
            move.w    (sp)+,d1
            tst.l     d0                        ; read successful?
            beq.s     dmaBoot4                  ; yes ->
            addq.l    #1,d0                     ; error == time out?
            dbeq      d1,dmaBoot3               ; timeout or another try left? ->
            bra.s     dmaexecl                  ; try next device
dmaBoot4:   movea.l   (_dskbufp).w,a0
            move.w    #$ff,d0                   ; 256 word checksum over the boot sector
            moveq     #0,d1
dmaBoot5:   add.w     (a0)+,d1
            dbra      d0,dmaBoot5
            cmp.w     #$1234,d1                 ; checksum == 0x1234?
            beq.s     dmaexec                   ; execute this valid boot sector ->
dmaexecl:
IF ROM_STBOOK
			move.w    (the_env).l,d4
            addq.w    #1,d4                     ; increment next device
            move.w    d4,(the_env).l
            cmpi.b    #$ff,dmaDevList(pc,d4.w)  ; end of the device list?
            bne.s     dmaBoot2                  ; no -> continue with the next one
ELSE
			addq.w    #1,d4
			move.w    d4,d0
			and.w     #7,d0
			bne.s     dmaBoot2
			cmp.w     #8,d4
			beq.s     dmaBootx
			moveq     #0,d4
			bra.s     dmaBoot2
dmaBootx:
ENDIF
            rts                                 ; no valid boot sector found -> return

IF ROM_STBOOK
dmaDevList: DC.B      $10,$11,$00,$01,$02,$03,$04,$05 ;boot order of DMA devices, $ff terminates the list
            DC.B      $06,$07,$ff
ENDIF

dmaexec:    movea.l   (_dskbufp).w,a0           ; pointer to the boot sector buffer
            move.l    #'DMAr',d3                ; d3 -> 'DMAr' magic
            move.w    d4,d7                     ; d4 -> pdev
            asl.w     #5,d7                     ; d7 = 0
            move.w    (defaultBootDevice).w,d5  ; defaultBootDevice = 0
IF !ROM_STBOOK
			move.l    d4,-(sp)
ENDIF
            move.l    (hdv_rw).l,-(sp)          ; save read sector function pointer
            jsr       (a0)                      ; execute boot sector
            move.l    (sp)+,d0
IF !ROM_STBOOK
			move.l    (sp)+,d4
ENDIF
            cmp.l     (hdv_rw).w,d0             ; did the read sector function change?
            beq.s     dmaexecl                  ; no -> no device driver was loaded -> continue to load boot sectors
            rts                                 ; a new device driver was loaded -> return


*+
* cartscan - scan cartridge memory for runable applications
* Passed:       d0 = bit# to test in application's initialization vector
* Returns:      after all applications have been examined
* Uses:         a0,d0
*-
IF ROM_STBOOK
cartscan_STBOOK_EXTROM:lea (STBOOK_EXTROM).l,a0
            bra.s     cartscan2
ENDIF

cartscan:   lea       (cartbase).l,a0           ; scan cartridge memory for runable applications, a0 -> cartridge memory
cartscan2:  cmpi.l    #$abcdef42,(a0)+          ; correct magic number?
            bne.s     ca_r                      ; (no, so return)
ca_1:       btst      d0,4(a0)                  ; test bit in MSB of INIT address
            beq.s     ca_2                      ; (not set, so don't execute)
            movem.l   d0-d7/a0-a6,-(sp)         ; save everything
            move.l    4(a0),d0                  ; d0 -> initialization address
            and.l     #$ffffff,d0               ; mask out bit# in the top 8 bits of the address
            movea.l   d0,a0
            jsr       (a0)                      ; call cartridge application
            movem.l   (sp)+,d0-d7/a0-a6         ; restore everything
ca_2:       tst.l     (a0)                      ; test link address
            movea.l   (a0),a0                   ; a0 -> next header (or NULL)
            bne.s     ca_1                      ; loop on next header
ca_r:       rts

_rts:       rts


*+
* memchk - check pattern written to memory
*       Passed:         d1.l = offset
*                       a0 = base of pattern ($1f8 bytes long)
*                       a5 -> return address
*
*       Returns:        EQ: the pattern matched
*                       NE: the pattern didn't match
*
*       Uses:           d0.w, a1
*       Called-by:      Coldstart memory-sizing routine.
*-
memchk:     adda.l    d1,a0                     ; a0 -> memory to check
            clr.w     d0                        ; zap pattern seed
            lea       $1f8(a0),a1               ; a1 -> ending address
memchk1:    cmp.w     (a0)+,d0                  ; match?
            bne.s     memchkr                   ; (no -- return NE)
            add.w     #$fa54,d0                 ; yes -- bump pattern
            cmpa.l    a0,a1                     ; matched entire pattern?
            bne.s     memchk1                   ; (no)
memchkr:    jmp       (a4)                      ; "return" to caller


*+
* val_memval - test memory configuration validation
*  Passed:      a6 -> return addressd
*  Returns:     a5 -> 0 (quick zeropage)
*               EQ: memory setup OK
*               NE: memory never configured succesfully
*
*-
val_memval: cmpi.l    #$752019f3,(memvalid).w   ; test memory configuration validation, check first magic number
            bne.s     val_mr                    ; (mismatched -- return NE)
            cmpi.l    #$237698aa,(memval2).w    ; check one more (for paranoia)
            bne.s     val_mr                    ; (mismatched -- return NE)
            cmpi.l    #$5555aaaa,(memval3).w    ; check a third time (for more paranoia)
val_mr:     jmp       (a6)                      ; return EQ/NE


*+
* Default palette assignments.
*  Sort of corresponding to the GSX spec.
*-
colors:     DC.W      $0fff                     ; 0 white
            DC.W      $0f00                     ; 1 red
            DC.W      $00f0                     ; 2 green
            DC.W      $0ff0                     ; 3 yellow
            DC.W      $000f                     ; 4 blue
            DC.W      $0f0f                     ; 5 magenta
            DC.W      $00ff                     ; 6 cyan
            DC.W      $0555                     ; 7 "low white"
            DC.W      $0333                     ; 8 grey
            DC.W      $0f33                     ; 9 light red
            DC.W      $03f3                     ; 10 light green
            DC.W      $0ff3                     ; 11 light yellow
            DC.W      $033f                     ; 12 light blue
            DC.W      $0f3f                     ; 13 light magenta
            DC.W      $03ff                     ; 14 light cyan
            DC.W      $0000                     ; 15 black

*+
* hbl - force caller to IPL
* Oh-well:      "Yeah, it sucks, but it works" (--lt)
*
* Note:         Hacks caller's IPL to 3 (if it was 0). This is
*               a kludge against fascist programs and certain
*               debuggers that insist on starting processes up
*               at IPL 0.
*
*-
hbl:        move.w    d0,-(sp)                  ; save d0
            move.w    2(sp),d0                  ; get pushed SR
            and.w     #$700,d0                  ; strip crufty bits
            bne.s     hbl_r                     ; not IPL 0, so punt
            ori.w     #$300,2(sp)               ; force caller to IPL 3
hbl_r:      move.w    (sp)+,d0                  ; restore d0, back to victim
            rte

*+
* vbl - vertical black interrupt handler
*
*-
vbl:        addq.l    #1,(_frclock).l           ; bump frame clock
            subq.w    #1,(vblsem).l             ; P(vblsem) -- vblank locked?
            bmi       vblret

            movem.l   d0-d7/a0-a6,-(sp)         ; save registers
            addq.l    #1,(_vbclock).l           ; bump unblocked-frame Clock

*------ Call deferred interrupt vectors
            move.w    (nvbls).l,d7              ; d7 = # of deferred vblank vectors
IF ROM_STBOOK
            beq.s     vbl12                     ; (punt if no vectors)
ELSE
			beq       vbl12                     ; (punt if no vectors)
ENDIF
            subq.l    #1,d7                     ; turn into DBRA count
            movea.l   (_vblqueue).w,a0          ; a0 -> vectors
vbl10:      movea.l   (a0)+,a1                  ; a1 -> deferred vector
            cmpa.w    #0,a1                     ; if(a1 == NULL) continue;
            beq.s     vbl11
            movem.l   d7-d7/a0,-(sp)            ; save registers
            jsr       (a1)                      ; call routine
            movem.l   (sp)+,d7-d7/a0            ; restore registers
vbl11:      dbra      d7,vbl10                  ; loop for more vectors

IF ROM_TOS206 || ROM_TOS306
			move.b    (gpip).w,d1
			tst.b     ($a02).l                  ; microwire interface available?
			beq.s     vbl11b                    ; (no)
			move      sr,-(sp)
			ori       #$700,sr
vbl11a:		move.b    (sndmactl+1).w,d0
			move.b    (gpip).w,d1
			btst      #7,d1
			sne       d1
			move.b    (gpip).w,d2
			btst      #7,d2
			sne       d2
			cmp.b     d1,d2
			bne.s     vbl11a
			cmp.b     (sndmactl+1).w,d0
			bne.s     vbl11a
			move      (sp)+,sr
			btst      #0,d0
			beq.s     vbl11b
			not.b     d1
vbl11b:

IF ROM_TOS206
			move.b    (v_shf_mod).w,d0
			and.b     #3,d0
			cmp.b     #2,d0
			bge.s     vbl11c
ELIF ROM_TOS306
			move.b    (shift_tt).w,d0
			and.b     #7,d0
			cmp.b     #6,d0
			beq.s     vbl11c
ENDIF
			btst      #7,d1
			bne.s     vbl11e
			bsr       _wvbl
IF ROM_TOS206
			move.b    #2,d0
ELIF ROM_TOS306
			move.b    #6,d0
ENDIF
			bra.s     vbl11d
vbl11c:		btst      #7,d1
			beq.s     vbl11e
			move.b    (defshiftmd).w,d0
IF ROM_TOS206
			cmp.b     #2,d0
			blt.s     vbl11d
ELIF ROM_TOS306
			cmp.b     #6,d0
			bne.s     vbl11d
ENDIF
			clr.b     d0
vbl11d:		
			move.b    d0,(sshiftmd).w
IF ROM_TOS206
			move.b    d0,(v_shf_mod).w
ELIF ROM_TOS306
			move.b    (shift_tt).w,d1
			and.b     #$f8,d1
			or.b      d0,d1
			move.b    d1,(shift_tt).w
ENDIF

			movea.l   (swv_vec).w,a0
			jsr       (a0)
vbl11e:
ENDIF
            jsr       (blink).l                 ; blink cursor

*--- reload color palettes
            tst.l     (colorptr).w              ; reload color palettes, if(colorptr != NULL)....
            beq.s     vbl1
            movea.l   (colorptr).w,a0           ; a0 -> user's color base
            lea       (palette).w,a1            ; a1 -> hardware palette base
            move.w    #7,d1                     ; d0 = count
vbl2:       move.l    (a0)+,(a1)+               ; load a palette
            dbra      d1,vbl2                   ; ...and repeat
            clr.l     (colorptr).w              ; zap colorptr

*--- reload display base register
vbl1:       tst.l     (screenpt).w              ; reload display base register, if(screenpt == NULL) don't
            beq.s     vbl5
            move.l    (screenpt).w,(_v_bas_adr).w ;set OS variable
            move.b    (_v_bas_adr+2).w,(v_bas_m).w ;load "middle" pointer
            move.b    (_v_bas_adr+1).w,(v_bas_h).w ;load "high" pointer
            move.b    (_v_bas_adr+3).w,(v_bas_l).w ;load "low" pointer

*------ Floppy drive-select timeout
vbl5:       bsr       _flopvbl                  ; (no args)

vbl12:      tst.w     (_prtcnt).w               ; monitor screen dump flag - printscreen active?
            bne.s     no_print                  ; no

*+
* printScreen
*
* We re-enable vblanks here, until the printScreen finishes -
*
*-
            bsr       _dumpit                   ; dump screen

no_print:   movem.l   (sp)+,d0-d7/a0-a6         ; restore registers & return (and a handy RTE)
vblret:     addq.w    #1,(vblsem).l             ; V(vblsem) [release vblank]
_rte:       rte


*+
* wvbl - wait for next vblank
* Passed:       nothing
* Returns:      at beginning of next vblank
* Uses:         D0
*-
wvbl:       move      sr,-(sp)                  ; save psw
            andi      #$fbff,sr                 ; enable vbl interrupts
            move.l    (_frclock).l,d0           ; d0 = frame clock
wvbl1:      cmp.l     (_frclock).l,d0           ; wait for clock to change
            beq.s     wvbl1
            move      (sp)+,sr                  ; then restore psw & return
            rts


*+
* _critic - critical error handler binding for C
* Falls-into:   _critich
* (screwy way to save two bytes....)
*
*-
crit_err:   move.l    (etv_critic).l,-(sp)      ; jump through critic vector

*+
* _critich - default critical error handler
* Loads -1 into D0 and returns
*
*-
_critich:   moveq     #-1,d0                    ; default return value = ERROR
            rts                                 ; return to trap invoker


*+
* trp13h - GEMDOS BIOS trap handler (trap 13)
* trp14h - Atari BIOS extensions (trap 14)
* traph  - trap handler
*
* On the stack:
*       From super-             From user
*       visor mode:             mode:
*       -----------             ------------
*       N(sp) args              N(usp) args
*       6(sp) func#             6(usp) func#
*       2(sp) ret               2(ssp) ret
*        (sp) SR                 (ssp) SR
*
* Returns:      anything in D0
* Uses:         d0-d2/a0-a2
* Keeps:        C registers
*
* Notes         BIOS traps are re-entrant to 'nlevels' (declared near the
*               beginning of this file).  Attempts to recurse more than
*               'nlevels' will probably result in a crash.
*
*               BIOS calls may be made from user mode.  (This differs from
*               the current GEMDOS spec, which states that BIOS traps are
*               available from supervisor mode only).
*
*-
trp14h:     lea       trp14tab(pc),a0           ; a0 -> trap14 jump table
            bra.s     traph
trp13h:     lea       trp13tab(pc),a0           ; a0 -> trap13 jump table
traph:      movea.l   (savptr).l,a1             ; a1 - registers save area
            move.w    (sp)+,d0                  ; pop SR and save it
            move.w    d0,-(a1)                  ; (need in D0 for user-mode test)
            move.l    (sp)+,-(a1)               ; save return addr
IF !ROM_TOS306
            tst.w     (_longframe).w
            beq.s     traph2
ENDIF
            tst.w     (sp)+
traph2:     movem.l   d3-d7/a3-a7,-(a1)         ; save C registers + super stack
            move.l    a1,(savptr).l             ; update save-area pointer

* make sure we have the right stack, call function:
            btst      #13,d0                    ; was in user mode?
            bne.s     b_supr                    ; (was in super: use super stack)
            move      usp,sp                    ; use user stack
b_supr:     move.w    (sp)+,d0                  ; get function#
            cmp.w     (a0)+,d0                  ; out of range?
            bge.s     b_exit                    ; (yes, so punt)
            move.w    d0,d1
            lsl.w     #2,d1                     ; turn D0 into longword index
            move.l    (a0,d1.w),d1              ; get pointer to function handler
IF ROM_TOS306
			bclr      #0,d1						; clear the indirect but set the old status in Z
			movea.l   d1,a0
			beq.s     b_1                       ; points to code
ELSE
            movea.l   d1,a0                     ; (quick and dirty test-for-negative)
            bpl.s     b_1                       ; points to code
ENDIF
            movea.l   (a0),a0                   ; indirect through RAM...
b_1:        suba.l    a5,a5                     ; a5 -> zero page
            jsr       (a0)                      ; call BIOS function

* restore registers, cleanup stack and return:
b_exit:     movea.l   (savptr).l,a1             ; a1 -> register save area
            movem.l   (a1)+,d3-d7/a3-a7         ; restore C registers + super stack
IF !ROM_TOS306
            tst.w     (_longframe).w
            beq.s     trph3
ENDIF
            clr.w     -(sp)
trph3:      move.l    (a1)+,-(sp)               ; push return address
            move.w    (a1)+,-(sp)               ; push old SR
            move.l    a1,(savptr).l             ; update save-pointer
            rte                                 ; return to caller


*------ jump table for GEMDOS functions:
trp13tab:   DC.W      $000c                     ; number of entries in jump table
            DC.L      _get_mpb
            DC.L      bconstat
            DC.L      bconin
            DC.L      bconout
IF ROM_TOS306
            DC.L      hdv_rw | 1
ELSE
			DC.L      hdv_rw
ENDIF
            DC.L      _setexc
            DC.L      _tickcal
IF ROM_TOS306
            DC.L      hdv_bpb | 1
ELSE
            DC.L      hdv_bpb
ENDIF
            DC.L      bcostat
IF ROM_TOS306
            DC.L      hdv_mediach | 1
ELSE
			DC.L      hdv_mediach
ENDIF
            DC.L      _drvmap
            DC.L      _shift

*------ jump table for Atari BIOS extensions:
trp14tab:
IF ROM_TOS306
			DC.W      $0060                     ; number of entry points
ELSE
			DC.W      $0041                     ; number of entry points
ENDIF
            DC.L      initmous
            DC.L      _rts
            DC.L      _physbase
            DC.L      _logbase
            DC.L      _getrez
            DC.L      _setscreen
            DC.L      _setpalette
            DC.L      _setcolor
            DC.L      _floprd
            DC.L      _flopwr
            DC.L      _flopfmt
            DC.L      _getdsb
            DC.L      midiws
            DC.L      mfpint
            DC.L      iorec
            DC.L      rsconf
            DC.L      keytrans
            DC.L      _rand
            DC.L      _proto_bt
            DC.L      _flopver
            DC.L      _dumpit
            DC.L      _cursconf
            DC.L      settime
            DC.L      gettime
            DC.L      bioskeys
            DC.L      ikbdws
            DC.L      jdisint
            DC.L      jenabint
            DC.L      giaccess
            DC.L      offgibit
            DC.L      ongibit
            DC.L      xbtimer
            DC.L      dosound
            DC.L      setprt
            DC.L      ikbdvecs
            DC.L      kbrate
            DC.L      _prtblk
            DC.L      wvbl
            DC.L      supexec
            DC.L      puntaes
            DC.L      _rts
            DC.L      floprate
            DC.L      _dmaread
            DC.L      _dmawrite
            DC.L      _bconmap
            DC.L      _rts
IF ROM_TOS306
			DC.L	  _NVMaccess
ELSE
            DC.L      _rts
ENDIF
IF ROM_STBOOK || ROM_TOS206
			DC.L      _Waketime
ELSE
            DC.L      _rts
ENDIF
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      blitmode
IF ROM_TOS306
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts

			DC.L      _EsetShift
			DC.L      _EgetShift
			DC.L      _EsetBank
			DC.L      _EsetColor
			DC.L      _EsetPalette
			DC.L      _EgetPalette
			DC.L      _EsetGrey
			DC.L      _EsetSmear

			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts
			DC.L      _rts

* this routine seems to be unused?
_e00fae:	btst      #0,1(sp)					; read?
			bne.s     _hdv_rw_ptc				; (no) => no patch necessary, write doesn't need to clear the cache
			move.l    #_hdv_rw_ptt,(sp)			; patch return address
_hdv_rw_ptc:movea.l   (hdv_rw).w,a0
			jmp       (a0)

_hdv_rw_ptt:move.l    d0,-(sp)
			bsr       _flush_cache				; flush the CPU cache
			move.l    (sp)+,d0
			jmp       (b_exit).l				; exit trap
ENDIF

*+
* supexec - execute some code in supervisor mode
*
*-
supexec:    movea.l   4(sp),a0                  ; a0 -> code
            jmp       (a0)                      ; execute it


*+
* Character device I/O
*
* No check is made for "bogus" device numbers.  A wierd device
* number will result in a crash.
*
*-
bconstat:   lea       (xconstat).w,a0           ; a0 -> stat table
            moveq     #0,d1
            bra.s     chsw
bconin:     lea       (xconin).w,a0             ; a0 -> input table
            moveq     #4,d1
            bra.s     chsw
bcostat:    lea       (xcostat).w,a0            ; a0 -> ostat table
            moveq     #8,d1
            bra.s     chsw
bconout:    lea       (xconout).w,a0            ; a0 -> output table
            moveq     #$c,d1
chsw:       move.w    4(sp),d0                  ; get device number
            cmp.w     #5,d0
            bls.s     chsw2
            subq.l    #6,d0
            cmp.w     (bdevcount).w,d0
            bcc.s     chsw1
            movea.l   (bdevtabptr).w,a0
            asl.w     #3,d0
            adda.w    d0,a0
            add.w     d0,d0
            adda.w    d0,a0
            movea.l   (a0,d1.w),a0
            jmp       (a0)

chsw1:      moveq     #0,d0
            rts

chsw2:      lsl.w     #2,d0                     ; turn into longword index
            movea.l   (a0,d0.w),a0              ; get address of handler
            jmp       (a0)                      ; jump to it

*+
* Jump tables for
*       0 - lst: (printer)
*       1 - aux: (rs232)
*       2 - con: (screen)
*       3 - Atari midi
*       4 - Atari keyboard (output only)
*       5 - raw console output (bypass vt52 pressure cooker)
*
* No range checking is performed.  If a bogus device number
* is passed to the BIOS' character I/O handler, the system
* will crash to become funky duex.
*
*-
tconstat:   DC.L      _rts
            DC.L      auxistat
            DC.L      constat
            DC.L      midstat
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
tconin:     DC.L      _lstin
            DC.L      auxin
            DC.L      conin
            DC.L      midin
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
tcostat:    DC.L      _lstostat
            DC.L      _auxostat
            DC.L      conoutst
            DC.L      ikbdost
            DC.L      midiost
            DC.L      _rts
            DC.L      _rts
            DC.L      _rts
tconout:    DC.L      _lstout
            DC.L      _auxout
            DC.L      conout
            DC.L      midiwc
            DC.L      ikbdwc
            DC.L      _asc_out
            DC.L      _rts
            DC.L      _rts

*+
* _drvmap - return "active drive" bit vector
* Passed:       nothing
* Returns:      D0.L = a bit vector of live (rwabs'able) block device
*
*-
_drvmap:    move.l    (_drvbits).w,d0
            rts


*+
* _shift - get/set keyboard shift state
* Synopsis:     LONG _shift(bits)
*               WORD bits
*
* Returns:      D0.B = shift/alt/ctl/shift' bits
*
* Note:         Since the shift bits are changed at interrupt
*               level, any set from a get of the shift state
*               must be done as a critical section.
*
*-
_shift:     moveq     #0,d0
            move.b    (kb_shift).w,d0
            move.w    4(sp),d1
            bmi.s     shifr
            move.b    d1,(kb_shift).w
shifr:      rts


*+
* _get_mpb - return initial memory parameter block
* Synopsis:     _get_mpb(mpb)
*               MPB *mpb;
*
* Returns:      The properly initialized MPB.
*               The MPB points to an MD somewhere in BSS. The MD /must/
*               be in RAM since DOS will modify it.
*-
_get_mpb:   movea.l   4(sp),a0                  ; a0 -> MPB
            lea       (themd).w,a1              ; a1 -> MD

*--- initialize MPB:
            move.l    a1,(a0)                   ; mp_mfl = &themd;
            clr.l     4(a0)                     ; mp_mal = NULL;
            clr.l     8(a0)                     ; mp_rover = NULL;

*---- initialize MD:
            clr.l     (a1)                      ; m_link = NULL;
            move.l    (_membot).w,4(a1)         ; m_start = _membot;
            move.l    (_memtop).w,d0            ; m_length = _memtop - membot;
            sub.l     (_membot).w,d0
            move.l    d0,8(a1)
            clr.l     $c(a1)                    ; m_own = NULL;

IF ROM_TOS306
			cmpi.l    #$1357bd13,(ramvalid).l	; fast mem available?
			bne.s     _get_mpbret				; (no)
			cmpi.l    #$1000000,(ramtop).l		; fast mem installed?
			bls.s     _get_mpbret				; (no)
			lea       ($0a02).w,a2				; themd_tt
			move.l    a2,(a1)					; themd.m_link = &themd_tt
			clr.l     (a2)						; m_link = NULL
			move.l    #$1000001,4(a2)			; m_start = base address of fast mem | 1
			move.l    (ramtop).l,d0
			sub.l     #$1000000,d0
			move.l    d0,8(a2)					; m_length = size of fast mem
			clr.l     $c(a2)					; m_own = NULL
_get_mpbret:
ENDIF
            rts

*+
* _setexc - set exception vector
* Synopsis:     setexc(vecno, addr)
*               If 'addr' < 0, the vector is not set.
*
*               Extended vectors ($100 though $107) are located in the
*               first eight longwords of BSS, at $400. This is for
*               convienience -- they could really be located anywhere.
*
* Returns:      D0.L = original vector value
*
*-
_setexc:    move.w    4(sp),d0                  ; d0 = vector #
            lsl.w     #2,d0                     ; turn into longword index
            suba.l    a0,a0
            lea       (a0,d0.w),a0              ; a0 -> vector
            move.l    (a0),d0                   ; d0 = current vector address
            move.l    6(sp),d1                  ; d1 = what_to_change_it_to
            bmi.s     setex1                    ; punt if (d1 < 0)
            move.l    d1,(a0)                   ; set vector address
setex1:     rts

*+
* _tickcal - return system timer calibration value (in ms)
*
*-
_tickcal:   moveq     #0,d0                     ; cast to unsigned longword
            move.w    (_timr_ms).w,d0           ; get calibration
            rts

*+
* _physbase - get physical display base
*
*-
_physbase:  moveq     #0,d0                     ; cleanup pointer-to-be
            move.b    (v_bas_h).w,d0            ; load and shift bits 16..23
            lsl.w     #8,d0
            move.b    (v_bas_m).w,d0            ; load and shift bits 8..15
            lsl.l     #8,d0
            tst.b     (STEFlag).l
            bne.s     _physbase2
            move.b    (v_bas_l).w,d0
_physbase2: rts                                 ; return pointer in d0

*+
* _logbase -get logical display base
*
*-
_logbase:   move.l    (_v_bas_adr).w,d0         ; set software shadow
            rts

*+
* _getrez - get current screen rez
*-
_getrez:    moveq     #0,d0                     ; cleanup dirty bits
IF ROM_STBOOK
            move.b    (sshiftmd).w,d0           ; get screen rezolution
            and.b     #3,d0                     ; strip garbage bits
ELIF ROM_TOS206
			move.b    (v_shf_mod).w,d0          ; get screen rezolution
            and.b     #3,d0                     ; strip garbage bits
ELIF ROM_TOS306
			move.b    (shift_tt).w,d0
			and.b     #7,d0
ENDIF
            rts                                 ; return rez

*+
* _setscreen - set screen location(s), rez
*       _setscreen(logicalLoc, physicalLoc, rez)
*       LONG logicalLoc, physicalLoc;
*       WORD rez;
*
*-

*--- set logical location:
_setscreen: tst.l     4(sp)                     ; if(logloc < 0) then ignore it
            bmi.s     f5a
            move.l    4(sp),(_v_bas_adr).w      ; set software pointer from logloc

*--- set physical location:
f5a:        tst.l     8(sp)                     ; if(physloc < 0) then ignore it
            bmi.s     f5b
            move.b    9(sp),(v_bas_h).w         ; set bits 16..23 of hardware pointer
            move.b    $a(sp),(v_bas_m).w        ; set bits 8..15 of hardware pointer
            move.b    $b(sp),(v_bas_l).w        ; set bits 0..7 of hardware pointer

*--- set screen resolution (clears the screen, clobbers the cursor):
f5b:        tst.w     $c(sp)                    ; if(rez < 0) then ignore it
            bmi.s     f5r
            bsr       _wvbl                     ; wait for start of vertical-blank
            move.b    $d(sp),(sshiftmd).w       ; set software shadow
IF ROM_TOS306
			move.b    (shift_tt).w,d0
			and.b     #$f8,d0
			or.b      $d(sp),d0
			move.b    d0,(shift_tt).w
ELSE
            move.b    (sshiftmd).w,(v_shf_mod).w ;set hardware location
ENDIF
            clr.w     (vblsem).w                ; disable vblank processing
            jsr       (esc_init).l              ; re-initialize glass tty routines
            move.w    #1,(vblsem).w             ; re-enable vblanks
f5r:        rts

*+
* _setpalette - set palette (on next vblank)
*       _setpalette(LONG palettePtr)
*
*-
_setpalette:move.l    4(sp),(colorptr).w        ; set software pointer
            rts                                 ; (updated by vbl handler)

*+
* _setcolor - set single color, return old color
*       _setcolor(WORD colorNum, WORD colorValue)
*
*-
_setcolor:  move.w    4(sp),d1                  ; get color number
            add.w     d1,d1                     ; turn into word index
            and.w     #$1f,d1                   ; force color range (prevent buserr)
            lea       (palette).w,a0            ; a0 -> base of palette memory
            move.w    (a0,d1.w),d0              ; return old color
            tst.b     (STEFlag).l
            beq.s     _setc0a
            and.w     #$777,d0                  ; mask dirty bits
            bra.s     _setc0b
_setc0a:    and.w     #$fff,d0
_setc0b:    tst.w     6(sp)                     ; if new color is <0, don't set it
            bmi.s     _setc1                    ; (punt)
            move.w    6(sp),(a0,d1.w)           ; set new color
_setc1:     rts

*+
* puntaes - throw-away AES, restart the system
*  Passed:      nothing
*  Uses:        everything
*  Returns:     if AES was already thrown away
*
*-
puntaes:    movea.l   osroml+20(pc),a0          ; get pointer to magic
            cmpi.l    #$87654321,(a0)           ; is the magic still there?
            bne.s     paes1                     ; no -- just return
            cmpa.l    (phystop).w,a0            ; is it in ROM?
            bge.s     paes1                     ; yes -- we can't do anything about it
            clr.l     (a0)                      ; clobber AES!
            bra       reseth                    ; restart the system
paes1:      rts

*+
* _term - terminate current process
* Called-by:    Uncaught traps (bus errors, and so on)
* Saves:        processor state (in a bailout area)
*
*-
_term:
IF !ROM_TOS306
		  	jsr       (savp_2).l                ; stack PC
savp_2:
ENDIF
			move.l    (sp)+,(proc_pc).w         ; save bogus PC + exception number
            movem.l   d0-d7/a0-a7,(proc_regs).w ; common registers
IF ROM_TOS306
			move.l    2(sp),(proc_pc).w
			move.w    6(sp),d0
			and.w     #$fff,d0
			asr.w     #2,d0
			move.b    d0,(proc_pc).w
ENDIF
            move      usp,a0                    ; save USP
            move.l    a0,(proc_usp).w
            moveq     #15,d0                    ; save 16 words off top of
            lea       (proc_stk).w,a0           ; the stack (enough for
            movea.l   sp,a1                     ; any possible 680000 exception)
savp_1:     move.w    (a1)+,(a0)+               ; save a word
            dbra      d0,savp_1
            move.l    #$12345678,(proc_lives).w ; set magic number (procdump lives)

*--- draw an appropriate number of 'shrooms on the screen:
            moveq     #0,d1
            move.b    (proc_pc).w,d1
            subq.w    #1,d1                     ; 2 for bus error, 3 for address, etc.
            bsr.s     do_shroom
            move.l    #savend,(savptr).w        ; clobber BIOS top level
            move.w    #-1,-(sp)                 ; "error" return condition
            move.w    #$4c,-(sp)                ; GEMDOS function #$4C
            trap      #1                        ; "terminate process"
            bra       reseth                    ; on return, reset system
*+
* do_shroom - draw little mushroom clouds on the screen
*  Passed:      d1.w = #shrooms to draw (DBRA count)
*  Returns:     some shrooms on display
*  Uses:        d0-d7/a0-a2
*
*  Discussion:  The graphics ain't all that great.   And this is silly.
*
*-
do_shroom:  move.b    (sshiftmd).w,d7
IF ROM_TOS306
			and.w     #7,d7
ELSE
            and.w     #3,d7
ENDIF
            add.w     d7,d7                     ; d7 = rez index
            moveq     #0,d0
            move.b    (v_bas_h).w,d0
            lsl.w     #8,d0
            move.b    (v_bas_m).w,d0
            lsl.l     #8,d0
            tst.b     (STEFlag).l
            bne.s     dmste
            move.b    (v_bas_l).w,d0
dmste:      movea.l   d0,a0                     ; a0 -> base of mem to draw at
            cmp.w     #6,d7
            blt.s     dm00
            adda.l    #768*100,a0
            bra.s     dm01
dm00:       adda.w    #160*100,a0
dm01:       lea       (mushroom).l,a1           ; a1 -> source from
            move.w    #15,d6                    ; d6 = scanline count
dm0:        move.w    d1,d2                     ; d3 = # to draw on this line
            movea.l   a0,a2                     ; safe ptr to beg of line
dm1:        move.w    mcount(pc,d7.w),d5        ; d5 = #words to replicate
dm2:        move.w    (a1),(a0)+                ; draw a word
            dbra      d5,dm2                    ; (complete single shroom)
            dbra      d2,dm1                    ; another, on the same line
            addq.w    #2,a1                     ; next source word
            adda.w    mwidth(pc,d7.w),a2        ; next dest line
            movea.l   a2,a0
            dbra      d6,dm0                    ; (loop for next line)
            moveq     #30-1,d7
dm3:        bsr       wvbl
            dbra      d7,dm3
            rts                                 ; byebye

mcount:     DC.W      $0003,$0001,$0000,$0000
            DC.W      $0003,$0000,$0000,$0007
mwidth:     DC.W      $00a0,$00a0,$0050,$0000
            DC.W      $0140,$0000,$00a0,$0140

*+
* _fastcpy - "fast" 512-byte copy
* Synopsis:     fastcpy(src, dest)
*
*               Used by _rwabs to fake disk DMA to odd addresses.  Therefore,
*               disk I/O on odd addresses is very slow.  Lose, lose.
*
*-
_fastcpy:   movea.l   4(sp),a0                  ; a0 -> src
            movea.l   8(sp),a1                  ; a1 -> dest
            move.w    #$3f,d0                   ; d0 = move count (64*8 = 512)
fast1:      move.b    (a0)+,(a1)+               ; copy 8 bytes at a time
            move.b    (a0)+,(a1)+               ; to minimize loop overhead
            move.b    (a0)+,(a1)+
            move.b    (a0)+,(a1)+
            move.b    (a0)+,(a1)+
            move.b    (a0)+,(a1)+
            move.b    (a0)+,(a1)+
            move.b    (a0)+,(a1)+
            dbra      d0,fast1
            rts

*+
* Go through hard-disk initialization vector
*
*-
_hinit:     move.l    (hdv_init).l,-(sp)
            rts

autopath:   DC.B      '\AUTO\*.PRG',0
            DC.W      $1234,$5678,$9abc,$def0

*+
* _auto - exec auto-startup files in the appropriate subdirectory
* _auto1 - exec (with filename args)
* Passed:       a0 -> full filespec (pathname)
*               a1 -> filename part of filespec
*               _drvbits: bit vector of active drives
*               _bootdev: contains device to exec from
*
* Returns:      nothing
*
* Note:         If _drvbits%%_bootdev is zero, _auto simply quits (since
*               the device isn't active....)
*
* Uses:         everything
*-
_auto:      move.l    #$bffff,-(sp)
            trap      #$d
            addq.l    #4,sp
            btst      #2,d0                     ; control key pressed?
            bne.s     autoq                     ; (then ignore the AUTO folders)
IF !ROM_TOS306
            move.l    (_drvbits).w,d0           ; d0 - active dev vector
            move.w    (_bootdev).l,d1           ; d1 - dev# to exec from
            btst      d1,d0                     ; is the dev alive?
            beq.s     autoq                     ; (no -- so punt)
ENDIF
            lea       autopath(pc),a0           ; -> path
            lea       autopath+6(pc),a1         ; -> filename
_auto1:     move.l    (sp)+,(autoret).l         ; return addr (used by execlr)
            move.l    a0,(pathname).w           ; setup filename/pathname ptrs
            move.l    a1,(filename).w
IF ROM_TOS306
			move.l    (_drvbits).w,d0           ; d0 - active dev vector
			move.w    (_bootdev).l,d1           ; d1 - dev# to exec from
			btst      d1,d0                     ; is the dev alive?
			beq.s     autoq                     ; (no -- so punt)
ENDIF
            lea       nullenv(pc),a0            ; a0 -> \0\0
            move.l    a0,-(sp)                  ; null enviorment
            move.l    a0,-(sp)                  ; null command tail
            move.l    a0,-(sp)                  ; null shell name
            move.w    #5,-(sp)                  ; Create-PSP subfunction
            move.w    #$4b,-(sp)                ; exec function#
            trap      #1                        ; do DOS CALL
            adda.w    #16,sp
            movea.l   d0,a0                     ; a0 -> PSP
            move.l    #fauto,8(a0)              ; initial PC -> autoexec prog
            move.l    a3,-(sp)                  ; null enviroment
            move.l    d0,-(sp)                  ; -> PSP
            move.l    a3,-(sp)                  ; null shell name
            move.w    #4,-(sp)                  ; just-go
            move.w    #$4b,-(sp)                ; function = exec
            trap      #1                        ; do it
            adda.w    #16,sp                    ; cleanup stack & goodbye
            move.l    (autoret).l,-(sp)
autoq:      rts


*+
* ST Book has another ROMDISK in the 2nd 256kb of the ROM
* which is mapped as drive 'P'.
*-
IF ROM_STBOOK
_auto_ROMDISK:lea     autopathROM(pc),a0        ; -> path
            lea       autopathROM+8(pc),a1      ; -> filename
            bra.s     _auto1

autopathROM:DC.B      'P:\AUTO\*.PRG',0
            DC.W      $1234,$5678,$9abc,$def0
ENDIF

*+
* fauto - exec'd by _auto to do autostartup
*
* Passed:       pathname -> path part of filespec
*               filename -> file path of filespec
*
*-
fauto:      clr.l     -(sp)                     ; get into super mode
            move.w    #$20,-(sp)
            trap      #1
            addq.w    #6,sp                     ; cleanup
            movea.l   d0,a4                     ; a4 -> saved super stack

*---- free up some memory
            movea.l   4(sp),a6                  ; a6 -> base page
            lea       $100(a6),sp               ; sp -> new, safer addr
            move.l    #$100,-(sp)               ; keep $100 (just the basepage)
            move.l    a6,-(sp)                  ; -> start of mem to keep
            clr.w     -(sp)                     ; junk word
            move.w    #$4a,-(sp)                ; setblock(...)
            trap      #1
            addq.w    #6,sp
            tst.w     d0
            bne.s     au_dn                     ; punt on error

            move.w    #7,-(sp)                  ; find r/o+hidden+system files
            move.l    (pathname).l,-(sp)        ; -> filename (on input)
            move.w    #$4e,-(sp)                ; searchFirst
            moveq     #8,d7                     ; d7 = cleanup amount
au1:        pea       (autodma).l               ; setup DTA (for search)
            move.w    #$1a,-(sp)
            trap      #1                        ; search first/search next
            addq.w    #6,sp
            trap      #1
            adda.w    d7,sp                     ; cleanup stack
            tst.w     d0                        ; test for match
            bne.s     au_dn                     ; (no match -- quit)

*--- construct filename from path and the name we just found:
            movea.l   (pathname).l,a0           ; copy pathname
            movea.l   (filename).l,a2           ; a2 -> end+1 of pathname
            lea       (autoname).l,a1
au3:        move.b    (a0)+,(a1)+               ; copy path part of name
            cmpa.l    a0,a2                     ; finished?
            bne.s     au3                       ; (no)
            lea       (autodma+30).l,a0         ; copy fname to end of pathname
au2:        move.b    (a0)+,(a1)+
            bne.s     au2
            pea       nullenv(pc)               ; null enviroment
            pea       nullenv(pc)               ; no command tail
            pea       (autoname).l              ; -> file to exec
            clr.w     -(sp)                     ; load-and-go
            move.w    #$4b,-(sp)                ; exec(...)
            trap      #1
            adda.w    #16,sp
            moveq     #2,d7                     ; reset cleanup amount
            move.w    #$4f,-(sp)                ; searchNext
            bra.s     au1

*+
* The first GEMDOS process can never terminate.
* This is not a good feature.
* Kludge around it - re-initialize the stack
* and return to the guy who called us to begin with.
*
*-
au_dn:      lea       (_supstk+2048).l,sp       ; setup supervisor stack
            move.l    (autoret).l,-(sp)         ; get return addr
            rts                                 ; just jump there ...


*+
* _dumpit: dump screen
*
*-
_dumpit:    movea.l   (scr_dump).l,a0
            jsr       (a0)
            move.w    #-1,(_prtcnt).l
            rts


*+
* _scrdmp - printScreen(), font-end to _prtblk()
*  Passed:      nothing
*  Returns:     nothing
*  Uses:        everything
*-
_scrdmp:    move.l    (_v_bas_adr).w,(p_blkptr).w ;-> screen mem
            clr.w     (p_offset).w              ; offset = 0
            clr.w     d0
            move.b    (sshiftmd).w,d0           ; get w & h
            move.w    d0,(p_srcres).w
            add.w     d0,d0
            lea       reztab(pc),a0
            move.w    (a0,d0.w),(p_width).w     ; set display width, height
            move.w    6(a0,d0.w),(p_height).w
            clr.w     (p_left).w                ; left = right = 0
            clr.w     (p_right).w
            move.l    #$ffff8240,(p_colpal).w   ; -> hardware palettes
            clr.w     (p_masks).w               ; default mask ptr

* draft or final mode
            move.w    (pconfig).w,d1            ; p_dstres = pconfig%%3
            lsr.w     #3,d1
            and.w     #1,d1
            move.w    d1,(p_dstres).w

* printer or rs232 port
            move.w    (pconfig).w,d1            ; p_port = pconfig%%4
            move.w    d1,d0
            lsr.w     #4,d0
            and.w     #1,d0
            move.w    d0,(p_port).w

* select printer flavor
            and.w     #7,d1                     ; p_type = ptype[pconfig & 7]
            move.b    ptype(pc,d1.w),d0
            move.w    d0,(p_type).l

* do it
            pea       (p_blkptr).w              ; -> beginning of parameter area
            move.w    #1,(_prtcnt).w
            bsr       _prtblk                   ; print it (finally)
            move.w    #-1,(_prtcnt).l
            addq.w    #4,sp                     ; cleanup stack
            rts                                 ; and return


*---- screen resolution table (pixel) for printScreen
reztab:     DC.W      $0140,$0280,$0280,$00c8
            DC.W      $00c8,$0190

*--- printer flavors (based on low 3 bits of pconfig)
ptype:      DC.B      $00,$02,$01,$ff,$03,$ff,$ff,$ff

*--- what it is:
mushroom:   DC.W      $0600,$2900,$0080,$4840
            DC.W      $11f0,$01f0,$07fc,$0ffe
            DC.W      $0dfe,$1fff,$1fef,$0fee
            DC.W      $0fde,$07fc,$03f8,$00e0

*+
* waitvbl - wait for the beam inside the vertical blank area
*-
IF ROM_TOS306
waitvbl:	clr.b     (tbcr).w
			clr.b     (tbdr).w
			move.b    #8,(tbcr).w
waitvbl2:	tst.b     (tbdr).w
			beq.s     waitvbl2
			jmp       (a6)
ELSE
waitvbl:    lea       (tbdr).w,a0               ; a0 -> timer B data register
            lea       (tbcr).w,a1               ; a1 -> timer B control register
            bclr      #0,(iera).w               ; disable IRQ of timer B
            moveq     #1,d4                     ; wait for the timer to expire
            clr.b     (a1)                      ; stop timer B
            move.b    #$f0,(a0)                 ; event every 240 scan lines
            move.b    #8,(a1)                   ; timer b: event count mode (HBL)
waitvbl2:   cmp.b     (a0),d4                   ; wait for HBL 239 scan lines to pass
            bne.s     waitvbl2
waitvbl3:   move.b    (a0),d4
            move.w    #615,d3                   ; wait till we are inside the vbl area
waitvbl4:   cmp.b     (a0),d4
            bne.s     waitvbl3
            dbra      d3,waitvbl4
            move.b    #$10,(a1)                 ; timer b: reset
            jmp       (a6)
ENDIF

_wvbl:      bra       wvbl

*+
* runresapps - search address space for 512-byte resident code chunks
*
*-
runresapps: movea.l   (phystop).l,a0            ; start at the top of the address space
runresappsl:suba.w    #$200,a0
            cmpa.w    #$400,a0                  ; reach the lower bottom?
            bls.s     runresappsx               ; (bail out)
            cmpi.l    #$12123456,(a0)           ; check for magic
            bne.s     runresappsl               ; (no magic -> next block)
            cmpa.l    4(a0),a0                  ; second long is equal the base address of the block?
            bne.s     runresappsl               ; (no -> next block)
            clr.w     d0
            movea.l   a0,a1
            move.w    #$ff,d1                   ; checksum over 256 words
runresappsc:add.w     (a1)+,d0                  ; checksum over the block
            dbra      d1,runresappsc
            cmp.w     #$5678,d0                 ; magic checksum?
            bne.s     runresappsl               ; (no -> next block)
            move.l    a0,-(sp)                  ; save current address
            jsr       8(a0)                     ; call code block
            movea.l   (sp)+,a0                  ; continue with next block
            bra.s     runresappsl
runresappsx:rts


gettime:    lea       (readRTCTime).l,a3
            lea       (readIKBDTime).l,a4
            bra.s     timecode
settime:    move.w    4(sp),(systemDate).l
            move.w    6(sp),(systemTime).l
            lea       (writeRTCTime).l,a3
            lea       (writeIKBDTime).l,a4
timecode:   bsr       checkRTC
            bcc.s     timecodertc
            movea.l   a4,a3
timecodertc:jmp       (a3)


*+
* sysbase2ram - copy sysbase into memory and patch it
*-
sysbase2ram:lea       osroml(pc),a0
            lea       (ramsysbase).l,a1
            moveq     #$2f,d0
sysbase2raml:move.b   (a0,d0.w),(a1,d0.w)       ; copy 48 bytes into memory
            dbra      d0,sysbase2raml
            move.w    sysbase2ramjmp(pc),$fffa(a1) ;copy JMP instruction just before
            move.l    4(a1),$fffc(a1)           ; copy ROM reseth into this JMP
            move.w    sysbase2rambra(pc),(a1)   ; patch BRA at the beginning of sysbase copy to hit the JMP
            move.w    $1e(a1),$1c(a1)           ; copy os_date into os_conf
            move.l    a1,(_sysbase).l           ; set new sysbase pointer
            rts

sysbase2ramjmp:jmp    ($0000).w                 ; destination address is patched to reseth address

sysbase2rambra:bra.s  sysbase2ramjmp            ; branch onto the JMP

*+
* blitmode - enabled/disable/test the blitter
*
* Returns: d0.w - bit 0 - blitter enabled
*                 bit 1 - blitter available
*-
blitmode:   bsr.s     blittest                  ; test if blitter is installed
            move.w    d0,d4                     ; d4 = blitter status
            move.w    d0,d5
            lsr.w     #1,d5
            or.w      #$fffe,d5                 ; d5 = blitter enable mask
            jsr       ($e0732a).l
            move.w    d0,d3                     ; d3 = blitter active
            move.w    4(sp),d0                  ; new blitter status
            bmi.s     blitmodex                 ; <0 just return the status
            and.w     d5,d0                     ; mask blitter status out
            or.w      d4,d0                     ; or'd blitter available status
            jsr       ($e07302).l               ; disable/enable blitter
blitmodex:  move.w    d3,d0
            rts

*+
* blittest - test if the blitter is installed
*-
blittest:   move      sr,d1
            move.w    #0,d0                     ; d0 = 0 -> blitter not installed
            suba.l    a0,a0
            movea.l   sp,a2
            ori       #$700,sr
            movea.l   8(a0),a1
            move.l    #blittestx,8(a0)
            tst.w     $8a00(a0)
            moveq     #2,d0                     ; d0 = 2 -> blitter installed
blittestx:  move.l    a1,8(a0)
            move      d1,sr
            movea.l   a2,sp
            rts

*+
* delay for a little bit via timer a in the mfp
*-
mfpdelay:   bsr.s     mfpdelaysetup
IF !ROM_TOS306
mfpdelayl:  btst      #5,(ipra).w               ; did timer a fire?
            beq.s     mfpdelayl                 ; wait more if not
            clr.b     (tacr).w                  ; stop timer a
ELSE
mfpdelayl:	btst      #5,(iprb_TT).w			; did timer a fire?
			beq.s     mfpdelayl					; wait more if not
ENDIF
			rts

*+
* setup timer a in the mfp
*-
mfpdelaysetup:
IF !ROM_TOS306
			move    sr,-(sp)
            ori       #$700,sr
            clr.b     (tacr).w                  ; timer A control register
            bclr      #5,(iera).w               ; disable timer A
            move.b    #$df,(ipra).w             ; clear a pending timer A interrupt
            bclr      #5,(imra).w               ; mask the timer A interrupt
            bset      #5,(iera).w               ; enable timer A
            move      (sp)+,sr
            move.b    d0,(tadr).w               ; set timer a data register
            ror.w     #8,d0
            move.b    d0,(tacr).w               ; set timer a control register
            rol.w     #8,d0
ELSE
			movem.w   d0-d1,-(sp)
			move      sr,-(sp)
			ori       #$700,sr
			move.b    (tcdcr_TT).w,d1
			and.b     #$f,d1
			move.b    d1,(tcdcr_TT).w
			bclr      #5,(ierb_TT).w
			move.b    #$df,(iprb_TT).w
			bclr      #5,(imrb_TT).w
			bset      #5,(ierb_TT).w
			move.b    d0,(tcdr_TT).w
			lsr.w     #4,d0
			and.b     #$f0,d0
			or.b      d0,d1
			move.b    d1,(tcdcr_TT).w
			move      (sp)+,sr
			movem.w   (sp)+,d0-d1
			rts
ENDIF
            rts

memclear:   movea.l   4(sp),a0                  ; a0 -> start address of the block
            movea.l   8(sp),a1                  ; a1 -> end address of the block
            movem.l   d3-d7/a3,-(sp)
            moveq     #0,d1
            moveq     #0,d2
            moveq     #0,d3
            moveq     #0,d4
            moveq     #0,d5
            moveq     #0,d6
            moveq     #0,d7
            movea.w   d7,a3
            move.l    a0,d0
            btst      #0,d0
            beq.s     memclear1
            move.b    d1,(a0)+                  ; clear a single byte to even-align
memclear1:  move.l    a1,d0
            sub.l     a0,d0
            and.l     #$ffffff00,d0
            beq.s     memclearb
            lea       (a0,d0.l),a0
            movea.l   a0,a2
            lsr.l     #8,d0
memclear256:movem.l   d1-d7/a3,-(a2)            ; clear 256 bytes
            movem.l   d1-d7/a3,-(a2)
            movem.l   d1-d7/a3,-(a2)
            movem.l   d1-d7/a3,-(a2)
            movem.l   d1-d7/a3,-(a2)
            movem.l   d1-d7/a3,-(a2)
            movem.l   d1-d7/a3,-(a2)
            movem.l   d1-d7/a3,-(a2)
            subq.l    #1,d0
            bne.s     memclear256
memclearb:  cmpa.l    a0,a1                     ; end address reached?
            beq.s     memcleare
            move.b    d1,(a0)+                  ; clear a single byte
            bra.s     memclearb
memcleare:  movem.l   (sp)+,d3-d7/a3
            rts

*+
* setup PMMU translation table
*-
IF ROM_TOS306
setupPMMU:	lea       ($700).l,a0
			lea       ($e363f0).l,a1			; translation table
			move.w    #$3f,d0					; 64 descriptors
setupPMMUl:	move.l    (a1)+,(a0)+
			dbra      d0,setupPMMUl

* PMMU Table:
* $x.......
*	$700: $00000742,$10000001,$20000001,$30000001,$40000001,$50000001,$60000001,$70000001,
*	$720: $80000041,$90000041,$A0000041,$B0000041,$C0000041,$D0000041,$E0000041,$00000782,

* $0.......
*	$740: $000007C2,$01000001,$02000001,$03000001,$04000001,$05000001,$06000001,$07000001,
*	$760: $08000001,$09000001,$0A000001,$0B000001,$0C000001,$0D000001,$0E000001,$0F000001,

* $Fx......
*	$780: $00000041,$01000041,$02000041,$03000041,$04000041,$05000041,$06000041,$07000041,
*	$7a0: $08000041,$09000041,$0A000041,$0B000041,$0C000041,$0D000041,$0E000041,$000007C2,

* $00...... or $FF......
*	$7c0: $00000001,$00100001,$00200001,$00300001,$00400001,$00500001,$00600001,$00700001,
*	$7e0: $00800001,$00900001,$00A00001,$00B00001,$00C00001,$00D00001,$00E00001,$00F00041,

* The purpose of the PMMU is to disable caching in the area of the address space that doesn't contain ram:
* $00F00000..$00FFFFFF
* $80000000..$EFFFFFFF
* $F0000000..$FEFFFFFF
* $FFF00000..$FFFFFFFF

			pmove     ($e364f0).l,crp			; CPU Root Pointer: $80000002,$00000700: 4 byte descriptor, at $700, lower limit: $00000
			pmove     ($e364f8).l,tc			; Translation Control Register: $80F04445: MMU enabled, pagesize = 15, bits: 4,4,4,5
			pmove     ($e364fc).l,tt0			; Transparent Translation Register 0: $017E8107
			pmove     ($e36500).l,tt1			; Transparent Translation Register 1: $807E8507
			rts
ENDIF

*+
* calccrc - calc a crc
*-
calccrc:    movea.l   4(sp),a0                  ; a0 -> buffer start
            move.l    8(sp),d2                  ; d2 -> number of bytes
            movea.w   $c(sp),a1                 ; a1 -> step offset through the buffer
            clr.w     d0
            clr.w     d1
            clr.w     d3
            lea       (crcTable).l,a2
calccrcl:   move.w    d0,d1
            lsl.w     #8,d0
            lsr.w     #8,d1
            move.b    (a0),d3
            adda.l    a1,a0
            eor.b     d3,d1
            add.w     d1,d1
            move.w    (a2,d1.w),d4
            eor.w     d4,d0
            subq.l    #1,d2
            bne.s     calccrcl
            rts

IF ROM_TOS306
_EsetShift: bsr       _wvbl
            moveq     #0,d0
            move.w    (shift_tt).w,-(sp)
            move.w    6(sp),(shift_tt).w
            move.w    (shift_tt).w,d0
            and.w     #7,d0
            move.b    d0,(sshiftmd).w
            clr.w     (vblsem).w
            jsr       (esc_init).l
            move.w    #1,(vblsem).w
            move.w    (sp)+,d0
            rts

_EgetShift: moveq     #0,d0
            move.w    (shift_tt).w,d0
            rts

_EsetBank:  moveq     #0,d0
            move.w    (shift_tt).w,d0
            and.w     #$f,d0
            tst.w     4(sp)
            bmi.s     _EsetBankx
            move.b    5(sp),(shift_tt+1).w
_EsetBankx: rts

_EsetColor: moveq     #0,d0
            lea       (TT_col).w,a0
            move.w    4(sp),d0
            and.w     #$ff,d0
            add.w     d0,d0
            adda.w    d0,a0
            move.w    (a0),d0
            and.w     #$fff,d0
            move.w    6(sp),d1
            bmi.s     _EsetColorx
            move.w    d1,(a0)
_EsetColorx:rts

_EsetPalette:move.w   4(sp),d0
            and.w     #$ff,d0
            movea.w   d0,a0
            adda.w    a0,a0
            sub.w     #$100,d0
            neg.w     d0
            move.w    6(sp),d1
            cmp.w     d0,d1
            ble.s     _EsetPal2
            move.w    d0,d1
_EsetPal2:  movea.l   8(sp),a1
            lea       $8400(a0),a0
            bra.s     _EsetPal4
_EsetPal3:  move.w    (a1)+,(a0)+
_EsetPal4:  dbra      d1,_EsetPal3
            rts

_EgetPalette:move.w   4(sp),d0
            and.w     #$ff,d0
            movea.w   d0,a0
            adda.w    a0,a0
            sub.w     #$100,d0
            neg.w     d0
            move.w    6(sp),d1
            cmp.w     d0,d1
            ble.s     _EgetPal2
            move.w    d0,d1
_EgetPal2:  movea.l   8(sp),a1
            lea       $8400(a0),a0
            bra.s     _EgetPal4
_EgetPal3:  move.w    (a0)+,(a1)+
_EgetPal4:  dbra      d1,_EgetPal3
            rts

_EsetGrey:  moveq     #0,d0
            move.b    (shift_tt).w,d1
            move.b    d1,d0
            lsr.b     #4,d0
            and.b     #1,d0
            bclr      #4,d1
            tst.w     4(sp)
            beq.s     _EsetGrey2
            bmi.s     _EsetGreyr
            bset      #4,d1
_EsetGrey2: move.b    d1,(shift_tt).w
_EsetGreyr: rts

_EsetSmear: moveq     #0,d0
            move.b    (shift_tt).w,d1
            move.b    d1,d0
            add.b     d0,d0
            subx.w    d0,d0
            neg.w     d0
            bclr      #7,d1
            tst.w     4(sp)
            beq.s     _EsetSmear2
            bmi.s     _EsetSmearr
            bset      #7,d1
_EsetSmear2:move.b    d1,(shift_tt).w
_EsetSmearr:rts
ENDIF

**************************************************************************
* DMAREAD.S from the AHDI source                                         *
**************************************************************************
*+
* dmaread() - read from a DMA device.
* dmawrite() - write to a DMA device.
*
* LONG	sectnum		$4(sp).l
* WORD	count		$8(sp).w
* BYTE	*buf;		$a(sp).l	$b(sp)=high $c(sp)=mid $d(sp)=low
* WORD	pdev;		$e(sp).w
*
* Returns 0 if successful.
* Returns a negative number if failure:
*	EUNDEV (-15L) if unknown device (bad dev number)
*	EREADF (-11L) if read failure
*	EWRITF (-10L) if write failure
*	       (-1L)  if timed-out
*
* Comments:
*	ACSI, SCSI and IDE-AT drives are supported.
*-
_dmaread:   move.b    #0,(rwflag).l             ; rwflag = 0 => a read
            move.w    #$08,d1                   ; d1 = opcode for READ
            bra.s     dmarw
_dmawrite:  move.b    #1,(rwflag).l             ; rwflag = 1 => a write
            move.w    #$0a,d1                   ; d1 = opcode for WRITE
dmarw:      cmpi.w    #$10,$e(sp)               ; unit# > IDE-AT unit# 16?
            bhi       undev                     ; if so, return unknow device
            cmpi.w    #$f,$e(sp)                ; an IDE-AT unit?
            bhi.s     ide0                      ; if so, talk IDE-AT
* else talk ACSI or SCSI
            lea       (_cmdblk).l,a0            ; a0 -> beginning of command block
            move.b    d1,(a0)+                  ; byte 0 = opcode
            move.b    5(sp),(a0)+               ; byte 1 = msb of logical block addr
            move.b    6(sp),(a0)+               ; byte 2 = logical block addr
            move.b    7(sp),(a0)+               ; byte 3 = lsb of logical block addr
            move.b    9(sp),(a0)+               ; byte 4 = transfer length (in blocks)
            clr.b     (a0)                      ; byte 5 = control byte
            move.w    $e(sp),d0                 ; d0 = physical unit number
IF ROM_TOS306
			moveq     #0,d1
			move.w    8(sp),d1
			lsl.l     #8,d1
			add.l     d1,d1
ENDIF
            moveq     #6,d2                     ; d2 = length of command
            movea.l   $a(sp),a0                 ; a0 = buffer
            tst.b     (rwflag).l                ; read or write?
            bne.s     rw0                       ; it's a write
            bsr       _dorcmd                   ; send a receive data command
            bra       rw1
rw0:        bsr       wracsi                    ; send a write data command
            bra       rw1                       ; branch will take place for non-IDE drives
*
* Before doing anything with the IDE bus, make sure it's there.
* (The label noide is before at just because it fits better.)
*
noide:      movea.l   a1,sp                     ; got the bus error on IDEASR;
            move.l    a0,(busexception).w       ; restore vector & sp, return EUNDEV
            bra       undev

ide0:       movea.l   (busexception).w,a0
            movea.l   sp,a1
            move.l    #noide,(busexception).w
            tst.b     (ide_stat2).l             ; read a register to probe for IDE bus
            movea.l   a1,sp                     ; hey! no bus error!
            move.l    a0,(busexception).w       ; restore vector & sp and continue.
            move.b    $f(sp),d0                 ; IDE unit#
            bsr       _iderdy                   ; wait for drive to be ready
IF ROM_TOS306
            beq       undev                     ; if drive never gets ready, return with unknown device else
ELSE
			beq.s     undev                     ; if drive never gets ready, return with unknown device else
ENDIF
            moveq     #2,d0                     ; D_IDENTIFY delay required by Conner drives
            add.l     (_hz_200).w,d0            ; between power on and identify()
ide1:       cmp.l     (_hz_200).w,d0
            bcc.s     ide1

			pea       (sbuf).l                  ; scratch buffer for drive parameters
            move.w    $12(sp),-(sp)             ; IDE unit#
            bsr       _identify                 ; identify()
            addq.w    #6,sp                     ; clean up stack
            tst.w     d0                        ; successful?
IF ROM_TOS306
            bmi       rwend                     ; if timed-out, return
ELSE
			bmi.s     rwend                     ; if timed-out, return
ENDIF
            bne.s     rw2                       ; if error, return with error code else can do read or write
            moveq     #2,d0                     ; D_IDENTIFY delay required by Conner drives
            add.l     (_hz_200).w,d0            ; between identify() and r/w
ide4:       cmp.l     (_hz_200).w,d0
            bcc.s     ide4

IF ROM_TOS306
			lea       (sbuf).l,a0
			move.w    $c(a0),-(sp)				; # of sectors per track
			move.w    6(a0),-(sp)				; # of heads
			move.w    $12(sp),-(sp)				; physical unit #
			bsr       _initdevpar
			addq.w    #6,sp
			tst.w     d0
			bmi.s     rwend
			bne.s     rw2
			lea       (sbuf).l,a0
ELSE
			pea       (sbuf).l                  ; beginning of _identify() data
			bsr       _gcparm                   ; get drive current parameters
			addq.w    #4,sp                     ; clean up stack
ENDIF
            move.w    $e(sp),-(sp)              ; physical unit #
            move.l    $c(sp),-(sp)              ; buffer
            move.w    $e(sp),-(sp)              ; count
            move.l    $c(sp),-(sp)              ; logical block address
IF ROM_TOS306
			move.w    $c(a0),-(sp)				; # of sectors per track
			move.w    6(a0),-(sp)				; # of heads
ELSE
            move.w    d2,-(sp)                  ; # sectors per track
            move.w    d1,-(sp)                  ; # data heads
ENDIF
            tst.b     (rwflag).l                ; read or write?
            bne.s     ide2                      ; (write)
            bsr       _ideread                  ; read sectors
            bra.s     ide3
ide2:       bsr       _idewrite                 ; write sectors
ide3:       adda.w    #16,sp                    ; clean up stack
rw1:
IF ROM_TOS306
			move      sr,-(sp)
			ori       #$700,sr
			movec     cacr,d1
			ori.w     #$808,d1
			movec     d1,cacr
			move      (sp)+,sr
ENDIF
			tst.w     d0                        ; successful?
            ble.s     rwend                     ; if no error or timed-out, return else
rw2:        moveq     #-11,d0                   ; assume it's a read error EREADF
            tst.b     (rwflag).l                ; read or write?
            beq.s     rwend                     ; if read, done
            moveq     #-10,d0                   ; else, it's a write error EWRITF
rwend:      rts

undev:      moveq     #-15,d0                   ; EUNDEV
            bra.s     rwend

*+
* dorcmd() - send a command which will receive data from the target
*
* Passed:
*	d0.w = physical unit number
*	d2.w = command length (NCMD or LCMD)
*	a0.l = buffer address
*-
_dorcmd:
IF ROM_TOS306
			cmp.w     #7,d0
			bls.s     _dorcmd2
			bsr       _rcvscsi
			rts
_dorcmd2:	bsr       _rcvacsi                  ; it's an ACSI device
ELSE
			bsr.s     _rcvacsi                  ; it's an ACSI device
ENDIF
			rts

*+
* dowcmd() - send a command which will write data to the target
*
* Passed:
*	d0.w = physical unit number
*	d2.w = command length (NCMD or LCMD)
*	a0.l = buffer address
*-
wracsi:
IF ROM_TOS306
			cmp.w     #7,d0
			bls.s     wracsi2
			bsr       _wrtscsi
			rts
wracsi2:
ENDIF
		 	bsr       _wrtacsi                  ; it's an ACSI device
            rts

*+
* LONG _qdone() - Wait for command byte handshake
* LONG _fdone() - Wait for operation complete
* Passed:	nothing
*
* Returns:	EQ: no time-out
*		MI: time-out condition
*
* Uses:		D0
*
*-
_fdone:     move.l    #600,d0                   ; ACLTMOUT
            bra.s     qd0

_qdone:     moveq     #20,d0                    ; ACSTMOUT

qd0:
IF ROM_TOS306
			move.l    d0,-(sp)                  ; save timeout value
            moveq     #2,d0                     ; busy-wait delay for slow ACSI
            add.l     (_hz_200).w,d0            ; minimum 20 microsec.
sdelay:     cmp.l     (_hz_200).w,d0
            bge.s     sdelay
            move.l    (sp)+,d0                  ; restore timeout value
ENDIF
            add.l     (_hz_200).w,d0
qd1:        cmp.l     (_hz_200).w,d0            ; time-out?
            bcs.s     qdq                       ; (i give up, return NE)
            btst      #5,(gpip).w               ; interrupt?
            bne.s     qd1                       ; (not yet)
            moveq     #0,d0                     ; return EQ (no time-out)
            rts

qdq:        moveq     #-1,d0
            rts

*+
* Wait for end of SASI command
*
* Passed:	d1 value to be written to wdl
*
* Returns:	EQ: success (error code in D0.W)
*		MI: time-out (-1 in D0.W)
*		NE: failure (SASI error code in D0.W)
*
* Uses:		d0
*-
_endcmd:    bsr.s     _fdone                    ; wait for operation complete
            bmi.s     endce                     ; (timed-out, so complain)
IF ROM_TOS306
			move      sr,-(sp)
			ori       #$700,sr
			movec     cacr,d0
			ori.w     #$800,d0
			movec     d0,cacr
			move      (sp)+,sr
ENDIF
            move.w    d1,(fifo).w
            move.w    (diskctl).w,d0            ; get the result
            and.w     #$ff,d0                   ; (clean it up) if non-0 should do a RequestSense command to learn more
endce:      move.l    (_hz_200).w,(lastacstm).l ; update controller last accessed time
            addq.l    #2,(lastacstm).l          ; lastacstm = _hz_200 + 2;
            rts

*+
*  Handle command time-out;
*  Unlock DMA chip and return completion status;
*-
_hdone:     move.w    #$80,(fifo).w             ; Landon's code seems to presume we put $80 there
IF ROM_TOS306
			clr.w     (flock).w                 ; NOW, signal that we are done
ELSE
            sf        (flock).w                 ; NOW, signal that we are done
ENDIF
            rts

*+
* delay()
*	5 - 10ms kludge delay for message byte sent back by controller.
*-
_delay:     move.l    d0,-(sp)                  ; preserve d0
            move.l    (lastacstm).l,d0          ; d0 = controller last accessed time
wait:       cmp.l     (_hz_200).w,d0            ; while (_hz_200 <= lastacstm)
            bcc.s     wait                      ; wait()
            move.l    (sp)+,d0                  ; restore d0
            rts

*+
* rcvacsi() - send a ACSI command which receives data from target.
*
* Passed:
*	d0.w = physical unit number
*	d2.w = command length (NCMD or LCMD)
*	a0.l = buffer address
*-
_rcvacsi:   movea.l   (busexception).w,a1       ; check to see if the ACSI bus actually exists! Needed for prototype PADs
            movea.l   sp,a2
            move.l    #noacsi,(busexception).w
            tst.w     (fifo).w                  ; harmless or bus erorr
            move.l    a1,(busexception).w       ; no bus error - restore & continue
            movea.l   a2,sp
            st        (flock).w                 ; lock FIFO
            bsr.s     _delay                    ; delay if necessary
            movea.w   #diskctl,a1               ; a1 = pointer to DMA chip
            bsr       setadma                   ; set DMA pointer
            move.w    #$190,2(a1)               ; WDL	; toggle DMA chip to direction
            bsr       rstdelay                  ; delay
            move.w    #$90,2(a1)                ; WDL	;  for receiving data
            bsr       rstdelay                  ; delay
            bsr       setacnt                   ; set DMA count
            lea       (_cmdblk).l,a0            ; a0 = address of command block
            moveq     #0,d1                     ; direction of DMA is IN
            bsr.s     sblkacsi                  ; send the command block
raend:      bra.s     _hdone                    ; cleanup after IRQ
*+
* wrtacsi() - send an ACSI command which will write data to the target
*
* Passed:
*	d0.w = physical unit number
*	d2.w = command length (NCMD or LCMD)
*	a0.l = buffer address
*-
*
_wrtacsi:   movea.l   (busexception).w,a1       ; check to see if the ACSI bus actually exists! Needed for prototype PADs
            movea.l   sp,a2
            move.l    #noacsi,(busexception).w
            tst.w     (diskctl).w               ; harmless or bus erorr
            move.l    a1,(busexception).w       ; no bus error - restore & continue
            movea.l   a2,sp
            st        (flock).w                 ; lock FIFO
            bsr.s     _delay
            movea.w   #diskctl,a1               ; a1 = pointer to DMA chip
            bsr.s     setadma                   ; set DMA pointer
            move.w    #$90,2(a1)                ; WDL	; toggle DMA chip for "send"
            bsr       rstdelay                  ; delay
            move.w    #$190,2(a1)               ; WDL
IF ROM_TOS306
			bsr       rstdelay                  ; delay
ELSE
            bsr.s     rstdelay                  ; delay
ENDIF
            bsr.s     setacnt                   ; set DMA count
            move.l    #$100,d1                  ; d1 = direction of DMA is OUT
            bsr.s     sblkacsi                  ; send the command block
waend:      bra       _hdone                    ; cleanup after IRQ
* You get to this label if there is a bus error when probing for
* the ACSI bus: it's meant for prototype PADs, which don't
* have an ACSI DMA chip.
noacsi:     moveq     #-15,d0                   ; return EUNDEV
            movea.l   a2,sp
            move.l    a1,(busexception).w
            rts

*+
* sblkacsi() - send command block
*
* Passed:
*	d0.w = physical unit number
*	d1.l = direction of DMA ($0000 for IN or $0100 for OUT)
*	d2.w = command length (NCMD or LCMD)
*	a1.l = pointer to DMA chip
*
* Returns:
*	d0.l =  0 if successful
*	d0.l = -1 if time-out
*
* Trashes:
*	d0, d1, d2, a2
*-
sblkacsi:   move.b    #$88,d1                   ; next byte is the opcode
            move.w    d1,2(a1)                  ; WDL
            move.b    #$8a,d1                   ; following bytes are operands
            lea       (_cmdblk).l,a2            ; a2 = address of command block
* integrate unit # into cmd blk
            lsl.b     #5,d0                     ; shift unit number into place
            or.b      d0,(a2)                   ; first command byte = unit # | opcode
* control byte is sent seperately
            subq.w    #2,d2                     ; and dbra likes one less
sa1:        swap      d1                        ; d1.hw = operand
            move.b    (a2)+,d1                  ; d1.lw = tells controller next byte
            swap      d1                        ; is an operand
            move.l    d1,(a1)                   ; WDCWDL
            bsr       _qdone
            bmi.s     sbaend                    ; if time-out, returns
            dbra      d2,sa1                    ; else send rest of command block
            move.w    d1,2(a1)                  ; WDL - get ready to send control byte
            move.b    #0,d1                     ; signal sending control byte
            swap      d1                        ; d1.hw = control byte
            move.b    (a2),d1                   ; d1.lw = tells controller it's end
            swap      d1                        ; of command
            move.l    d1,(a1)                   ; send it
            move.b    #$8a,d1                   ; d1 = wdl value
            bsr       _endcmd                   ; wait for command completion
sbaend:     rts                                 ; heading home

*+
* setadma() - set the ACSI DMA pointer
*
* Passed:
*	a0.l = buffer address
*-
setadma:    move.l    a0,-(sp)                  ; move it on stack
            move.b    3(sp),(dmalow).w          ; set low-byte of address
            move.b    2(sp),(dmamid).w          ; set mid-byte of address
            move.b    1(sp),(dmahigh).w         ; set high-byte of address
            addq.l    #4,sp                     ; clean up stack
            rts


*+
* setacnt() - set the ACSI DMA counter
*
* Passed:
*	a1.l = pointer to DMA chip
*-
setacnt:
IF ROM_TOS306
			move.l    d1,-(sp)
			lsr.l     #8,d1
			add.l     d1,d1
			move.w    d1,(a1)
			move.l    (sp)+,d1
ELSE
			move.w    #$ff,(a1)                 ; set the ACSI DMA counter to 255 512-byte-blocks
ENDIF
            rts


*	After talking to the DMA chip in a way that may reset it,
* we need a 8 8Mhz clocks (ie. 1 microsec) delay, before we can
* talk to the chip again.
rstdelay:   tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            tst.b     (gpip).w
            rts

IF ROM_TOS306
_rcvscsi:	move.l    a0,-(sp)
			andi.w    #7,d0
			bsr.s     _scsicmd
			movea.l   (sp)+,a0
			tst.w     d0
			bmi.s     _rcvscsir
			movea.l   a0,a1
			movea.w   #s_data,a2
			move.b    #1,(s_tcr).w
			move.b    (s_inircv).w,d0
_rcvscsil:	bsr       _scsidelay250ms
			bsr       _scsiwait
			bmi.s     _rcvscsir
			btst      #3,$a(a2)
			beq       _scsiaction
			move.b    (a2),(a1)+
			bsr       _scsidone
			bra.s     _rcvscsil
_rcvscsir:	rts

_wrtscsi:	andi.w    #7,d0
			move.l    a0,-(sp)
			move.w    d2,-(sp)
			bsr.s     _scsicmd
			move.w    (sp)+,d2
			movea.l   (sp)+,a0
			tst.w     d0
			bmi.s     _wrtscsir
			movea.l   a0,a1
			movea.w   #s_data,a2
			move.b    #0,(s_tcr).w
			move.b    (s_inircv).w,d0
_wrtscsil:	bsr       _scsidelay250ms
			bsr       _scsiwait
			bmi.s     _wrtscsir
			btst      #3,$a(a2)
			beq       _scsiaction
			move.b    (a1)+,(a2)
			bsr       _scsidone
			bra.s     _wrtscsil
_wrtscsir:	rts

_scsicmd:	move.l    d2,-(sp)
			move.w    d0,-(sp)
			bsr.s     _scsido
			addq.w    #2,sp
			move.l    (sp)+,d2
			tst.w     d0
			bmi.s     _scsicmdr
			move.b    #2,(s_tcr).w
			move.b    #1,(s_icr).w
			lea       ($a96).l,a1
			subq.w    #1,d2
			bsr       _scsidelay250ms
_scsicmdl:	move.b    (a1)+,d0
			bsr       _scsisuba
			tst.w     d0
			bmi.s     _scsicmdr
			dbra      d2,_scsicmdl
			moveq     #0,d0
_scsicmdr:	rts

_scsido:	bsr       _scsidelay250ms
_scsidow:	btst      #6,(s_idstat).w
			beq.s     _scsido2
			cmp.l     (a0),d1
			bhi.s     _scsidow
			bra.s     _scsidoerr
_scsido2:	move.b    #0,(s_tcr).w
			move.b    #0,(s_idstat).w
			move.b    #$c,(s_icr).w
			clr.w     d0
			move.w    4(sp),d1
			bset      d1,d0
			move.b    d0,(s_data).w
			move.b    #$d,(s_icr).w
			andi.b    #$fe,(s_mode).w
			andi.b    #$f7,(s_icr).w
			bsr       _scsidelay250ms
_scsidow2:	btst      #6,(s_idstat).w
			bne.s     _scsidook
			cmp.l     (a0),d1
			bhi.s     _scsidow2
_scsidoerr: moveq     #-1,d0
			bra.s     _scsidowr
_scsidook:	clr.w     d0
_scsidowr:	move.b    #0,(s_icr).w
			rts

_scsiaction:bsr       _scsidelay250ms
			move.b    #3,(s_tcr).w
			move.b    (s_inircv).w,d0
			bsr.s     _scsiwait
			bmi.s     _scsiactionr
			moveq     #0,d0
			move.b    (s_data).w,d0
			bsr       _scsidelay250ms
			move.l    d0,-(sp)
			bsr.s     _scsidone
			tst.w     d0
			beq.s     _scsiaction3
_scsiaction2:addq.l    #4,sp
			bra.s     _scsiactionr
_scsiaction3:bsr.s     _scsidelay250ms
			bsr.s     _scsiwait
			bmi.s     _scsiaction2
			move.b    (s_data).w,d0
			bsr.s     _scsidone
			tst.w     d0
			bmi.s     _scsiaction2
			move.l    (sp)+,d0
_scsiactionr:rts

_scsiwait:  btst      #5,(s_idstat).w
			bne.s     _scsiwaitok
			cmp.l     (a0),d1
			bhi.s     _scsiwait
			moveq     #-1,d0
			bra.s     _scsiwaitr
_scsiwaitok:moveq     #0,d0
_scsiwaitr: rts

_scsidone:	ori.b     #$11,(s_icr).w
_scsidonel:	btst      #5,(s_idstat).w
			beq.s     _scsidoneok
			cmp.l     (a0),d1
			bhi.s     _scsidonel
			moveq     #-1,d0
			bra.s     _scsidoner
_scsidoneok:moveq     #0,d0
_scsidoner: andi.b    #$ef,(s_icr).w
			rts

_scsisuba:	move.w    d0,-(sp)
			bsr.s     _scsiwait
			bmi.s     _scsisuba2
			move.b    1(sp),(s_data).w
			bsr.s     _scsidone
_scsisuba2:	addq.l    #2,sp
			rts

_scsisubb:	move.b    #$80,(s_icr).w
			bsr.s     _scsidelay250ms
_scsisubbl: cmp.l     (a0),d1
			bhi.s     _scsisubbl
			move.b    #0,(s_icr).w
			bsr.s     _scsidelay1s
_scsisubbl2:cmp.l     (a0),d1
			bhi.s     _scsisubbl2
			rts

_scsidelay250ms:
			movea.w   #_hz_200,a0
			moveq     #51,d1
			add.l     (a0),d1
			rts

_scsidelay1s:
			movea.w   #_hz_200,a0
			move.l    #201,d1
			add.l     (a0),d1
			rts
ENDIF

*+
* Wait for status to come back
*-
w4int:      move.l    #10*200,d0                ; d0 = time-out limit (D_WORST)
            add.l     (_hz_200).w,d0            ; d0 = expiration time
wi0:        btst      #5,(gpip).w               ; interrupt?
            beq.s     wi0                       ; if so, out of the loop
            cmp.l     (_hz_200).w,d0            ; time-out?
            bhi.s     wi0                       ; if not, wait some more
            moveq     #-1,d0                    ; else, return time-out
            bra.s     wi3
wi0:        moveq     #0,d0                     ; clear d0
            move.b    (ide_comst).l,d0          ; d0.b = status returned
            btst      #0,d0                     ; any error?
            bne.s     wi2                       ; if yes, return error code
            btst      #3,d0                     ; else DRQ?
            bne.s     wi3                       ; if so, just return
            moveq     #0,d0                     ; else return OK
            bra.s     wi3
wi2:        move.b    (ide_param).l,d0          ; else d0.b = error bits
wi3:        rts                                 ; return status or error code

*+
* ideread() - reads from 1 to 256 sectors as specified in the Task File,
*		beginning at the specified sector.
*	   - sector count equal to 0 requests 256 sectors.
*
* ideread(nhd, nspt, sectnum, count, buf, pdev)
* WORD	nhd;		4(sp).w		; # of data heads on pdev
* WORD	nspt;		6(sp).w		; # of physical sectors per track
* LONG	sectnum;	8(sp).l		; logical block address
* WORD	count;		$c(sp).w	; # of sectors to read
* BYTE	*buf;		$e(sp).l	; $f(sp)=high $10(sp)=mid $11(sp)=low
* WORD	pdev;		$12(sp).w	; physical device number
*-
_ideread:   bsr       set_dhcs                  ; set physical address
            movea.l   $e(sp),a0                 ; a0 -> buffer to read into
            move.b    $d(sp),(ide_seccn).l      ; set sector count
            move.w    $c(sp),d1                 ; d1.w = # of sectors to read
            subq.w    #1,d1                     ; dbra likes one less
            move.b    #0,(ide_stat2).l          ; enable interrupt
            move.b    #$20,(ide_comst).l        ; set command code IDEREAD
ider0:      bsr.s     w4int                     ; wait for interrupt
            tst.w     d0                        ; successful?
            bmi.s     ider1                     ; if timed-out, return
            btst      #3,d0                     ; DRQ?
            beq.s     ider1                     ; if not, return
            bsr       readbuf                   ; fill sector buffer
            dbra      d1,ider0                  ; more to read?
            moveq     #0,d0                     ; everything's fine
ider1:      rts

*+
* idewrite() - writes from 1 to 256 sectors as specified in the Task File,
*		beginning at the specified sector.
*	    - sector count equal to 0 requests 256 sectors.
*
* idewrite(nhd, nspt, sectnum, count, buf, pdev)
* WORD	nhd;		4(sp).w		; # of data heads on pdev
* WORD	nspt;		6(sp).w		; # of physical sectors per track
* LONG	sectnum;	8(sp).l		; logical block address
* WORD	count;		$c(sp).w	; # sectors to read
* BYTE	*buf;		$e(sp).l	; $f(sp)=high $10(sp)=mid $11(sp)=low
* WORD	pdev;		$12(sp).w	; physical device number
*-
_idewrite:  bsr.s     set_dhcs                  ; set physical address
            movea.l   $e(sp),a0                 ; a0 -> buffer to write from
            move.b    $d(sp),(ide_seccn).l      ; set sector count
            move.w    $c(sp),d1                 ; d1.w = # of sectors to read
            subq.w    #1,d1                     ; dbra likes one less
            move.b    #0,(ide_stat2).l          ; enable interrupt
            move.b    #$30,(ide_comst).l        ; set command code IDEWRITE
idew0:      btst      #3,(ide_stat2).l          ; DRQ?
            beq.s     idew0                     ; if not, wait longer
idew1:      bsr       wrtbuf                    ; fill sector buffer
            bsr       w4int                     ; wait for interrupt
            tst.w     d0                        ; successful?
            bmi.s     idew2                     ; if timed-out, return
            btst      #3,d0                     ; DRQ?
            beq.s     idew2                     ; if not, return
            dbra      d1,idew1                  ; else go transfer data
            moveq     #0,d0                     ; everything's fine
idew2:      rts

*+
* set_dhcs() - convert a logical block address into a physical address.
*	     - set drive #, head #, cylinder # and sector # in task file.
*
* Passed:
*	8(sp).w = nhd = # of data heads
*	$a(sp).w = nspt = # of physical sectors per track
*	$c(sp).l = logical block address
*	$16(sp).w = physical unit #
*-
set_dhcs:   move.l    $c(sp),d1                 ; d1.l = logical block address
            move.w    8(sp),d2                  ; d2.w = # of data heads
            move.w    $a(sp),d0                 ; d0.w = # of physical sectors per track
            mulu.w    d0,d2                     ; d2.l = # of sectors per cylinder = # heads * # of sectors per track
            divu.w    d2,d1                     ; d1.w = cylinder # = log block addr / #spc
            move.b    d1,(ide_zylow).l          ; set cylinder low
            lsr.l     #8,d1                     ; d1.b = cylinder high
            move.b    d1,(ide_zyhig).l          ; set cylinder high
            lsr.l     #8,d1                     ; d1.l = sector # within the cyl
            divu.w    d0,d1                     ; d1.w = head # = sector # within cyl / #spt
            move.w    $16(sp),d0                ; d0.w = physical unit #
            andi.b    #7,d0                     ; mask off flags from physical unit #
            lsl.b     #4,d0                     ; shift unit # to place
            or.b      d0,d1                     ; or in drive #
            move.b    d1,(ide_headn).l          ; set drive and head #
            swap      d1                        ; d1.w = sector # (base 0)
            addq.w    #1,d1                     ; = sector # + 1 (base 1)
            move.b    d1,(ide_stars).l          ; set sector #
            rts

*+
* identify() - allows the Host to receive parameter information from
*	       the drive.
*
* identify(pdev, buf)
* WORD	pdev;	4(sp).w		; physical unit #
* BYTE	*buf;	6(sp).l		; buffer to put data
*-
IF ROM_TOS306
_initdevpar:move.w    4(sp),d0                  ; d0 = physical unit #
			andi.b    #7,d0                     ; mask off flags (if any)
			lsl.w     #4,d0                     ; shift unit # to place
			move.b    d0,(ide_headn).l          ; set drive #
			move.w    6(sp),d0
			subq.b    #1,d0
			or.b      d0,(ide_headn).l			; # of heads - 1
			move.b    9(sp),(ide_seccn).l		; # of sectors per track
			move.b    #0,(ide_stat2).l          ; enable interrupt
			move.b    #$91,(ide_comst).l        ; set command code INITIALIZE DEVICE PARAMETERS
			bra       w4int
ENDIF

*+
* identify() - allows the Host to receive parameter information from
*	       the drive.
*
* identify(pdev, buf)
* WORD	pdev;	4(sp).w		; physical unit #
* BYTE	*buf;	6(sp).l		; buffer to put data
*-
_identify:  move.w    4(sp),d0                  ; d0 = physical unit #
            andi.b    #7,d0                     ; mask off flags (if any)
            lsl.b     #4,d0                     ; shift unit # to place
            move.b    d0,(ide_headn).l          ; set drive #
            movea.l   6(sp),a0                  ; a0 -> buffer
            move.b    #0,(ide_stat2).l          ; enable interrupt
            move.b    #$ec,(ide_comst).l        ; set command code IDENTIFY
            bsr       w4int                     ; wait for interrupt
            tst.w     d0                        ; successful?
            bmi.s     id0                       ; if timed-out, return
            btst      #3,d0                     ; DRQ?
            beq.s     id0                       ; if not, return with error
            bsr.s     readbuf                   ; read data
            moveq     #0,d0                     ; everything's fine
id0:        rts

*+
* readbuf() - reads 512 bytes (128 longs) of data into sector buffer.
*
* Passed:
*	a0.l = buffer to store data read from sector buffer
*-
readbuf:
IF ROM_TOS306
			moveq     #$7f,d0                   ; d0 = (# of words of data to read) - 1
			lea       (ide_datar).l,a1          ; a1 -> data bus
rb0:		move.l    (a1),(a0)+                ; read data from bus
			dbra      d0,rb0                    ; repeat until all done
ELSE
			moveq     #$1f,d0                   ; d0 = (# of words of data to read / 8) - 1
            lea       (ide_datar).l,a1          ; a1 -> data bus
rb0:        move.w    (a1),(a0)+                ; read data from bus
            move.w    (a1),(a0)+                ; read data from bus
            move.w    (a1),(a0)+                ; read data from bus
            move.w    (a1),(a0)+                ; read data from bus
            move.w    (a1),(a0)+                ; read data from bus
            move.w    (a1),(a0)+                ; read data from bus
            move.w    (a1),(a0)+                ; read data from bus
            move.w    (a1),(a0)+                ; read data from bus
            dbra      d0,rb0                    ; repeat until all done
ENDIF
            rts

*+
* wrtbuf() - writes 512 bytes (128 longs) of data to sector buffer.
*
* Passed:
*	a0.l = buffer with data to write to sector buffer
*-
wrtbuf:
IF ROM_TOS306
			moveq     #$7f,d0					; d0 = (# of words of data to read) - 1
			lea       (ide_datar).l,a1			; a1 -> data bus
wb0:		move.l    (a0)+,(a1)				; write data to bus
			dbra      d0,wb0					; repeat until all done
ELSE
		 	moveq     #$1f,d0                   ; d0 = (# of words of data to read / 8) - 1
            lea       (ide_datar).l,a1          ; a1 -> data bus
wb0:        move.w    (a0)+,(a1)                ; write data to bus
            move.w    (a0)+,(a1)                ; write data to bus
            move.w    (a0)+,(a1)                ; write data to bus
            move.w    (a0)+,(a1)                ; write data to bus
            move.w    (a0)+,(a1)                ; write data to bus
            move.w    (a0)+,(a1)                ; write data to bus
            move.w    (a0)+,(a1)                ; write data to bus
            move.w    (a0)+,(a1)                ; write data to bus
            dbra      d0,wb0                    ; repeat until all done
ENDIF
            rts

*+
* _iderdy() - test if the IDE drive is ready
*
* Passed:
*	d0.b = IDE drive unit #
*
* Returns: 0 - if drive is NOT ready
*	   1 - if drive is ready
*-
_iderdy:    andi.b    #7,d0                     ; mask off flags (if any)
            lsl.b     #4,d0                     ; shift unit # to place
            move.b    d0,(ide_headn).l          ; set drive #
IF ROM_TOS306
			move.l    #5*200,d0                 ; set up timer IDERDY
			add.l     (_hz_200).w,d0
ir0:		btst      #6,(ide_stat2).l
			bne.s     ir1
ELSE
            move.b    #$50,d1                   ; ready status
            move.l    #5*200,d0                 ; set up timer IDERDY
            add.l     (_hz_200).w,d0
ir0:        cmp.b     (ide_stat2).l,d1          ; is drive ready and not busy?
            beq.s     ir1                       ; if so, return with drive ready
ENDIF
            cmp.l     (_hz_200).w,d0            ; time-out yet?
            bcc.s     ir0                       ; if not, wait longer
            moveq     #0,d0                     ; else return drive NOT ready
            rts

ir1:        moveq     #1,d0                     ; else, drive is ready
            rts

*+
* gcparm() - get current drive parameters
*
* gcparm(buf)
* char	*buf;	$4(sp).l    /* -> data returned by identify() */
*
* Returns:
*	d0.w = # of default cylinders
*	d1.w = # of default heads
*	d2.w = # of default sectors per track
*-
IF !ROM_TOS306
_gcparm:    movea.l   4(sp),a0                  ; a0 -> data buffer
            adda.w    #$50,a0                   ; a0 -> where Conner model number is (CONMDL)
            move.l    a0,-(sp)
            pea       (cp2024).l
            move.w    #6,-(sp)
            bsr.s     strcmp                    ; compare model# with "CP2024"
            adda.w    #$a,sp                    ; clean up stack
            tst.w     d0                        ; is unit the CP2024 (Kato 20Mb)?
            bne.s     gcp0                      ; if not, handle the normal way else return default values of CP2024
            move.w    #$267,d0                  ; d0.w = # of cylinders (CP20NCYL)
            move.w    #4,d1                     ; d1.w = # of heads (CP20NHEAD)
            move.w    #$11,d2                   ; d2.w = # of spt (CP20NSPT)
            bra.s     gcpend
gcp0:       movea.l   4(sp),a0
            move.w    2(a0),d0                  ; d0.w = # of cylinders
            move.w    6(a0),d1                  ; d1.w = # of heads
            move.w    $c(a0),d2                 ; d2.w = # of sectors per track
gcpend:     rts

conner:     DC.B      'Conner',0
            DC.B      $00
cp2024:     DC.B      'CP2024',0
            DC.B      $00

*+
* strcmp() - compare two strings
*
* Passed:
*	4(sp).w  = n (# of bytes to compare)
*	6(sp).l  = address of first string
*	10(sp).l = address of second string
*
* Returns:
*	d0.w = 0	if first n bytes of the 2 strings are the same
*	     = non-0    otherwise
*-
strcmp:     movem.l   d1/a0-a1,-(sp)            ; save registers d1, a0 and a1
            move.w    $10(sp),d1                ; d1 = byte count
            subq.w    #1,d1                     ; dbra likes one less
            movea.l   $12(sp),a0                ; a0 -> string 1
            movea.l   $16(sp),a1                ; a1 -> string 2
            moveq     #1,d0                     ; assume strings are not the same
str0:       cmpm.b    (a0)+,(a1)+               ; characters the same?
            bne.s     str1                      ; if not, return
            dbra      d1,str0                   ; else compare next character
            moveq     #0,d0                     ; the strings are the same
str1:       movem.l   (sp)+,d1/a0-a1            ; restore registers d1, a0 and a1
            rts
ENDIF

**************************************************************************
*                                                                        *
**************************************************************************
IF ROM_TOS306
readCurrentTime:
			bsr       checkRTC
			bcs.s     readCurrentTime2
			move.b    #$d,(rtcadd).w
			move.b    (rtcdat).w,d0
			btst      #7,d0
			bne.s     readCurrentTime2
			move.l    #$12c80000,-(sp)			; default date: July 4th, 1989, 12:00am
			bsr       writeRTCTime
			addq.w    #4,sp
readCurrentTime:
			bsr.s     readRTCTime
			cmp.l     #-1,d0
			beq.s     readCurrentTime2
			moveq     #0,d0
readCurrentTime2:
	 		rts

readRTCTime:bsr       checkRTC
			bcs.s     readCurrentTime2
			move.b    #$d,(rtcadd).w
			btst      #7,(rtcdat).w
			beq       readRTCTimeErr
			move      sr,d2
			move.w    d2,d0
			or.w      #$700,d0
readRTCTime3:move.b    #$a,(rtcadd).w
			btst      #7,(rtcdat).w
			bne.s     readRTCTime3
			moveq     #0,d0
			move.l    d0,d1
			move.b    #0,(rtcadd).w
			move.b    (rtcdat).w,d0
			asr.w     #1,d0
			move.b    #2,(rtcadd).w
			move.b    (rtcdat).w,d1
			bfins     d1,d0{21:6}
			move.b    #4,(rtcadd).w
			move.b    (rtcdat).w,d1
			bfins     d1,d0{16:5}
			move.b    #7,(rtcadd).w
			move.b    (rtcdat).w,d1
			bfins     d1,d0{11:5}
			move.b    #8,(rtcadd).w
			move.b    (rtcdat).w,d1
			bfins     d1,d0{7:4}
			move.b    #9,(rtcadd).w
			move.b    (rtcdat).w,d1
			sub.b     #12,d1
			bfins     d1,d0{0:7}
			move      d2,sr
			move      sr,d2
			ori       #$700,sr
			move.w    d0,(systemDate).l
			swap      d0
			move.w    d0,(systemTime).l
			swap      d0
			move      d2,sr
			rts

readRTCTimeErr:
			moveq     #-1,d0
			rts

writeRTCTime:bsr       checkRTC
			bcs       readCurrentTime2
			move.l    4(sp),d0
			move.b    #$b,(rtcadd).w
			move.b    #$80,(rtcdat).w
			move.b    #$a,(rtcadd).w
			move.b    #$2a,(rtcdat).w
			move.b    #$b,(rtcadd).w
			move.b    #$8e,(rtcdat).w
			move.b    #0,(rtcadd).w
			bfextu    d0{27:5},d1
			add.b     d1,d1
			move.b    d1,(rtcdat).w
			move.b    #2,(rtcadd).w
			bfextu    d0{21:6},d1
			move.b    d1,(rtcdat).w
			move.b    #4,(rtcadd).w
			bfextu    d0{16:5},d1
			move.b    d1,(rtcdat).w
			move.b    #7,(rtcadd).w
			bfextu    d0{11:5},d1
			move.b    d1,(rtcdat).w
			move.b    #8,(rtcadd).w
			bfextu    d0{7:4},d1
			move.b    d1,(rtcdat).w
			move.b    #9,(rtcadd).w
			bfextu    d0{0:7},d1
			add.b     #$c,d1
			move.b    d1,(rtcdat).w
			move.b    #$b,(rtcadd).w
			move.b    #$e,(rtcdat).w
			rts

checkRTC:	movea.l   sp,a0
			movea.l   (busexception).w,a1
			move.l    #checkRTC2,(busexception).w
			move.b    #0,(rtcadd).w
			move.b    (rtcdat).w,d0
			move.l    a1,(busexception).w
			andi      #$fe,ccr
			rts
checkRTC2:	movea.l   a0,sp
			move.l    a1,(busexception).w
			ori       #1,ccr
			rts



_NVMaccess: moveq     #-5,d0			
            move.w    4(sp),d1					; op = 0: read NVRAM
            beq.s     _NVMread
            cmp.w     #2,d1						; op = 2: initialize NVRAM and update checksum
            beq.s     _NVMinit
            bhi.s     _NVMaccessr				; (unknown op)

* write into NVRAM and update the checksum
			bsr.s     _NVMvalid					; op = 1: write NVRAM
			tst.w     d0
			bne.s     _NVMaccessr
			movea.l   $a(sp),a0					; buffer
			bra.s     _NVMwrt2
_NVMwrtl:	move.b    d1,(a1)					; select register
			move.b    (a0)+,(a2)				; write value
			addq.w    #1,d1						; next reg
_NVMwrt2:	dbra      d2,_NVMwrtl
			bsr       _NVMchksum				; calculate the checksum
			move.b    #$3f,(a1)
			move.b    d0,(a2)					; checksum into reg 63
			not.b     d0
			move.b    #$3e,(a1)
			move.b    d0,(a2)					; inverted checksum into reg 62
			moveq     #0,d0						; (ok)
_NVMaccessr:rts

* read from NVRAM and also validate the checksum
_NVMread:	bsr.s     _NVMvalid
			cmp.w     #-5,d0					; argument error?
			beq.s     _NVMaccessr				; (fail)
			movea.l   $a(sp),a0					; buffer
			bra.s     _NVMread2
_NVMreadl:	move.b    d1,(a1)					; select register
			move.b    (a2),(a0)+				; read value
			addq.w    #1,d1						; next reg
_NVMread2:	dbra      d2,_NVMreadl
			rts

* erase NVRAM and update the checksum
_NVMinit:	lea       (rtcadd).w,a1
			lea       (rtcdat).w,a2
			moveq     #0,d0						; fill value = 0
			moveq     #14,d1
			moveq     #50-1,d2					; fill 48 bytes plus checksum
_NVMinitl:	move.b    d1,(a1)					; select register
			move.b    d0,(a2)					; clear
			addq.w    #1,d1						; next reg
			dbra      d2,_NVMinitl
			move.b    #$3e,(a1)					; inverted checksum of 0 is 0xFF
			move.b    #$ff,(a2)
			rts

* validate checksum in NVRAM and function parameters
_NVMvalid:	bsr.s     _NVMchksum				; calculate the checksum
			move.b    d0,d1
			moveq     #-12,d0					; default: checksum error
			move.b    #$3f,(a1)					; register 63
			cmp.b     (a2),d1					; contains the checksum
			bne.s     _NVMvalidr				; (error)
			not.b     d1
			move.b    #$3e,(a1)					; register 62
			cmp.b     (a2),d1					; contains the inverted checksum
			bne.s     _NVMvalidr				; (error)
			moveq     #-5,d0					; default: argument error
			move.w    $a(sp),d1					; count
			cmp.w     #48,d1					; more than 48 bytes?
			bcc.s     _NVMvalidr				; (error)
			move.w    $c(sp),d2					; start
			bmi.s     _NVMvalidr				; negative => error
			add.w     d1,d2						; start + count
			cmp.w     #48,d2					; >48
			bhi.s     _NVMvalidr				; (error)
			moveq     #0,d0						; no error
_NVMvalidr:	move.w    $c(sp),d2					; d2: count
			move.w    $a(sp),d1
			add.w     #14,d1					; d1: first register
			rts

_NVMchksum:	lea       (rtcadd).w,a1
			lea       (rtcdat).w,a2
			moveq     #0,d0						; checksum = 0
			moveq     #14,d1					; NVRAM starts at register 14
			moveq     #48-1,d2					; 48 bytes
_NVMcheckl:	move.b    d1,(a1)					; select register
			add.b     (a2),d0					; sum values to checksum
			addq.w    #1,d1
			dbra      d2,_NVMcheckl
			rts

ELSE

readCurrentTime:bsr.s checkRTC
            bcs.s     readCurrentTime2
            bsr.s     readRTCTime
            cmp.l     #-1,d0
            beq.s     readCurrentTime2
            moveq     #0,d0
readCurrentTime2:rts

IF ROM_STBOOK
checkRTC:
			movea.w   #rtc_sec,a0
            move.b    #$00,$1d(a0)              ; Clear RTC Test register
            move.b    #$0c,$1f(a0)              ; Reset 1 Hz & 16 Hz alarm pulse
            move.b    #$08,$1b(a0)              ; Clock Start, Alarm off, Bank 0
            moveq     #0,d0                     ; The RTC is always available
            rts
ELSE
checkRTC:	movea.w   #rtc_sec,a0
			move.l    (busexception).w,d2
			movea.l   sp,a2
			move.l    #checkRTC2,(busexception).w
			bset      #0,$1b(a0)
			move.l    d2,(busexception).w
			move.w    #$a05,d0
			movep.w   d0,5(a0)
			movep.w   5(a0),d1
			and.w     #$f0f,d1
			cmp.w     d0,d1
			bne.s     checkRTC3
			move.b    #1,1(a0)
			bclr      #0,$1b(a0)
			move.b    #0,$1d(a0)
			rts
checkRTC2:	movea.l   a2,sp
			move.l    d2,(busexception).w
checkRTC3:	ori       #1,ccr					; RTC not available
			rts
ENDIF

readRTCTime:bsr.s     checkRTC
            bcs       readRTCTimeErr
            lea       (rtcbufa).w,a1
            lea       (rtcbufb).w,a2
            bsr       readRTCTimeMask
readRTCTime2:exg      a1,a2
            bsr       readRTCTimeMask
            moveq     #$c,d0
readRTCTime3:move.b   (a1,d0.w),d1
            cmp.b     (a2,d0.w),d1
            bne.s     readRTCTime2
            dbra      d0,readRTCTime3
            moveq     #0,d0
            move.b    $b(a1),d0
            mulu.w    #10,d0
            add.b     $c(a1),d0
            asr.w     #1,d0
            move.w    d0,d1
            moveq     #0,d0
            move.b    9(a1),d0
            mulu.w    #10,d0
            add.b     $a(a1),d0
            asl.w     #5,d0
            add.w     d0,d1
            moveq     #0,d0
            move.b    7(a1),d0
            mulu.w    #10,d0
            add.b     8(a1),d0
            asl.w     #8,d0
            asl.w     #3,d0
            add.w     d0,d1
            swap      d1
            moveq     #0,d0
            move.b    4(a1),d0
            mulu.w    #10,d0
            add.b     5(a1),d0
            move.w    d0,d1
            moveq     #0,d0
            move.b    2(a1),d0
            mulu.w    #10,d0
            add.b     3(a1),d0
            asl.w     #5,d0
            add.w     d0,d1
            moveq     #0,d0
            move.b    (a1),d0
            mulu.w    #10,d0
            add.b     1(a1),d0
            asl.w     #8,d0
            asl.w     #1,d0
            add.w     d0,d1
            move      sr,d2
            ori       #$700,sr
            move.w    d1,(systemDate).l
            swap      d1
            move.w    d1,(systemTime).l
            move      d2,sr
            move.l    d1,d0
            rts

readRTCTimeMask:moveq #$c,d0
            moveq     #1,d1
readRTCTimeMask2:move.b (a0,d1.w),d2
            and.b     #$f,d2
            move.b    d2,(a1,d0.w)
            addq.w    #2,d1
            dbra      d0,readRTCTimeMask2
            rts

readRTCTimeErr:moveq  #-1,d0
            rts

writeRTCTime:bsr      checkRTC
            bcs       no_RTC_found
            lea       (rtcbufa).w,a1
            movea.w   4(sp),a2
            bsr       calcRTCWeekday
            move.b    d0,6(a1)
            move.w    6(sp),d1
            move.w    d1,d0
            and.l     #$1f,d0
            add.w     d0,d0
            divu.w    #10,d0
            move.b    d0,$b(a1)
            swap      d0
            move.b    d0,$c(a1)
            move.w    d1,d0
            lsr.w     #5,d0
            and.l     #$3f,d0
            divu.w    #10,d0
            move.b    d0,9(a1)
            swap      d0
            move.b    d0,$a(a1)
            lsr.w     #8,d1
            lsr.w     #3,d1
            ext.l     d1
            divu.w    #10,d1
            move.b    d1,7(a1)
            swap      d1
            move.b    d1,8(a1)
            move.w    4(sp),d1
            move.w    d1,d0
            and.l     #$1f,d0
            divu.w    #10,d0
            move.b    d0,4(a1)
            swap      d0
            move.b    d0,5(a1)
            move.w    d1,d0
            lsr.w     #5,d0
            and.l     #$f,d0
            divu.w    #10,d0
            move.b    d0,2(a1)
            swap      d0
            move.b    d0,3(a1)
            lsr.w     #1,d1
            lsr.w     #8,d1
            ext.l     d1
            move.l    d1,d2
            divu.w    #10,d1
            move.b    d1,(a1)
            swap      d1
            move.b    d1,1(a1)
            divu.w    #4,d2
            swap      d2
            move.b    #$e,$1f(a0)
            bset      #0,$1b(a0)
            move.b    #1,$15(a0)
            move.b    d2,$17(a0)
            bclr      #0,$1b(a0)
            bclr      #3,$1b(a0)
            moveq     #$c,d0
            moveq     #1,d1
writeRTCTime2:move.b  (a1,d0.w),(a0,d1.w)
            addq.w    #2,d1
            dbra      d0,writeRTCTime2
            bset      #3,$1b(a0)
            moveq     #0,d0
            rts

no_RTC_found:moveq    #-1,d0
            rts

calcRTCWeekday:moveq  #2,d2
            move.w    a2,d0
            lsr.w     #8,d0
            lsr.w     #1,d0
            add.w     d0,d2
            move.w    d0,d1
            lsr.w     #2,d1
            add.w     d1,d2
            move.w    a2,d1
            lsr.w     #5,d1
            and.w     #$f,d1
            and.w     #3,d0
            bne.s     calcRTCWeekday2
            cmp.w     #2,d1
            bhi.s     calcRTCWeekday2
            subq.w    #1,d2
calcRTCWeekday2:subq.w #1,d1
            add.w     d1,d1
            add.w     daysOffsetToMonths(pc,d1.w),d2
            move.w    a2,d1
            and.w     #$1f,d1
            add.w     d1,d2
            divu.w    #7,d2
            swap      d2
            moveq     #0,d0
            move.w    d2,d0
            rts

daysOffsetToMonths:DC.W $0000,$001f,$003b,$005a
            DC.W      $0078,$0097,$00b5,$00d4
            DC.W      $00f3,$0111,$0130,$014e

_Waketime:  move.l    4(sp),d0
            beq       clearRTCWaketime
            cmp.l     #-1,d0
            beq       readRTCWaketime
            cmp.l     #1,d0
            beq       enableRTCWaketime
            bsr       readRTCTime
            cmp.l     #-1,d0
            beq       enableRTCWaketimeDone
            move.l    4(sp),d1
            and.b     #$e0,d0
            and.b     #$e0,d1
            cmp.l     d1,d0                     ; is the new wakeup time already in the past?
            bcc.s     _Waketime_ret1
            move.l    (waketimeAlarm).l,d2
            beq.s     _Waketime2
            cmp.l     d2,d0
            bhi.s     _Waketime2
            cmp.l     d2,d1
            bcc.s     _Waketime_ret2
_Waketime2: movea.l   d0,a0
            move.l    d1,d2
            and.l     #$1fffff,d2
            and.l     #$1fffff,d0
            cmp.l     d2,d0                     ; new wakeup time is _now_?
            beq.s     _Waketime3
            move.l    d1,(waketimeAlarm).l
            moveq     #0,d0
            rts

_Waketime3: and.l     #$1f0000,d2
            cmp.l     #$10000,d2
            beq.s     _Waketime_ret5
            and.l     #$ffe0ffff,d1
            or.l      #$10000,d1
            move.l    d1,(waketimeAlarm).l
            moveq     #4,d0
            rts

_Waketime_ret5:move.l a0,d0
            add.l     #$10000,d0
            move.l    d0,(waketimeAlarm).l
            moveq     #5,d0
            rts

_Waketime_ret1:moveq  #1,d0
            rts

_Waketime_ret2:moveq  #2,d0
            rts

readRTCWaketime:move.l (waketimeAlarm).l,d0
            rts

clearRTCWaketime:bsr  checkRTC
            bcs       no_RTC_found
            bclr      #2,$1b(a0)                ; RTC Alarm off
            move.b    #$d,$1f(a0)               ; Reset 1Hz, 16 Hz and Alarm
            clr.l     (waketimeAlarm).l
            moveq     #3,d0
            rts

enableRTCWaketime:bsr readRTCTime
            cmp.l     #-1,d0
            beq       enableRTCWaketimeDone
            move.l    (waketimeAlarm).l,d1
            beq       enableRTCWaketimeDone
            and.b     #$e0,d1
            move.l    d1,d2
            and.b     #$e0,d0
            cmp.l     d1,d0                     ; is the new wakeup time already in the past?
            bhi.s     enableRTCWaketimeDone
            bne.s     enableRTCWaketime2
            and.b     #$1f,d2
            cmp.b     #7,d2
            bcs.s     enableRTCWaketime3
enableRTCWaketime2:movea.w #rtc_sec,a0
            bset      #0,$1b(a0)                ; RTC select Bank 1
            move.l    (waketimeAlarm).l,d1
            move.w    d1,d0
            lsr.w     #5,d0                     ; Minutes
            and.l     #$3f,d0
            divu.w    #10,d0
            move.b    d0,7(a0)                  ; RTC 1-minute alarm register
            swap      d0
            move.b    d0,5(a0)                  ; RTC 10-minute alarm register
            move.w    d1,d0
            lsr.w     #8,d0
            lsr.w     #3,d0                     ; Hours
            ext.l     d0
            divu.w    #10,d0
            move.b    d0,$b(a0)                 ; RTC 1-hour alarm register
            swap      d0
            move.b    d0,9(a0)                  ; RTC 10-hour alarm register
            move.l    d1,d0
            swap      d0
            and.w     #$1f,d0                   ; Days
            ext.l     d0
            divu.w    #10,d0
            move.b    d0,$11(a0)                ; RTC 1-hour alarm register
            swap      d0
            move.b    d0,$f(a0)                 ; RTC 10-day alarm register
            movea.w   (waketimeAlarm).l,a2
            bsr       calcRTCWeekday
            move.b    d0,$d(a0)                 ; RTC day-of-the-week alarm register
            bset      #2,$1b(a0)                ; RTC Alarm on
            bclr      #0,$1b(a0)                ; RTC select Bank 0
enableRTCWaketimeDone:moveq #0,d0
            rts

enableRTCWaketime3:clr.l (waketimeAlarm).l
            moveq     #-1,d0
            rts
ENDIF

**************************************************************************
*                                                                        *
*                cp/m-68k atari rbp bios                                 *
*                basic input/output subsystem                            *
*                copyright 1984, atari corporation                       *
*                all rights reserved.                                    *
*                atari confidential                                      *
*                                                                        *
**************************************************************************
**************************************************************************
*                                                                        *
*        convert ikbd real-time clock format to jdos format              *
*                                                                        *
**************************************************************************
jdostime:   lea       (clkrec).w,a0
            bsr       bcdbin
            subi.b    #80,d0                    ; adjust so that 1980 => 0 for time base
            move.b    d0,d2
            asl.l     #4,d2

            bsr       bcdbin
            add.b     d0,d2
            asl.l     #5,d2

            bsr       bcdbin
            add.b     d0,d2
            asl.l     #5,d2

            bsr       bcdbin
            add.b     d0,d2
            asl.l     #6,d2

            bsr       bcdbin
            add.b     d0,d2
            asl.l     #5,d2

            bsr       bcdbin

            lsr.b     #1,d0                     ; adjust to provide two second increments...
            add.b     d0,d2                     ; ...another @!#%@#$% kludge, thank you !
            move.l    d2,(datetime).w
            move.b    #0,(newtod).w             ; clear handshaking flag
            rts


readIKBDTime:move.b   #$ff,(newtod).w
            move.b    #$1c,d1
            bsr       ikbdput
            movea.l   (_hz_200).w,a0
            adda.w    #200,a0
            moveq     #0,d0
readIKBDTime2:cmpa.l  (_hz_200).w,a0
            bcs.s     readIKBDTimex
            tst.b     (newtod).w
            bne.s     readIKBDTime2
            move.l    (datetime).w,d0
readIKBDTimex:rts

writeIKBDTime:move.l  4(sp),(newtime).w
            lea       (kmbuf).l,a0
            move.l    (newtime).w,d2
            move.b    d2,d0
            andi.b    #$1f,d0
            asl.b     #1,d0
            bsr.s     binbcd
            lsr.l     #5,d2
            move.b    d2,d0
            andi.b    #$3f,d0
            bsr.s     binbcd
            lsr.l     #6,d2
            move.b    d2,d0
            andi.b    #$1f,d0
            bsr.s     binbcd
            lsr.l     #5,d2
            move.b    d2,d0
            andi.b    #$1f,d0
            bsr.s     binbcd
            lsr.l     #5,d2
            move.b    d2,d0
            andi.b    #$f,d0
            bsr.s     binbcd
            lsr.l     #4,d2
            move.b    d2,d0
            andi.b    #$7f,d0
            bsr.s     binbcd
            addi.b    #$80,(a0)
            move.b    #$1b,d1
            bsr       ikbdput
            moveq     #5,d3
            lea       (oclkrec).l,a2
            bsr       ikbdstr
            move.b    #$1c,d1
            bsr       ikbdput
            rts

**************************************************************************
*                                                                        *
*                  convert a byte from binary to bcd format              *
*                                                                        *
*       entry:     d0.l  - value                                         *
*                                                                        *
**************************************************************************
binbcd:     moveq     #0,d1
            move.b    d0,d1
            divs.w    #10,d1
            asl.w     #4,d1
            move.w    d1,d0
            swap      d1
            add.w     d1,d0
            move.b    d0,-(a0)                  ; transfer to output clock buffer
            rts

**************************************************************************
*                                                                        *
*                  convert a byte from bcd format to binary              *
*                                                                        *
*       entry:     a0.l  - pointer to byte                               *
*                                                                        *
**************************************************************************
bcdbin:     move.b    (a0)+,d0                  ; get bcd byte
            move.b    d0,d1
            and.w     #$0f,d0                   ; dump high nibble
            and.w     #$f0,d1                   ; isolate high nibble
            asr.w     #4,d1                     ; dump low nibble
            mulu.w    #10,d1                    ; high nibble * 10
            add.w     d1,d0                     ; + low nibble
            rts

**************************************************************************
*                                                                        *
*                  midi output status                                    *
*                                                                        *
*       entry:                                                           *
*                                                                        *
*       word       midiost()                                             *
*                                                                        *
*       returns true/okay to send = -1,  false/not ready = 0             *
*                                                                        *
**************************************************************************
midiost:    moveq     #-1,d0                    ; pre-set to true
            move.b    (midictl).w,d2            ; grab midi status
            btst      #1,d2
            bne.s     midiox                    ; status okay to send
            moveq     #0,d0                     ; status not okay
midiox:     rts

**************************************************************************
*                                                                        *
*                  write char to midi port                               *
*                                                                        *
*       entry:                                                           *
*                                                                        *
*       void       midiwc(chr)                                           *
*       word       chr                                                   *
*                                                                        *
**************************************************************************
midiwc:     move.w    6(sp),d1
midiput:    lea       (midictl).w,a1            ; point to midi register base
midput1:    move.b    (a1),d2                   ; grab midi status
            btst      #1,d2
            beq.s     midput1
            move.b    d1,2(a1)
            rts                                 ; done for now

**************************************************************************
*                                                                        *
*                  put string to midi routine                            *
*                                                                        *
*       entry:                                                           *
*                                                                        *
*       void       midiws(size,ptr)                                      *
*       word       size                                                  *
*       long       ptr                                                   *
*                                                                        *
**************************************************************************
midiws:     moveq     #0,d3
            move.w    4(sp),d3                  ; get size of string buffer - 1
            movea.l   6(sp),a2                  ; get string address
midp1:      move.b    (a2)+,d1
            bsr.s     midiput
            dbra      d3,midp1
            rts

**************************************************************************
*                                                                        *
*                  get midi receiver buffer status                       *
*                                                                        *
*       entry:                                                           *
*                                                                        *
*       word       midistat()                                            *
*                                                                        *
*       -1 signifies true/okay  0 - signifies false/no characters        *
*                                                                        *
**************************************************************************
midstat:    lea       (mbufrec).w,a0            ; point to midi i/o bufrec
            lea       (midictl).w,a1            ; point to midi register base
            moveq     #-1,d0                    ; set result to true
            lea       6(a0),a2
            lea       8(a0),a3
            cmpm.w    (a3)+,(a2)+               ; atomic buffer empty test
            bne.s     midist1                   ; branch of not, assume d0 is "clr.w"'ed
            moveq     #0,d0                     ; set result to false
midist1:    rts

**************************************************************************
*                                                                        *
*                  getchar routine for midi port                         *
*                                                                        *
*       this routine transfers characters from a input queue that is     *
*       filled by an automatic interrupt routine.  the interrupt         *
*       routine handles the actual transfer of the character from the    *
*       i/o port.                                                        *
*                                                                        *
*       entry:                                                           *
*                                                                        *
*       long       midiin()                                              *
*                                                                        *
*       long data returned represents upper three bytes of time stramp   *
*       and least significant byte as data                               *
*                                                                        *
**************************************************************************
* assume that a0/a1 are inited by the midstat call for the rest of
* this routine.

midin:      bsr.s     midstat                   ; see if key pressed
            tst.w     d0
            beq.s     midin                     ; wait until byte comes in
            move      sr,-(sp)                  ; protect this upcoming test
            ori       #$700,sr
            move.w    6(a0),d1                  ; get current head pointer offset from buffer
            cmp.w     8(a0),d1                  ; head=tail?
            beq.s     mwi2                      ; yes

* check for wrap of pointer

            addq.w    #1,d1                     ; i=j+1
            cmp.w     4(a0),d1                  ; ? i>= current bufsiz?
            bcs.s     mwi1                      ; no...
            moveq     #0,d1                     ; wrap pointer
mwi1:       movea.l   (a0),a1                   ; get base address of buffer
            and.l     #$ffff,d1
IF !ROM_TOS306
            moveq     #0,d0
ENDIF
            move.b    (a1,d1.l),d0              ; get character
            move.w    d1,6(a0)                  ; store new head pointer to buffer record
mwi2:       move      (sp)+,sr
            rts

**************************************************************************
*                                                                        *
*                  parallel i/o port service routine                     *
*                                                                        *
*       this set of routines is for general parallel i/o                 *
*                                                                        *
*       entry to listout                                                 *
*                                                                        *
*       entry to listin                                                  *
*                                                                        *
*       exit from listin                                                 *
*                                                                        *
**************************************************************************
_lstout:    btst      #4,(pconfig).w
            bne       _auxout

            move.l    (_hz_200).w,d2            ; d2 = hz_200 - prt_to
            sub.l     (prt_to).w,d2             ; (compute time since last timeout)
            cmpi.l    #5*200,d2                 ; do "fake" timeout if we timed out within
            bcs.s     lperr                     ; the last five seconds
            move.l    (_hz_200).w,d2            ; d2 = starting time for this char
pt0:        bsr       _lstostat                 ; go get parallel port status
            tst.w     d0                        ; ...and check for high (busy)
            bne.s     pt1                       ; port is ready -- print the char

            move.l    (_hz_200).w,d3            ; d3 = hz_200 - d2
            sub.l     d2,d3
            cmpi.l    #$1770,d3                 ; check for 30 second delta
            blt.s     pt0                       ; continue of no timeout

lperr:      moveq     #0,d0                     ; return value of 0 indicates timeout
            move.l    (_hz_200).w,(prt_to).w    ; record time of last timeout
            rts

pt1:        move      sr,d3                     ; save status register
            ori       #$700,sr                  ; protect upcoming switching of the port setting
            moveq     #7,d1                     ; get current io enable register contents
            bsr       gientry
            ori.b     #$80,d0                   ; set port b for output
            move.b    #$87,d1                   ; set to write to io enable
            bsr       gientry
            move      d3,sr                     ; restore status register

            move.w    6(sp),d0                  ; retrieve byte to be sent and...
            move.b    #$8f,d1                   ; write out byte to parallel port
            bsr       gientry

            move      sr,-(sp)
            ori       #$700,sr
            bsr.s     strobeon
            bsr.s     strobeon
            bsr.s     strobeoff
            move      (sp)+,sr
            moveq     #-1,d0                    ; set d0=-1 for good transfer status
            rts


strobeoff:  moveq     #$20,d2                   ; set strobe off
            bra       onbit                     ; go set it!!
strobeon:   move.b    #$df,d2                   ; set strobe on
            bra       offbit                    ; set strobe now...


_lstin:     moveq     #7,d1                     ; get current io enable register contents
            bsr       gientry
            andi.b    #$7f,d0                   ; set port b for input
            move.b    #$87,d1                   ; set to write to io enable
            bsr       gientry

            bsr.s     strobeoff                 ; busy off!
lstibusy:   bsr.s     _lstostat                 ; go get parallel port status
            tst.w     d0                        ; ...and check for high (busy)
            bne.s     lstibusy                  ; loop till high...
            bsr.s     strobeon
            moveq     #$f,d1                    ; init to use gientry routine to read
            bra       gientry                   ; now get the byte from the parallel port
* d0.l contains the byte of data from the port
* the 'bra' is implied rts from this routine

**************************************************************************
*                                                                        *
*                  parallel port status routine                          *
*                                                                        *
**************************************************************************
_lstostat:  lea       (gpip).w,a0               ; point to mfp register base
            moveq     #-1,d0                    ; pre-init to true (parallel port ready)
            btst      #0,(a0)
            beq.s     lst1
            moveq     #0,d0                     ; parallel port busy
lst1:       rts


**************************************************************************
*                                                                        *
*                  auxillary port input status routine                   *
*                                                                        *
**************************************************************************
auxistat:   lea       (rbufrec).w,a0            ; point to rs-232 buffer record
_auxistat:	moveq     #-1,d0                    ; set result to true
            lea       6(a0),a1                  ; head
            lea       8(a0),a0                  ; tail
            cmpm.w    (a0)+,(a1)+               ; atomic buffer empty test
            bne.s     auxist1
            moveq     #0,d0                     ; set result to false
auxist1:    rts

**************************************************************************
*                                                                        *
*                  auxillary input routine                               *
*                                                                        *
**************************************************************************
auxin:      lea       (rbufrec).w,a0            ; point to rs-232 buffer record
            lea       (gpip).w,a2
_auxin:     bsr       rs232get
            move.w    d0,-(sp)
            tst.b     $20(a0)                   ; flow control active?
            beq.s     auxinrts                  ; (no)
            move.w    8(a0),d0                  ; tail
            sub.w     6(a0),d0                  ; - head
            bpl.s     auxin2                    ; underflow?
            add.w     4(a0),d0                  ; + size
auxin2:     cmp.w     $a(a0),d0
            bgt.s     auxinrts
            tst.b     $1e(a0)                   ; high-water flag already set?
            beq.s     auxinrts                  ; no...exit...
            bsr.s     auxinclrhw
auxinrts:   move.w    (sp)+,d0
            rts

auxinclrhw: clr.b     $1e(a0)                   ; clear high-water flag
            btst      #0,$20(a0)                ; is the rs232 mode xon/xoff?
            bne.s     auxinclrhw2               ; (yes)
            bra       rtson
auxinclrhw2:move.b    #$11,$21(a0)              ; "xon"
            bra.s     _auxoutwr

**************************************************************************
*                                                                        *
*                  auxillary port output status routine                  *
*                                                                        *
**************************************************************************
_auxostat:  lea       (rbufrec+14).w,a0         ; point to rs-232 buffer record
__auxostat:	move.w    8(a0),d1                  ; tail
            bsr       wrapin
            moveq     #-1,d0                    ; set result to true
            cmp.w     6(a0),d1                  ; head
            bne.s     _auxostatrts
            moveq     #0,d0                     ; set result to false
_auxostatrts:rts

**************************************************************************
*                                                                        *
*                  auxillary output routine                              *
*                                                                        *
**************************************************************************
_auxout:    move.w    6(sp),d0                  ; get data
            lea       (rbufrec+14).w,a0
			bsr       rs232put                  ; exit via rs-232 output routine
            lea       (rbufrec).w,a0
            lea       (gpip).w,a2

_auxoutwr:  tst.b     $2c(a2)                   ; transmitter status
            bpl.s     _auxoutrts                ; not ready
            move      sr,-(sp)
            ori       #$700,sr
            bsr       wr_rs232
            move      (sp)+,sr
_auxoutrts: rts

**************************************************************************
*                                                                        *
*               ikbd output status                                       *
*                                                                        *
*       entry:                                                           *
*                                                                        *
*       word    ikbdost()                                                *
*                                                                        *
*       returns true/okay to send = 1,  false/not ready = 0              *
*                                                                        *
**************************************************************************
ikbdost:    moveq     #-1,d0                    ; pre-set to true
            move.b    (keyctl).w,d2             ; grab ikbd status
            btst      #1,d2
            bne.s     ikbdox                    ; status okay to send
            moveq     #0,d0                     ; status not okay
ikbdox:     rts

**************************************************************************
*                                                                        *
*               write char to ikbd port                                  *
*                                                                        *
*       entry:                                                           *
*                                                                        *
*       void    ikbdwc(chr)                                              *
*       word    chr                                                      *
*                                                                        *
**************************************************************************
ikbdwc:     move.w    6(sp),d1
ikbdput:    lea       (keyctl).w,a1             ; point to ikbd register base
ikput1:     move.b    (a1),d2                   ; grab keyboard status
            btst      #1,d2
            beq.s     ikput1
IF ROM_TOS306
			move.w    #$400,d0
			bsr       mfpdelay
ELSE
            lea       (tcdr).w,a0
            move.w    #$bf,d0
ikput2:     move.b    (a0),d2
ikput3:     cmp.b     (a0),d2
            beq.s     ikput3
            dbra      d0,ikput2
ENDIF
            move.b    d1,2(a1)                  ; write char to the ikbd port
            rts                                 ; done for now

**************************************************************************
*                                                                        *
*               put string to to ikbd routine                            *
*                                                                        *
*       entry:                                                           *
*                                                                        *
*       void    ikbdws(size,ptr)                                         *
*       word    size                                                     *
*       long    ptr                                                      *
*                                                                        *
**************************************************************************
ikbdws:
IF ROM_TOS306
			moveq     #0,d3
ENDIF
		 	move.w    4(sp),d3
            movea.l   6(sp),a2
ikbdstr:    move.b    (a2)+,d1
            bsr.s     ikbdput
            dbra      d3,ikbdstr
            rts


constat:    lea       (kbufrec).w,a0            ; point to ikbd buffer record
            moveq     #-1,d0                    ; set result to true
            lea       6(a0),a2                  ; head
            lea       8(a0),a3                  ; tail
            cmpm.w    (a3)+,(a2)+               ; atomic buffer empty test
            bne.s     const1                    ; branch of not, assume d0 is "clr.w"'ed
            moveq     #0,d0                     ; set result to false
const1:     rts


conin:      bsr.s     constat                   ; see if key pressed
            tst.w     d0
            beq.s     conin                     ; wait until key pressed
            move      sr,-(sp)                  ; protect this upcoming test
            ori       #$700,sr
            move.w    6(a0),d1                  ; get current head pointer offset
            cmp.w     8(a0),d1                  ; head=tail?
            beq.s     cwi2                      ; yes

* check for wrap of pointer

            addq.w    #4,d1                     ; i=h+4
            cmp.w     4(a0),d1
            bcs.s     cwi1                      ; no...
            moveq     #0,d1                     ; wrap pointer
cwi1:       movea.l   (a0),a1                   ; get base address of buffer
            and.l     #$ffff,d1
            move.l    (a1,d1.l),d0              ; get character
            move.w    d1,6(a0)                  ; store new head pointer to buffer
cwi2:       move      (sp)+,sr
            rts


conoutst:   moveq     #-1,d0
            rts                                 ; jdos requirement


***********************************************************************************
*                                                                                 *
*                routine to set up the general interrupt port registers           *
*                        (gpip,are,ddr)                                           *
*                                                                                 *
*                algorithm to set up the port                                     *
*                                                                                 *
*                1.  mask off all interrupts via the imrx register,               *
*                2.  clear all enable and pending bits in the ierx and iprx       *
*                         registers;                                              *
*                3.  check the inerrupt in-service registers and loop till        *
*                         clear;                                                  *
*                4.  init the aer register bits as desired (default = 11111111);  *
*                5.  init the ddr register bits as desired (default = 10000000);  *
*                6.  clear the gpip register;                                     *
*                7.  enable all desired interrupt enable bits;                    *
*                8.  mask on all desired interrupt mask bits;                     *
*                                                                                 *
***********************************************************************************
initmfp:    lea       (gpip).w,a0               ; init mfp address pointer
            moveq     #0,d0                     ; init to zero for clearing mfp
            movep.l   d0,0(a0)                  ; clear gpip thru iera
            movep.l   d0,8(a0)                  ; clear ierb thru isrb
            movep.l   d0,$10(a0)                ; clear isrb thru vr

            move.b    #$48,$16(a0)              ; set mfp autovector to $100 and s-bit
            bset      #2,2(a0)                  ; set cts to low to high transition

IF ROM_TOS306
			lea       (gpip_TT).w,a0			; init mfp #2 address pointer
			moveq     #0,d0						; init to zero for clearing mfp
			movep.l   d0,0(a0)					; clear gpip thru iera
			movep.l   d0,8(a0)					; clear ierb thru isrb
			movep.l   d0,$10(a0)				; clear isrb thru vr
			move.b    #$58,$16(a0)				; set mfp autovector to $140 and s-bit
ENDIF
			
            clr.b     (privexception).w

* init the "c" timer
            move.w    #$1111,(tc_rot).w         ; setup bitstream for /4 on timer c interrupts
            move.w    #$14,(_timr_ms).w         ; set timer calibration value

            moveq     #2,d0                     ; set to timer C
            moveq     #$50,d1                   ; set to /64 for 200 hz tick
            move.w    #$c0,d2                   ; set to 192
            bsr       settimer                  ; setup timer and init interrupt vector.....

            lea       (timercint).l,a2          ; point to the timer C interrupt routine...
            moveq     #5,d0                     ; point the the timer C interrupt number
            bsr       initint

* init the "d" timer
            moveq     #3,d0                     ; select the d timer
            moveq     #1,d1                     ; init for /4 for 9600 baud
            moveq     #2,d2                     ; init for 9600 baud
            bsr       settimer                  ; branch to out timer initialier...

            move.b    #1,(rbufrec+34).w

* now init the 3 rs232 chip registers
            lea       (gpip).w,a0
            move.l    #$880105,d0
            movep.l   d0,$26(a0)                ; inits scr,ucr,rsr,tsr

IF ROM_TOS306
* now init the 3 rs232 chip registers in mfp #2
			lea       (gpip_TT).w,a0
			move.l    #$880105,d0
			movep.l   d0,$26(a0)                ; inits scr,ucr,rsr,tsr
			move.b    #1,(tcdcr_TT).w			; timer c+d control register: delay 1:4
			move.b    #2,(tddr_TT).w			; timer d data register
			move.b    #1,($1380).w				; ???
ENDIF

* check for SCC chip
            tst.b     (STEFlag).l
            bne.s     initmfp2
            bsr       initscc

* initialize the default rs-232 control line settings
initmfp2:   bsr       dtron
            bsr       rtson

* initialize the rs-232 buffer record structure
            lea       (rbufrec).w,a0
            lea       (rs232init).l,a1
            moveq     #$23,d0
            bsr       lbmove                    ; do block move and return

IF ROM_TOS306
			lea       (ttmfp_rbufrec).w,a0
			lea       (ttrs232init).l,a1
			moveq     #$23,d0
			bsr       lbmove

*			ttrs232init:DC.L      iobuf_ttrs232i            ; ibufptr
*			            DC.W      $0100                     ; ibufsiz
*			            DC.W      $0000                     ; ibufhead
*			            DC.W      $0000                     ; ibuftail
*			            DC.W      $0080                     ; ibuflow
*			            DC.W      $00c0                     ; ibufhigh
*			            DC.L      iobuf_ttrs232o            ; obufptr
*			            DC.W      $0100                     ; obufsiz
*			            DC.W      $0000                     ; obufhead
*			            DC.W      $0000                     ; obuftail
*			            DC.W      $0080                     ; obuflow
*			            DC.W      $00c0                     ; obufhigh
*			            DC.B      $00                       ; rsrbyte
*			            DC.B      $00                       ; tsrbyte
*			            DC.B      $00                       ; rxoff
*			            DC.B      $00                       ; txoff
*			            DC.B      $01                       ; rsmode_xon
*			            DC.B      $00                       ; rsmode_filler
*			            DC.W      $01ff

ENDIF

* initialize the midi buffer record structure
            lea       (mbufrec).w,a0
            lea       (minit).l,a1
            moveq     #$d,d0
            bsr       lbmove                    ; do block move and return

            move.w    #-1,(altdigits).l         ; reset alt-numpad ascii entering
            move.l    #aciaexit,d0              ; init ikbd and midi error handler address
            move.l    d0,(vkbderr).w            ; init keyboard error handler address
            move.l    d0,(vmiderr).w            ; init midi error handler address
            move.l    #sysmidi,(midivec).w      ; point to system midi interrupt vector
            move.l    #astatusmidi,(astatusmidip).w
            move.l    #astatuskeyboard,(astatuskeyp).w
            move.l    #itsakey,(itsakeyp).w

* init the midi acia next
            move.b    #$03,(midictl).w          ; init the midi acia via master reset

* init the acia to divide by 16x clock, 8 bit data, 1 stop bit, no parity,
* rts low, transmitting interrupt disabled, receiving interrupt enabled
            move.b    #$95,(midictl).w

* initialize the keyboard acia interrupt vector exception address
            move.b    #7,(conterm).w            ; enabled keyclick, repeat key, bell functions

            move.l    #jdostime,(clkintvec).w
            move.l    #genrts,d0                ; generalized rts for ikbd subsystems
            move.l    d0,(statintvec).w
            move.l    d0,(msintvec).w           ; init user mouse interrupt adr to rts
            move.l    d0,(joyintvec).w
            bsr       initdevstables
*
* Sound routine initialization - uses the pre-init'ed d0.l=0000 !!
*
            moveq     #0,d0                     ; init 'd0' to clear sound variables
            move.l    d0,(cursnd).w             ; clear sound ptr
            move.b    d0,(timer).w              ; clear delay timer
            move.b    d0,(auxd).w               ; clear temp value
            move.l    d0,(prt_to).w             ; init printer timeout to 0

            bsr       strobeoff                 ; init strobe to off (line high!)
            move.b    #15,(cdelay1).w           ; init system default key repeat values
            move.b    #2,(cdelay2).w

* within the mouse relative routine
* initialize the ikbd buffer record structure
            lea       (kbufrec).w,a0
            lea       (kinit).l,a1
            moveq     #$d,d0
            bsr.s     lbmove                    ; do block move and return

            bsr       bioskeys                  ; point key translation address to
* the rom based translation tables

* init the acia next
            move.b    #$03,(keyctl).w           ; init the acia via master Reset

* now that the vector is initialized, we can allow interrupts to occur!
* init the acia do divice by 64 clock, 8 bit data, 1 stop bit, no parity,
* rts low, transmitting interrupt disabled, receiving interrupt enabled

            move.b    #$96,(keyctl).w

            movea.l   #mfpvectr,a3              ; point to initializing array of exception vec's
            moveq     #3,d1                     ; init branch counter/index
st1:        move.l    d1,d2
            move.l    d1,d0                     ; load in interrupt # to setup
            addi.b    #9,d0                     ; add constand to point to proper mfp interrupt
            asl.l     #2,d2
            movea.l   (a3,d2.w),a2
            bsr       initint                   ; go to service routine
            dbra      d1,st1
            lea       (midikey).l,a2
            moveq     #6,d0                     ; load in interrupt # to setup
            bsr       initint                   ; go to service routine

* initializing code which sets the enable
* and mask bits...
            lea       (ctsint).l,a2             ; point to the CTS interrupt routine...
            moveq     #2,d0                     ; point to the CTS interrrupt number
            bsr       initint

IF ROM_TOS306
			movea.l   #$e36704,a3				; ttmfp_txerr,ttmfp_txrint,ttmfp_rxerr,ttmfp_rcvrint
			movea.w   #mfpTT_SendError,a0		; $0164
			move.l    (a3)+,(a0)+				; mfpTT_SendError = ttmfp_txer
			move.l    (a3)+,(a0)+				; mfpTT_SendBufferEmpty = ttmfp_txrint
			move.l    (a3)+,(a0)+				; mfpTT_ReceiveError = ttmfp_rxerr
			move.l    (a3)+,(a0)+				; mfpTT_ReceiveBufferFull = ttmfp_rcvrint
			ori.b     #$1e,(iera_TT).w
			ori.b     #$1e,(imra_TT).w
ENDIF

genrts:     rts

lbmove:     move.b    (a1)+,(a0)+
            dbra      d0,lbmove
            rts

kinit:      DC.L      iobuf_keyb                ; ibufptr
            DC.W      $0100                     ; ibufsiz
            DC.W      $0000                     ; ibufhead
            DC.W      $0000                     ; ibuftail
            DC.W      $0040                     ; ibuflow
            DC.W      $00c0                     ; ibufhigh
minit:      DC.L      iobuf_midi                ; ibufptr
            DC.W      $0080                     ; ibufsiz
            DC.W      $0000                     ; ibufhead
            DC.W      $0000                     ; ibuftail
            DC.W      $0020                     ; ibuflow
            DC.W      $0060                     ; ibufhigh
rs232init:  DC.L      iobuf_rs232i              ; ibufptr
            DC.W      $0100                     ; ibufsiz
            DC.W      $0000                     ; ibufhead
            DC.W      $0000                     ; ibuftail
            DC.W      $0080                     ; ibuflow
            DC.W      $00c0                     ; ibufhigh
            DC.L      iobuf_rs232o              ; obufptr
            DC.W      $0100                     ; obufsiz
            DC.W      $0000                     ; obufhead
            DC.W      $0000                     ; obuftail
            DC.W      $0080                     ; obuflow
            DC.W      $00c0                     ; obufhigh
            DC.B      $00                       ; rsrbyte
            DC.B      $00                       ; tsrbyte
            DC.B      $00                       ; rxoff
            DC.B      $00                       ; txoff
            DC.B      $01                       ; rsmode_xon
            DC.B      $00                       ; rsmode_filler
            DC.W      $01ff

* array of exception vector addresses for the above interrupts, including
* dummy vectors that point to "rte's".

mfpvectr:   DC.L      txerror
            DC.L      txrint
            DC.L      rxerror
            DC.L      rcvrint

***********************************************************************************
*                                                                                 *
*                routine to set up a timer                                        *
*                                                                                 *
*                algorithm to init a timer                                        *
*                                                                                 *
*                1.  determine which timer and set d0.b = to timer's index value  *
*                    as shown below;                                              *
*                2.  disable the associated interrupt;                            *
*                3.  disable the timer itself via it's timer control register;    *
*                4.  initialize the timer's data register                         *
*                5.  repeat step #4 until the data register's contents are        *
*                    verified, per the errata sheet to the 68901 description;     *
*                6.  turn on the timer by using the value that you previously     *
*                    stored in d1;                                                *
*                                                                                 *
*                note:    the interrupt vector for the associated timer           *
*                         is not set in this routine, so it is the user's         *
*                         responsiblity to set it if so desired!                  *
*                                                                                 *
*                                                                                 *
*                registers used:          d0-d3/a0-a3                             *
*                registers saved:         d0-d3/a0-a3                             *
*                entry:                                                           *
*                        d0.l - timer to be set                                   *
*                                0 - timer a                                      *
*                                1 - timer b                                      *
*                                2 - timer c                                      *
*                                3 - timer d                                      *
*                        d1.b - timer's new control setting                       *
*                        d2.b - timer's data register data                        *
*                                                                                 *
*                exit:   no values to pass                                        *
*                                                                                 *
*                        d3   - used and abused by call to mskreg routine         *
*                        a0.l - set to mfp register base                          *
*                        a1.l - temporary location for a3                         *
*                        a2.l - used to pass table address to mskreg routine      *
*                        a3.l - used to pass table address to mskreg routine      *
*                                                                                 *
***********************************************************************************
settimer:   movem.l   d0-d4/a0-a3,-(sp)         ; save all registers to be messed with!!
            movea.w   #gpip,a0                  ; set mfp chip address pointer

            movea.l   #imrt,a3                  ; mask off the timer's interrupt maskable bit
            movea.l   #imrmt,a2
            bsr.s     mskreg

            movea.l   #iert,a3                  ; mask off the timer's interrupt enable bit
            movea.l   #imrmt,a2
            bsr.s     mskreg

            movea.l   #iprt,a3                  ; mask off the timer's interrupt pending bit
            movea.l   #imrmt,a2
            bsr.s     mskreg

            movea.l   #isrt,a3                  ; mask off the timer's interrupt inservice bit
            movea.l   #imrmt,a2
            bsr.s     mskreg

            movea.l   #tcrtab,a3                ; mask off the timer's control bits
            movea.l   #tcrmsk,a2
            bsr.s     mskreg

            exg       a3,a1                     ; save address pointer for restoring control

            lea       (tdrtab).l,a3             ; initialize the timer data register
            moveq     #0,d3                     ; to prevent false effective address generation
            move.b    (a3,d0.w),d3
verify:     move.b    d2,(a0,d3.w)
            cmp.b     (a0,d3.w),d2
            bne.s     verify

            exg       a3,a1                     ; grab that register address back
            or.b      d1,(a3)                   ; mask the timer control register value

            movem.l   (sp)+,d0-d4/a0-a3         ; restore all registers that were saved
            rts

***********************************************************************************
*                generalize mask register bit(s) routine                          *
*                                                                                 *
*       entry                                                                     *
*       static  d0 - contains the timer #                                         *
*               d3 - used and abused                                              *
*               d4 - used and abused                                              *
*       static  a0 - mfp register base                                            *
*               a3 - points to table of similar timer registers                   *
*       static  a2 - points to table of similar timer data registers              *
***********************************************************************************

mskreg:     bsr.s     getmask
            move.b    (a2),d3                   ; grab mask now
            and.b     d3,(a3)                   ; and have masked off the desired bit(s)
            rts

getmask:    moveq     #0,d3                     ; to prevent false effective address generation
            adda.w    d0,a3                     ; have got pointer to mfp register now
            move.b    (a3),d3                   ; now have the address offset to mfp
            add.l     a0,d3
            movea.l   d3,a3                     ; now have address pointing to desired mfp reg.
* now we get the mask to turn off interrupt
            adda.w    d0,a2                     ; have got pointer to mask now
            rts


iert:       DC.B      $06,$06,$08,$08
iprt:       DC.B      $0a,$0a,$0c,$0c
isrt:       DC.B      $0e,$0e,$10,$10
imrt:       DC.B      $12,$12,$14,$14
imrmt:      DC.B      $df,$fe,$df,$ef
tcrtab:     DC.B      $18,$1a,$1c,$1c
tcrmsk:     DC.B      $00,$00,$8f,$f8
tdrtab:     DC.B      $1e,$20,$22,$24

***********************************************************************************
*                initialize mfp interrupt via GEMDOS                              *
*                                                                                 *
*                entry                                                            *
*                                                                                 *
*                void    mfpint(numint,intvec)                                    *
*                word    numint                                                   *
*                long    intvec                                                   *
*                                                                                 *
***********************************************************************************
mfpint:     move.w    4(sp),d0
            movea.l   6(sp),a2
            andi.l    #$f,d0                    ; to ensure masking of 0-$f

***********************************************************************************
*                                                                                 *
*                routine to init an mfp associated interrupt vector               *
*                                                                                 *
*                algorithm                                                        *
*                                                                                 *
*                1.  block the interrupt via it's mask bit;                       *
*                2.  disable the interrupt's enable and pending bits;             *
*                3.  check the interrupt's in-service register and loop till      *
*                    clear;                                                       *
*                4.  init the interrupt's associated vector;                      *
*                5.  set the interrupt's enable bit;                              *
*                6.  set the interrupt's mask bit;                                *
*                                                                                 *
*                entry                                                            *
*                         d0 - contains interrupt # to aff                        *
*                         a2 - contains new vector address                        *
***********************************************************************************

initint:    movem.l   d0-d2/a0-a2,-(sp)         ; save affected registers
            bsr.s     disint                    ; disable the interrupts
            move.l    d0,d2                     ; get a copy so as to determine where to...
            asl.w     #2,d2                     ; place the a2 address into the int. vector
            addi.l    #$100,d2                  ; interrupt vector addr = (4 * int) + $000100
            movea.l   d2,a1                     ; transfer the calculated address to a register
            move.l    a2,(a1)                   ; ...that can act upon it thus<--vector init'ed
            bsr.s     enabint                   ; enable interrupts
            movem.l   (sp)+,d0-d2/a0-a2         ; restore affected registers
            rts

*************************************************************************
*                disable an mfp interrupt via GEMDOS                    *
*                                                                       *
*                entry                                                  *
*                                                                       *
*                void    jdisint(numint)                                *
*                word    numint                                         *
*                                                                       *
*************************************************************************

jdisint:    move.w    4(sp),d0
            andi.l    #$f,d0                    ; to ensure masking of 0-$f

*************************************************************************
*                interrupt disable routine                              *
*************************************************************************

disint:     movem.l   d0-d1/a0-a1,-(sp)         ; save affected registers
            lea       (gpip).w,a0               ; set mfp chip address pointer
            lea       $12(a0),a1                ; set a1 to the mskoff routine
            bsr.s     bselect                   ; generate the appropriate bit to clear
            bclr      d1,(a1)                   ; and clear the bit...
            lea       6(a0),a1                  ; set a1 for another mskoff call
            bsr.s     bselect
            bclr      d1,(a1)                   ; and clear the bit...
            lea       $e(a0),a1                 ; now set up to check for interrupts in progress
            bsr.s     bselect                   ; get proper a/b version...
            move.b    #$fe,d0
            rol.b     d1,d0
            move.b    d0,(a1)
            movem.l   (sp)+,d0-d1/a0-a1         ; restore affected registers
            rts

*************************************************************************
*                enable/re-enabled an mfp interrupt via GEMDOS          *
*                                                                       *
*                entry                                                  *
*                                                                       *
*                void    jenabint(numint)                               *
*                word    numint                                         *
*                                                                       *
*************************************************************************

jenabint:   move.w    4(sp),d0
            andi.l    #$f,d0                    ; to ensure masking of 0-$f

*************************************************************************
*                enable interrupt routine                               *
*************************************************************************

enabint:    movem.l   d0-d1/a0-a1,-(sp)         ; save affected registers
            lea       (gpip).w,a0               ; set mfp chip address pointer
            lea       6(a0),a1                  ; set up to enable the interrupt enable bit
            bsr.s     bselect
            bset      d1,(a1)                   ; and set the bit...
            lea       $12(a0),a1                ; set up to enable the interrupt mask bit
            bsr.s     bselect
            bset      d1,(a1)                   ; and set the bit...
            movem.l   (sp)+,d0-d1/a0-a1         ; restore affected registers
            rts


*************************************************************************
*                                                                       *
*       the following routine generates the appropriate bset/bclr #     *
*       for the interrupt # specified in d0.    valid interrupt #'s are *
*       0 --> 15 as shown in the 68901 chip specification.  It also     *
*       selects between the ixra and the ixrb version of the register   *
*       as is appropriate.                                              *
*                                                                       *
*       entry   d0 - contains the interrupt number                      *
*               a1 - contains the pointer to the "ixra" version of      *
*                    the interrupt byte to mask                         *
*       exit            d0 - same as upon entry                         *
*                       d1 - contains the number of the bit             *
*************************************************************************
bselect:    move.b    d0,d1                     ; copy d0 to d1 for scratch work
            cmpi.b    #8,d1                     ; see if desired int # >= 8...
            blt.s     skip0                     ; ...and branch if it ain't...
            subq.w    #8,d1                     ; adjust for using ixrb instead
            rts

skip0:      addq.l    #2,a1                     ; adjust for using ixrb instead
            rts


*************************************************************************
*                                                                       *
*               receiver buffer full interrupt routine                  *
*                                                                       *
*               grabs data from the rs-232 receiver port                *
*                                                                       *
*************************************************************************
rcvrint:    movem.l   d0-d1/a0-a2,-(sp)         ; save affected registers
            lea       (rbufrec).w,a0            ; point to current output buffer record
            lea       (gpip).w,a2               ; set mfp chip address pointer
_rcvrint:   move.b    $2a(a2),$1c(a0)           ; do the required rsr read before the udr read!
            move.b    $2e(a2),d0                ; get incoming data byte

            btst      #0,$20(a0)                ; is the rs232 mode xon/xoff?
            beq.s     ri3                       ; no... so process normally

            cmpi.b    #$13,d0                   ; is the data an "xoff" signal?
            bne.s     ri2                       ; no...now check for xon
            move.b    #$ff,$1f(a0)              ; set to halted transmission to host
            bra.s     ri8

ri2:        cmpi.b    #$11,d0                   ; is the data an "xon" signal?
            bne.s     ri3                       ; neither xon/xoff value, must be normal data...
            clr.b     $1f(a0)                   ; set to normal transmission status
            bra.s     ri7                       ; abnormal exit condition!!

ri3:        move.w    8(a0),d1                  ; get current tail pointer offset
            bsr       wrapin                    ; do wrap of input pointer if needed
            cmp.w     6(a0),d1                  ; head=tail?
            beq.s     ri8                       ; yes...exit...
            bsr       rs232put                  ; store data in ring buffer

* now check for highwater mark triggering of flow-control
            tst.b     $20(a0)                   ; flow control active?
            beq.s     ri8                       ; no...exit...
            move.w    8(a0),d0                  ; current tail pointer
            sub.w     6(a0),d0                  ; - current head
            bpl.s     ri4                       ; wrap around?
            add.w     4(a0),d0                  ; + size to correct the wrap around
ri4:        cmp.w     $c(a0),d0                 ; high-water mark not reached?
            blt.s     ri8                       ; correct...exit...
            tst.b     $1e(a0)                   ; high-water flag already set?
            bne.s     ri8                       ; yes...exit...
            move.b    #$ff,$1e(a0)              ; set high-water flag
            btst      #0,$20(a0)                ; are we using xon/xoff flow control?
            bne.s     ri6                       ; yes...
            bsr       rtsoff                    ; we're ready now for more data...yum! yum!
            bra.s     ri8
ri6:        move.b    #$13,$21(a0)              ; xoff
ri7:        tst.b     $2c(a2)                   ; transmitter status?
            bpl.s     ri8                       ; not ready
            bsr       wr_rs232
ri8:        move.b    #$ef,$e(a2)
            movem.l   (sp)+,d0-d1/a0-a2         ; restore affected registers
            rte

*************************************************************************
*                                                                       *
*               transmit buffer empty interrupt routine                 *
*                                                                       *
*************************************************************************
txrint:     movem.l   d0-d1/a0-a2,-(sp)         ; save affected registers
            lea       (gpip).w,a2               ; set mfp chip address pointer
            lea       (rbufrec).w,a0            ; point to current output buffer record
_txrint:    bsr       wr_rs232
            move.b    #$fb,$e(a2)               ; xmit buffer empty
            movem.l   (sp)+,d0-d1/a0-a2         ; restore affected registers
            rte

*************************************************************************
*                                                                       *
*               Clear-To-Send interrupt routine                         *
*                                                                       *
*************************************************************************
ctsint:     movem.l   d0-d1/a0-a2,-(sp)         ; save affected registers
            lea       (rbufrec).w,a0            ; point to current output buffer record
            lea       (gpip).w,a2               ; set mfp chip address pointer

            btst      #1,$20(a0)                ; are we using CTS/RTS flow control?
            beq.s     ctsexit                   ; no...
            btst      #2,(a2)                   ; CTS already active?
            bne.s     ctsoff                    ; yes...
            clr.b     $1f(a0)                   ; xon_flag = 0
            bset      #2,2(a2)                  ; CTS set
            tst.b     $2c(a2)                   ; transmitter status?
            bpl.s     ctsexit                   ; not empty...
            bsr.s     wr_rs232                  ; send a byte
            bra.s     ctsexit
ctsoff:     move.b    #$ff,$1f(a0)              ; xon_flag = -1
            bclr      #2,2(a2)
ctsexit:    move.b    #$fb,$10(a2)              ; clear CTS
            movem.l   (sp)+,d0-d1/a0-a2         ; restore affected registers
            rte


*************************************************************************
*               routines to handle tx or rx errors                      *
*************************************************************************
rxerror:    movem.l   d0/a0,-(sp)               ; save all registers
            lea       (gpip).w,a0               ; set mfp chip address pointer
_rxerror:   move.b    $2a(a0),(rbufrec+28).l    ; receiver status register
            move.b    $2e(a0),d0                ; dummy read of data register
            move.b    #$f7,$e(a0)
            movem.l   (sp)+,d0/a0               ; restore all registers
            rte

txerror:    move.l    a0,-(sp)
            lea       (gpip).w,a0               ; set mfp chip address pointer
_txerror:   move.b    $2c(a0),(rbufrec+29).l    ; transmitter status register
            move.b    #$fd,$e(a0)
            movea.l   (sp)+,a0                  ; restore all registers
            tst.b     (tsr).w
            rte


wr_rs232:   move.l    a0,-(sp)                  ; save all registers
            move.b    $21(a0),d0                ; send handshake byte pending?
            beq.s     wr_rs2322                 ; no...
            clr.b     $21(a0)                   ; clear handshake byte
            bra.s     wr_rs232o                 ; output the handshake byte
wr_rs2322:  move.b    $20(a0),d0                ; flow control active?
            and.b     $1f(a0),d0                ; and xon-flag
            bne.s     wr_rs2324                 ; yes...
            adda.w    #$e,a0                    ; switch to output ring buffer
            move.w    6(a0),d0                  ; head-index
            cmp.w     8(a0),d0                  ; = tail-index?
            beq.s     wr_rs2324                 ; buffer empty? => out
            bsr.s     rs232get                  ; get next character from the buffer
wr_rs232o:  tst.b     $2c(a2)                   ; transmit buffer empty?
            bpl.s     wr_rs232o                 ; no...wait...
            move.b    $2c(a2),(rbufrec+29).l    ; save transmitter status
            move.b    d0,$2e(a2)                ; output the byte
wr_rs2324:  movea.l   (sp)+,a0                  ; restore all registers
            rts

rs232put:   move.w    8(a0),d1                  ; tail-index
            bsr.s     wrapin
rs232putl:  cmp.w     6(a0),d1                  ; = head-index?
            beq.s     rs232putl                 ; buffer full...wait...
            movea.l   (a0),a1                   ; ptr to send buffer
            and.l     #$ffff,d1
            move.b    d0,(a1,d1.l)              ; byte into the send buffer
            move.w    d1,8(a0)                  ; update tail-index
            rts

rs232get:   move.w    6(a0),d1                  ; head-index
            cmp.w     8(a0),d1                  ; = tail-index?
            beq.s     rs232get                  ; empty? wait...
            bsr.s     wrapin
            movea.l   (a0),a1                   ; ptr to receive buffer
            moveq     #0,d0
            and.l     #$ffff,d1
            move.b    (a1,d1.l),d0              ; byte from the receive buffer
            move.w    d1,6(a0)                  ; update head-index
            rts

rtson:      lea       (psgsel).w,a1
            move      sr,-(sp)
            ori       #$700,sr
            move.b    #14,(a1)
            move.b    (a1),d1
            and.b     #$f7,d1                   ; clear bit 3 in port A
            move.b    d1,2(a1)
            move      (sp)+,sr
            rts

rtsoff:     lea       (psgsel).w,a1
            move      sr,-(sp)
            ori       #$700,sr
            move.b    #14,(a1)
            move.b    (a1),d1
            ori.b     #8,d1                     ; set bit 3 in port A
            move.b    d1,2(a1)
            move      (sp)+,sr
            rts

wrapin:     addq.w    #1,d1                     ; i=h+1
            cmp.w     4(a0),d1                  ; > i => current bufsiz?
            bcs.s     wi1                       ; no...
            moveq     #0,d1                     ; wrap pointer
wi1:        rts

iorec:      move.w    4(sp),d1                  ; device number
            beq.s     iorecaux                  ; rs232? =>
            asl.l     #2,d1
            move.l    devtab(pc,d1.w),d0
            rts

iorecaux:   move.l    (bsrecauxp).w,d0
            rts

devtab:     DC.L      rbufrec                   ; RS232
            DC.L      kbufrec                   ; IKBD
            DC.L      mbufrec                   ; MIDI

rsconf:     movea.l   (bsrsconfp).w,a0
            jmp       (a0)

auxrsconf:  lea       (rbufrec).w,a0            ; point to current output buffer record
            lea       (gpip).w,a2               ; set mfp chip address pointer
_auxrsconf:	moveq     #0,d0
            cmpi.w    #-2,4(sp)                 ; -2 = return last baudrate
            bne.s     auxrsconf1
            move.b    $22(a0),d0                ; last baudrate
            rts

auxrsconf1: ori       #$700,sr                  ; no interrupts for now
*
*      first we grab the old ucs,rsr,tsr,scr contents
*
            movep.l   $28(a2),d7
*
*      set flow control mode(s)
*
            move.w    6(sp),d0                  ; if -1 then don't change
            cmp.w     #3,d0
            bhi.s     auxc1                     ; no =>
            bne.s     auxrsconf2
            moveq     #1,d0                     ; set flag for handshake
auxrsconf2: cmp.b     $20(a0),d0                ; state unchanged? => continue
            beq.s     auxc1
            move.w    d0,-(sp)                  ; save new handshake state
            tst.b     $1f(a0)                   ; xon flag set?
            beq.s     auxrsconf3
            clr.b     $1f(a0)                   ; clear xon flag
            bsr       wr_rs232                  ; transmit xon to continue
auxrsconf3: tst.b     $1e(a0)                   ; RTS set?
            beq.s     auxrsconf4                ; no...
            bsr       auxinclrhw                ; reenable RTS
auxrsconf4: move.w    (sp)+,d0                  ; new handshake state
            move.b    d0,$20(a0)                ; set new handshake state
*
*      set timer baud rate
*
auxc1:      tst.w     4(sp)                     ; if -1 then don't change
            bmi.s     auxc2
            bclr      #0,$2a(a2)                ; disable receiver
            bclr      #0,$2c(a2)                ; disable transmitter
            move.w    4(sp),d1                  ; new baudrate
            move.b    d1,$22(a0)                ; store for later
            move.b    baudctrl(pc,d1.w),d0      ; get baudrate control register settings mask
            move.b    bauddata(pc,d1.w),d2      ; get baudrate data register value
            andi.b    #$70,$1c(a2)
            move.b    d2,$24(a2)                ; set tuner D to new baud rate
            or.b      d0,$1c(a2)
            bset      #0,$2a(a2)                ; enable receiver
            bset      #0,$2c(a2)                ; enable transmitter

*      set rs-232 registers

auxc2:      tst.w     8(sp)                     ; if -1 then don't change
            bmi.s     auxc3
            move.b    9(sp),$28(a2)             ; set ucr
auxc3:      tst.w     $a(sp)                    ; if -1 then don't change
            bmi.s     auxc4
            move.b    $b(sp),$2a(a2)            ; set rsr
auxc4:      tst.w     $c(sp)                    ; if -1 then don't change
            bmi.s     auxc5
            move.b    $d(sp),$2c(a2)            ; set tsr
auxc5:      tst.w     $e(sp)                    ; if -1 then don't change
            bmi.s     auxc6
            move.b    $f(sp),$26(a2)            ; set scr
auxc6:      move.l    d7,d0                     ; move old contents of rs-232 registers to d0.l
            rts

* baudrate table - control register setting

baudctrl:   DC.B      $01,$01,$01,$01,$01,$01,$01,$01
            DC.B      $01,$01,$01,$01,$01,$01,$02,$02

* baudrate table - data register setting

bauddata:   DC.B      $01,$02,$04,$05,$08,$0a,$0b,$10
            DC.B      $20,$40,$60,$80,$8f,$af,$40,$60

rs232devs:  DC.L      auxistat                  ; Dev 6: "Modem 1" - ST MFP
            DC.L      auxin
            DC.L      _auxostat
            DC.L      _auxout
            DC.L      auxrsconf
            DC.L      rbufrec

            DC.L      sccbistat                 ; Dev 7: "Modem 2" - SCC Channel B
            DC.L      sccbin
            DC.L      sccbostat
            DC.L      sccbout
            DC.L      sccbrsconf
            DC.L      sccbbufrec

IF TOS_ROM306
			DC.L      ttmfp_auxistat            ; Dev 8: "Seriell 1" - TT MFP
            DC.L      ttmfp_auxin
            DC.L      ttmfp__auxostat
            DC.L      ttmfp__auxout
            DC.L      ttmfp_auxrsconf
            DC.L      ttmfp_rbufrec
ENDIF

            DC.L      sccaistat                 ; Dev 8/9: "Seriell 2" - SCC Channel A
            DC.L      sccain
            DC.L      sccaostat
            DC.L      sccaout
            DC.L      sccarsconf
            DC.L      sccabufrec

_bconmap:   moveq     #0,d0
            move.w    4(sp),d1
            move.w    (bdevmappedAux).w,d0
            cmp.w     #-1,d1
            beq.s     _bconmap_rts
            move.l    #bdevtabptr,d0
            cmp.w     #-2,d1
            beq.s     _bconmap_rts
            moveq     #0,d0
            subq.w    #6,d1
            bmi.s     _bconmap_rts
            cmp.w     (bdevcount).w,d1
            bcc.s     _bconmap_rts
            move.w    (bdevmappedAux).w,d1
            subq.w    #6,d1
            asl.w     #3,d1
            move.w    d1,d2
            add.w     d1,d1
            add.w     d1,d2
            movea.l   (bdevtabptr).w,a0
            adda.w    d2,a0
            lea       (xconstat+4).w,a1
            move.l    (a1),(a0)+
            move.l    $20(a1),(a0)+
            move.l    $40(a1),(a0)+
            move.l    $60(a1),(a0)+
            move.l    (bsrsconfp).l,(a0)+
            move.l    (bsrecauxp).w,(a0)+
            move.w    4(sp),d1
            subq.w    #6,d1
            asl.w     #3,d1
            move.w    d1,d2
            add.w     d1,d1
            add.w     d1,d2
            movea.l   (bdevtabptr).w,a0
            adda.w    d2,a0
            move.l    (a0)+,(a1)
            move.l    (a0)+,$20(a1)
            move.l    (a0)+,$40(a1)
            move.l    (a0)+,$60(a1)
            move.l    (a0)+,(bsrsconfp).w
            move.l    (a0)+,(bsrecauxp).w
            move.w    (bdevmappedAux).w,d0
            move.w    4(sp),(bdevmappedAux).w
_bconmap_rts:rts


initdevstables:lea    (bdevtable).w,a0
            move.l    a0,(bdevtabptr).w
IF TOS_ROM306
			move.w    #4,(bdevcount).w
ELSE
            move.w    #3,(bdevcount).w
            moveq     #1,d0                     ; only MFP RS232 is available
            movea.l   sp,a1
            movea.l   (busexception).w,a2
            move.l    #initdevnoscc,(busexception).w
            tst.b     (scu_gp1).w
            move.w    #3,d0                     ; also 2 SCC RS232 are available
initdevnoscc:movea.l  a1,sp
            move.l    a2,(busexception).w
            move.w    d0,(bdevcount).w
ENDIF
            lea       rs232devs(pc),a1
IF TOS_ROM306
            move.w    #$17,d0					; 4 devices
ELSE
			move.w    #$11,d0					; 3 devices
ENDIF
initdev1:   move.l    (a1)+,(a0)+
            dbra      d0,initdev1
            move.w    #6,(bdevmappedAux).w      ; Device 6 is mapped into device 1 (Aux)
            lea       (rs232devs).l,a1          ; Map MFP RS232 to dev 1
            lea       (xconstat+4).w,a0
            move.l    (a1)+,(a0)                ; Fix tconstat for dev 1
            move.l    (a1)+,$20(a0)             ; Fix tconin for dev 1
            move.l    (a1)+,$40(a0)             ; Fix tcostat for dev 1
            move.l    (a1)+,$60(a0)             ; Fix tconout for dev 1
            move.l    (a1)+,(bsrsconfp).w
            move.l    (a1)+,(bsrecauxp).w
            rts

IF ROM_TOS306
ttmfp_auxistat:
			lea       (ttmfp_rbufrec).w,a0
			bra       _auxistat
ttmfp_auxin:lea       (ttmfp_rbufrec).w,a0
			lea       (gpip_TT).w,a2
			bra       _auxin
ttmfp__auxostat:
			lea       (ttmfp_rbufrec+14).w,a0
			bra       __auxostat
ttmfp__auxout:
			move.w    6(sp),d0
			lea       (ttmfp_rbufrec+14).w,a0
			bsr       rs232put
 			lea       (ttmfp_rbufrec).w,a0
			lea       (gpip_TT).w,a2
			bra       _auxoutwr
ttmfp_rcvrint:movem.l   d0-d1/a0-a2,-(sp)
			lea       (ttmfp_rbufrec).w,a0
			lea       (gpip_TT).w,a2
			bra       _rcvrint
ttmfp_txrint:movem.l   d0-d1/a0-a2,-(sp)
			lea       (gpip_TT).w,a2
			lea       (ttmfp_rbufrec).w,a0
			bra       _txrint
ttmfp_rxerr:movem.l   d0/a0,-(sp)
			lea       (gpip_TT).w,a0
			bra       _rxerror
ttmfp_txerr:move.l    a0,-(sp)
			lea       (gpip_TT).w,a0
			bra       _txerror
ttmfp_auxrsconf:lea       (ttmfp_rbufrec).w,a0
			lea       (gpip_TT).w,a2
			bra       _auxrsconf
ENDIF

sccinit:    move.b    (a1)+,d0                  ; SCC register number
            bmi.s     sccina                    ; end of the register list? => exit
            move.b    d0,(a0)                   ; select register
            move.b    (a1)+,(a0)                ; write into register
            bra.s     sccinit
sccina:     rts


* SCC initialization
scctbl:     DC.B      $04,$44                   ; x16, 1 stop, no parity
            DC.B      $01,$04
            DC.B      $02,$60
            DC.B      $03,$c0                   ; rx 8 bit, no crc, rx disabled
            DC.B      $05,$e2                   ; tx 8 bit, no crc, tx disabled, rts,dtr on
            DC.B      $06,$00
            DC.B      $07,$00
            DC.B      $09,$01
            DC.B      $0a,$00                   ; NRZ
            DC.B      $0b,$50                   ; tx & rx clk = brg, trxc=in
            DC.B      $0c,$18                   ; clock divisor for 9600 (pclk)
            DC.B      $0d,$00                   ; divisor high
            DC.B      $0e,$02                   ; BRG = PCLK
            DC.B      $0e,$03                   ; enable BRG
            DC.B      $03,$c1                   ; enable Rx, 8 bits
            DC.B      $05,$ea                   ; enable Tx, 8 bits, rts, dtr on
            DC.B      $0f,$20
            DC.B      $00,$10
            DC.B      $00,$10
            DC.B      $01,$17
            DC.B      $09,$09
            DC.B      $ff                       ; end of the register list
            DC.B      $00

* disconnect LAN from port a
nolan:      move      sr,d1
            ori       #$700,sr
            move.b    #14,(psgsel).w
            move.b    (psgsel).w,d0
            bset      #7,d0
            move.b    #14,(psgsel).w
            move.b    d0,(psgwr).w
            move      d1,sr
            rts

initscc:    movea.l   (busexception).w,a0
            movea.l   sp,a1
            move.l    #initscc_none,(busexception).w
            tst.b     (sccdat_a).w
            move.l    a0,(busexception).w
            lea       (SCC).w,a0
            lea       sccvect(pc),a1
            moveq     #15,d0
initscc2:   move.l    (a1)+,(a0)+
            dbra      d0,initscc2
IF ROM_TOS306
			clr.w     (scdmactl).w
ENDIF
            bsr.s     nolan
            lea       (sccabufrec).w,a0
            lea       sccbufinit(pc),a1
            moveq     #$25,d0
            bsr       lbmove
            lea       (sccbbufrec).w,a0
            lea       sccbufinit(pc),a1
            moveq     #$25,d0
            bsr       lbmove
            move.l    #$1434,(sccbbufrec).w
            move.l    #$1534,(sccbbufrec+14).w
            lea       (sccctl_a).w,a2
            move.b    #9,(a2)
            move.b    #$c0,(a2)
            move.w    #$104,d0                  ; Delay Mode, /4 Prescale, data = 4 (about 6us delay)
            jsr       (mfpdelay).l
            lea       (sccctl_a).w,a0
            lea       scctbl(pc),a1
            bsr       sccinit
            lea       (sccctl_b).w,a0
            lea       scctbl(pc),a1
            bsr       sccinit
            bset      #5,(vme_mask).w           ; Enable IRQ5 from VMEBUS/SCC
            rts

initscc_none:move.l   a0,(busexception).w
            movea.l   a1,sp
            rts


**************************************************************************
*                                                                        *
*                  83C30 SCC interrupt vectors                           *
*                                                                        *
**************************************************************************
sccvec_portbtrnbempty:movem.l d0-d1/a0-a2,-(sp)
            lea       (sccctl_b).w,a2
            lea       (sccbbufrec).w,a0
            bra       scc23
sccvec_portbextstchg:movem.l d0-d1/a0-a2,-(sp)
            lea       (sccctl_b).w,a2
            lea       (sccbbufrec).w,a0
            bra       scc24
sccvec_portbrecchr:movem.l d0-d1/a0-a2,-(sp)
            lea       (sccctl_b).w,a2
            lea       (sccbbufrec).w,a0
            bra.s     scc09
sccvec_portbschrrec:movem.l d0-d1/a0-a2,-(sp)
            lea       (sccctl_b).w,a2
            lea       (sccbbufrec).l,a0
            bra       scc16

sccvec_portatrnbempty:movem.l d0-d1/a0-a2,-(sp)
            lea       (sccctl_a).w,a2
            lea       (sccabufrec).w,a0
            bra       scc23
sccvec_portaextstchg:movem.l d0-d1/a0-a2,-(sp)
            lea       (sccctl_a).w,a2
            lea       (sccabufrec).w,a0
            bra       scc24
sccvec_portarecchr:movem.l d0-d1/a0-a2,-(sp)
            lea       (sccctl_a).w,a2
            lea       (sccabufrec).w,a0
            bra.s     scc09
sccvec_portaschrrec:movem.l d0-d1/a0-a2,-(sp)
            lea       (sccctl_a).w,a2
            lea       (sccabufrec).l,a0
            bra       scc16

scc09:      move.b    #8,(a2)                   ; select scc register #8
            move.b    (a2),d0                   ; read "Receive Data register"
            and.b     $23(a0),d0
            btst      #0,$20(a0)
            beq.s     scc11
            cmp.b     #$13,d0
            bne.s     scc10
            st        $1f(a0)
            bra.s     scc15
scc10:      cmp.b     #$11,d0
            bne.s     scc11
            tst.b     $1f(a0)
            sf        $1f(a0)
            bne.s     scc14
scc11:      move.w    8(a0),d1
            bsr       wrapin
            cmp.w     6(a0),d1
            beq.s     scc15
            bsr       rs232put
            tst.b     $20(a0)
            beq.s     scc15
            tst.b     $1e(a0)
            bne.s     scc15
            move.w    8(a0),d0
            sub.w     6(a0),d0
            bpl.s     scc12
            add.w     4(a0),d0
scc12:      cmp.w     $c(a0),d0
            blt.s     scc15
            st        $1e(a0)
            btst      #0,$20(a0)
            bne.s     scc13
            move.b    $1d(a0),d0
            bclr      #1,d0                     ; clear RTS bit
            move.b    d0,$1d(a0)
            move.b    #5,(a2)                   ; select scc register #5
            move.b    d0,(a2)                   ; write "Transmit Parameters and Controls" register
            bra.s     scc15
scc13:      move.b    #$13,$21(a0)
scc14:      move.b    #0,(a2)                   ; select scc register #0
            move.b    (a2),d0                   ; read register #0
            btst      #2,d0                     ; Tx Buffer Empty?
            beq.s     scc15
            bsr.s     wr_scc
scc15:      move.b    #0,(a2)                   ; select scc register #0
            move.b    #$38,(a2)                 ; write "Reset Highest IUS Command" - clear pending interrupt
            movem.l   (sp)+,d0-d1/a0-a2
            rte

scc16:      move.b    #1,(a2)                   ; select scc register #1
            move.b    (a2),d0                   ; read special receive condition status bits
            move.b    #8,(a2)                   ; select scc register #8
            move.b    (a2),d0                   ; read "Receive Data register"
            move.b    #0,(a2)                   ; select scc register #0
            move.b    #$30,(a2)                 ; write "Error Reset Command" - clear error condition
            bra.s     scc15

wr_scc:     move.l    a0,-(sp)
            tst.b     $21(a0)
            bne.s     scc19
IF ROM_STBOOK
            btst      #0,$20(a0)
            beq.s     scc18
            tst.b     $1f(a0)
            bne.s     scc22
scc18:      btst      #1,$20(a0)
            beq.s     scc19
            move.b    #0,(a2)                   ; select scc register #0
            move.b    (a2),d0                   ; read register #0
            btst      #5,d0                     ; CTS?
            beq.s     scc22
ELSE
			move.b    $20(a0),d0
			and.b     $1f(a0),d0
			bne.s     scc2
ENDIF
scc19:      move.b    #0,(a2)                   ; select scc register #0
            move.b    (a2),d0                   ; read register #0
            btst      #2,d0                     ; Tx Buffer Empty?
            beq.s     scc22
            move.b    $21(a0),d0
            beq.s     scc20
            clr.b     $21(a0)
            bra.s     scc21
scc20:      adda.w    #$e,a0
            move.w    6(a0),d0
            cmp.w     8(a0),d0
            beq.s     scc22
            bsr       rs232get
scc21:      move.b    #8,(a2)                   ; select scc register #8
            move.b    d0,(a2)                   ; write transmit buffer
scc22:      movea.l   (sp)+,a0
            rts

scc23:      move.b    #0,(a2)                   ; select scc register #0
            move.b    #$28,(a2)                 ; write "Reset Tx Int Pending"
            move.b    #0,(a2)                   ; select scc register #0
            move.b    #$38,(a2)                 ; write "Reset Highest IUS Command" - clear pending interrupt
            bsr.s     wr_scc
            movem.l   (sp)+,d0-d1/a0-a2
            rte

scc24:      btst      #1,$20(a0)
            beq.s     scc25
            move.b    #0,(a2)                   ; select scc register #0
            move.b    (a2),d0                   ; read register #0
            btst      #5,d0                     ; CTS?
            seq       $1f(a0)
            beq.s     scc25
IF ROM_STBOOK
            bsr       wr_scc
ELSE
			bsr.s     wr_scc
ENDIF
scc25:      move.b    #0,(a2)                   ; select scc register #0
            move.b    #$10,(a2)                 ; write "Reset Ext/Status Interrupts"
            move.b    #0,(a2)                   ; select scc register #0
            move.b    #$38,(a2)                 ; write "Reset Highest IUS Command" - clear pending interrupt
            movem.l   (sp)+,d0-d1/a0-a2
            rte


**************************************************************************
*                                                                        *
*                  scc BIOS callbacks                                    *
*                                                                        *
**************************************************************************
sccbistat:  lea       (sccbbufrec).w,a0
            lea       (sccctl_b).w,a2
            bra.s     sccistat
sccbin:     lea       (sccbbufrec).w,a0
            lea       (sccctl_b).w,a2
            bra.s     sccin
sccbostat:  lea       (sccbbufrec).w,a0
            lea       (sccctl_b).w,a2
            bra.s     _sccostat
sccbout:    lea       (sccbbufrec).w,a0
            lea       (sccctl_b).w,a2
            bra       _sccout
sccaistat:  lea       (sccabufrec).w,a0
            lea       (sccctl_a).w,a2
            bra.s     sccistat
sccain:     lea       (sccabufrec).w,a0
            lea       (sccctl_a).w,a2
            bra.s     sccin
sccaostat:  lea       (sccabufrec).w,a0
            lea       (sccctl_a).w,a2
            bra.s     _sccostat
sccaout:    lea       (sccabufrec).w,a0
            lea       (sccctl_a).w,a2
            bra.s     _sccout

**************************************************************************
*                                                                        *
*                  scc port input status routine                         *
*                                                                        *
**************************************************************************
sccistat:   moveq     #0,d0                     ; set result to false
            lea       6(a0),a1                  ; head
            lea       8(a0),a0                  ; tail
            cmpm.w    (a0)+,(a1)+               ; atomic buffer empty test
            beq.s     sccist1
            moveq     #-1,d0                    ; set result to true
sccist1:    rts


**************************************************************************
*                                                                        *
*                  scc port output status routine                        *
*                                                                        *
**************************************************************************
_sccostat:  move.w    8(a0),d1                  ; tail
            bsr       wrapin
            cmp.w     6(a0),d1                  ; head
            beq.s     _sccost1
            moveq     #-1,d0                    ; set result to true
            rts

_sccost1:   moveq     #0,d0                     ; set result to false
            rts


**************************************************************************
*                                                                        *
*                  scc input routine                                     *
*                                                                        *
**************************************************************************
sccin:      bsr       rs232get
            move.w    d0,-(sp)
            tst.b     $20(a0)                   ; flow control active?
            beq.s     sccinrts                  ; (no)
            tst.b     $1e(a0)                   ; high-water flag already set?
            beq.s     sccinrts                  ; no...exit...
            move.w    8(a0),d0                  ; tail
            sub.w     6(a0),d0                  ; - head
            bpl.s     sccin2                    ; underflow?
            add.w     4(a0),d0                  ; + size
sccin2:     cmp.w     $a(a0),d0
            bgt.s     sccinrts
            bsr.s     sccinclrhw
sccinrts:   move.w    (sp)+,d0
            rts

sccinclrhw: clr.b     $1e(a0)                   ; clear high-water flag
            btst      #0,$20(a0)                ; is the rs232 mode xon/xoff?
            beq.s     sccinclrhw2
            move.b    #$11,$21(a0)              ; "xon"
            bra.s     _sccoutwr
sccinclrhw2:move.b    $1d(a0),d0
            bset      #1,d0                     ; set RTS
            move.b    d0,$1d(a0)
            move.b    #5,(a2)                   ; select scc register #5
            move.b    d0,(a2)                   ; write "Transmit Parameters and Controls" register
            rts

**************************************************************************
*                                                                        *
*                  scc output routine                                    *
*                                                                        *
**************************************************************************
_sccout:    move.w    6(sp),d0                  ; get data
            adda.w    #$e,a0
            bsr       rs232put                  ; exit via rs-232 output routine
            suba.w    #$e,a0
_sccoutwr:  move.b    #0,(a2)                   ; select scc register #0
            move.b    (a2),d0                   ; read register #0
            btst      #2,d0                     ; Tx Buffer Empty?
            beq.s     _sccoutrts                ; not ready
            move      sr,-(sp)
            ori       #$700,sr
            bsr       wr_scc
            move      (sp)+,sr
_sccoutrts: rts

**************************************************************************
*                                                                        *
*                  scc rsconf() routine                                  *
*                                                                        *
**************************************************************************
sccarsconf: lea       (sccabufrec).w,a0         ; point to current output buffer record
            lea       (sccctl_a).w,a2
            bra.s     sccrsconf
sccbrsconf: lea       (sccbbufrec).w,a0         ; point to current output buffer record
            lea       (sccctl_b).w,a2

sccrsconf:  moveq     #0,d0
            cmpi.w    #-2,4(sp)                 ; -2 = return last baudrate
            bne.s     sccrsconf2
            move.b    $22(a0),d0                ; last baudrate
            rts

sccrsconf2: ori       #$700,sr                  ; no interrupts for now
*
*      first we grab the old ucs,rsr,tsr,scr contents
*
            moveq     #0,d7
            move.b    $1c(a0),d7
            asl.w     #8,d7
            swap      d7
            move.b    $1d(a0),d7
            lsr.b     #1,d7
            and.b     #4,d7
            asl.w     #8,d7
*
*      set flow control mode(s)
*
            move.w    6(sp),d0                  ; if -1 then don't change
            cmp.w     #3,d0
            bhi.s     sccrsconf6
            bne.s     sccrsconf3
            moveq     #1,d0                     ; set flag for handshake
sccrsconf3: cmp.b     $20(a0),d0                ; state unchanged? => continue
            beq.s     sccrsconf6
            tst.b     $1f(a0)                   ; xon flag set?
            beq.s     sccrsconf4
            clr.b     $1f(a0)                   ; clear xon flag
            bsr       wr_scc
sccrsconf4: tst.b     $1e(a0)                   ; RTS set?
            beq.s     sccrsconf5                ; no...
            move.w    d0,-(sp)                  ; save new handshake state
            bsr       sccinclrhw                ; reenable RTS
            move.w    (sp)+,d0                  ; new handshake state
sccrsconf5: move.b    d0,$20(a0)                ; set new handshake state
*
*      set timer baud rate
*
sccrsconf6: move.w    4(sp),d0                  ; new baudrate
            cmp.w     #$f,d0                    ; if -1 (out of range 0..15) then don't change
            bhi.s     sccrsconf7
            move.b    d0,$22(a0)                ; new baudrate
            asl.w     #1,d0
            lea       (sccbaudtab).l,a1
            move.w    (a1,d0.w),d0
            move.b    #$c,(a2)                  ; select scc register #12
            move.b    d0,(a2)                   ; write "Lower Byte of Baud Rate Generator Time Constant" register
            lsr.w     #8,d0
            move.b    #$d,(a2)                  ; select scc register #13
            move.b    d0,(a2)                   ; write "Upper Byte of Baud Rate Generator Time Constant" register
*      set rs-232 registers

sccrsconf7: move.w    8(sp),d0                  ; if -1 then don't change
            bmi.s     sccrsconf12
            move.b    d0,$1c(a0)
            move.b    d0,d1
            and.b     #$60,d1
            lsr.b     #5,d1
            moveq     #$ff,d2
            lsr.b     d1,d2
            move.b    d2,$23(a0)
            move.b    d0,d1
            and.b     #$60,d1
            beq.s     sccrsconf8
            cmp.b     #$60,d1
            bne.s     sccrsconf9
sccrsconf8: eori.b    #$60,d1
sccrsconf9: move.b    $1d(a0),d2
            and.b     #$9f,d2
            or.b      d1,d2
            move.b    d2,$1d(a0)
            move.b    #5,(a2)                   ; select scc register #5
            move.b    d2,(a2)                   ; write "Transmit Parameters and Controls" register
            asl.b     #1,d1
            or.b      #1,d1                     ; "Rx Enable"
            move.b    #3,(a2)                   ; select scc register #3
            move.b    d1,(a2)                   ; write "Receive Parameters and Control" register
            move.b    d0,d1
            and.b     #$1e,d1
            lsr.b     #1,d1                     ; mask stop bits (bit 2..3) and parity (bit 0..1)
            bclr      #1,d1
            sne       d2
            bclr      #0,d1                     ; disable parity
            bne.s     sccrsconf10
            bclr      #1,d2                     ; "odd parity"
            bra.s     sccrsconf11
sccrsconf10:bset      #1,d2                     ; "even parity"
sccrsconf11:and.b     #3,d2                     ; mask parity bits
            or.b      d2,d1
            or.b      #$40,d1                   ; "X16 Clock Mode"
            move.b    #4,(a2)                   ; select scc register #4
            move.b    d1,(a2)                   ; write "Transmit/Receive Mis- cellaneous Parameters and Modes" register

sccrsconf12:
IF ROM_TOS306
			move.w    $a(sp),d0                 ; if -1 then don't change
ELSE
			move.w    $c(sp),d0                 ; if -1 then don't change
ENDIF
            bmi.s     sccrsconf14
            btst      #3,d0
            beq.s     sccrsconf13
            bset      #4,$1d(a0)                ; set "Tx Enable"
            bne.s     sccrsconf14
            move.b    #5,(a2)                   ; select scc register #5
            move.b    $1d(a0),(a2)              ; write "Transmit Parameters and Controls" register
            bra.s     sccrsconf14
sccrsconf13:bclr      #4,$1d(a0)                ; clear "Tx Enable"
            beq.s     sccrsconf14
            move.b    #5,(a2)                   ; select scc register #5
            move.b    $1d(a0),(a2)              ; write "Transmit Parameters and Controls" register
sccrsconf14:move.l    d7,d0                     ; move old contents of rs-232 registers to d0.l
            rts

sccbaudtab: DC.W      $000b,$0018,$0032,$0044
            DC.W      $0067,$007c,$008a,$00d0
            DC.W      $01a1,$0345,$04e8,$068c
            DC.W      $074d,$08ee,$0d1a,$13a8
sccvect:    DC.L      sccvec_portbtrnbempty
            DC.L      $000000
            DC.L      sccvec_portbextstchg
            DC.L      $000000
            DC.L      sccvec_portbrecchr
            DC.L      $000000
            DC.L      sccvec_portbrecchr
            DC.L      $000000
            DC.L      sccvec_portatrnbempty
            DC.L      $000000
            DC.L      sccvec_portaextstchg
            DC.L      $000000
            DC.L      sccvec_portarecchr
            DC.L      $000000
            DC.L      sccvec_portarecchr
            DC.L      $000000
sccbufinit: DC.L      $001210                   ; ibufptr
            DC.W      $0100                     ; ibufsiz
            DC.W      $0000                     ; ibufhead
            DC.W      $0000                     ; ibuftail
            DC.W      $0080                     ; ibuflow
            DC.W      $00c0                     ; ibufhigh
            DC.L      $001310                   ; obufptr
            DC.W      $0100                     ; obufsiz
            DC.W      $0000                     ; obufhead
            DC.W      $0000                     ; obuftail
            DC.W      $0080                     ; obuflow
            DC.W      $00c0                     ; obufhigh
            DC.B      $08                       ; rsrbyte
            DC.B      $ea                       ; tsrbyte
            DC.B      $00                       ; rxoff
            DC.B      $00                       ; txoff
            DC.B      $01                       ; rsmode_xon
            DC.B      $00                       ; rsmode_filler
            DC.W      $01ff

*************************************************************************
*       this code handles the midi/keyboard interrupt exception         *
*************************************************************************
midikey:    movem.l   d0-d3/a0-a3,-(sp)         ; save all registers
keymidi:    movea.l   (astatusmidip).w,a2
            jsr       (a2)
            movea.l   (astatuskeyp).w,a2
            jsr       (a2)
            btst      #4,(gpip).w               ; check for pending interrupt occurance
            beq.s     keymidi                   ; repeat this interrupt processing
            move.b    #$bf,(isrb).w             ; clear in-service bit
            movem.l   (sp)+,d0-d3/a0-a3         ; restore all registers
            rte                                 ; go back to what was happening!

astatusmidi:lea       (mbufrec).w,a0
            lea       (midictl).w,a1
            movea.l   (vmiderr).w,a2
            bra.s     astatus2
astatuskeyboard:lea   (kbufrec).w,a0
            lea       (keyctl).w,a1
            movea.l   (vkbderr).w,a2
astatus2:   move.b    (a1),d2                   ; grab device status
            btst      #7,d2                     ; make sure it was an interrupt request
            beq.s     aciaexit                  ; nope..it's empty
            btst      #0,d2                     ; see if receiver buffer is full
            beq.s     mk1                       ; nope..it's empty
            movem.l   d2/a0-a2,-(sp)
            bsr.s     arcvrint                  ; yes...get byte
            movem.l   (sp)+,d2/a0-a2
mk1:        andi.b    #$20,d2                   ; mask off bits already tested
            beq.s     aciaexit                  ; see if any other status bits are on
            move.b    2(a1),d0                  ; grab data byte from acia data register
            jmp       (a2)                      ; yes to branch to pre-inited error subroutine

aciaexit:   rts

*************************************************************************
*       acia receiver buffer full interrupt routine                     *
*************************************************************************

arcvrint:   move.b    2(a1),d0                  ; grab data byte from acia data register
            cmpa.l    #kbufrec,a0
            bne       midibyte                  ; don't treat midi acia data as anything other than as pure data...
            tst.b     (kstate).w
            bne.s     ML3
            cmpi.b    #$f6,d0
            bcc.s     itsnotakey                ; is it a ikbd header?
            move.l    (itsakeyp).w,-(sp)        ; jump into ikbd processing function
            rts

itsnotakey: subi.b    #$f6,d0                   ; generate true index into tables now
            andi.l    #$ff,d0                   ; clear high 3 bytes for indexing
            lea       (ikbdev).l,a3             ; point to ikbd device state codes
            move.b    (a3,d0.w),(kstate).w      ; set ikbd state
            lea       (ikbdlen).l,a3            ; point to ikbd device buffer length table
            move.b    (a3,d0.w),(kindex).w      ; set ikbd device index counter
            addi.w    #$f6,d0                   ; re-constitute original value
            cmpi.b    #$f8,d0                   ; mouse position record?
            blt.s     ML8                       ; no
            cmpi.b    #$fb,d0                   ; mouse position record?
            bgt.s     ML8                       ; no
            move.b    d0,(mousebuf).w           ; store mouse byte
            rts

ML8:        cmpi.b    #$fd,d0                   ; joystick record?
            blt.s     ML8b                      ; no
            move.b    d0,(joyrec).w             ; store joystick byte
ML8b:       rts

ikbdev:     DC.B      $01,$02,$03,$03,$03,$03,$04,$05
            DC.B      $06,$07
ikbdlen:    DC.B      $07,$05,$02,$02,$02,$02,$06,$02
            DC.B      $01,$01

ML3:        cmpi.b    #6,(kstate).w
            bcc       ML35                      ; a joystick 0/1 record byte, not both!
            lea       (ikbdparams).l,a2         ; point to ikbd subsystem parameters table
            moveq     #0,d2
            move.b    (kstate).w,d2             ; load to generate longword offset
            subq.b    #1,d2                     ; d2 = (kstate-1) * 12
            asl.w     #1,d2
            add.b     (kstate).w,d2
            subq.b    #1,d2
            asl.w     #2,d2
            movea.l   (a2,d2.w),a0              ; load in subsystem's record pointer
            movea.l   4(a2,d2.w),a1             ; load in subsystem's index base+record pointer
            movea.l   8(a2,d2.w),a2             ; load in subsystem's pointer variable that contains the pointer to the subsystem's interrupt routine
            movea.l   (a2),a2                   ; get the address of the interrupt routine
            moveq     #0,d2                     ; clear out 'd2' for address manipulation
            move.b    (kindex).w,d2
            suba.l    d2,a1
            move.b    d0,(a1)                   ; store byte into the subsystems buffer
            subq.b    #1,(kindex).w             ; decrement the number of bytes in the package
            tst.b     (kindex).w                ; package received?
            bne.s     ML1                       ; not yet...
ikserve:    move.l    a0,-(sp)                  ; stuff buffer pointer to stack
            jsr       (a2)                      ; go service the subsystem interrupt routine
            addq.w    #4,sp                     ; re-adjust stack
            clr.b     (kstate).w                ; reset ikbd state
ML1:        rts

ikbdparams: DC.L      statrec
            DC.L      amrec
            DC.L      statintvec

            DC.L      amrec
            DC.L      mousebuf
            DC.L      msintvec

            DC.L      mousebuf
            DC.L      clkrec
            DC.L      msintvec

            DC.L      clkrec
            DC.L      joyrec
            DC.L      clkintvec

            DC.L      joyrec
            DC.L      joyrec+2
            DC.L      joyintvec

ML35:       move.l    #joyrec+1,d1
            add.b     (kstate).w,d1             ; kstate reflects joy0 or joy1 state
            subq.b    #6,d1
            movea.l   d1,a2                     ; create index to joyrec table for record byte
            move.b    d0,(a2)
            movea.l   (joyintvec).w,a2          ; get user's joystick interrupt routine adr
            lea       (joyrec).w,a0             ; send along address of joystick data
            bra.s     ikserve

itsakey:    move.b    (kb_shift).w,d1           ; load in kbshift for manipulation
* check the special keys
            cmpi.b    #$2a,d0                   ; left shift?
            bne.s     ari2
            bset      #1,d1
            bra       ari10
ari2:       cmpi.b    #$aa,d0
            bne.s     ari3
            bclr      #1,d1
            bra       ari10
ari3:       cmpi.b    #$36,d0                   ; right shift?
            bne.s     ari4
            bset      #0,d1
            bra.s     ari10
ari4:       cmpi.b    #$b6,d0
            bne.s     ari5
            bclr      #0,d1
            bra.s     ari10
ari5:       cmpi.b    #$1d,d0                   ; CTRL
            bne.s     ari6
            bset      #2,d1
            bra.s     ari10
ari6:       cmpi.b    #$9d,d0
            bne.s     ari7
            bclr      #2,d1
            bra.s     ari10
ari7:       cmpi.b    #$38,d0                   ; ALT
            bne.s     ari8
            bset      #3,d1
            bra.s     ari10
ari8:       cmpi.b    #$b8,d0
            bne.s     ari9
            bclr      #3,d1

            tst.w     (altdigits).l             ; ascii code via numpad active?
            bmi.s     ari10                     ; no...
            move.b    d1,(kb_shift).w           ; store shift status
            move.l    a0,-(sp)
            moveq     #0,d1                     ; no scancode
            move.w    d1,d0                     ; clear ascii code reg
            move.b    (altdigits+1).l,d0        ; take entered ascii code
            move.w    #-1,(altdigits).l         ; reset alt-numpad ascii entering
            bra       conin25

ari9:       cmpi.b    #$3a,d0                   ; CAPS LOCK
            bne.s     ari11
            btst      #0,(conterm).w
            beq.s     ari9a                     ; no click please!
            movem.l   d0-d2/a0-a2,-(sp)
            movea.l   (kcl_hook).w,a0
            jsr       (a0)
            movem.l   (sp)+,d0-d2/a0-a2
ari9a:      bchg      #4,d1                     ; toggle CAPS LOCK state
ari10:      move.b    d1,(kb_shift).w           ; restore new kbshift value
            rts                                 ; ignore CAPS LOCK break


ari11:      btst      #7,d0                     ; is it a break code?
            bne.s     ari12                     ; no... a break code!
            move.b    d0,(keyrep).w             ; save for repeat purposes
            move.b    (cdelay1).l,(keydelay1).w
            move.b    (cdelay2).l,(keydelay2).w
            bra.s     ari16
ari12:      move.b    d0,d1
            bclr      #7,d1
            cmp.b     (keyrep).w,d1
            bne.s     ari15
            moveq     #0,d1
            move.b    d1,(keyrep).w
            move.b    d1,(keydelay1).w
            move.b    d1,(keydelay2).w

ari15:      cmpi.b    #$c7,d0                   ; is it a "home" break-code?
            beq.s     ari18a                    ; yes...allow it to pass
            cmpi.b    #$d2,d0                   ; is it an "insert" break-code?
            bne       ari14                     ; no...regular break junk...
ari18a:     btst      #3,(kb_shift).w           ; early "ALT" test to prevent double "nulls"
            beq       ari14                     ; no ALT...so exit now...
ari16:      btst      #0,(conterm).w
            beq.s     ari16a                    ; no click please!
            movem.l   d0-d2/a0-a2,-(sp)
            movea.l   (kcl_hook).w,a1
            jsr       (a1)
            movem.l   (sp)+,d0-d2/a0-a2
ari16a:     move.l    a0,-(sp)                  ; store kbufrec pointer

            moveq     #0,d1
            move.b    d0,d1

default: use regular ascii table:movea.l (skeytran).w,a0
            andi.w    #$7f,d0
            btst      #4,(kb_shift).w           ; caps-lock pressed?
            beq.s     conin21
            movea.l   (skeycl).w,a0             ; use caps-lock table
conin21:    btst      #0,(kb_shift).w
            bne.s     conin22
            btst      #1,(kb_shift).w
            beq.s     conin23
conin22:    cmpi.b    #$3b,d0                   ; see if a possible function key
            bcs.s     conin22a                  ; unsigned less than lowest function scancode
            cmpi.b    #$44,d0                   ; see if a possible function key
            bhi.s     conin22a                  ; unsigned greater than highest function scan
            addi.w    #$19,d1                   ; add to change to GSX standard
            moveq     #0,d0                     ; change to GSX standard
            bra       conin25

conin22a:   movea.l   (skeyshif).w,a0           ; use the shift table
conin23:    move.b    (a0,d0.w),d0
            btst      #2,(kb_shift).w           ; is the control key down?
            beq.s     conin24a
            cmpi.b    #$d,d0                    ; is it a carriage return?
            bne.s     conin23a
            moveq     #$a,d0                    ; change to a linefeed according to GSX spec...
            beq.s     conin24x
conin23a:   cmpi.b    #$47,d1                   ; convert CONTROL-home to gsx standard
            bne.s     conin23b
            addi.w    #$30,d1                   ; by adding #$30...
            bra       conin25
conin23b:   cmpi.b    #$4b,d1                   ; convert CONTROL-left arrow to gsx standard
            bne.s     conin23c
            moveq     #$73,d1                   ; change according to gsx spec
            moveq     #0,d0
            bra       conin25
conin23c:   cmpi.b    #$4d,d1                   ; convert CONTROL-right arrow to gsx standard
            bne.s     conin24x
            moveq     #$74,d1                   ; change according to gsx spec
            moveq     #0,d0
            bra       conin25

conin24x:   cmpi.b    #$32,d0                   ; Control-M?
            bne.s     conin240
            moveq     #0,d0                     ; ascii code 0
            bra       conin25
conin240:   cmpi.b    #$36,d0                   ; Control-Shift-Right?
            bne.s     conin241
            moveq     #$1e,d0                   ; ascii code RS
            bra       conin25
conin241:   cmpi.b    #$2d,d0                   ; Control-X?
            bne.s     conin24a
            moveq     #$1f,d0                   ; ascii code US
            bra       conin25

* this routine allows entering any ascii code by using the numpad
* and holding down ALT during it.
conin24a:   btst      #3,(kb_shift).w           ; is the alt key down?
            beq       conin2x
            cmp.b     #$67,d1                   ; key 0 ... 9 on the number pad?
            bcs.s     outside                   ; no...
            cmp.b     #$70,d1
            bhi.s     outside                   ; no...
            move.w    (altdigits).l,d0          ; already entering an ascii code?
            bpl.s     conin24b                  ; yes
            moveq     #0,d0                     ; start with 0
conin24b:   mulu.w    #10,d0                    ; previous digit * 10
            ext.w     d1
            move.b    (a0,d1.w),d1              ; ascii code of current digit
            sub.b     #'0',d1                   ; convert into number
            add.b     d1,d0                     ; plus old number
            move.w    d0,(altdigits).l          ; store as new number code
            movea.l   (sp)+,a0
            rts

outside:    cmpi.b    #$62,d1                   ; is it an "alt help" signal to dump the screen?
            bne.s     alt15a                    ; no...
            addq.w    #1,(_prtcnt).w            ; yes...switch the signal flag on!...
            movea.l   (sp)+,a0                  ; restore kbufrec pointer
            bra       ari14                     ; ...and exit
*
*       check the alt-insert/alt-home key make/break combinations, first
*
alt15a:     lea       (mousekey1).l,a2          ; get pointer to first alt. mouse scancode table
            moveq     #3,d2                     ; create countdown
mkloop1:    cmp.b     (a2,d2.w),d1              ; is table's scancode value = current value?
            beq       keymaus1                  ; yes...go preprocess it...
            dbra      d2,mkloop1                ; no...loop back to check next table value

            cmpi.b    #$48,d1                   ; is it an up arrow?
            bne.s     alt11
            move.b    #0,d1                     ; x value for up arrow
            move.b    #$f8,d2                   ; y value for up arrow
            move.b    (kb_shift).w,d0           ; grab current setting
            andi.b    #3,d0                     ; KBRSH+KBLSH bits
            beq       keymaus
            move.b    #$ff,d2                   ; y value for up arrow
            bra       keymaus
alt11:      cmpi.b    #$4b,d1                   ; is it an left arrow?
            bne.s     alt12
            move.b    #0,d2                     ; x value for left arrow
            move.b    #$f8,d1                   ; y value for left arrow
            move.b    (kb_shift).w,d0           ; grab current setting
            andi.b    #3,d0                     ; KBRSH+KBLSH bits
            beq       keymaus
            move.b    #$ff,d1                   ; x value for left arrow
            bra       keymaus
alt12:      cmpi.b    #$4d,d1                   ; is it an right arrow?
            bne.s     alt13
            move.b    #8,d1                     ; x value for right arrow
            move.b    #0,d2                     ; y value for right arrow
            move.b    (kb_shift).w,d0           ; grab current setting
            andi.b    #3,d0                     ; KBRSH+KBLSH bits
            beq       keymaus
            move.b    #1,d1                     ; x value for right arrow
            bra       keymaus
alt13:      cmpi.b    #$50,d1                   ; is it an down arrow?
            bne.s     alt14
            move.b    #0,d1                     ; x value for down arrow
            move.b    #8,d2                     ; y value for down arrow
            move.b    (kb_shift).w,d0           ; grab current setting
            andi.b    #3,d0                     ; KBRSH+KBLSH bits
            beq       keymaus
            move.b    #1,d2                     ; y value for down arrow
            bra       keymaus
alt14:      btst      #2,(kb_shift).w           ; CTRL key?
            bne.s     conin24                   ; yes
            cmpi.b    #2,d1
            bcs.s     alt1                      ; not >= the '1' key scancode
            cmpi.b    #$d,d1
            bhi.s     alt1                      ; not <= the '=' key scancode
            addi.b    #$76,d1                   ; scancode is a key between '1' key and '=' key
            bra.s     alt2
alt1:       cmpi.b    #$41,d0                   ; is the key an ascii 'A' or greater?
            bcs.s     alt3                      ; no skip to check if 'a'-'z'...
            cmpi.b    #$5a,d0                   ; is the key an ascii 'Z' or less?
            bhi.s     alt3                      ; no skip to check if 'a'-'z'...
alt2:       moveq     #0,d0
            bra.s     conin25
alt3:       cmpi.b    #$61,d0                   ; is the key an ascii 'a' or greater?
            bcs.s     conin25                   ; not...skip to finish normal processing
            cmpi.b    #$7a,d0                   ; is the key an ascii 'z' or less?
            bhi.s     conin25                   ; not...skip to finish normal processing
            bra.s     alt2

conin2x:    btst      #2,(kb_shift).w           ; CTRL key?
            beq.s     conin25                   ; no
conin24:    andi.w    #$1f,d0                   ; yep, so CTRLize the key
conin25:    asl.w     #8,d1                     ; shift the scan code to the word's high byte
            add.w     d1,d0                     ; form the outgoing word

            movea.l   (sp)+,a0                  ; restore kbufrec pointer

            move.w    8(a0),d1                  ; get current tail pointer offset
            addq.w    #4,d1                     ; index = tail + 4
            cmp.w     4(a0),d1                  ; check to see if buffer should wrap
            bcs.s     ari13                     ; no...
            moveq     #0,d1                     ; wrap pointer
ari13:      cmp.w     6(a0),d1                  ; head=tail?
            beq.s     ari14                     ; yes
            movea.l   (a0),a2                   ; get buffer pointer

* move kb_shift into the upper 8 bits of the keycode
            swap      d0
            clr.w     d0
            move.b    (kb_shift).w,d0
            swap      d0
            lsl.l     #8,d0
            lsr.w     #8,d0

            move.l    d0,d2                     ; save keycode for our reset key commands
            bclr      #$1c,d2                   ; clear capslock state
            swap      d2                        ; isolate kb_shift & scancode
            cmp.w     #$c53,d2                  ; control+alt+delete?
            beq       reseth                    ; => reset
            cmp.w     #$d53,d2                  ; control+alt+rshift+delete?
            beq       _coldboot                 ; => cold boot by erasing all memory

            btst      #3,(conterm).w            ; should Bconin() to return the shift status?
            bne.s     ari13b                    ; yes...
            andi.l    #$ffffff,d0               ; no, filter it out (default case)
ari13b:     and.l     #$ffff,d1
            move.l    d0,(a2,d1.l)              ; store the data
            move.w    d1,8(a0)                  ; store the new buftail pointer
ari14:      rts

keyclicksnd:move.l    #gkeyclicksnd,(cursnd).w  ; sound data for key click
            move.b    #0,(timer).w              ; enable sound timer
            rts


midibyte:   movea.l   (midivec).w,a2            ; get contents of midivec for indirect branch
            jmp       (a2)                      ; jump to midi interrupt handler


sysmidi:    move.w    8(a0),d1                  ; get current tail pointer offset
            addq.w    #1,d1                     ; index = tail + 1
            cmp.w     4(a0),d1                  ; check to see if buffer should wrap
            bcs.s     mi13                      ; no...
            moveq     #0,d1                     ; wrap pointer
mi13:       cmp.w     6(a0),d1                  ; head=tail?
            beq.s     mi14                      ; yes
            movea.l   (a0),a2                   ; get buffer pointer
            and.l     #$ffff,d1
            move.b    d0,(a2,d1.l)              ; store the data
            move.w    d1,8(a0)                  ; store the new buftail pointer
mi14:       rts


keymaus1:   moveq     #5,d3                     ; pre-init to "keyboard" right mouse button
            btst      #4,d1                     ; see if it is a left or right button...
            beq.s     keym1                     ; it's a right button ($47/$c7)
            moveq     #6,d3                     ; it's a left button ($52/$d2)
keym1:      btst      #7,d1                     ; see if it is a make or break action
            beq.s     keym2                     ; it's a set button action (make code)
            bclr      d3,(kb_shift).w           ; it's a clear button action (break code)
            bra.s     keym3                     ; go to further pre-init action...
keym2:      bset      d3,(kb_shift).w           ; it's a set button action (set code)
keym3:      moveq     #0,d1
            moveq     #0,d2
*
* finish up at the actual pseudo mouse routine
*

keymaus:    lea       (kmbuf).w,a0              ; point to key-emulating mouse buffer
            movea.l   (msintvec).w,a2           ; grab mouse interrupt vector
            clr.l     d0
            move.b    (kb_shift).w,d0           ; get current button status
            lsr.b     #5,d0                     ; shift right button bit to 'd0'
            addi.b    #$f8,d0                   ; add relative mouse header
            move.b    d0,(a0)                   ; store in first byte of record header
            move.b    d1,1(a0)                  ; store x value in second byte of record buffer
            move.b    d2,2(a0)                  ; store y value in third byte of record buffer
            jsr       (a2)
            movea.l   (sp)+,a0                  ; restore kbufrec pointer
            rts


mousekey1:  DC.B      $47,$c7,$52,$d2

*************************************************************************
*                                                                       *
*************************************************************************
_coldboot:  move      #$2700,sr                 ; disable all IRQSR
IF ROM_STBOOK
            move.l    ($0004).w,(busexception).w ;a bus error triggers a reset
            movea.w   #addrexception,a0         ; start erasing from $c on
            moveq     #0,d0
            move.l    #$3fffc,d1                ; erase 1MB
_coldboot2: move.l    d0,(a0)+
            dbra      d1,_coldboot2
            movea.l   ($0004).w,a0              ; jump into the ROM to reset the system
            jmp       (a0)

ELIF ROM_TOS206
			move.w    #5,d0
			lea       _coldbootr(pc),a0
			movea.w   #addrexception,a1
_coldboot1:	move.l    (a0)+,(a1)+				; copy erase routine to $000c...
			dbra      d0,_coldboot1
			jmp       (addrexception).w

_coldbootr:	move.l    ($0004).w,(busexception).w
			lea       _coldbootre(pc),a0
			moveq     #0,d0
_coldbootl:	move.l    d0,(a0)+
			move.l    d0,(a0)+
			move.l    d0,(a0)+
			move.l    d0,(a0)+
			bra.s     _coldbootl
_coldbootre:nop

ELIF ROM_TOS306
			move.l    #$808,d0
			movec     d0,cacr

			moveq     #0,d0
			movec     d0,vbr					; set vector base to address 0
			pmove     ($e36738).l,tc			; 0L - disable PMMU
			pmove     ($e36738).l,tt0			; 0L
			pmove     ($e36738).l,tt1			; 0L

			move.w    #11,d0
			lea       _coldbootr(pc),a0
			movea.w   #addrexception,a1
_coldboot1:	move.l    (a0)+,(a1)+				; copy erase routine to $000c...
			dbra      d0,_coldboot1
			jmp       (addrexception).w

_coldbootr:	lea       _coldboott(pc),a0
			move.l    a0,(busexception).w
			lea       _coldbootre(pc),a0
			moveq     #0,d0
_coldbootl:	move.l    d0,(a0)+					; erase ST RAM
			move.l    d0,(a0)+
			move.l    d0,(a0)+
			move.l    d0,(a0)+
			bra.s     _coldbootl
_coldboott: move.l    ($0004).w,(busexception).w
			lea       ($1000000).l,a0
_coldbootfl:move.l    d0,(a0)+					; erase fast RAM
			move.l    d0,(a0)+
			move.l    d0,(a0)+
			move.l    d0,(a0)+
			bra.s     _coldbootfl
_coldbootre:nop

ENDIF

*************************************************************************
*                                                                       *
*       protocol for accessing a gi sound chip register                 *
*                                                                       *
*       this bios call must be accessed in supervisor state             *
*       because it affects the 'sr' register                            *
*                                                                       *
*       entry                                                           *
*                                                                       *
*       word    giaccess(data,register)                                 *
*       word    data,register                                           *
*                                                                       *
*               data -- data register read/write date                   *
*               register -- chip register to select                     *
*               d1 = #$0000     ;selects read operation of the register *
*               d1 = #$80 .or .xx       ;selects write xx to register   *
*               example write to portb - $80 .or. $0f = $8f             *
*                                                                       *
*       exit                                                            *
*       read operations                                                 *
*       d0.b -- data register contains byte of date                     *
*       write operations                                                *
*       d0.b -- data register contains a verification of written data   *
*                                                                       *
*************************************************************************
giaccess:   move.w    4(sp),d0
            move.w    6(sp),d1
gientry:    move      sr,-(sp)
            ori       #$700,sr
            movem.l   d1-d2/a0,-(sp)            ; save affected registers
            lea       (psgsel).w,a0             ; init desired gi register addr
            move.b    d1,d2                     ; make a copy to test for read or write
            andi.b    #$f,d1                    ; turn off any extraneous bits
            move.b    d1,(a0)                   ; select register
            asl.b     #1,d2                     ; shift once for carry bit detection
            bcc.s     giread                    ; carry clear, so do a read operation
giwrit:     move.b    d0,2(a0)                  ; init the memory location
giread:     moveq     #0,d0                     ; clear our register
            move.b    (a0),d0                   ; grab the data from the gi register
            movem.l   (sp)+,d1-d2/a0            ; restore affected registers
            move      (sp)+,sr
            rts


*********************************************************************
*       routine to turn on the dtr signal                           *
*********************************************************************

dtron:      move.b    #$ef,d2
            bra.s     offbit

*********************************************************************
*                                                                   *
*       routine to set any bit in the gi port a area                *
*                                                                   *
*       entry                                                       *
*                                                                   *
*       word    ongibit(bitnum)                                     *
*       word    bitnum                                              *
*                                                                   *
*               bitnum - byte size mask with desired bit set to "1" *
*                                                                   *
*********************************************************************

ongibit:    moveq     #0,d2
            move.w    4(sp),d2
onbit:      movem.l   d0-d2,-(sp)
            move      sr,-(sp)
            ori       #$700,sr
            moveq     #14,d1                    ; get ready to read in the port a contents
            move.l    d2,-(sp)
            bsr.s     gientry                   ; go get it...
            move.l    (sp)+,d2
            or.b      d2,d0                     ; set bit(s) on
            move.b    #$8e,d1                   ; setup to write to port a
            bsr.s     gientry                   ; go set it and return
            move      (sp)+,sr
            movem.l   (sp)+,d0-d2
            rts


*********************************************************************
*                                                                   *
*       routine to clear any bit in the gi port a area              *
*                                                                   *
*       entry                                                       *
*                                                                   *
*       word    offgibit(bitnum)                                    *
*       word    bitnum                                              *
*                                                                   *
*               bitnum - byte size mask with desired bit set to "0" *
*                                                                   *
*********************************************************************

offgibit:   moveq     #0,d2
            move.w    4(sp),d2
offbit:     movem.l   d0-d2,-(sp)
            move      sr,-(sp)
            ori       #$700,sr
            moveq     #14,d1                    ; get ready to read in the port a contents
            move.l    d2,-(sp)
            bsr.s     gientry                   ; go get it...
            move.l    (sp)+,d2
            and.b     d2,d0                     ; turn bit(s) off
            move.b    #$8e,d1                   ; setup to write to port a
            bsr.s     gientry                   ; go set it and return
            move      (sp)+,sr
            movem.l   (sp)+,d0-d2
            rts


*************************************************************************
*                                                                       *
*               EXTENDED RBP BIOS TIMER INIT CALL                       *
*                                                                       *
*       entry:                                                          *
*                                                                       *
*       word    initmous(type,param,intvec)                             *
*       word    type                                                    *
*       long    param,initvec                                           *
*                                                                       *
*               type - key/abs/rel/off  mouse function requested        *
*                       4/  2/  1/  0   value                           *
*               param - address of parameter block                      *
*               intvec - mouse interrupt vector                         *
*                                                                       *
*                                                                       *
*       parameter block definition:                                     *
*                                                                       *
*       byte 0 - y=0 at top/bottom; if non-zero then y=0 at bottom      *
*               otherwise y=0 at top                                    *
*       byte 1 - parameter for set mouse buttons command                *
*       byte 2 - x threshold/scale/delta parameter                      *
*       byte 3 - y threshold/scale/delta parameter                      *
*                                                                       *
*       the following bytes are required for the absolute mode only     *
*                                                                       *
*       byte 4 - xmsb for absolute mouse maximum position               *
*       byte 5 - xlsb for absolute mouse maximum position               *
*       byte 6 - ymsb for absolute mouse maximum position               *
*       byte 7 - ylsb for absolute mouse maximum position               *
*       byte 8 - xmsb for absolute mouse initial position               *
*       byte 9 - xlsb for absolute mouse initial position               *
*       byte a - ymsb for absolute mouse initial position               *
*       byte b - ylsb for absolute mouse initial position               *
*                                                                       *
*************************************************************************

* first we determine if the init is for a absolute, relative or keycode
* mouse action.

initmous:   tst.w     4(sp)                     ; turn mouse off?
            beq.s     im1                       ; yes...disable mouse
            move.l    $a(sp),(msintvec).w       ; init the mouse interrupt vector
            movea.l   6(sp),a3
            cmpi.w    #1,4(sp)                  ; relative mouse request?
            beq.s     im2                       ; yes...
            cmpi.w    #2,4(sp)                  ; absolute mouse request?
            beq.s     im3                       ; yes...
            cmpi.w    #4,4(sp)                  ; keycode mouse request?
            beq.s     im4                       ; yes...
            moveq     #0,d0                     ; error condition returned -- improper request
            rts

im1:        moveq     #$12,d1                   ; disable mouse
            bsr       ikbdput
            move.l    #xbtexit,(msintvec).w     ; re-init the mouse interrupt vector
            bra.s     imexit

im2:        lea       (transbuf).w,a2           ; set transfer buffer pointer
            move.b    #8,(a2)+                  ; set to relative mouse
            move.b    #$b,(a2)+                 ; set relative mouse threshold x,y
            bsr.s     setmouse
            moveq     #6,d3                     ; set length of the string -1 to transfer
            lea       (transbuf).w,a2           ; set transfer buffer pointer
            bsr       ikbdstr                   ; do transfer to ikbd
            bra.s     imexit

im3:        lea       (transbuf).w,a2           ; set transfer buffer pointer
            move.b    #9,(a2)+                  ; set to absolute mouse
            move.b    4(a3),(a2)+               ; set xmsb max
            move.b    5(a3),(a2)+               ; set xlsb max
            move.b    6(a3),(a2)+               ; set ymsb max
            move.b    7(a3),(a2)+               ; set ylsb max
            move.b    #$c,(a2)+                 ; set absolute mouse scale
            bsr.s     setmouse
            move.b    #$e,(a2)+                 ; load initial absolute mouse position
            move.b    #0,(a2)+                  ; filler load
            move.b    8(a3),(a2)+               ; initial xmsb absolute mouse position
            move.b    9(a3),(a2)+               ; initial xlsb absolute mouse position
            move.b    $a(a3),(a2)+              ; initial ymsb absolute mouse position
            move.b    $b(a3),(a2)+              ; initial ylsb absolute mouse position
            moveq     #$10,d3                   ; set length of string -1 to transfer
            lea       (transbuf).w,a2           ; set transfer buffer pointer
            bsr       ikbdstr                   ; do transfer to ikbd
            bra.s     imexit

im4:        lea       (transbuf).w,a2           ; set transfer buffer pointer
            move.b    #$a,(a2)+                 ; set to mouse keycode mode
            bsr.s     setmouse
            moveq     #5,d3                     ; set length of string -1 to transfer
            lea       (transbuf).w,a2           ; set transfer buffer pointer
            bsr       ikbdstr                   ; do transfer to ikbd
imexit:     moveq     #-1,d0                    ; set to true to indicate good init
            rts


setmouse:   move.b    2(a3),(a2)+               ; set x threshold/scale/delta
            move.b    3(a3),(a2)+               ; set y threshold/scale/delta
            moveq     #$10,d1                   ; setup to determine if top/bottom
            sub.b     (a3),d1                   ; set y=0 at ?
            move.b    d1,(a2)+
            move.b    #7,(a2)+                  ; set mouse button action
            move.b    1(a3),(a2)+               ; mouse button parameter
            rts


*************************************************************************
*                                                                       *
*               EXTENDED RBP BIOS TIMER INIT CALL                       *
*                                                                       *
*       entry:                                                          *
*                                                                       *
*       void    xbtimer(id,control,data,intvec)                         *
*       word    id,control,data                                         *
*       long    intvec                                                  *
*                                                                       *
*               intvec - timer interrupt vector                         *
*               control - timer's control setting                       *
*               data - timer's data register setting                    *
*               id - timer id   a-0, b-1, c-2, d-3                      *
*                                                                       *
*       Special Note:                                                   *
*                                                                       *
*       In the interest of preserving as many features for the user     *
*       in the future, timer A should be reserved for the end-user      *
*       or independent software vendor's application program.  System   *
*       software or those application needing just a "tick" should      *
*       constrain themselves to timer C, which is adequate for delay    *
*       and other timing uses.  Future hardware may or may not bring    *
*       out the timer A input line out...giving software developers     *
*       another useful aspect of the machine to utilize.                *
*                                                                       *
*       The recommended usage of the timers is as follows:              *
*                                                                       *
*       Timer A - Reserved for end-users and stand-alone applications.  *
*       Timer B - Reserved for screen graphics, primarily.              *
*       Timer C - Reserved for system timing (GSX,GEM,DESKTOP,ET.AL).   *
*       Timer D - Reserved for baud rate control of RS-232 port,        *
*                the interrupt vector is available to anyone.           *
*                                                                       *
*************************************************************************

xbtimer:    moveq     #0,d0
            moveq     #0,d1
            moveq     #0,d2
            move.w    4(sp),d0
            move.w    6(sp),d1
            move.w    8(sp),d2
            bsr       settimer                  ; setup the timer
            tst.l     $a(sp)                    ; if >$7fffffff then skip and exit
            bmi.s     xbtexit
            movea.l   $a(sp),a2                 ; setup for initint call
            moveq     #0,d1                     ; clear long
            lea       xbtim(pc),a1              ; point to timer -> interrupt # translation tab
            andi.l    #$ff,d0                   ; mask off the highest three bytes in register
            move.b    (a1,d0.w),d0              ; setup for initint call
            bsr       initint
xbtexit:    rts

xbtim:      DC.B      $0d,$08,$05,$04

*************************************************************************
*                                                                       *
*               KEYBOARD TRANSLATION TABLE CHANGE CALL                  *
*                                                                       *
*       entry:                                                          *
*                                                                       *
*       long    keytrans(unshift,shift,capslock)                        *
*       long    unshift,shift,capslock                                  *
*                                                                       *
*               -1 signifies no change to vector                        *
*                                                                       *
*       exit:                                                           *
*               d0.l - returns pointer to beginning of                  *
*                       key translation address pointers                *
*               order of pointers is:                                   *
*               unshifted,shifted,caps-locked                           *
*               Note:  buffer space for each table should $80!!         *
*                                                                       *
*************************************************************************

keytrans:   tst.l     4(sp)
            bmi.s     kt1
            move.l    4(sp),(skeytran).w
kt1:        tst.l     8(sp)
            bmi.s     kt2
            move.l    8(sp),(skeyshif).w
kt2:        tst.l     $c(sp)
            bmi.s     kt3
            move.l    $c(sp),(skeycl).w
kt3:        move.l    #skeytran,d0
            rts


*************************************************************************
*                                                                       *
*               RESTORE BIOS KEYBOARD TRANSLATION TABLE                 *
*                                                                       *
*       entry:                                                          *
*                                                                       *
*       void    bioskeys()                                              *
*                                                                       *
*                                                                       *
*************************************************************************

bioskeys:   move.l    #gskeytran,(skeytran).w
            move.l    #gskeyshif,(skeyshif).w
            move.l    #gskeycl,(skeycl).w
IF !ROM_TOS306
            clr.b     (keyrep).w
ENDIF
            rts

*************************************************************************
*                                                                       *
*               RETURN IKBD SUBSYSTEM INTERRUPT TABLE POINTER           *
*                                                                       *
*       entry:                                                          *
*                                                                       *
*       void    dosound(ptr)                                            *
*       long    ptr     ;points to start of sound interpreter table     *
*                                                                       *
*                                                                       *
*************************************************************************

dosound:    move.l    (cursnd).w,d0             ; return current status in D0.L
            move.l    4(sp),d1                  ; if new ptr < 0, then just return
            bmi.s     ds_r                      ; (invalid ptr, so return)
            move.l    d1,(cursnd).w             ; setup new sound ptr
            clr.b     (timer).w                 ; zap sound timer register
ds_r:       rts

*************************************************************************
*                                                                       *
*               SET/RETURN PRINTER CONFIGURATION WORD                   *
*                                                                       *
*       entry:                                                          *
*                                                                       *
*       word    setptr(pconfig)                                         *
*       word    pconfig ;sets/gets printer information word             *
*                                                                       *
*                                                                       *
*************************************************************************

setprt:     move.w    (pconfig).w,d0            ; get current config word before we change it
            tst.w     4(sp)                     ; see if we don't change the word
            bmi.s     nosetp                    ; don't set printer word
            move.w    4(sp),(pconfig).w         ; set printer config word
nosetp:     rts

*************************************************************************
*                                                                       *
*               SET/RETURN KEY REPEAT VALUES                            *
*                                                                       *
*       entry:                                                          *
*                                                                       *
*       word    kbrate(initial,repeat)                                  *
*       word    initial,repeat                                          *
*                                                                       *
*       initial determines the number of 50 hz cycles to wait before    *
*       a keyrepeat is to commence.  repeat determines the interval     *
*       between keyrepeats after the initial pause.                     *
*                                                                       *
*************************************************************************

kbrate:     move.w    (cdelay1).w,d0            ; get current initial/repeat values
            tst.w     4(sp)                     ; see if we don't change the word
            bmi.s     kbrate1                   ; don't set key repeat values
            move.w    4(sp),d1                  ; set key repeat values
            move.b    d1,(cdelay1).w            ; set initial delay
            tst.w     6(sp)                     ; see if we don't change the word
            bmi.s     kbrate1                   ; don't set key repeat values
            move.w    6(sp),d1                  ; set key repeat values
            move.b    d1,(cdelay2).w            ; set subsequent delay
kbrate1:    rts


*************************************************************************
*                                                                       *
*               RETURN POINTER TO IKBD/MIDI INTERRUPT VECTORS           *
*                                                                       *
*       entry:                                                          *
*                                                                       *
*       long    ikbdvecs()                                              *
*               returns a pointer to the midi interrupt vector and      *
*               ikbd subsystem interrupt vector table.  the table       *
*               structure is as follows:                                *
*                                                                       *
*       midivec         ds.l    1       ;midi interrupt handler vector  *
*       vkbderr         ds.l    1       ;keyboard error handler address *
*       vmiderr         ds.l    1       ;midi error handler address     *
*       statintvec      ds.l    1       ;ikbd status interrupt vector   *
*       msintvec        ds.l    1       ;mouse interrupt vector         *
*       clkintvec       ds.l    1       ;realtime clk interrupt vector  *
*       joyintvec       ds.l    1       ;joystick interrupt vector      *
*                                                                       *
*       note:   msintvec is modified via the initmouse system function  *
*               call.  since gem uses this vector, modifying it can be  *
*               fatal while running under gem.  clkintvec is used by    *
*               gemdos.  its pre-inited vector must be restored for     *
*               proper gemdos operation.  Caveat hacker!                *
*                                                                       *
*                                                                       *
*************************************************************************

ikbdvecs:   move.l    #midivec,d0
            rts

*************************************************************************
*                                                                       *
*       C Timer interrupt routine to process the PSG sound table        *
*                                                                       *
*************************************************************************
*+ (lmd)
* timercint - timer c interrupt handler
* divide 200 Hz interrupt frequency to 50 hz, and do:
*       sound handler processing
*       key-repeat processing;
*       control-g bell and keyclick if enabled via sound handler
*       system timer-tick handoff
*       updates:        tc_rot (every tick)
*
*       imports:        etv_timer (timer handoff vector)
*                       _timr_ms (timer calibration value)
*
*-
timercint:  addq.l    #1,(_hz_200).l            ; increment raw tick counter
            rol       (tc_rot).l                ; rotate divisor bits
            bpl.s     t_punt                    ; if not 4th interrupt, then return
            movem.l   d0-d7/a0-a6,-(sp)

            bsr.s     sndirq                    ; process sounds

            btst      #1,(conterm).w            ; check for key repeat enabled
            beq.s     krexit                    ; not enabled

* process for repeat key function first because it can affect the sound
* table if enabled and the user is 'using'...

            tst.b     (keyrep).w
            beq.s     krexit
            tst.b     (keydelay1).w
            beq.s     kr1
            subq.b    #1,(keydelay1).w
            bne.s     krexit
kr1:        subq.b    #1,(keydelay2).w
            bne.s     krexit
            move.b    (cdelay2).w,(keydelay2).w
            move.b    (keyrep).w,d0
            lea       (kbufrec).w,a0
            bsr       ari16                     ; repeat key stroke and stuff into buffer

*+ (lmd)
* Call system timer vector
* (first guy in the system daisy-chain)
*
*-
krexit:     move.w    (_timr_ms).w,-(sp)        ; push #ms/tick
            movea.l   (etv_timer).w,a0          ; get vector
            jsr       (a0)                      ; call it
            addq.w    #2,sp                     ; cleanup stack
tick1:      movem.l   (sp)+,d0-d7/a0-a6
t_punt:     move.b    #$df,(isrb).w             ; clear the interrupt channel
            rte

*********************************************************
*
*  Quick & dirty sound stuff
*
*
*  Programmed by Dave Staugas
*                14 Mar 1985
*
*
*
*
*********************************************************
*
*
*
*
*  To start a sound, load the 32-bit address of the
*                       byte stream for that sound in 32-bit
*                       "cursnd", & zero the 8-bit "timer"
*
*
*
*
*   Sound interrupt routine
*   Called from timer C irq
*
sndirq:     movem.l   d0-d1/a0,-(sp)
            move.l    (cursnd).w,d0             ; get current sound ptr
            beq.s     snd1                      ; br to exit if zero, inactive
            movea.l   d0,a0                     ; ptr to a0
            move.b    (timer).w,d0              ; check delay timer
            beq.s     snd3                      ; br over delay timer update if not on
*
            subq.b    #1,d0                     ; tick off delay timer
            move.b    d0,(timer).w              ; save new
            bra.s     snd1                      ; skip sound update this time

snd3:       move.b    (a0)+,d0                  ; pick up next sound command
            bmi.s     snd2                      ; if minus, go do special
*
            move.b    d0,(psgsel).w             ; else, register load command--select this
            cmpi.b    #7,d0                     ; reg. 7 selected?
            bne.s     sn1                       ; br if no
*
            move.b    (a0)+,d1                  ; get data to write to reg 7
            andi.b    #$3f,d1                   ; always leave i/o port settings alone
            move.b    (psgsel).w,d0             ; get mixer contents
            andi.b    #$c0,d0                   ; mask off non-useful info...
            or.b      d1,d0                     ; generate new setting
            move.b    d0,(psgwr).w              ; write data
            bra.s     snd3                      ; go for next command

sn1:        move.b    (a0)+,(psgwr).w           ; write next byte as data directly to reg
            bra.s     snd3                      ; go for next command
*
*  special case command
*
snd2:       addq.b    #1,d0                     ; was command 255?
            bpl.s     snd5                      ; br if yes--set delay timer
*
            cmpi.b    #$81,d0                   ; was command 128 (before increment)
            bne.s     snd6                      ; br if not
*
*  command 128
*
            move.b    (a0)+,(auxd).w            ; 128--set aux data from next byte in stream
            bra.s     snd3                      ; go for next command
*
*  command > 128
*
snd6:       cmpi.b    #$82,d0                   ; command greater than 129
            bne.s     snd5                      ; br if yes--must be set timer
*
*  command 129
*
            move.b    (a0)+,(psgsel).w          ; 129--select register
            move.b    (a0)+,d0                  ; get increment step (signed)
            add.b     d0,(auxd).w               ; add to aux data
            move.b    (a0)+,d0                  ; terminating value
            move.b    (auxd).w,(psgwr).w        ; load reg from data in auxd
            cmp.b     (auxd).w,d0               ; reached end of cycle?
            beq.s     snd4                      ; br if so
*
*  still within loop, reset sound pointer to iterate for next irq
*
            subq.w    #4,a0                     ; back up sound ptr to repeat this command
            bra.s     snd4                      ; update ptr & exit
*
*  set delay timer
*
snd5:       move.b    (a0)+,(timer).w           ; set delay timer from next byter in stream
            bne.s     snd4                      ; if non-zero real delay here
            movea.w   #0,a0                     ; else, sound terminator--set ptr to null

snd4:       move.l    a0,(cursnd).w             ; update sound ptr
snd1:       movem.l   (sp)+,d0-d1/a0            ; pop stack & exit
            rts


*
* sound data...
*
*
* format:
*
*      sound data usually is found in byte pairs, the first of which is the command
*      and the second is the argument.  However, some commands take on more than
*      1 argument
*
*      cmd     function        argument(s)
*      00      load reg0       data0
*      01      load reg1       data0
*      02      load reg2       data0
*      03      load reg3       data0
*      04      load reg4       data0
*      05      load reg5       data0
*      06      load reg6       data0
*      07      load reg7       data0   note: b7 & b6 forced set for all data to r...
*      08      load reg8       data0
*      09      load reg9       data0
*      0A      load reg10      data0
*      0B      load reg11      data0
*      0C      load reg12      data0
*      0D      load reg13      data0
*
*
*      80      init temp w/    data0
*
*      81      loop defined
*              by 3 args       data0 as register to load using temp
*                              data1 as increment/decrement (signed) of temp
*                              data2 as loop terminator value of temp
*
*      82-FF   set delay
*              timer           data0 is # of counts till next update
*                                      note: if data0 = 0, sound is terminated
*
*
*
*
*********************************************************
* VT52 emulator calls for the bell sound
*********************************************************
ringbel:    btst      #2,(conterm).w            ; console bell enabled?
            beq.s     rgbel                     ; (no sound)
            movea.l   (bell_hook).l,a0
            jsr       (a0)                      ; go through the bell vector
            rts


keybellsnd: move.l    #bellsnd,(cursnd).w       ; sound data for console bell
            move.b    #0,(timer).w              ; enable sound timer
rgbel:      rts


*+
* flopini - initialize floppies
* Passed (on the stack):
*        $c(sp) devno
*        $8(sp) ->DSB
*        $4(sp) ->buffer (unused)
*        $0(sp) return address
*
* Returns:       EQ if initialization succeeded (drive attached).
*                NE if initialization failed (no drive attached).
*-
_flopini:   clr.l     (frbufcookie).l
            lea       (dsb0).l,a1               ; get ptr to correct DSB
            tst.w     $c(sp)
            beq.s     fi_1
            lea       (dsb1).l,a1
fi_1:       move.w    (seekrate).l,6(a1)        ; setup default seek rate
            move.w    #3,4(a1)                  ; default: HD mode
            moveq     #-1,d0                    ; (default error)
            clr.w     2(a1)                     ; fake clean drive
            clr.w     (frbufscnt).l
IF !ROM_TOS306
            move.w    #$ff00,2(a1)              ; default = recal drive (it's dirty)
ENDIF
IF ROM_STBOOK
            bsr.s     checkfdc
            beq.s     fi_ndrv
ENDIF
            bsr       floplock                  ; setup parameters
            bsr       select                    ; select drive an side
IF ROM_TOS306
            move.w    #$ff00,2(a1)              ; default = recal drive (it's dirty)
ENDIF
            bsr       restore                   ; attempt restore
            beq.s     fi_ok                     ; (quick exit if that won)
            moveq     #10,d7                    ; attempt seek to track 10
            bsr       hseek1                    ; (hard seek to 'd7')
            bne.s     fi_nok                    ; (failed: drive unusable)
            bsr       restore                   ; attempt restore after seek
fi_ok:      beq       flopok                    ; return OK (on win)
fi_nok:     bra       flopfail                  ; return failure
IF ROM_STBOOK
fi_ndrv:    move.w    #$0202,(diskmode).w       ; both drives are "unsure"
            moveq     #0,d0
            tst.w     (_nflops).w               ; any floppy drives installed?
            beq.s     fi_ret                    ; (no, return NO_ERROR)
            moveq     #-15,d0                   ; return "unknown device"
fi_ret:     rts

drvnrdy:    moveq     #-2,d0                    ; return DRIVE_NOT_READY
            rts
ENDIF

IF ROM_TOS306
			move.l    a0,(busexception).w
			movea.l   a1,sp
			movem.l   (sp)+,d7/a6
drvnrdy:	moveq     #-15,d0
			rts
ENDIF

*+
* checkfdc - check if the FDC track/sector register are available
* Returns:      EQ, if the FDC is not available
*               NE, if the FDC is available
*-
IF ROM_STBOOK
checkfdc:   addq.w    #1,(flock).w              ; lock floppies (allow 'recursion')
            movem.l   d0-d7/a6,-(sp)
            lea       (fifo).w,a6
            move.w    #$82,(a6)                 ; setup 1770 track register
            moveq     #0,d7
            bsr       wdiskctl
            move.w    #$84,(a6)                 ; load 1770 sector register
            moveq     #-1,d7
            bsr       wdiskctl
            move.w    #$82,(a6)                 ; read 1770 track register
            bsr       rdiskctl
            move.b    d0,d1                     ; not equal to 0?
            bne.s     checkfdc2
            move.w    #$84,(a6)                 ; read 1770 sector register
            bsr       rdiskctl
            subq.w    #1,(flock).w              ; unlock floppies
            tst.b     d1                        ; track register wrong?
            bne.s     checkfdc2
            cmp.b     #$ff,d0                   ; sector register wrong?
checkfdc2:  movem.l   (sp)+,d0-d7/a6
            eori      #4,sr                     ; flip Z bit (return NE if bit is zero)
            rts
ENDIF

*+
* floprd - read sector from floppy
* Passed (on the stack):
*       $14(sp) count
*       $12(sp) sideno
*       $10(sp) trackno
*        $e(sp) sectno
*        $c(sp) devno
*        $8(sp) ->DSB
*        $4(sp) ->buffer
*        $0(sp) return address
*
* Returns       EQ, the read won (on all sectors),
*               NE, the read failed (on some sector).
*-
_floprd:
IF ROM_STBOOK
			bsr.s     checkfdc
            beq.s     drvnrdy
ENDIF
IF ROM_TOS306
			tst.w     (_nflops).w               ; any floppy drives installed?
			beq.s     drvnrdy                   ; (no, return "unknown device")
ENDIF
            bsr       change                    ; test for disk change
            moveq     #-11,d0                   ; set default error#
            bsr       frbcheckrd
            bsr       floplock                  ; lock floppies, setup parameters
frd1:       bsr       select                    ; select drive, setup registers
            bsr       go2track                  ; seek appropriate track
            bcs       flopfail
            bne       frde                      ; retry on seek failure
frd1l:      move.w    #-1,(curr_err).l          ; set general error#
            move.w    #$190,(a6)                ; toggle DMA data direction,
            tst.b     (gpip).w
            tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            move.w    #$90,(a6)                 ; leave hardware in READ state
            tst.b     (gpip).w
            tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            move.w    #1,(diskctl).w            ; set sector count register
            move.w    #$80,(a6)                 ; startup 1770 "read sector" command
            move.w    #$80,d7                   ; (read single)
            bsr       wdiskctl
            move.l    (_hz_200).w,d7
            add.l     #300,d7                   ; set timeout timer

*--- Wait for read completion:
frd2:       btst      #5,(gpip).w               ; 1770 done yet?
            beq.s     frd4                      ; (yes)
            cmp.l     (_hz_200).w,d7            ; timeout reached?
            bhi.s     frd2                      ; (punt on timeout)

*---- check status after read
            move.w    #-2,(curr_err).w          ; set "timeout" error
            bsr       reset1770                 ; (clobber 1770)
            bra.s     frde                      ; (go retry)

*--- check status after read:
frd4:       move.w    #$90,(a6)                 ; examine DMA status register
            move.w    (a6),d0
            btst      #0,d0                     ; bit zero inidcates DMA ERROR
            beq.s     frde                      ; (when its zero -- retry)

            move.w    #$80,(a6)                 ; exeamine 1770 status register
            bsr       rdiskctl
            and.b     #$1c,d0                   ; check for RNF, checksum, lost-data
            bne.s     frd4                      ; (bail on error)
            move.w    #2,(retrycnt).w           ; reset retry count for next sector
            addq.w    #1,(csect).w              ; advance sector #
            addi.l    #$200,(cdma).w            ; advance buffer by 512 bytes
            subq.w    #1,(ccount).w             ; decrement sector count
            beq       flopok                    ; (done)
            bsr       select1
            bra       frd1l

frd4:       bsr.s     err_bits                  ; set error# from 1770 bits
frde:       cmpi.w    #1,(retrycnt).w           ; are we on the "middlemost" retry?
            bne.s     frd5
frde1:      bsr       reseek                    ; yes, home and reseek the head
frd5:       subq.w    #1,(retrycnt).w           ; drop retry count
            bpl       frd1                      ; (continue of any retries left)
            bra       flopfail                  ; fail when we run out of patience

*+
* err_bits - set "curr_err" according to 1770 error status
* Passed:       d0 = 1770 status
*
* Returns:      curr_err, containing current error number
*
* Uses:         d1
*-
err_bits:   moveq     #-13,d1                   ; write protect?
            btst      #6,d0
            bne.s     eb1
            moveq     #-8,d1                    ; record-not-found?
            btst      #4,d0
            bne.s     eb1
            moveq     #-4,d1                    ; CRC error?
            btst      #3,d0
            bne.s     eb1
            move.w    (def_error).w,d1          ; use default error#
eb1:        move.w    d1,(curr_err).w           ; set current error number & return
            rts

*+
* flopwr - write sector to floppy
* Passed (on the stack):
*       $14(sp) count
*       $12(sp) sideno
*       $10(sp) trackno
*        $e(sp) sectno
*        $c(sp) devno
*        $8(sp) ->DSB
*        $4(sp) ->buffer (unused)
*        $0(sp) return address
*
* Returns:      EQ, the write won (on all sectors),
*               NE, the write failed (on some sectors).
*-
_flopwr:
IF ROM_STBOOK
			bsr       checkfdc
            beq       drvnrdy
ENDIF
IF ROM_TOS306
			tst.w     (_nflops).w               ; any floppy drives installed?
			beq       drvnrdy                   ; (no, return "unknown device")
ENDIF
            bsr       change                    ; check for disk swap
            moveq     #-10,d0                   ; set default error number
            bsr       frbcheckwr
            bsr       floplock                  ; lock floppies

*+
* If the boot sector is written to,
* set the media change mode to "unsure".
* (Kludge, kludge, kludge....)
*-
            move.w    (csect).w,d0              ; sector 1
            subq.w    #1,d0
            or.w      (ctrack).w,d0             ; track 0
            or.w      (cside).w,d0              ; side 0
            bne.s     fwr1                      ; if not boot sector, then OK
            moveq     #2,d0                     ; set media change mode to unsure
            bsr       setdmode                  ; (boy, is this /ugly/)

fwr1:       bsr       select                    ; select drive
            bsr       go2track                  ; seek
            bcs       flopfail
            bne       fwre1                     ; (retry on seek failure)
fwr1a:      move.w    #-1,(curr_err).w          ; set general error#
            move.w    #$90,(a6)                 ; toggle DMA chip to clear status
            tst.b     (gpip).w
            tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            move.w    #$190,(a6)                ; leave in WRITE mode
            tst.b     (gpip).w
            tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            move.w    #1,d7                     ; load sector-count register
            bsr       wdiskctl
            move.w    #$180,(a6)                ; load "WRITE SECTOR" command
            move.w    #$a0,d7                   ; into 1770 cmdreg
            bsr       wdiskctl
            move.l    (_hz_200).w,d7
            add.l     #300,d7                   ; d7 = timeout timer

fwr2:       btst      #5,(gpip).w               ; done yet?
            beq.s     fwr4                      ; (yes, check status)
            cmp.l     (_hz_200).w,d7            ; timeout reached?
            bhi.s     fwr2                      ; (still tickin')
            bsr       reset1770                 ; timed out -- reset 1770
            bra.s     fwre                      ; and retry

fwr4:       move.w    #$180,(a6)                ; get 1770 status
            bsr       rdiskctl
            bsr       err_bits                  ; compute 1770 error bits
            btst      #6,d0                     ; if write protected, don't retry
            bne       flopfail                  ; (can't write, so punt)
            and.b     #$5c,d0                   ; check WriteProt+RecNtFnd+CHKSUM+LostD
            bne.s     fwre                      ; retry on error

            move.w    #2,(retrycnt).w           ; reset retry count
            addq.w    #1,(csect).w              ; bump sector number
            addi.l    #$200,(cdma).w            ; add DMA pointer for next sector
            subq.w    #1,(ccount).w             ; if(!--count) return OK;
            beq       flopok
            bsr       select1                   ; setup sector#, DMA pointer
            bra       fwr1a                     ; write next (no seek)

fwre:       cmpi.w    #1,(retrycnt).w           ; re-seek head in "middle" retry
            bne.s     fwr5                      ; (not middle retry)
fwre1:      bsr       reseek                    ; home head and seek
fwr5:       subq.w    #1,(retrycnt).w           ; decrement retry count
            bpl       fwr1                      ; loop if there's still hope
            bra       flopfail                  ; otherwise return error status
*+
* _flopfmt - format a track
* Passed (on the stack):
*       $1a(sp) initial sector data
*       $16(sp) magic number
*       $14(sp) interleave
*       $12(sp) side
*       $10(sp) track
*        $e(sp) spt
*        $c(sp) devno
*        $8(sp) pointer to state block
*        $4(sp) dma address
*        $0(sp) [return]
*
* Returns:      EQ: track successfully written.  Zero.W-terminated list of
*               bad sectors left in buffer (they might /all/ be bad.)
*
*               NE: could not write track (write-protected, drive failure,
*               or something catastrophic happened).
*-
_flopfmt:   cmpi.l    #$87654321,$16(sp)        ; check for magic# on stack
            bne       flopfail                  ; no magic, so we just saved the world
IF ROM_STBOOK
            bsr       checkfdc
            beq       drvnrdy
ENDIF
IF ROM_TOS306
			tst.w     (_nflops).w               ; any floppy drives installed?
			beq       drvnrdy                   ; (no, return "unknown device")
ENDIF
            bsr       change                    ; check for disk flip
            moveq     #-10,d0                   ; set default error number
            bsr       frbcheckfmt
            bsr       floplock
            bsr       select                    ; select drive and side
            move.w    $e(sp),(spt).w            ; save sectors-per-track
            move.w    $14(sp),(interlv).w       ; save interleave factor
            move.w    $1a(sp),(virgin).w        ; save initial sector data
            move.l    8(sp),(fmtstateblk).w

*--- put drive into "changed" mode
            moveq     #2,d0                     ; d0 = "CHANGED"
            bsr       setdmode                  ; set media change mode
            moveq     #3,d0                     ; HD mode
            cmpi.w    #$d,(spt).w               ; 13 sectors or more require HD mode
            bcc.s     flopfmt1
            moveq     #0,d0                     ; SD mode
flopfmt1:
IF ROM_STBOOK
	   		bsr       hdselect                  ; select SD/HD mode of floppy
ELSE
IF ROM_TOS206
			tst.b     (STEFlag).l               ; no STE hardware available?
			bne.s     flopfmt2                  ; (correct)
ENDIF
			move.w    d0,(fdccs).w
flopfmt2:
ENDIF
	   		move.w    d0,4(a1)

*--- seek to track (hard seek):
            bsr       hseek                     ; hard seek to 'ctrack'
            bne       flopfail                  ; (return error on seek failure)
            move.w    (ctrack).w,2(a1)          ; record current track#

*--- format track, then verify it:
            move.w    #-1,(curr_err).w          ; vanilla error mode
            bsr.s     fmtrack                   ; format track
            bne       flopfail                  ; (return error on seek failure)
            move.w    (spt).w,(ccount).w        ; set number of sectors to verify
            move.w    #1,(csect).w              ; starting sector# = 1
            bsr       verify1                   ; verify sectors

*--- if there are any bad sectors, return /that/ error...
            movea.l   (cdma).w,a2               ; a2 -> bad sector list
            tst.w     (a2)                      ; any bad sectors?
            beq       flopok                    ; no -- return OK
            move.w    #-16,(curr_err).w         ; set error number
            bra       flopfail                  ; return error

*+
* fmtrack - format a track
* Passed:       variables setup by _flopfmt
* Returns:      NE on failure, EQ on success
* Uses:         almost everything
* Called-by:    _flopfmt
*
*-
fmtrack:    move.w    #-10,(def_error).w        ; set default error number
            movea.l   (cdma).w,a2               ; a2 -> prototyping area
            movea.l   (fmtstateblk).w,a3
            moveq     #120-1,d1                 ; 120 x $4e (track leadin)
            cmpi.w    #$d,(spt).w
            bcc.s     fmtrack1
            moveq     #60-1,d1                  ; 60 x $4e (track leadin)
fmtrack1:   moveq     #$4e,d0
            bsr       wmult

            clr.w     d3                        ; interleave index = table start
            tst.w     (interlv).w               ; interleave < 0
            bmi       fmtrack2                  ; use custom interleave table ->
            moveq     #1,d3                     ; first sector = 1

*---- address mark
ot3:        move.w    d3,d4                     ; d4 = starting sector (this pass)
ot1:        moveq     #12-1,d1                  ; 12 x $00
            clr.b     d0
            bsr       wmult
            moveq     #3-1,d1                   ; 3 x $f5
            moveq     #$f5,d0
            bsr       wmult
            move.b    #$fe,(a2)+                ; $fe -- address mark intro
            move.b    (ctrack+1).w,(a2)+        ; track#
            move.b    (cside+1).w,(a2)+         ; side#
            move.b    d4,(a2)+                  ; sector#
            move.b    #2,(a2)+                  ; sector size (512)
            move.b    #$f7,(a2)+                ; write checksum

*--- gap between AM and data:
            moveq     #22-1,d1                  ; 22 x $4e
            moveq     #$4e,d0
            bsr       wmult
            moveq     #12-1,d1                  ; 12 x $00
            clr.b     d0
            bsr       wmult
            moveq     #3-1,d1                   ; 3 x $f5
            moveq     #$f5,d0
            bsr       wmult

*--- data block:
            move.b    #$fb,(a2)+                ; $fb -- data intro
            move.w    #256-1,d1                 ; 256 x virgin.W (initial sector data)
ot2:        move.b    (virgin).w,(a2)+          ; copy high byte
            move.b    (virgin+1).w,(a2)+        ; copy low byte
            dbra      d1,ot2                    ; fill 512 bytes
            move.b    #$f7,(a2)+                ; $f7 -- write checksum
            moveq     #40-1,d1                  ; 40 x $4e
            moveq     #$4e,d0
            bsr       wmult

            tst.w     (interlv).w               ; interleave < 0
            bmi       fmtrack2                  ; use custom interleave table ->
            add.w     (interlv).w,d4            ; bump sector#
            cmp.w     (spt).w,d4                ; if(d4 <= spt) then_continue;
            ble.s     ot1                       ; proto more sectors this pass
            addq.w    #1,d3                     ; bump pass start count
            cmp.w     (interlv).w,d3            ; if(d3 <= interlv) then_continue;
            ble.s     ot3

*--- end-of-track
ot5:        move.w    #2800,d1                  ; 2801 x $4e -- end of track trailer
            cmpi.w    #$d,(spt).w
            bcc.s     ot4
            move.w    #1400,d1                  ; 1401 x $4e -- end of track trailer
ot4:        moveq     #$4e,d0
            bsr       wmult

*--- setup to write the track:
            move.b    (cdma+3).w,(dmalow).w     ; load dma pointer
            move.b    (cdma+2).w,(dmamid).w
            move.b    (cdma+1).w,(dmahigh).w
            move.w    #$90,(a6)                 ; toggle R/W flag and
            tst.b     (gpip).w
            tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            move.w    #$190,(a6)                ; select sector-count register
            tst.b     (gpip).w
            tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            moveq     #$60,d7                   ; (absurd sector count)
            bsr       wdiskctl
            move.w    #$180,(a6)                ; select 1770 cmd register
            move.w    #$f0,d7                   ; write format_track command
            bsr       wdiskctl
            move.l    (_hz_200).w,d7
            add.l     #300,d7                   ; d7 = timeout timer

*--- wait for 1770 complete:
otw1:       btst      #5,(gpip).w               ; is 1770 done?
            beq.s     otw2                      ; (yes)
            cmp.l     (_hz_200).w,d7            ; timeout reached?
            bhi.s     otw1                      ; (still tickin')
            bsr       reset1770                 ; timed out -- reset 1770
oterr:      moveq     #1,d7                     ; return NE (error status)
            rts

fmtrack2:   cmp.w     (spt).w,d3                ; last sector reached?
            beq       ot5                       ; yes -> end of track
            move.w    d3,d6
            add.w     d6,d6
            move.w    (a3,d6.w),d4              ; pick new sector number from the table
            addq.w    #1,d3                     ; increment interleave index
            bra       ot1

*--- see if the write-track won:
otw2:       move.w    #$190,(a6)                ; check DMA status bit
            move.w    (a6),d0
            btst      #0,d0                     ; if its zero, there was a DMA error
            beq.s     oterr                     ; (so return NE)
            move.w    #$180,(a6)                ; get 1770 status
            bsr       rdiskctl
            bsr       err_bits                  ; set 1770 error bits
            and.b     #$44,d0                   ; check for writeProtect & lostData
            rts                                 ; return NE on 1770 error

*----- write 'D1+1' copies of D0.B into A2, A2+1, ...
wmult:      move.b    d0,(a2)+                  ; record byte in proto buffer
            dbra      d1,wmult                  ; (do it again)
            rts


*+
* _flopver - verify sectors on a track
*       $14(sp) count
*       $12(sp) sideno
*       $10(sp) trackno
*        $e(sp) sectno
*        $c(sp) devno
*        $8(sp) ->DSB
*        $4(sp) ->buffer (at least 1K long)
*        $0(sp) return address
*
* Returns:      NULL.W-terminated list of bad sectors in the buffer if D0 == 0,
*               OR some kind of error (D0 < 0).
*
*-
_flopver:
IF ROM_STBOOK
	   		bsr       checkfdc
            beq       drvnrdy
ENDIF
IF ROM_TOS306
			tst.w     (_nflops).w               ; any floppy drives installed?
			beq       drvnrdy                   ; (no, return "unknown device")
ENDIF
            bsr       change                    ; hack disk change
            moveq     #-11,d0                   ; set default error#
            bsr       frbcheckfmt
            bsr       floplock                  ; lock floppies, setup parameters
            bsr       select                    ; select floppy
            bsr       go2track                  ; go to track
            bne       flopfail                  ; (punt if that fails)
            bsr.s     verify1                   ; verify some sectors
            bra       flopok                    ; return "OK"

*+
* verify1 - verify sectors on a single track
* Passed:       csect = starting sector#
*               ccount = number of sectors to verify
*               cdma -> 1K buffer (at least)
*
* Returns:      NULL.W-terminated list of bad sectors (in the buffer)
*               (buffer+$200..buffer+$3ff used as DMA buffer)
*
* Enviroment:   Head seeked to correct track;
*               Drive and side already selected;
*               Motor should be spinning (go2track and fmttrack do this).
*
* Uses:         Almost everything.
*
* Called-by:    _flopfmt, _flopver
*
*-
verify1:    move.w    #-11,(def_error).w        ; set default error number
            movea.l   (cdma).w,a2               ; a2 -> start of bad sector list
            addi.l    #$200,(cdma).w            ; bump buffer up 512 bytes

*--- setup for (next) sector
tvrlp:      move.w    #2,(retrycnt).w           ; init sector-retry count
            move.w    #$84,(a6)                 ; load 1770 sector register
            move.w    (csect).w,d7              ; with 'csect'
            bsr       wdiskctl

*--- setup for sector read
tvr1:       move.b    (cdma+3).w,(dmalow).w     ; load dma pointer
            move.b    (cdma+2).w,(dmamid).w
            move.b    (cdma+1).w,(dmahigh).w
            move.w    #$190,(a6)                ; toggle R/W (leave in W state)
            tst.b     (gpip).w
            tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            move.w    #$90,(a6)
            tst.b     (gpip).w
            tst.b     (gpip).w                  ; delay for 1 microsec
            tst.b     (gpip).w                  ; this amounts to 16 16Mhz clocks
            tst.b     (gpip).w
            move.w    #1,d7                     ; set DMA sector count to 1
            bsr       wdiskctl
            move.w    #$80,(a6)                 ; load 1770 command register
            move.w    #$80,d7                   ; with ReadSector command
            bsr       wdiskctl
            move.l    (_hz_200).w,d7
            add.l     #300,d7                   ; d7 = timeout timer

*--- wait for command completion
tvr2:       btst      #5,(gpip).w               ; test for 1770 done
            beq.s     tvr4                      ; (yes, it completed)
            cmp.l     (_hz_200).w,d7            ; timeout reached?
            bhi.s     tvr2                      ; (still tickin')
            bsr       reset1770                 ; reset controller and return error
            bra.s     tvre

*--- got "done" interrupt, check DMA status:
tvr4:       move.w    #$90,(a6)                 ; read DMA error status
            move.w    (a6),d0
            btst      #0,d0                     ; if DMA_ERROR is zero, then retry
            beq.s     tvre

*--- check 1770 completion status (see if it's happy):
            move.w    #$80,(a6)                 ; read 1770 status register
            bsr       rdiskctl
            bsr       err_bits                  ; set error# from 1770 register
            and.b     #$1c,d0                   ; check for record-not-found, crc-error,
            bne.s     tvre                      ; and lost data; return on error

*--- read next sector (or return if done)
tvr6:       addq.w    #1,(csect).w              ; bump sector count
            subq.w    #1,(ccount).w             ; while(--count) read_another;
            bne       tvrlp
            subi.l    #$200,(cdma).w            ; readjust DMA pointer
            clr.w     (a2)                      ; terminate bad sector list
            rts                                 ; and return EQ


*--- read failure: retry or record bad sector
tvre:       cmpi.w    #1,(retrycnt).w           ; re-seek head?
            bne.s     tvr5                      ; (no)
            bsr       reseek                    ; yes: back to home and then back
tvr5:       subq.w    #1,(retrycnt).w           ; to the current track...
            bpl       tvr1
            move.w    (csect).w,(a2)+           ; record bad sector
            bra.s     tvr6                      ; do next sector

*+
* _flopvbl - floppy vblank handler
* Deselects floppies after the motor stops
*-
_flopvbl:
IF ROM_TOS306
			tst.w     (_nflops).w               ; any floppy drives installed?
			beq       fvblr	                    ; (no, return "unknown device")
ENDIF
	   		lea       (fifo).w,a6               ; a6 -> fifo
            st        (_motoron).w              ; assume motor is on
            tst.w     (flock).w                 ; floppies locked?
            bne.s     fvblr                     ; (yes, so don't touch them)
IF ROM_STBOOK
            bsr       checkfdc
            beq.s     fvblr
ENDIF

*--- write-protect monitor:
            move.l    (_frclock).w,d0           ; check a drive every 8 jiffies
            move.b    d0,d1                     ; (save jiffy count)
            and.b     #7,d1                     ; time yet?
            bne.s     fvbl1                     ; (no)
            move.w    #$80,(a6)                 ; select 1770 command/status register

*--- select drive, record it's WP status:
            lsr.b     #3,d0                     ; use bit 4 as drive# to check
            and.w     #1,d0
            lea       (wpstatus).w,a0
            adda.w    d0,a0
            cmp.w     (_nflops).w,d0
            bne.s     fvbl2
            clr.w     d0
fvbl2:      addq.b    #1,d0
            lsl.b     #1,d0                     ; (magic shift left)
            eori.b    #7,d0                     ; invert select bits, select side 0
            bsr       setporta                  ; set port A (d2 = old bits)
            bsr       rdiskctl                  ; get 1770 status
            move.w    d0,d1
            btst      #6,d1                     ; test Write-Protect status bit
            sne       (a0)                      ; set WP status to $00 or $FF.
            move.b    d2,d0                     ; restore old drive-select bits
            bsr       setporta

fvbl1:      move.w    (wpstatus).w,d0           ; or _wpstatus into _wplatch
            or.w      d0,(wplatch).w            ; (catch any WP transitions)

*--- floppy deselect test
            tst.w     (deselflg).w              ; floppies already deselected?
            bne.s     fvblr1                    ; (yes, so don't do it again)
            move.l    (_hz_200).w,d0
            cmp.l     (fltimeout).l,d0          ; did we hit our 5s timeout?
            bcc.s     fvbl2

            bsr       rdiskctl                  ; read 1770 status register
            btst      #7,d0                     ; is the motor still on?
            bne.s     fvblr                     ; (yes, so don't deselect)
fvbl2:      move.b    #7,d0                     ; deselect both drives
            bsr       setporta                  ; (set bits 0..3 in portA of PSG)
            move.w    #1,(deselflg).w           ; indicate floppies deselected
fvblr1:     clr.w     (_motoron).w              ; indicate motor is OFF
fvblr:      rts                                 ; back to vbl


*+
* floplock - lock floppies and setup floppy parameters
*
* Passed (on the stack):
*       $18(sp) - count.W (sector count)
*       $16(sp) - side.W (side#)
*       $14(sp) - track.W (track#)
*       $12(sp) - sect.W (sector#)
*       $10(sp) - dev.W (device#)
*        $c(sp) - obsolte.L
*         8(sp) - dma.L (dma pointer)
*         4(sp) - ret1.L (caller's return address)
*         0(sp) - ret.L (floplock's return address)
*
* Passed:       D0.W = default error number
*-
floplock:   movem.l   d3-d7/a3-a6,($168a).l     ; save C registers

            lea       (fifo).w,a6               ; a6 -> fifo
            st        (_motoron).w              ; kludge motor state = ON
            move.w    d0,(def_error).w          ; set default error number
            move.w    d0,(curr_err).w           ; set current error number
            move.w    #1,(flock).w              ; tell vbl not to touch floppies
            move.l    8(sp),(cdma).w            ; cdma -> /even/ DMA address
            move.w    $10(sp),(cdev).w          ; save devide# (0 . 1)
            move.w    $12(sp),(csect).w         ; save sector# (1 . 9, usually)
            move.w    $14(sp),(ctrack).w        ; save track# (0 . 39 .  79   )
            move.w    $16(sp),(cside).w         ; save side# (0 . 1)
            move.w    $18(sp),(ccount).w        ; save sector count (1..spt)
            move.w    #2,(retrycnt).w           ; setup retry count

*--- pick a DSB
            lea       (dsb0).w,a1
            tst.w     (cdev).w
            beq.s     flock2
            lea       (dsb1).w,a1

*--- recalibrate drive (if it needs it)
flock2:     tst.w     2(a1)                     ; if (curtrack < 0) recalibrate()
            bpl.s     flockr

            bsr       select                    ; select drive & side
            clr.w     2(a1)                     ; we're optimistic -- assume winnage
            bsr       restore                   ; attempt restore
            beq.s     flockr                    ; (it won)
            moveq     #10,d7                    ; attempt seek to track 10
            bsr       hseek1
            bne.s     flock1                    ; (failed)
            bsr       restore                   ; attempt restore again
            beq.s     flockr                    ; (it won)
flock1:     move.w    #$ff00,2(a1)              ; complete failure (what can we do?)
flockr:     rts

*+
* flopfail - unlock floppies and return error.
*
*-
flopfail:   moveq     #1,d0                     ; disk change mode = unsure
            bsr       setdmode                  ; set media change mode
            move.w    (curr_err).w,d0           ; get current error number
            ext.l     d0                        ; extent to long
            bra.s     unlok1                    ; clobber floppy lock & return

*+
* flopok - unlock floppies and return success status:
*
*-
flopok:     clr.l     d0                        ; return 0 (success)
unlok1:     move.l    d0,-(sp)                  ; (save return value)
IF ROM_TOS306
			bsr       flushCaches
ENDIF
            move.w    #$86,(a6)                 ; force WP to real-time mode
            move.w    2(a1),d7                  ; dest-track = current track
            bsr       wdiskctl
            move.w    #$10,d6                   ; cmd = seek w/o verify
            bsr       flopcmds                  ; do it

            move.l    (_hz_200).l,d0
            add.l     #5*200,d0                 ; 5s floppy deselect timeout
            move.l    d0,(fltimeout).l

            move.w    (cdev).w,d0               ; set last-access time for 'cdev'
            lsl.w     #2,d0
            lea       (_acctim).w,a0
            move.l    (_frclock).w,(a0,d0.w)
            cmpi.w    #1,(_nflops).w            ; if (nflops == 1) set other time, too
            bne.s     unlok2
            move.l    (_frclock).w,4(a0)        ; set last-access time for floppy 1

unlok2:     move.l    (sp)+,d0                  ; restore return value
            movem.l   ($168a).w,d3-d7/a3-a6     ; restore C registers
            clr.w     (flock).l                 ; unlock floppies
            bsr       frbbackcopy               ; if necessary copy FRB buffer back
            rts


*+
* hseek  - seek to 'ctrack' without verify
* hseek1 - seek to 'd7' without verify
* hseek2 - seek to 'd7' without verify, keep current error number
*
* Returns:      NE on seek failure ("cannot happen"?)
*               EQ if seek wins
*
* Uses:         d7, d6, ...
* Jumps-to:     flopcmds
* Called-by:    _flopfmt, _flopinit
*
*-
hseek:      move.w    (ctrack).l,d7             ; dest track = 'ctrack'
hseek1:     move.w    #-6,(curr_err).l          ; possible error = "seek error"
            move.w    #$86,(a6)                 ; write destination track# to data reg
            bsr       wdiskctl
            move.w    #$10,d6                   ; execute "seek" command
            bra       flopcmds                  ; (without verify...)

*+
* reseek - home head, then reseek track
* Returns:      EQ/NE on success/failure
* Falls-into:   go2track
*
*-
reseek:     move.w    #-6,(curr_err).l          ; set "seek error"
IF ROM_STBOOK
            bsr.s     restore                   ; restore head
            bne.s     go2trr                    ; (punt if home fails)
ELSE
			bsr       restore                   ; restore head
			bne       go2trr                    ; (punt if home fails)
ENDIF

            clr.w     2(a1)                     ; current track = 0
            move.w    #$82,(a6)                 ; set "current track" reg on 1770
            clr.w     d7
            bsr       wdiskctl

            move.w    #$86,(a6)                 ; seek out to track five
            move.w    #5,d7
            bsr       wdiskctl                  ; dest track = 5
            move.w    #$10,d6
            bsr.s     flopcmds                  ; seek
            bne.s     go2trr                    ; return error on seek failure
            move.w    #5,2(a1)                  ; set current track#

*+
* go2track - seek proper track
* Passed:       Current floppy parameters (ctrack, et al.)
* Returns:      EQ/NE on success/failure
* Calls:        flopcmds
*-
go2track:   move.w    #1,(fseekrtr).l           ; set seek retry count
go2track1:  move.w    #-6,(curr_err).l          ; set "seek error"
            move.w    #$86,(a6)                 ; set destination track# in
            move.w    (ctrack).w,d7             ; 1770's data register
            bsr       wdiskctl                  ; (write track#)
            moveq     #$14,d6                   ; execute 1770 "seek_with_verify"
            bsr.s     flopcmds                  ; (include seek-rate bits)
            bcs.s     go2trr
            bne.s     go2track2                 ; return error on seek failure
            and.b     #$18,d7                   ; check for RNF, CRC_error, lost_data
            beq.s     go2track3                 ; (exit on no error)
go2track2:  move.w    4(a1),d0
            and.w     #3,d0
            eori.w    #3,d0                     ; toggle between SD/HD mode for a second seek try
IF ROM_STBOOK
			move.w    d0,4(a1)
            bsr       hdselect                  ; select SD/HD mode of floppy
ELIF ROM_TOS206
			move.w    d0,4(a1)
			tst.b     (STEFlag).l               ; no STE hardware available?
			bne.s     go2track2b                ; (correct)
			move.w    d0,(fdccs).w
go2track2b:
ELIF ROM_TOS306
			move.w    d0,(fdccs).w
			move.w    d0,4(a1)
ENDIF
            subq.w    #1,(fseekrtr).l           ; --seek retry count
            bne.s     go2trr                    ; (out of retries -- punt)
            bsr.s     restore                   ; move head to track 00 to reset it
            bra.s     go2track1                 ; try again
go2track3:  move.w    (ctrack).w,2(a1)          ; update current track number
            clr.w     d7                        ; set EQ
go2trr:     rts


*+
* restore - home head
* Passed:       nothing
* Returns:      EQ/NE on success/failure
* Falls-into:   flopcmds
*-
restore:    clr.w     d6                        ; $00 = 1770 "restore" command
            bsr.s     flopcmds                  ; do restore
            bne.s     res_r                     ; punt on timeout
            btst      #2,d7                     ; test TRK00 bit
            eori      #4,ccr                    ; flip Z bit (return NE if bit is zero)
            bne.s     res_r                     ; (punt if didn't win)
            clr.w     2(a1)                     ; set current track#
res_r:      rts


*--- seek rate conversion table for HD mode
* HD is using twice the clock speed (16MHz instead of 8MHz) for the FDC,
* so the FDC has to seek "slower"
dseekrt:    DC.B      $01,$01,$00,$00           ; 6ms and 12ms -> 12ms,  2ms and 3ms -> 6ms


*+
* flopcmds - floppy command (on-in seek speed bits from database)
* Passed:       d6.w = 1770 command
* Sets-up:      seek bits (bits 0 and 1) in d6.w
* Falls-into:   flopcmds
* Returns:      EQ/NE on success/failure
*-
flopcmds:   move.w    6(a1),d0                  ; get floppy's seek rate bits
            and.w     #3,d0                     ; OR into command
            tst.w     4(a1)                     ; SD mode?
            beq.s     flopcmds1                 ; (yes)
            lea       (dseekrt).l,a0            ; use a HD-compatible seek rate table
            move.b    (a0,d0.w),d0
flopcmds1:  or.b      d0,d6
            or.b      (a1),d6                   ; (a1) == 8 for HD, with sets the verify bit in the command
IF ROM_STBOOK
            move.w    4(a1),d0                  ; get SD/HD mode
            bsr.s     hdselect                  ; select SD/HD mode of floppy
ELSE
IF ROM_TOS206
			tst.b     (STEFlag).l               ; no STE hardware available?
			bne.s     flopcmds2                 ; (correct)
ENDIF
			move.w    4(a1),(fdccs).w
flopcmds2:
ENDIF

*+
* flopcmd - execute 1770 command (with timeout)
* Passed:       d6.w = 1770 command
*
* Returns:      EQ/NE on success/failure
*               d7 = 1770 status bits
*
*-
flopcmd:    move.l    (_hz_200).w,d7            ; setup timeout (assume short)
            add.l     #300,d7
            move.w    #$80,(a6)                 ; select 1770 command register
            bsr       rdiskctl                  ; read it to clobber READY status
            btst      #7,d0                     ; is motor on?
            bne.s     flopcm                    ; (yes, keep short timeout)
            move.l    (_hz_200).w,d7            ; extra timeout for motor startup
            add.l     #600,d7
flopcm:     bsr       wdiskct6                  ; write command (in d6)
flopc1:     cmp.l     (_hz_200).w,d7            ; timeout?
            bcs.s     flopcto                   ; (yes, reset and return failure)
            btst      #5,(gpip).w               ; 1770 completion?
            bne.s     flopc1                    ; (not yet, so wait some more)
            bsr       rdiskct7                  ; return EQ + 1770 status in d7
            clr.w     d6
            rts

flopcto:    bsr.s     reset1770                 ; bash controller
            moveq     #0,d6
            subq.w    #1,d6                     ; and return NE
            rts


*+
* hdselect - select HD mode for 1.4MB floppies and SD for 720kd floppies
* Passed:          d0.w SD/HD mode
*-
IF ROM_STBOOK
hdselect:   tst.w     d0                        ; if != 0 then select HD
            bne.s     hdselect2
            clr.b     d0                        ; SD mode
            bra.s     hdselect3
hdselect2:  move.b    #$80,d0                   ; HD mode is bit 7 in PORT A on the sound chip
hdselect3:  move      sr,d2                     ; save status register
            move.b    #14,(psgsel).w            ; select port on GI chip
            move.b    (psgsel).w,d1             ; get current bits
            bclr      #7,d1                     ; clear HD bit
            or.b      d0,d1                     ; or-in our new bit
            move.b    #14,(psgsel).w            ; select port on GI chip
            move.b    d1,(psgwr).w              ; and write 'em back out there
            move      d2,sr                     ; restore status register
            rts
ENDIF

*+
* reset1770 - reset disk controller after a catastrophe
* Passed:       nothing
* Returns:      nothing
* Uses:         d7
*-
reset1770:  move.w    #$80,(a6)                 ; execute 1770 "reset" command
            move.w    #$d0,d7
            bsr       wdiskctl
IF ROM_TOS306
			move.w    d0,-(sp)
			move.w    #$114,d0
			bsr       mfpdelay
			move.w    (sp)+,d0
ELSE
            move.w    #2,d0
r1770a:     move.b    (tcdr).w,d1               ; wait for 1770 to stop convulsing
r1770b:     cmp.b     (tcdr).w,d1
            beq.s     r1770b
            dbra      d0,r1770a
ENDIF
            bsr.s     rdiskct7                  ; return 1770 status in d7
            rts


*+
* select - setup drive select, 1770 and DMA registers
* Passed:       cside, cdev
* Returns:      appropriate drive and side select
*-
select:     clr.w     (deselflg).w              ; floppies NOT deselected
            move.w    (cdev).w,d0               ; get device number
            addq.b    #1,d0                     ; add and shift to get select bits
            lsl.b     #1,d0                     ; into bits 1 and 2
            or.w      (cside).w,d0              ; or-in side number (bit 0)
            eori.b    #7,d0                     ; negate bits for funky hardware select
            and.b     #7,d0                     ; strip anything else out there
            bsr.s     setporta                  ; do drive select

            move.w    #$82,(a6)                 ; setup 1770 track register
            move.w    2(a1),d7                  ; for current track number
            bsr.s     wdiskctl

select1:    move.w    #$84,(a6)                 ; setup requested sector_number from
            move.w    (csect).w,d7              ; caller's parameters
            bsr.s     wdiskctl
            move.b    (cdma+3).w,(dmalow).w     ; setup DMA chip's DMA pointer
            move.b    (cdma+2).w,(dmamid).w
            move.b    (cdma+1).w,(dmahigh).w
            rts


*+
* setporta - set floppy select bits in PORT A on the sound chip
* Passed:       d0.b (low three bits)
* Returns:      d1 = value written to port A
*               d2 = old value read from port A
* Uses:         d1
*-
setporta:   move      sr,-(sp)                  ; save our IPL
            ori       #$700,sr                  ; start critical section
            move.b    #14,(psgsel).w            ; select port on GI chip
            move.b    (psgsel).w,d1             ; get current bits
            move.b    d1,d2                     ; save old bits for caller
            and.b     #$f8,d1                   ; strip low three bits there
            or.b      d0,d1                     ; or-in our new bits
            move.b    d1,(psgwr).w              ; and write 'em back out there
            move      (sp)+,sr                  ; restore IPL to terminate CS, return
            rts


*+
* Primitives to read/write 1770 controller chip (DISKCTL register).
*
* The 1770 can't keep up with full-tilt CPU access, so
* we have to surround read and writes with delay loops.
* This is not really as slow as it sounds.
*
*-
wdiskct6:   bsr.s     rwdelay                   ; delay
            move.w    d6,(diskctl).w            ; write d6 to diskctl
            rts

wdiskctl:   bsr.s     rwdelay                   ; delay
            move.w    d7,(diskctl).w            ; write d7 to diskctl
            rts

rdiskct7:   bsr.s     rwdelay                   ; delay
            move.w    (diskctl).w,d7            ; read diskctl into d7
            rts

rdiskctl:   bsr.s     rwdelay                   ; delay
            move.w    (diskctl).w,d0            ; read diskctl into d0
            rts


rwdelay:
IF ROM_TOS306
			move.w    d0,-(sp)
			move.w    #$119,d0
			bsr       mfpdelay
			move.w    (sp)+,d0
ELSE
			movem.l   d0-d1,-(sp)               ; save registers
            move.w    #2,d0
rwdly1:     move.b    (tcdr).w,d1               ; busy-loop: give 1770 time to settle
rwdly2:     cmp.b     (tcdr).w,d1
            beq.s     rwdly2
            dbra      d0,rwdly1
            movem.l   (sp)+,d0-d1               ; restore registers
ENDIF
            rts


*+
* change - check to see if the "right" floppy bas been inserted
* On the stack:
*       $10(sp) - dev.W (device#)
*        $c(sp) - dsb.L (pointer to Device State Block)
*         8(sp) - dma.L (dma pointer)
*         4(sp) - ret1.L (caller's return address)
*         0(sp) - ret.L (change's return address)
*
* Returns:      both media "might have changed" condition
*
* Uses:         C registers
*
*-
change:     cmpi.w    #1,(_nflops).l            ; if there are zero or two floppies
            bne.s     ch_r                      ; then do nothing (return OK)
            move.w    $10(sp),d0                ; if cdev == _curflop
            cmp.w     (_curflop).l,d0           ; (...current disk == current drive?)
            beq.s     ch_ok1                    ; then return OK (but use drive #0)

*--- ask the user to stick in the other floppy (via critical error handler)
push disk# we want inserted:move.w d0,-(sp)
            move.w    #-17,-(sp)                ; push "INSERT_A_DISK" error number
            bsr       crit_err                  ; use critical error handler and
            addq.w    #4,sp                     ; hope somebody handles it
            move.w    #$ffff,(wplatch).l        ; set "might have changed" on both drvs
            lea       (_acctim).l,a0
            clr.l     (a0)+
            clr.l     (a0)
            move.w    $10(sp),(_curflop).l      ; set current disk#
ch_ok1:     clr.w     $10(sp)                   ; use drive 0
ch_r:       rts

*+
* setdmode - set drive-change mode
* Passed:         d0.b = mode to put current drive in (0, 1, 2)
* Uses:           a0
*
*-
setdmode:   lea       (diskmode).w,a0           ; a0 -> disk mode table
            move.b    d0,-(sp)                  ; (save mode)
            move.w    (cdev).w,d0               ; d0.w = drive# (index into table)
            cmpi.w    #1,(_nflops).l
            bne.s     setdmode2
            move.w    (_curflop).l,d0
setdmode2:  move.b    (sp)+,(a0,d0.w)           ; set drive's mode
            rts


*+
* floprate - sets the seek rate of the specified floppy drive
* On the stack:
*         6(sp) - rate.w (0: 6ms, 1: 12ms, 2:2ms, 3:3ms)
*         4(sp) - dev.W (device#)
*         0(sp) - return address
*
* Returns:      prior seek rate for the specified drive
*
*-

*--- pick a DSB
floprate:   lea       (dsb0).l,a1
            tst.w     4(sp)
            beq.s     floprate2
            lea       (dsb1).l,a1
floprate2:  move.w    6(a1),d0                  ; current rate
            move.w    6(sp),d1                  ; new seek rate
            cmp.w     #-1,d1                    ; new seek rate == -1
            beq.s     floprateret               ; (just return the current rate)
            cmp.w     #-2,d1                    ; new seek rate == -2
            beq.s     flopratehd                ; (select HD floppy density)
            cmp.w     #-3,d1                    ; new seek rate == -3
            beq.s     flopratesd                ; (select SD floppy density)
            cmp.w     #-4,d1                    ; new seek rate == -4
            beq.s     floprateretdens           ; (return current density)
            move.w    d1,6(a1)                  ; set new seek rate in dsb
floprateret:ext.l     d0
            rts

flopratehd: move.b    #8,(a1)                   ; set density to HD
            moveq     #0,d0                     ; return no error
            rts

flopratesd: clr.b     (a1)                      ; set density to SD
            moveq     #0,d0                     ; return no error
            rts

floprateretdens:tst.b (a1)                      ; return current density setting
            sne       d0                        ; -1:HD, 0:SD
            ext.w     d0
            ext.l     d0
            rts


*+
* Fast RAM doesn't support DMA transfers, we use a temporary buffer
* for loads/stores from and into Fast RAM. The buffer is in _FRB cookie.
*-
frbbackcopy:move.w    (frbufscnt).l,d1          ; sectors to copy back into memory?
            beq.s     frbcheckrts               ; (no, punt)
IF ROM_TOS306
			move.l    d0,-(sp)
			bsr       flushCaches
			move.l	  (sp)+,d0
ENDIF
            clr.w     (frbufscnt).l             ; reset sector flag
            movea.l   (frbufaddress).l,a0       ; requested buffer address in memory
            movea.l   (frbufcookie).l,a1        ; used DMA buffer address
frbcopya1_a0:asl.w    #5,d1
            subq.w    #1,d1
frbcopyl:   move.l    (a1)+,(a0)+
            move.l    (a1)+,(a0)+
            move.l    (a1)+,(a0)+
            move.l    (a1)+,(a0)+
            dbra      d1,frbcopyl
frbcheckrts:rts

frbcheckrd:
IF ROM_TOS306
	 		cmpi.l    #$a00000,8(sp)            ; beyond the 10MB limit?
ELSE
			cmpi.l    #$400000,8(sp)            ; beyond the 4MB limit?
ENDIF
            bcs.s     frbcheckrts               ; (no, punt)
            bsr.s     findFRBc
            move.l    8(sp),(frbufaddress).l    ; save buffer address
            move.l    (frbufcookie).l,8(sp)     ; inject FRB buffer into the parameter
            move.w    $18(sp),(frbufscnt).l     ; save number of sectors
            rts

frbcheckwr:
IF ROM_TOS306
	 		cmpi.l    #$a00000,8(sp)            ; beyond the 10MB limit?
ELSE
			cmpi.l    #$400000,8(sp)            ; beyond the 4MB limit?
ENDIF
            bcs.s     frbcheckrts               ; (no, punt)
            bsr.s     findFRBc
            movea.l   8(sp),a1                  ; get the source buffer address
            movea.l   (frbufcookie).l,a0
            move.l    a0,8(sp)                  ; inject FRB buffer into the parameter
            move.w    $18(sp),d1                ; number of sectors
            bra.s     frbcopya1_a0              ; copy D1 sectors from A1 to A0

frbcheckfmt:
IF ROM_TOS306
	 		cmpi.l    #$a00000,8(sp)            ; beyond the 10MB limit?
ELSE
			cmpi.l    #$400000,8(sp)            ; beyond the 4MB limit?
ENDIF
            bcs.s     frbcheckrts               ; (no, punt)
            bsr.s     findFRBc
            move.l    8(sp),(frbufaddress).l    ; save buffer address
            move.l    (frbufcookie).l,8(sp)     ; inject FRB buffer into the parameter
            move.w    #1,(frbufscnt).l          ; 1 sector
            rts

* get FRB buffer pointer from the cookies
findFRBc:   tst.l     (frbufcookie).l           ; variable already set?
            bne.s     findFRBcrts               ; (yes, return)
            movea.l   (_p_cookies).w,a0
            cmpa.w    #0,a0                     ; no cookie jar?
            beq.s     findFRBcx
findFRBcl:  tst.l     (a0)                      ; end of the cookie jar?
            beq.s     findFRBcx
            cmpi.l    #'_FRB',(a0)+
            beq.s     findFRBcf
            addq.l    #4,a0
            bra.s     findFRBcl
findFRBcf:  move.l    (a0)+,(frbufcookie).l
findFRBcrts:rts

findFRBcx:  addq.l    #8,sp                     ; go 2 return levels up
            moveq     #-12,d0                   ; return -12 if no _FRB cookie
            rts
