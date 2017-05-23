main:       pea       ($0000).w
            move.l    #'_PWR',-(sp)
            jsr       (_getcookie).l            ; _PWR cookie installed?
            addq.l    #8,sp
            tst.w     d0
            beq       install                   ; (no -> install app)
_term:      clr.w     -(sp)
            trap      #1                        ; Pterm0()
            illegal

install:    move.l    #infoText,-(sp)
            move.w    #9,-(sp)
            trap      #1                        ; Cconws()
            addq.w    #6,sp

            pea       (cookiePtr).l
            pea       ($0000).w
            jsr       (_getcookie).l
            addq.l    #8,sp
            addq.l    #8,(cookiePtr).l

            movea.l   4(sp),a0                  ; BASEPAGE
            move.l    (cookiePtr).l,d0
            lsl.l     #3,d0
            move.l    $18(a0),d1                ; Pointer to beginning of BSS segment
            add.l     $1c(a0),d1                ; + BSS segment size
            sub.l     a0,d1                     ; - BASEPAGE address = size of the app memory
            add.l     d1,d0
            move.l    (cookiePtr).l,-(sp)
            move.l    d0,(cookiePtr).l
            move.l    a0,-(sp)
            move.l    #cookie_blankScreenTimer,-(sp)
            move.l    #'_PWR',-(sp)
            jsr       (setCookie).l
            adda.l    #$10,sp
            tst.l     d0
            bmi.s     _term
            bpl       install2
            subi.l    #$40,(cookiePtr).l
            clr.b     (v_opnwk_called).l

install2:   clr.l     -(sp)
            move.w    #$20,-(sp)
            trap      #1                        ; oldval = Super(0L)
            addq.w    #6,sp
            move.l    d0,-(sp)
            move.w    #$20,-(sp)                ; Super(oldval)

            jsr       (setupVectors).l

            move.b    #0,(ahdiDetected).l       ; AHDI check
            movea.l   (pun_ptr).w,a0
            cmpi.l    #'AHDI',$52(a0)           ; P_cookie in PUN_INFO in the AHDI driver
            bne       install3
            lea       $52(a0),a1
            cmpa.l    $56(a0),a1                ; check P_cookptr in the AHDI driver
            bne       install3
            cmpi.w    #$500,$5a(a0)             ; check P_version in the AHDI driver
            blt       install3
            move.b    #1,(ahdiDetected).l
            move.b    $63(a0),(ahdiStandbyTime).l ;idle time limit to spin down IDE unit 0

install3:   move.l    #powerAlarm,(mfp_MonochromeDetect).w ;power alarm
            bclr      #7,(aer).w                ; clear pending "Monochrome Monitor Detect"
            bset      #7,(iera).w               ; enable "Monochrome Monitor Detect" interrupt
            bset      #7,(imra).w               ; enable "Monochrome Monitor Detect" interrupt mask
            move.l    #powerAlarm,(Level7IRQ).w ; power fail

            move.w    #3,(countdownVBLTimer).l  ; reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            move.l    (cookie_blankScreenTimer).l,(blankScreenTimerCounter).l
            bclr      #3,(lcdPowerControlShadow).w ;LCD on
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l
            move.l    (cookie_sleepTimer).l,(sleepTimerCounter).l

            trap      #1                        ; Super(oldval)
            addq.w    #6,sp
            clr.w     -(sp)
            move.l    (cookiePtr).l,-(sp)
            move.w    #$31,-(sp)
            trap      #1                        ; Ptermres()
            illegal

***************************************************************************
*
***************************************************************************
_getcookie: movem.l   d6-d7/a6,-(sp)
            move.l    $10(sp),d6                ; cookie
            movea.l   $14(sp),a6                ; ptr to the cookie value, if found
            clr.l     d7
            move.l    #1,-(sp)
            move.w    #$20,-(sp)
            trap      #1
            tst.l     d0
            bne       _getcookies
            move.l    #0,2(sp)
            move.w    #$20,(sp)
            trap      #1
            move.l    d0,d7
_getcookies:addq.l    #6,sp
            movea.l   (_p_cookies).w,a0
            move.l    a0,d0
            beq       _getcookiee
_getcookiel:move.l    (a0),d0
            cmp.l     d0,d6
            beq       _getcookie2
            tst.l     d0
            beq       _getcookiee
            addq.l    #8,a0
            bra.s     _getcookiel
_getcookie2:move.l    a6,d0
            beq       _getcookie3
            move.l    4(a0),(a6)
_getcookie3:moveq     #1,d0
_getcookiee:move.l    d0,d6
            tst.l     d7
            beq       _getcookiex
            move.l    d7,-(sp)
            move.w    #$20,-(sp)
            trap      #1
            addq.l    #6,sp
_getcookiex:move.l    d6,d0
            movem.l   (sp)+,d6-d7/a6
            rts

***************************************************************************
*
***************************************************************************
setCookie:  link      a6,#0
            move.l    d7,-(sp)
            moveq     #0,d7
            move.l    #1,-(sp)
            move.w    #$20,-(sp)
            trap      #1
            tst.l     d0
            bne       setCookies
            move.l    #0,2(sp)
            move.w    #$20,(sp)
            trap      #1
            move.l    d0,d7
setCookies: addq.l    #6,sp
            movea.l   (_p_cookies).w,a0
            move.l    a0,d0
            beq       setCookiee
            moveq     #0,d0
setCookiel: addq.l    #1,d0
            tst.l     (a0)
            beq       setCookie2
            addq.l    #8,a0
            bra.s     setCookiel
setCookie2: cmp.l     4(a0),d0
            beq       setCookiee2
            move.l    4(a0),$c(a0)
            clr.l     8(a0)
            move.l    8(a6),(a0)
            move.l    $c(a6),4(a0)
            moveq     #0,d0
            bra       setCookiee5
setCookiee: cmpi.l    #2,$14(a6)
            blt       setCookiee4
            move.l    (resvector).w,(oldcookieresvector).l
            move.l    (resvalid).w,(oldcookieresvalid).l
            move.l    #cookieResetVector,(resvector).w
            move.l    #$31415926,(resvalid).w
            movea.l   $10(a6),a0
            move.l    a0,(_p_cookies).w
            move.l    8(a6),(a0)+
            move.l    $c(a6),(a0)+
            clr.l     (a0)+
            move.l    $14(a6),(a0)
            moveq     #1,d0
            bra       setCookiee5
cookieResetVector:clr.l (_p_cookies).w
            move.l    (oldcookieresvalid).l,(resvalid).w
            move.l    (oldcookieresvector).l,(resvector).w
            jmp       (a6)

setCookiee2:cmp.l     $14(a6),d0
            ble       setCookiee4
            move.l    d0,d1
            subq.l    #2,d1
            movea.l   (_p_cookies).w,a0
            movea.l   $10(a6),a1
            move.l    a1,(_p_cookies).w
setCookiee3:move.l    (a0)+,(a1)+
            move.l    (a0)+,(a1)+
            dbra      d1,setCookiee3
            move.l    8(a6),(a1)+
            move.l    $c(a6),(a1)+
            clr.l     (a1)+
            move.l    $14(a6),(a1)
            moveq     #1,d0
            bra       setCookiee5
setCookiee4:moveq     #$ff,d0
setCookiee5:tst.l     d7
            beq       setCookiex
            move.l    d0,8(a6)
            move.l    d7,-(sp)
            move.w    #$20,-(sp)
            trap      #1
            addq.l    #6,sp
            move.l    8(a6),d0
setCookiex: move.l    (sp)+,d7
            unlk      a6
            rts

***************************************************************************
*       reset vector - used to wake up the machine back                   *
***************************************************************************
resetVector:move.l    (oldresvalid).l,(resvalid).w
            move.l    (oldresvector).l,(resvector).w
            bclr      #2,(lcdPowerControlShadow).w ;power is on
            bclr      #3,(lcdPowerControlShadow).w ;LCD on
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l
            movea.l   (oldUSP).l,a0
            move      a0,usp
            movea.l   (oldSSP).l,sp
            clr.w     (saveOrRestoreFlag).l     ; Restore chip registers

            subi.l    #$2e,(savptr).w
            bsr       saveState_video
            bsr       saveState_halftone
            bsr       saveState_MFP
            bsr       saveState_MIDI
            bsr       saveState_PSG
            bsr       saveState_time
            bsr       saveState_Keyboard
            bsr       saveState_AHDI
            move.l    #0,-(sp)
            move.w    #$2f,-(sp)
            trap      #$e                       ; Waketime(0L) - clear Waketime
            addq.w    #6,sp
            addi.l    #$2e,(savptr).w

            bset      #7,(imra).w
            andi.b    #$f,(tcdcr).w
            move.b    #$df,(iprb).w
            movea.l   (etv_timer).w,a6
            subq.l    #2,sp
            move.w    #511-1,d7
resetVector2:clr.w    (sp)
            jsr       (a6)
            dbra      d7,resetVector2
            addq.l    #2,sp
            ori.b     #$50,(tcdcr).w

resetVector3:move.w   #30,d1
resetVector4:move.b   (STConfig+1).l,d0
            cmp.b     #$fe,d0                   ; Power button still pressed?
            beq.s     resetVector3              ; yes -> continue to wait
            cmp.b     #$ff,d0                   ; no pressed?
            bne       resetVector5              ; -> done
            btst      #7,(gpip).w               ; power alarm IRQ pending?
            beq.s     resetVector3              ; -> continue to wait
            btst      #5,(iprb).w
            beq.s     resetVector4
            move.b    #$df,(iprb).w
            dbra      d1,resetVector4

resetVector5:move.b   #$7f,(ipra).w
            movem.l   (saveRegs).l,d0-d7/a0-a6
            rte

***************************************************************************
*       NMI or MFP IRQ #15                                                *
***************************************************************************
powerAlarm: btst      #2,(STConfig+1).l         ; wait for RTC alarm to be off
            bne       powerAlarm2
            bclr      #2,(rtc_mode+1).l         ; disable RTC alarm
            bra.s     powerAlarm

powerAlarm2:movem.l   d0-d2,(saveRegs).l
            move.b    (STConfig+1).l,d0         ; IRQ source
            move.b    d0,d1
            and.b     #$a4,d1
            cmp.b     #$a4,d1                   ; _only_ expansion wake, modem wake or RTC wake?
            bne       powerAlarmExit            ; these are always ignored ->
            btst      #3,d0                     ; power dead?
            beq       gotoSleep                 ; -> force sleep
            btst      #0,d0                     ; power switch?
            beq       gotoSleep                 ; -> force sleep
            btst      #1,d0                     ; top closed?
            beq       gotoSleep                 ; -> force sleep
            cmp.b     #$ff,d0                   ; anything else?
            bne       powerAlarmExit            ; -> ignore it

* we got no IRQ source, wait for a bit to see if that changes
            move.w    #$1e,d1                   ; 31 scan lines
            move      sr,d2
            move      #$2700,sr
powerAlarml:move.b    (STConfig+1).l,d0         ; read the IRQ source
            cmp.b     #$ff,d0                   ; anything changed?
            bne       gotoSleep                 ; -> force sleep
            btst      #5,(iprb).w               ; scan line IRQ?
            beq.s     powerAlarml               ; no -> continue waiting
            move.b    #$df,(iprb).w             ; confirm IRQ
            dbra      d1,powerAlarml            ; wait 31 scan lines ->
            move      d2,sr
powerAlarmExit:movem.l (saveRegs).l,d0-d2
            move.b    #$7f,(isra).w             ; re-enable MFP IRQ #15
            rte                                 ; continue normally

***************************************************************************
*       let the system go to sleep                                        *
***************************************************************************
gotoSleep:  move      #$2700,sr                 ; stop all interrupts
            movem.l   d3-d7/a0-a6,(saveRegs+4*3).l ;save all remaining registers
            btst      #3,(STConfig+1).l         ; power dead?
            beq       shutdownPower             ; -> force shutdown

            subi.l    #$2e,(savptr).w           ; decrement re-entrancy stack for xbios
            move.l    #1,-(sp)
            move.w    #$2f,-(sp)
            trap      #$e                       ; Waketime(1L) - enable Waketime
            addq.w    #6,sp
            addi.l    #$2e,(savptr).w           ; re-increment re-entrancy stack for xbios
            tst.l     d0
            beq       shutdownPower

            move.b    #$c,(ide_seccn).l
            move.b    #$e2,(ide_comst).l        ; IDE standby command
            addi.l    #11*50,(blankScreenTimerCounter).l    ; wait for 11s
            addi.l    #11*50,(sleepTimerCounter).l
            movea.l   (etv_timer).w,a6
            subq.l    #2,sp
            move.w    #511-1,d7
gotoSleep2: clr.w     (sp)                      ; calling etv_timer as ~10s have passed
            jsr       (a6)
            dbra      d7,gotoSleep2
            addq.l    #2,sp

gotoSleep3: move.b    #30,d0
gotoSleep4: btst      #7,(gpip).w
            beq.s     gotoSleep3
            btst      #5,(iprb).w
            beq.s     gotoSleep4
            move.b    #$df,(iprb).w
            dbra      d0,gotoSleep4
            move.b    #$7f,(isra).w
            movem.l   (saveRegs).l,d0-d7/a0-a6
            rte

***************************************************************************
*       shut the system down because of a power failure                   *
***************************************************************************
shutdownPower:move    usp,a0
            move.l    a0,(oldUSP).l
            move.l    sp,(oldSSP).l
            move.w    #1,(saveOrRestoreFlag).l  ; Save chip registers
            bclr      #7,(imra).w
            bset      #3,(lcdPowerControlShadow).w ;LCD off
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l
            subi.l    #$2e,(savptr).w
            bsr       saveState_video
            bsr       saveState_halftone
            bsr       saveState_FDC
            bsr       saveState_bios_rwabs
            bsr       saveState_AHDI
            bsr       saveState_PSG
            bsr       saveState_MFP
            addi.l    #$2e,(savptr).w
            move.l    (resvalid).w,(oldresvalid).l
            move.l    (resvector).w,(oldresvector).l
            move.l    #$31415926,(resvalid).w
            move.l    #resetVector,(resvector).w
            move.b    (lcdPowerControlShadow).w,d0
            bset      #4,d0                     ; REFRESH_MACHINE output
            move.b    d0,(lcdPowerControl).l
            bset      #0,(synmod+1).l           ; disable combo -> shadow controller video transfer
debouncel:  move.w    #100,d0                   ; wait for up to 500ms for the power button to be released
            clr.b     (tacr).w                  ; stop timer a
            bclr      #5,(iera).w               ; disable timer a
            bclr      #5,(imra).w               ; clear pending timer a interrupts
            bset      #5,(iera).w               ; enable timer a
            move.b    #$c0,(tadr).w
            move.b    #5,(tacr).w               ; timer a at 5ms
debouncel2: cmpi.b    #$fe,(STConfig+1).l       ; Power button still pressed?
            beq.s     debouncel
            btst      #5,(ipra).w               ; timer a not triggered
            beq.s     debouncel2
            move.b    #$df,(ipra).w             ; clear timer a interrupt
            dbra      d0,debouncel2             ; continue waiting
            bset      #2,(lcdPowerControlShadow).w ;power off
            move.b    (lcdPowerControlShadow).w,d0
            bset      #4,d0                     ; REFRESH_MACHINE output
            move.b    d0,(lcdPowerControl).l
* The following opcode is pre-fetched, the CPU doesn't need to touch this memory after the previous opcode anymore
endlessLoop:bra.s     endlessLoop               ; wakeup via reset!
            trap      #1                        ; this is useless stuff...
            addq.w    #6,sp
            illegal

***************************************************************************
*   save/restore the XBIOS time
*   Force reading the time from the RTC to update the GEMDOS time after sleep
***************************************************************************
saveState_time:tst.w  (saveOrRestoreFlag).l
            bne       saveState_timee
            move.w    #$17,-(sp)
            trap      #$e                       ; Gettime()
            addq.w    #2,sp
saveState_timee:rts

***************************************************************************
*   save/restore the MFP state
***************************************************************************
saveState_MFP:lea     (sav_MFP).l,a0
            tst.w     (saveOrRestoreFlag).l
            beq       saveState_MFPe
            move.b    (aer).w,(a0)+
            move.b    (ddr).w,(a0)+
            move.b    (iera).w,(a0)+
            move.b    (ierb).w,(a0)+
            move.b    (imra).w,(a0)+
            move.b    (imrb).w,(a0)+
            move.b    (vr).w,(a0)+
            move.b    (tacr).w,(a0)+
            move.b    (tbcr).w,(a0)+
            move.b    (tcdcr).w,(a0)+
            move.b    (scr).w,(a0)+
            move.b    (ucr).w,(a0)+
            move.b    (rsr).w,(a0)+
            move.b    (tsr).w,(a0)+
            move.b    #$c0,(sav_MFP+16).l
            lea       (tadr).w,a0
            lea       (tacr).w,a1
            bsr       saveState_MFPsave
            move.b    d0,(sav_MFP+14).l
            lea       (tbdr).w,a0
            lea       (tbcr).w,a1
            bsr       saveState_MFPsave
            move.b    d0,(sav_MFP+15).l
            lea       (tddr).w,a0
            lea       (tcdcr).w,a1
            bsr       saveState_MFPsave
            move.b    d0,(sav_MFP+17).l
            rts

saveState_MFPe:move.b (a0)+,(aer).w
            move.b    (a0)+,(ddr).w
            move.b    (a0)+,(iera).w
            move.b    (a0)+,(ierb).w
            move.b    (a0)+,(imra).w
            move.b    (a0)+,(imrb).w
            move.b    (a0)+,(vr).w
            move.b    (a0)+,(tacr).w
            move.b    (a0)+,(tbcr).w
            move.b    (a0)+,(tcdcr).w
            move.b    (a0)+,(scr).w
            move.b    (a0)+,(ucr).w
            move.b    (a0)+,(rsr).w
            move.b    (a0)+,(tsr).w
            move.b    (sav_MFP+14).l,(tadr).w
            move.b    (sav_MFP+15).l,(tbdr).w
            move.b    (sav_MFP+16).l,(tcdr).w
            move.b    (sav_MFP+17).l,(tddr).w
            move.b    (gpip).w,d0
            move.b    (aer).w,d1
            eor.b     d1,d0
            btst      #7,d0
            bne       saveState_MFP2
            btst      #7,(ipra).w
            bne       saveState_MFP2
            bchg      #7,(aer).w
            bchg      #7,(aer).w
saveState_MFP2:btst   #6,d0
            bne       saveState_MFP3
            btst      #6,(ipra).w
            bne       saveState_MFP3
            bchg      #6,(aer).w
            bchg      #6,(aer).w
saveState_MFP3:btst   #5,d0
            bne       saveState_MFP4
            btst      #7,(iprb).w
            bne       saveState_MFP4
            bchg      #5,(aer).w
            bchg      #5,(aer).w
saveState_MFP4:btst   #4,d0
            bne       saveState_MFP5
            btst      #6,(iprb).w
            bne       saveState_MFP5
            bchg      #4,(aer).w
            bchg      #4,(aer).w
saveState_MFP5:btst   #3,d0
            bne       saveState_MFP6
            btst      #3,(iprb).w
            bne       saveState_MFP6
            bchg      #3,(aer).w
            bchg      #3,(aer).w
saveState_MFP6:btst   #2,d0
            bne       saveState_MFP7
            btst      #2,(iprb).w
            bne       saveState_MFP7
            bchg      #2,(aer).w
            bchg      #2,(aer).w
saveState_MFP7:btst   #1,d0
            bne       saveState_MFP8
            btst      #1,(iprb).w
            bne       saveState_MFP8
            bchg      #1,(aer).w
            bchg      #1,(aer).w
saveState_MFP8:btst   #0,d0
            bne       saveState_MFP9
            btst      #0,(iprb).w
            bne       saveState_MFP9
            bchg      #0,(aer).w
            bchg      #0,(aer).w
saveState_MFP9:rts

saveState_MFPsave:move.b (tcdcr).w,d0
            and.b     #7,d0
            move.b    d0,(tcdcr).w
            move.b    #4,(tcdr).w
            or.b      #$70,d0
            move.b    (a1),d2
            or.b      #7,d2
            move.b    d2,(a1)
            moveq     #1,d1
            lea       (tcdr).w,a1
            lea       (tcdcr).w,a2
            cmp.b     (a0),d1
            bne.s     $107f8
            move.b    d0,(a2)
saveState_MFPsavel:cmp.b (a0),d1
            bne       saveState_MFPsavee
            cmp.b     (a1),d1
            bne.s     saveState_MFPsavel
saveState_MFPsavee:move.b (a0),d0
            rts

***************************************************************************
*   save/restore the keyboard state
*   Reset IKBD on restore, and reset keyboard tables
***************************************************************************
saveState_Keyboard:tst.w (saveOrRestoreFlag).l
            bne       saveState_Keyboarde
            move.b    #3,(keyctl).w
            move.b    #$96,(keyctl).w
            move.l    #ikbdResetCommand,-(sp)
            move.w    #2,-(sp)
            move.w    #$19,-(sp)
            trap      #$e                       ; Ikbdws(2, ikbdResetCommand)
            addq.w    #8,sp
            move.l    #-1,-(sp)
            move.l    #-1,-(sp)
            move.l    #-1,-(sp)
            move.w    #$10,-(sp)
            trap      #$e                       ; Keytbl(-1L, -1L, -1L)
            adda.w    #$e,sp
            movea.l   d0,a0
            movea.l   (a0)+,a3
            movea.l   (a0)+,a4
            movea.l   (a0)+,a5
            move.w    #$18,-(sp)
            trap      #$e                       ; Bioskeys()
            addq.w    #2,sp
            move.l    a5,-(sp)
            move.l    a4,-(sp)
            move.l    a3,-(sp)
            move.w    #$10,-(sp)
            trap      #$e                       ; Keytbl(,,)
            adda.w    #$e,sp
saveState_Keyboarde:rts

ikbdResetCommand:DC.B $80,$01

***************************************************************************
*   save/restore the MIDI port state
*   Just reset MIDI on restore
***************************************************************************
saveState_MIDI:tst.w  (saveOrRestoreFlag).l
            bne       saveState_MIDIe
            move.b    #3,(midictl).w
            move.b    #$95,(midictl).w
saveState_MIDIe:rts

***************************************************************************
*   save/restore the sound chip state
***************************************************************************
saveState_PSG:movem.l d3-d4/a3,-(sp)
            moveq     #$f,d3
            lea       (save_psg).l,a3
            tst.w     (saveOrRestoreFlag).l
            beq       saveState_PSGe
            moveq     #$ff,d4
saveState_PSGl:addq.w #1,d4
            move.w    d4,-(sp)
            move.w    #0,-(sp)
            move.w    #$1c,-(sp)
            trap      #$e                       ; Giaccess()
            addq.w    #6,sp
            move.b    d0,(a3)+
            dbra      d3,saveState_PSGl
            ori.b     #$37,(save_psg+7).l
            movem.l   (sp)+,d3-d4/a3
            rts

saveState_PSGe:moveq  #$7f,d4
saveState_PSGel:addq.w #1,d4
            move.b    (a3)+,d0
            move.w    d4,-(sp)
            move.w    d0,-(sp)
            move.w    #$1c,-(sp)
            trap      #$e                       ; Giaccess()
            addq.w    #6,sp
            dbra      d3,saveState_PSGel
            movem.l   (sp)+,d3-d4/a3
            rts

***************************************************************************
*   save/restore the shifter state
***************************************************************************
saveState_video:tst.w (saveOrRestoreFlag).l
            beq       saveState_videoe
            move.b    (v_bas_h).w,(sav_v_bas_h).l
            move.b    (v_bas_m).w,(sav_v_bas_m).l
            move.b    (v_bas_l).w,(sav_v_bas_l).l
            move.b    (synmod+1).w,(sav_synmod).l
            move.w    (palette).w,(sav_palette0).l
            move.b    (v_shf_mod+1).w,(sav_v_shf_mod).l
            rts
saveState_videoe:move.b (sav_v_bas_h).l,(v_bas_h).w
            move.b    (sav_v_bas_m).l,(v_bas_m).w
            move.b    (sav_v_bas_l).l,(v_bas_l).w
            move.b    (sav_synmod).l,(synmod+1).w
            move.w    (sav_palette0).l,(palette).w
            move.b    (sav_v_shf_mod).l,(v_shf_mod+1).w
            move.b    #0,(v_pixscroll+1).w
            rts

***************************************************************************
*   save/restore the blitter halftone data
***************************************************************************
saveState_halftone:move.w #$1e,d0
            lea       (Halftone).w,a0
            lea       (sav_halftone).l,a1
            tst.w     (saveOrRestoreFlag).l
            beq       saveState_halftonee
saveState_halftonel1:move.w (a0)+,(a1)+
            dbra      d0,saveState_halftonel1
            rts
saveState_halftonee:move.w (a1)+,(a0)+
            dbra      d0,saveState_halftonee
            rts

***************************************************************************
*   save/restore the BIOS device driver state
*   Read the boot sector to detect disk change and update BIOS disk status
***************************************************************************
saveState_bios_rwabs:tst.w (saveOrRestoreFlag).l
            beq       saveState_bios_rwabsl
            move.w    #0,-(sp)
            move.w    #0,-(sp)
            move.w    #1,-(sp)
            move.l    #0,-(sp)
            move.w    #0,-(sp)
            move.w    #4,-(sp)
            trap      #$d                       ; Rwabs(0, 0L, 1, 0, 0)
            adda.w    #$e,sp
            move.w    #1,-(sp)
            move.w    #0,-(sp)
            move.w    #1,-(sp)
            move.l    #0,-(sp)
            move.w    #0,-(sp)
            move.w    #4,-(sp)
            trap      #$d                       ; Rwabs(0, 0L, 1, 0, 1)
            adda.w    #$e,sp
saveState_bios_rwabsl:rts

***************************************************************************
*   save/restore the FDC state
***************************************************************************
saveState_FDC:tst.w   (saveOrRestoreFlag).l
            beq.s     saveState_bios_rwabsl
            move.w    #$190,(diskctl).w
            move.w    #$90,(diskctl).w
            rts

***************************************************************************
*   save/restore the AHDI state
*   Send harddrive to sleep/wakeup
***************************************************************************
saveState_AHDI:tst.b  (ahdiDetected).l
            bne       saveState_AHDIe
saveState_AHDI6:rts

saveState_AHDIe:moveq #0,d0
            andi.w    #7,d0
            lsl.w     #4,d0
            move.b    d0,(ide_headn).l
            tst.w     (saveOrRestoreFlag).l
            beq       saveState_AHDIe2
            move.b    #$e0,(ide_comst).l        ; IDE standby immediate
saveState_AHDI7:move.l #10*200,d0
            add.l     (_hz_200).w,d0
saveState_AHDIl:btst  #5,(gpip).w
            beq.s     saveState_AHDI3
            btst      #7,(ide_stat2).l
            beq.s     saveState_AHDI3
            cmp.l     (_hz_200).w,d0
            bhi.s     saveState_AHDIl
            moveq     #-1,d0
            bra.s     saveState_AHDIr
saveState_AHDI3:move.b (ide_comst).l,d0         ; IDE status register
            btst      #0,d0
            bne.s     saveState_AHDI5
            btst      #3,d0
            bne.s     saveState_AHDIr
            moveq     #0,d0
            bra.s     saveState_AHDIr
saveState_AHDI5:move.b (ide_param).l,d0
saveState_AHDIr:rts

saveState_AHDIe2:bsr  saveState_AHDIs
            tst.w     d0
            beq.s     saveState_AHDI6
            moveq     #0,d0
            andi.w    #7,d0
            lsl.w     #4,d0
            move.b    d0,(ide_headn).l
            move.b    (ahdiStandbyTime).l,(ide_seccn).l
            move.b    #$e2,(ide_comst).l        ; IDE standby command
            bra.s     saveState_AHDI7
saveState_AHDIs:andi.b #7,d0
            lsl.b     #4,d0
            move.b    d0,(ide_headn).l
            move.l    #5*200,d0
            add.l     (_hz_200).w,d0
            move.b    #$50,d1
saveState_AHDIs2:cmp.b (ide_stat2).l,d1
            beq.s     saveState_AHDIs3
            cmp.l     (_hz_200).w,d0
            bcc.s     saveState_AHDIs2
            moveq     #0,d0
            rts
saveState_AHDIs3:moveq #1,d0
            rts

***************************************************************************
*   patch all vectors to monitor image updates and/or keyboard/mouse input
*   as well as timeouts for sleep
***************************************************************************
setupVectors:
            * patch AES/VDI trap
            moveq    #$ff,d0
            trap      #2
            move.l    d0,(sav_screendrvfuncptr).l
            move.l    (trap_aesvdi).w,(oldtrap_aesvdi).l
            move.l    #new_trap2,(trap_aesvdi).w

            * patch BIOS trap
            move.l    (trap_bios).w,(oldtrap_bios).l
            move.l    #new_bios,(trap_bios).w

            * patch XBIOS trap
            move.l    (trap_xbios).w,(oldtrap_xbios).l
            move.l    #new_xbios,(trap_xbios).w

            * patch line A
            linea     #0                        ; Line-A Initialization
            move.l    d0,(lineA_variables).l
            move.l    a1,(lineA_fontHeaders).l
            move.w    #15,d0
            lea       (sav_lineA).l,a3
setupVectorsl:move.l  (a2)+,(a3)+
            dbra      d0,setupVectorsl
            move.l    (lineAexception).w,(old_lineA).l
            move.l    #new_lineA,(lineAexception).w

            * patch IKBD vectors (keyboard, joystick, MIDI, mouse)
            move.w    #$22,-(sp)
            trap      #$e                       ; Kbdvbase()
            addq.w    #2,sp
            movea.l   d0,a1
            lea       $10(a1),a1
            move.l    a1,(old_mousevecAddr).l
            move.l    (a1),(old_mousevec).l
            move.l    #new_mousevec,(a1)
            movea.l   d0,a1
            lea       -4(a1),a1
            move.l    (a1),(old_ikbd_neg4).l
            move.l    #new_ikbd_neg4,(a1)
            movea.l   d0,a1
            lea       $18(a1),a1
            move.l    (a1),(old_joyvec).l
            move.l    #new_joyvec,(a1)
            movea.l   d0,a1
            lea       $1c(a1),a1
            move.l    (a1),(old_midisysvec).l
            move.l    #new_midisysvec,(a1)

            * patch 50Hz timer
            move.l    (etv_timer).w,(old_evt_timer).l
            move.l    #new_etv_timer,(etv_timer).w

            * insert code into VBL queue
            movea.l   (_vblqueue).w,a0
            addq.l    #4,a0
setupVectorsl2:tst.l  (a0)+
            bne.s     setupVectorsl2
            move.l    #new_vbl,-4(a0)
            rts

***************************************************************************
*   Called 50 times per second
*   - if serial port monitoring is active, re-enable LCD
*   - if screen saver is active and timeout is reached, disable LCD
*   - if sleep timeout is active and reached, sleep machine
***************************************************************************
new_etv_timer:btst    #2,(rsr).w                ; serial port active?
            beq       new_etv_timer2            ; (no)
            move.l    (cookie_sleepTimer).l,(sleepTimerCounter).l
            tst.w     (cookie_watchSerialPortFlag).l ;monitor the RS232?
            beq       new_etv_timer2            ; (no)
            move.l    (cookie_blankScreenTimer).l,(blankScreenTimerCounter).l
            bclr      #3,(lcdPowerControlShadow).w ;LCD on
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l

new_etv_timer2:tst.l  (cookie_blankScreenTimer).l
            beq       new_etv_timer3
            subq.l    #1,(blankScreenTimerCounter).l
            bne       new_etv_timer3
            move.l    (cookie_blankScreenTimer).l,(blankScreenTimerCounter).l
            bset      #3,(lcdPowerControlShadow).w ;LCD off
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l

new_etv_timer3:tst.l  (cookie_sleepTimer).l
            beq       new_etv_timer4
            subq.l    #1,(sleepTimerCounter).l
            bne       new_etv_timer4
            move.l    (cookie_sleepTimer).l,(sleepTimerCounter).l
            pea       new_etv_timer4(pc)
            move      sr,-(sp)
            movem.l   d0-d2,(saveRegs).l
            jmp       (gotoSleep).l

new_etv_timer4:move.l (old_evt_timer).l,-(sp)
            rts

***************************************************************************
*   VBL vector patch
*   - After 3 VBLs disable the combo -> shadow controller video transfer
***************************************************************************
new_vbl:    tst.w     (cookie_videoPowerSaverFlag).l    ; disable the LCD after timeout?
            beq       new_vbl2                  ; (no)
            subq.w    #1,(countdownVBLTimer).l
            bne       new_vbl2
            bset      #4,(lcdPowerControlShadow).w ;REFRESH_MACHINE output
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l
            bset      #0,(synmod+1).l           ; disable combo -> shadow controller video transfer
new_vbl2:   nop
            rts

***************************************************************************
*   mouse vector patch
*   Any mouse activity:
*   - enables the transfer to the shadow controller for at least 3 VBL.
*   - reset screen saver, enable LCD
*   - re-trigger the sleep timer. 
***************************************************************************
new_mousevec:move.w   #3,(countdownVBLTimer).l  ; reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            move.l    (cookie_blankScreenTimer).l,(blankScreenTimerCounter).l
            bclr      #3,(lcdPowerControlShadow).w ;LCD on
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l
            move.l    (cookie_sleepTimer).l,(sleepTimerCounter).l

            move.l    (old_mousevec).l,-(sp)
            rts

***************************************************************************
*   joystick vector patch
*   Any joystick activity:
*   - enables the transfer to the shadow controller for at least 3 VBL.
*   - reset screen saver, enable LCD
*   - re-trigger the sleep timer. 
***************************************************************************
new_joyvec: move.w    #3,(countdownVBLTimer).l  ; reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            move.l    (cookie_blankScreenTimer).l,(blankScreenTimerCounter).l
            bclr      #3,(lcdPowerControlShadow).w ;LCD on
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l
            move.l    (cookie_sleepTimer).l,(sleepTimerCounter).l

            move.l    (old_joyvec).l,-(sp)
            rts

***************************************************************************
*   keyboard driver patch
*   Any keyboard activity:
*   - enables the transfer to the shadow controller for at least 3 VBL.
*   - reset screen saver, enable LCD
*   - re-trigger the sleep timer. 
***************************************************************************
new_ikbd_neg4:move.w  #3,(countdownVBLTimer).l  ; reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            move.l    (cookie_blankScreenTimer).l,(blankScreenTimerCounter).l
            bclr      #3,(lcdPowerControlShadow).w ;LCD on
            move.b    (lcdPowerControlShadow).w,(lcdPowerControl).l
            move.l    (cookie_sleepTimer).l,(sleepTimerCounter).l

            move.l    (old_ikbd_neg4).l,-(sp)
            rts

***************************************************************************
*   MIDI vector patch
*   - any MIDI activity re-triggers the sleep timer.
***************************************************************************
new_midisysvec:move.l (cookie_sleepTimer).l,(sleepTimerCounter).l
            move.l    (old_midisysvec).l,-(sp)
            rts

***************************************************************************
*   AES/VDI patch
*   - #-1 enables the transfer to the shadow controller for at least 3 VBL.
*   - VDI calls except calls waiting for keyboard input also enable the
*     transfer to the shadow controller for at least 3 VBL.
*   - VDI v_opnwk() patches the mouse vector on the first call.
***************************************************************************
new_trap2:  cmp.w     #$73,d0                   ; VDI?
            bne       new_trap2b                ; (no)
            move.l    a0,-(sp)
            movea.l   d1,a0
            movea.l   (a0),a0
            move.w    (a0),(vdi_functionCode).l
            movea.l   (sp)+,a0
            cmpi.w    #$21,(vdi_functionCode).l ; vsin_mode() - Set input mode
            beq       new_trap2out
            cmpi.w    #$1f,(vdi_functionCode).l ; vrq_string() / vsm_string() - keyboard string input
            beq       new_trap2out
            cmpi.w    #$80,(vdi_functionCode).l ; vq_key_s() - Get shift key status
            beq       new_trap2out
            move.l    #trap_vdi_tailPatch,-(sp)
            move.w    4(sp),-(sp)
            ori.w     #$2000,(sp)
            move.l    (oldtrap_aesvdi).l,-(sp)
            rts

new_trap2b: cmp.w     #$ffff,d0
            bne       new_trap2out
            move.l    #trap_aesvdi_m1,d0
            rte

new_trap2out:move.l   (oldtrap_aesvdi).l,-(sp)
            rts

trap_vdi_tailPatch:move.w #3,(countdownVBLTimer).l ;reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            cmpi.w    #1,(vdi_functionCode).l   ; v_opnwk()
            beq       trap_aesvdi_tailPatch2
            rte

trap_aesvdi_tailPatch2:bset #0,(v_opnwk_called).l
            bne       trap_aesvdi_tailPatch3
            move.l    a1,-(sp)
            movea.l   (old_mousevecAddr).l,a1
            move.l    (a1),(old_mousevec).l
            move.l    #new_mousevec,(a1)
            movea.l   (sp)+,a1
trap_aesvdi_tailPatch3:rte

trap_aesvdi_m1:movea.l (sav_screendrvfuncptr).l,a0
            jsr       (a0)
            move.w    #3,(countdownVBLTimer).l  ; reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            rts

***************************************************************************
*   BIOS patch
*   - Bconout() enables the transfer to the shadow controller
*     for at least 3 VBL.
*   - Bconin(), Rwabs() and Getbpb() re-trigger the sleep timer.
***************************************************************************
new_bios:   move      usp,a0                    ; USP stack is active
            btst      #5,(sp)                   ; called from user mode?
            beq       new_bios2                 ; yes ->
            movea.l   sp,a0                     ; SSP stack is active
            addq.l    #6,a0
new_bios2:  move.w    (a0),d0                   ; bios function number
            cmp.w     #3,d0                     ; Bconout()
            beq       new_bios_bconout
            cmp.w     #2,d0                     ; Bconin()
            beq       new_bios_updatetimer
            cmp.w     #4,d0                     ; Rwabs()
            beq       new_bios_updatetimer
            cmp.w     #7,d0                     ; Getbpb()
            beq       new_bios_updatetimer
new_bios_old:movea.l  (oldtrap_bios).l,a0
            jmp       (a0)

new_bios_updatetimer:move.l (cookie_sleepTimer).l,(sleepTimerCounter).l
            bra.s     new_bios_old

new_bios_bconout:move.l (cookie_sleepTimer).l,(sleepTimerCounter).l
            move.l    2(a0),-(sp)
            move.w    #3,-(sp)                  ; Bconout()
            pea       new_bios_tailPatch(pc)
            move.w    $a(sp),-(sp)
            ori.w     #$2000,(sp)
            movea.l   (oldtrap_bios).l,a0
            jmp       (a0)

new_bios_tailPatch:move.w #3,(countdownVBLTimer).l ;reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            addq.l    #6,sp
            rte

***************************************************************************
*   XBIOS patch
*   - Setscreen() and Vsync() enable the transfer to the shadow controller
*     for at least 3 VBL.
*   - Midiws(), Flopver() and DMAread()/DMAwrite() re-trigger the
*     sleep timer.
***************************************************************************
new_xbios:  move      usp,a0                    ; USP stack is active
            btst      #5,(sp)                   ; called from user mode?
            beq       new_xbios2                ; yes ->
            lea       6(sp),a0                  ; SSP stack is active
new_xbios2: move.w    (a0),d0                   ; xbios function number
            cmp.w     #5,d0                     ; Setscreen()
            beq       new_xbios_setscreen
            cmp.w     #$25,d0                   ; Vsync()
            beq       new_xbios4
            cmp.w     #8,d0                     ; Floprd()
            blt       new_xbios_old
            cmp.w     #$c,d0                    ; Midiws()
            ble       new_xbios_updatetimer
            cmp.w     #$13,d0                   ; Flopver()
            beq       new_xbios_updatetimer
            cmp.w     #$2a,d0                   ; DMAread()
            beq       new_xbios_updatetimer
            cmp.w     #$2b,d0                   ; DMAwrite()
            beq       new_xbios_updatetimer
new_xbios_old:movea.l (oldtrap_xbios).l,a0
            jmp       (a0)

new_xbios_updatetimer:move.l (cookie_sleepTimer).l,(sleepTimerCounter).l
            bra.s     new_xbios_old

new_xbios_setscreen:move.w $a(a0),-(sp)         ; mode
            move.l    6(a0),-(sp)               ; physbase
            move.l    2(a0),-(sp)               ; logbase
            move.w    #5,-(sp)                  ; # Setscreen()
            pea       new_xbios_tailPatch(pc)
            move.w    $10(sp),-(sp)
            ori.w     #$2000,(sp)
            movea.l   (oldtrap_xbios).l,a0
            jmp       (a0)

new_xbios_tailPatch:move.w #3,(countdownVBLTimer).l ;reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            adda.l    #$c,sp                    ; remove parameters and function number from the stack
            rte

new_xbios4: move.w    #3,-(sp)                  ; # Logbase()
            pea       new_xbios7(pc)            ; push return address onto stack
            move.w    6(sp),-(sp)               ; push SR onto stack (for a rte)
            ori.w     #$2000,(sp)               ; set supervisor mode bit
            movea.l   (oldtrap_xbios).l,a0
            jmp       (a0)

new_xbios7: move.w    #3,(countdownVBLTimer).l  ; reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            addq.l    #2,sp                     ; remove function number from the stack
            rte

***************************************************************************
*   line A patch
*   After every line A operation the video transfer to the shadow controller
*   is enabled and kept on for at least 3 VBLs. Without this the video
*   changes would not be visible to the user.
***************************************************************************
new_lineA:  movea.l   2(sp),a1                  ; PC which points to the current opcode
            move.w    (a1),d2                   ; actual line-A opcode
            and.w     #$fff,d2                  ; mask our the upper 4 bit ($Axxx)
            addq.l    #2,a1                     ; increment PC by 2 (behind the lineA opcode)
            move.l    a1,2(sp)                  ; write back new PC
            cmp.w     #$f,d2                    ; lineA range 0..15
            bhi       new_lineA2                ; out of range...punt
            lsl.w     #2,d2                     ; pointer into lineA jump table
            movea.l   new_lineA_FuncPtr(pc,d2.w),a1
            movem.l   d3-d7/a3-a5,-(sp)         ; save all registers
            jsr       (a1)                      ; call our lineA handler
            movem.l   (sp)+,d3-d7/a3-a5         ; restore all registers
new_lineA2: rte                                 ; return from interrupt

new_lineA_FuncPtr:
            DC.L lineA_0,lineA_1,lineA_2,lineA_3,lineA_4,lineA_5,lineA_6,lineA_7
            DC.L lineA_8,lineA_9,lineA_a,lineA_b,lineA_c,lineA_d,lineA_e,lineA_f

lineA_0:    movea.l   (lineA_variables).l,a0
            move.l    a0,d0
            movea.l   (lineA_fontHeaders).l,a1
            lea       (new_lineA_FuncPtr).l,a2
            rts

lineA_1:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4).l,-(sp)
            rts

lineA_2:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*2).l,-(sp)
            rts

lineA_3:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*3).l,-(sp)
            rts

lineA_4:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*4).l,-(sp)
            rts

lineA_5:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*5).l,-(sp)
            rts

lineA_6:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*6).l,-(sp)
            rts

lineA_7:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*7).l,-(sp)
            rts

lineA_8:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*8).l,-(sp)
            rts

lineA_9:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*9).l,-(sp)
            rts

lineA_a:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*10).l,-(sp)
            rts

lineA_b:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*11).l,-(sp)
            rts

lineA_c:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*12).l,-(sp)
            rts

lineA_d:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*13).l,-(sp)
            rts

lineA_e:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*14).l,-(sp)
            rts

lineA_f:    pea       lineA_tailPatch(pc)
            move.l    (sav_lineA+4*15).l,-(sp)
            rts

lineA_tailPatch:move.w #3,(countdownVBLTimer).l ; reset VBL timer
            bclr      #0,(synmod+1).l           ; enable combo -> shadow controller video transfer
            rts

***************************************************************************
*
***************************************************************************
infoText:           DC.B '**********************************\r\n'
                    DC.B 'Shutdown   version 1.0   Oct 17 91\r\n'
                    DC.B 'Copyright  1991 Atari Corporation\r\n'
                    DC.B 'All Rights Reserved\r\n'
                    DC.B '**********************************\r\n'
                    DC.B 0
oldcookieresvector: DS.L 1
oldcookieresvalid:  DS.L 1
saveOrRestoreFlag:  DS.B 1
oldresvalid:        DS.L 1
oldresvector:       DS.L 1
sav_v_bas_h:        DS.B 1
sav_v_bas_m:        DS.B 1
sav_v_bas_l:        DS.B 1
sav_synmod:         DS.B 1
sav_palette0:       DS.W 1
sav_v_shf_mod:      DS.B 1
save_psg:           DS.B 16
saveRegs:           DS.L 16
oldSSP:             DS.L 1
oldUSP:             DS.L 1
sav_MFP:            DS.B 18
sav_halftone:       DS.W 31
sav_screendrvfuncptr:DS.L 1
cookiePtr:          DS.L 1
old_mousevec:       DS.L 1
old_mousevecAddr:   DS.L 1
old_joyvec:         DS.L 1
old_ikbd_neg4:      DS.L 1
old_midisysvec:     DS.L 1
lineA_variables:    DS.L 1
lineA_fontHeaders:  DS.L 1
countdownVBLTimer:  DS.W 1
vdi_functionCode:   DS.W 1
ahdiDetected:       DS.B 1
ahdiStandbyTime:    DS.B 1
blankScreenTimerCounter:DS.L 1
sleepTimerCounter:  DS.L 1
cookie_blankScreenTimer:DS.L 1
cookie_sleepTimer:  DS.L 1
cookie_videoPowerSaverFlag:DS.W 1
cookie_watchSerialPortFlag:DS.W 1
old_evt_timer:      DS.L 1
oldtrap_aesvdi:     DS.L 1
oldtrap_bios:       DS.L 1
oldtrap_xbios:      DS.L 1
old_lineA:          DS.L 1
sav_lineA:          DS.L 16
v_opnwk_called:     DS.B 1
