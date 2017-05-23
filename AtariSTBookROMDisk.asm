***
*  Half the ST Book ROM contains a ROM disk, which is mapped as drive 'P' into GEMDOS
***
$e40000 : abcd ef42                STBOOK_EXTROM:DC.L    $abcdef42

$e40004 : 00e4 0200                CA_NEXT:    DC.L      CA2_NEXT                   ; pointer to next application
$e40008 : 08e4 0026                CA_INIT:    DC.L      ROMDISK_CODE
$e4000c : 00e4 0026                CA_RUN:     DC.L      ROMDISK_CODE
$e40010 : 1776                     CA_TIME:    DC.W      $1776
$e40012 : 69b6                     CA_DATE:    DC.W      $69b6
$e40014 : 0000 016c                CA_SIZE:    DC.L      $0000016c
$e40018 : 424f 4f4b 524f 4d2e 5052+CA_NAME:    DC.B      'BOOKROM.PRG',0
$e40024 : ffff                                 DC.W      $ffff

$e40026 : 601a                     ROMDISK_CODE:bra.s    ROMDISK_INIT              ; PRG_magic 
$e40028 : 0000 014c                            DC.L      $0000014c                 ; PRG_tsize
$e4002c : 0000 0000                            DC.L      $00000000                 ; PRG_dsize
$e40030 : 0000 0000                            DC.L      $00000000                 ; PRG_bsize
$e40034 : 0000 0000                            DC.L      $00000000                 ; PRG_ssize
$e40038 : 0000 0000                            DC.L      $00000000                 ; PRG_res1
$e4003c : 0000 0000                            DC.L      $00000000                 ; PRGFLAGS
$e40040 : 0000                                 DC.W      $0000                     ; ABSFLAG

$e40042 : 303c 00fc                ROMDISK_INIT:move.w   #252,d0                   ; 252kb ROMDISK 
$e40046 : 31fc 0200 0800                       move.w    #512,(ROMDISK_BPB).w      ; 512 bytes per sector 
$e4004c : 31fc 0001 0802                       move.w    #1,(ROMDISK_BPB+2).w      ; 1 sector per cluster 
$e40052 : 31fc 0200 0804                       move.w    #512,(ROMDISK_BPB+4).w    ; 512 bytes per cluster 
$e40058 : 31fc 0001 0806                       move.w    #1,(ROMDISK_BPB+6).w      ; sector length of root directory 
$e4005e : 31fc 0001 0810                       move.w    #1,(ROMDISK_BPB+16).w     ; 16 bit FAT 
$e40064 : e340                                 asl.w     #1,d0
$e40066 : 3200                                 move.w    d0,d1
$e40068 : 31c1 080e                            move.w    d1,(ROMDISK_BPB+14).w     ; 504 clusters per disk 
$e4006c : d27c 00ff                            add.w     #$ff,d1
$e40070 : e041                                 asr.w     #8,d1
$e40072 : 31c1 0808                            move.w    d1,(ROMDISK_BPB+8).w      ; 2 sectors per FAT 
$e40076 : 31c1 080a                            move.w    d1,(ROMDISK_BPB+10).w     ; starting sector of second FAT = 2 
$e4007a : 31c1 080c                            move.w    d1,(ROMDISK_BPB+12).w     ; starting sector of data = 2*2+1 = 5 
$e4007e : d378 080c                            add.w     d1,(ROMDISK_BPB+12).w
$e40082 : 0678 0001 080c                       addi.w    #1,(ROMDISK_BPB+12).w

$e40088 : 303c 000f                            move.w    #15,d0                    ; ROMDISK is drive P 
$e4008c : 7e01                                 moveq     #1,d7
$e4008e : e1af                                 lsl.l     d0,d7
$e40090 : 8fb8 04c2                            or.l      d7,(_drvbits).w           ; enable drive 

$e40094 : 21f8 0472 0816                       move.l    (hdv_bpb).w,(ROMDISK_HDV_BPB).w
$e4009a : 21f8 0476 081a                       move.l    (hdv_rw).w,(ROMDISK_HDV_RW).w
$e400a0 : 21f8 047e 081e                       move.l    (hdv_mediach).w,(ROMDISK_HDV_MEDIACH).w

$e400a6 : 41fa 0024                            lea       ROMDISK_BPB(pc),a0
$e400aa : 21c8 0472                            move.l    a0,(hdv_bpb).w
$e400ae : 41fa 002c                            lea       ROMDISK_RW(pc),a0
$e400b2 : 21c8 0476                            move.l    a0,(hdv_rw).w
$e400b6 : 41fa 0034                            lea       ROMDISK_MEDIACH(pc),a0
$e400ba : 21c8 047e                            move.l    a0,(hdv_mediach).w

$e400be : 487a 00ae                            pea       ROMDISK_insttext(pc)
$e400c2 : 3f3c 0009                            move.w    #9,-(sp)
$e400c6 : 4e41                                 trap      #1
$e400c8 : 5c4f                                 addq.w    #6,sp
$e400ca : 4e75                                 rts       

$e400cc : 302f 0004                ROMDISK_BPB:move.w    4(sp),d0
$e400d0 : 2078 0816                            movea.l   (ROMDISK_HDV_BPB).w,a0
$e400d4 : 43fa 002e                            lea       ROMDISK_BPB_IMP(pc),a1
$e400d8 : 6000 001e                            bra       ROMDISK_CHKDRV
$e400dc : 302f 000e                ROMDISK_RW: move.w    $e(sp),d0
$e400e0 : 2078 081a                            movea.l   (ROMDISK_HDV_RW).w,a0
$e400e4 : 43fa 0026                            lea       ROMDISK_RW_IMP(pc),a1
$e400e8 : 6000 000e                            bra       ROMDISK_CHKDRV
$e400ec : 302f 0004                ROMDISK_MEDIACH:move.w 4(sp),d0
$e400f0 : 2078 081e                            movea.l   (ROMDISK_HDV_MEDIACH).w,a0
$e400f4 : 43fa 0074                            lea       ROMDISK_MEDIACH_IMP(pc),a1
$e400f8 : b07c 000f                ROMDISK_CHKDRV:cmp.w  #15,d0                    ; ROMDISK is drive P 
$e400fc : 6600 0004                            bne       ROMDISK_CHKDRVx
$e40100 : 2049                                 movea.l   a1,a0
$e40102 : 4ed0                     ROMDISK_CHKDRVx:jmp   (a0)

$e40104 : 203c 0000 0800           ROMDISK_BPB_IMP:move.l #ROMDISK_BPB,d0          ; return ROMDISK_BPB 
$e4010a : 4e75                                 rts       

$e4010c : 207c 00e4 0222           ROMDISK_RW_IMP:movea.l #ROMDISK_BASE,a0
$e40112 : 7000                                 moveq     #0,d0
$e40114 : 302f 000c                            move.w    $c(sp),d0                 ; start sector number 
$e40118 : e188                                 lsl.l     #8,d0
$e4011a : e388                                 lsl.l     #1,d0
$e4011c : d1c0                                 adda.l    d0,a0
$e4011e : 7400                                 moveq     #0,d2
$e40120 : 342f 000a                            move.w    $a(sp),d2                 ; number of sectors 
$e40124 : e98a                                 lsl.l     #4,d2
$e40126 : 226f 0006                            movea.l   6(sp),a1                  ; destination buffer 
$e4012a : 2009                                 move.l    a1,d0
$e4012c : 082f 0000 0005                       btst      #0,5(sp)                  ; write access? That will fail! 
$e40132 : 6704                                 beq.s     ROMDISK_RW_IMP2
$e40134 : 70f3                                 moveq     #-13,d0
$e40136 : 4e75                                 rts       

$e40138 : 0800 0000                ROMDISK_RW_IMP2:btst  #0,d0                     ; the destination has to be an even address for fast copying 
$e4013c : 6618                                 bne.s     ROMDISK_RW_IMP4
$e4013e : 22d8                     ROMDISK_RW_IMP3:move.l (a0)+,(a1)+
$e40140 : 22d8                                 move.l    (a0)+,(a1)+
$e40142 : 22d8                                 move.l    (a0)+,(a1)+
$e40144 : 22d8                                 move.l    (a0)+,(a1)+               ; copy 32 bytes 
$e40146 : 22d8                                 move.l    (a0)+,(a1)+
$e40148 : 22d8                                 move.l    (a0)+,(a1)+
$e4014a : 22d8                                 move.l    (a0)+,(a1)+
$e4014c : 22d8                                 move.l    (a0)+,(a1)+
$e4014e : 5382                                 subq.l    #1,d2
$e40150 : 6b14                                 bmi.s     ROMDISK_RW_IMP6
$e40152 : 66ea                                 bne.s     ROMDISK_RW_IMP3
$e40154 : 6010                                 bra.s     ROMDISK_RW_IMP6
$e40156 : e74a                     ROMDISK_RW_IMP4:lsl.w #3,d2
$e40158 : 12d8                     ROMDISK_RW_IMP5:move.b (a0)+,(a1)+
$e4015a : 12d8                                 move.b    (a0)+,(a1)+
$e4015c : 12d8                                 move.b    (a0)+,(a1)+
$e4015e : 12d8                                 move.b    (a0)+,(a1)+
$e40160 : 5382                                 subq.l    #1,d2
$e40162 : 6b02                                 bmi.s     ROMDISK_RW_IMP6
$e40164 : 66f2                                 bne.s     ROMDISK_RW_IMP5
$e40166 : 4280                     ROMDISK_RW_IMP6:clr.l d0                     ; no error
$e40168 : 4e75                                 rts       

$e4016a : 4280                     ROMDISK_MEDIACH_IMP:clr.l d0                 ; no error
$e4016c : 4e75                                 rts       

$e4016e : 3032 3533 4b20 524f 4d44+ROMDISK_insttext:DC.B '0253K ROMDISK installed as P:\r\n',0
$e4018e : 0000 0000 ffff ffff                  DC.L      0          ; no relocation table
$e40196 : ffff ffff ffff ffff                  DCB.B     110,$ff    ; fill the 512-page to the end with $FF

$e40200 : 0000 0000                CA2_NEXT:   DC.L      $000000
$e40204 : 00e4 0222                CA2_INIT:   DC.L      ROMDISK_BASE
$e40208 : 00e4 0222                CA2_RUN:    DC.L      ROMDISK_BASE
$e4020c : 1869                     CA2_TIME:   DC.W      $1869
$e4020e : 99c6                     CA2_DATE:   DC.W      $99c6
$e40210 : 0003 fa00                CA2_SIZE:   DC.L      509 * 512
$e40214 : 4341 5254 524f 4d2e 494d+CA2_NAME:   DC.B      'CARTROM.IMG',0
$e40220 : ffff                                 DC.W      $ffff

$e40222 : 0000 0000 ffff 0400      ROMDISK_BASE:DC.B     $00,$00,$00,$00,$ff,$ff,$04,$00 ;509 * 512-byte sectors
$e4022a : 0500 0600 0700 0800                  DC.B      $05,$00,$06,$00,$07,$00,$08,$00
$e40232 : 0900 0a00 0b00 ffff                  DC.B      $09,$00,$0a,$00,$0b,$00,$ff,$ff
$e4023a : 0d00 0e00 0f00 1000                  DC.B      $0d,$00,$0e,$00,$0f,$00,$10,$00
$e40242 : 1100 1200 1300 1400                  DC.B      $11,$00,$12,$00,$13,$00,$14,$00
$e4024a : 1500 1600 1700 1800                  DC.B      $15,$00,$16,$00,$17,$00,$18,$00
$e40252 : 1900 1a00 1b00 1c00                  DC.B      $19,$00,$1a,$00,$1b,$00,$1c,$00
$e4025a : 1d00 1e00 1f00 2000                  DC.B      $1d,$00,$1e,$00,$1f,$00,$20,$00
$e40262 : 2100 2200 2300 2400                  DC.B      $21,$00,$22,$00,$23,$00,$24,$00
$e4026a : 2500 2600 2700 2800                  DC.B      $25,$00,$26,$00,$27,$00,$28,$00
...
