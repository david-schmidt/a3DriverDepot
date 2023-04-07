; Target assembler: cc65 v2.18.0 [--target none -C df1.driver_code_0x2000_cc65.cfg]
; 6502bench SourceGen v1.8.1
         .setcpu "6502"
XREQCODE =       $20
XCTLCODE =       $21
XBADOP   =       $26
XNODRIVE =       $28
XBYTECNT =       $2c
XBLKNUM  =       $2d

SIR_Len  =       $05
ReqCode  =       $c0
SOS_Unit =       $c1
CtlStat  =       $c2
SosBuf   =       $c2
CSList   =       $c3
ReqCnt   =       $c4
SosBlk   =       $c6
QtyRead  =       $c8
AllocSIR =       $1913
SysErr   =       $1928
MOTOROFF =       $c088
Q6L      =       $c08c

;   UNIFILE / DUOFILE SOS Driver
;  
;   Device identification Block (DIB) #0
;  
         .org    $2000
DIB0:    .word   DIB1       ;Link pointer to next DIB
         .word   Entry      ;Entry pointer
         .byte   $04        ;Name length byte
         .byte   ".DF1"     ;Device name
         .res    11,$00
         .byte   $80        ;Active, no page alignment
DIB0_Slot:
         .byte   $02        ;Slot number
         .byte   $00        ;Unit number
         .byte   $f1        ;Type
         .byte   $03        ;Subtype
         .byte   $00        ;Filler
DIB0_Blks:
         .word   $06a6      ;# Blocks in device
         .word   $0001      ;Manufacturer
         .word   $100a      ;Driver version
         .word   $0003      ;DCB length followed by DCB
L2022:   .byte   $00
L2023:   .byte   $ff
         .byte   $02
; 
;   Device identification Block (DIB) #1
; 
DIB1:    .word   $0000      ;Link pointer to next DIB
         .word   Entry      ;Entry pointer
         .byte   $04        ;Name length byte
         .byte   ".DF2"     ;Device name
         .res    12,$00
         .byte   $02        ;Slot number
         .byte   $01        ;Unit number
         .byte   $f1        ;Type
         .byte   $03        ;Subtype
         .byte   $00        ;Filler
         .word   $06a6      ;# Blocks in device
         .word   $0001      ;Manufacturer
         .word   $100a      ;Driver version
         .word   $0000      ;DCB length followed by DCB
;  
         .byte   "(C) APPLE COMPUTER INC. 1982"
;  
SIR_Addr:
         .word   $2065
Var01:   .byte   $10
Var02:   .byte   $00
Var03:   .byte   $00
Var04:   .byte   $00
Var05:   .byte   $00
Var06:   .byte   $00
Var07:   .byte   $00
Var08:   .byte   $00
Var09:   .byte   $00
Var0A:   .byte   $00
Var0B:   .byte   $00
Var0C:   .byte   $00
Var0D:   .byte   $00
Var0E:   .byte   $00
Var0F:   .byte   $00
VarTbl1: .res    5,$00
VarTbl2: .res    8,$00
InitStatus:
         .byte   $00
Var1E:   .byte   $06
Var1F:   .byte   $00
Var20:   .byte   $00
Var21:   .byte   $17

Entry:   lda     ReqCode
         cmp     #$08
         beq     @L2091
         lda     Var06
         bne     L20BB
@L2091:  lda     ReqCode
         cmp     #$09
         beq     @L209A
         sta     VarTbl1+1
@L209A:  jsr     @L20A8
         lda     #$96
         jsr     L230A
         lda     #$00
         jsr     $1922
         rts

@L20A8:  lda     ReqCode
         cmp     #$0a
         bcs     BadReq
         asl     A
         tay
         lda     DoTable+1,y
         pha
         lda     DoTable,y
         pha
         rts

BadReq:  lda     #XREQCODE  ;Bad request code!
L20BB:   jsr     SysErr
; 
BadOp:   lda     #XBADOP    ;Invalid operation!
         jsr     SysErr
; 
         lda     #XNODRIVE
         jsr     SysErr
; 
; 
DoTable: .word   DRead-1    ;0 Read request
         .word   DWrite-1   ;1 Write request
         .word   DStatus-1  ;2 Status request
         .word   DControl-1 ;3 Control request
         .word   BadReq-1   ;4 Unused
         .word   BadReq-1   ;5 Unused
         .word   BadOp-1    ;6 Unused
         .word   BadOp-1    ;7 Unused
         .word   DInit-1    ;8 Init request
         .word   DRepeat-1  ;9 Repeat last read or write request

; 
; D_INIT call processing
; 
DInit:   bit     InitStatus
         bmi     @L2121
         dec     InitStatus
         lda     DIB0_Slot
         clc
         adc     Var01
         sta     Var01
         lda     #SIR_Len
         ldx     SIR_Addr
         ldy     SIR_Addr+1
         jsr     AllocSIR
         bcs     @L2126
         lda     DIB0_Slot
         asl     A
         asl     A
         asl     A
         asl     A
         sta     Var07
         lda     #$01
         jsr     L22DB
         bcc     @L2116
         jsr     DCtrl00
         lda     #$01
         jsr     L22DB
         bcs     @L2122
@L2116:  lda     $ca28
         cmp     #$d5
         beq     @L2121
         cmp     #$ad
         bne     @L2122
@L2121:  rts

@L2122:  lda     #XNODRIVE  ;Drive not connected
         bne     @L2128

@L2126:  lda     #$25       ;Resource not available
@L2128:  sta     Var06
         jsr     SysErr
; 
; D_READ call processing
; 
DRead:   lda     #$00       ;Zero # bytes read
         tay
         sta     (QtyRead),y
         iny
         sta     (QtyRead),y
         jsr     L21DF
L2139:   jsr     L2224
         jsr     L22D8
         bcs     IO_Error
L2141:   lda     #$08
         jsr     L2325
         lda     #$aa
         jsr     L230A
         jsr     L22D8
         bcs     IO_Error
L2150:   jsr     L2241
         beq     @L2165
         bcc     @L215A
         jsr     SysErr
@L215A:  dec     Var08
         ldx     Var08
         bpl     L2141
         jsr     SysErr
@L2165:  ldx     #$7f
         ldy     #$00
@L2169:  lda     $c980,x
         sta     ($ca),y
         lda     $c900,x
         sta     ($cc),y
         lda     $c880,x
         sta     ($ce),y
         lda     $c800,x
         sta     ($d0),y
         dex
         iny
         bpl     @L2169
         lda     #$02
         ldy     #$01
         adc     (QtyRead),y
         sta     (QtyRead),y
         jsr     L220B
         bne     L2139
         rts

IO_Error:
         lda     #$27       ;I/O error
         jsr     SysErr
; 
; D_WRITE call processing
; 
DWrite:  jsr     L21DF
@L2197:  jsr     L22D8
         bcs     IO_Error
         lda     #$05
         jsr     L2325
         ldy     #$7f
@L21A3:  lda     ($ca),y
         sta     $c80c,y
         lda     ($cc),y
         sta     $c88c,y
         lda     ($ce),y
         sta     $c90c,y
         lda     ($d0),y
         sta     $c98c,y
         dey
         bpl     @L21A3
         lda     #$aa
         jsr     L230A
         jsr     L22D8
         bcs     IO_Error
         jsr     L2241
         beq     @L21D9
         bcc     @L21CE
         jsr     SysErr
@L21CE:  dec     Var08
         ldx     Var08
         bpl     @L2197
         jsr     SysErr
@L21D9:  jsr     L220B
         bne     @L2197
         rts

L21DF:   lda     SOS_Unit
         sta     VarTbl1
         lda     L2022
         sta     Var08
         lda     ReqCnt
         bne     @L21F5
         lda     $c5
         bne     @L21F5
@L21F2:  jmp     SysErr

@L21F5:  lda     SosBlk
         cmp     DIB0_Blks
         lda     SosBlk+1
         sbc     DIB0_Blks+1
         lda     #XBLKNUM   ;Block number too large
         bcs     @L21F2
         jsr     L227C
         lda     #XBYTECNT  ;Byte count not a multiple of 512
         bcc     @L21F2
         rts

L220B:   ldx     #$06
@L220D:  inc     $cb,x
         inc     $cb,x
         dex
         dex
         bpl     @L220D
         inc     Var0C
         bne     @L221D
         inc     Var0D
@L221D:  jsr     L2224
         dec     Var09
         rts

L2224:   lda     $cb
         cmp     #$82
         bcc     @L2240
         ldx     $14cb
         bpl     @L2240
         cpx     #$8f
         bcs     @L2240
         ldx     #$08
@L2235:  inc     $14c9,x
         asl     $c9,x
         lsr     $c9,x
         dex
         dex
         bne     @L2235
@L2240:  rts

L2241:   ldx     #$06
@L2243:  lda     $ca29,x
         sta     VarTbl2,x
         dex
         bpl     @L2243
         sec
         lda     $ca29
         bpl     @L226D
         and     #$70
         sta     Var0E
         ldx     #$05
@L2259:  lda     @L2276,x
         bit     Var0E
         bne     @L2268
         dex
         dex
         bpl     @L2259
@L2265:  lda     #$27
         rts

@L2268:  dex
         lda     @L2276,x
         rts

@L226D:  lda     $ca2a
         clc
         bmi     @L2265
         lda     #$00
         rts

@L2276:  rol     DCtrl04+2
         jsr     $402b
L227C:   lda     ReqCnt
         bne     @L22D6
         lda     $c5
         lsr     A
         bcs     @L22D6
         sta     Var09
         clc
         adc     SosBlk
         sta     Var0E
         lda     #$00
         adc     SosBlk+1
         sta     Var0F
         lda     DIB0_Blks
         cmp     Var0E
         lda     DIB0_Blks+1
         sbc     Var0F
         bcc     @L22D6
         lda     SosBlk
         sta     Var0C
         lda     SosBlk+1
         sta     Var0D
; Sets up 4 zero page pointers ca, cc, ce, d0 to be able to copy the 512 bytes
; of data in four 128 byte chunks
         lda     SosBuf
         ldx     SosBuf+1
         sta     $ca        ;set up buffer pointer one
         stx     $cb
         ldy     #$02       ;now set up 3 more pointers, in $80 increments
@L22B7:  clc
         adc     #$80
         sta     $00ca,y
         bcc     @L22C0
         inx
@L22C0:  stx     $cb,y
         iny
         iny
         cpy     #$08
         bne     @L22B7
         lda     $14c3      ;set up the xbyte for the pointers
         ldx     #$06
@L22CD:  sta     $14cb,x
         dex
         dex
         bpl     @L22CD
         sec
         rts

@L22D6:  clc
         rts

L22D8:   lda     L2023
L22DB:   sta     Var0B
@L22DE:  jsr     @L22E9
         bcc     @L22E8
         dec     Var0B
         bne     @L22DE
@L22E8:  rts

@L22E9:  lda     #$a0
         sta     Var0A
         ldy     #$00
         ldx     Var07
@L22F3:  lda     Q6L,x
         bmi     @L2302
         dey
         bne     @L22F3
         dec     Var0A
         bne     @L22F3
         sec
         rts

@L2302:  clc
         lda     DIB0_Slot
         jsr     $1922
         rts

L230A:   sta     $ca28
         ldx     Var07
         ldy     #$00
@L2312:  lda     MOTOROFF,x
         bpl     @L231C
         dey
         bne     @L2312
         sec
         rts

@L231C:  clc
         rts

; 
; control code $00
; 
DCtrl00: ldx     Var07
         lda     $c084,x
         rts

L2325:   tay
         sta     $ca20
         lda     Var1E
         sta     $ca24
         lda     Var1F
         sta     $ca25
         lda     SOS_Unit
         sta     $ca23
         lda     Var0C
         sta     $ca22
         lda     Var0D
         sta     $ca21
         lda     Var21
         sta     $ca26
         lda     Var20
         sta     $ca27
         cpy     #$06
         bne     @L236B
         lda     #$00
         sta     $ca24
         lda     #$13
         sta     $ca26
         lda     #$00
         sta     $ca27
         ldx     DIB1-1
         stx     $ca21
@L236B:  cpy     #$03
         bne     @L237C
         ldy     #$00
         lda     (CSList),y
         sta     $ca26
         iny
         lda     (CSList),y
         sta     $ca27
@L237C:  rts

; 
; D_STATUS call processing
; 
DStatus: ldx     #$01
         cpx     CtlStat
         beq     DStat01
         dex
         cpx     CtlStat
         beq     DStat00
         dex
         dex
         cpx     CtlStat
         beq     DStatFE
         lda     #XCTLCODE  ;Invalid control/status code
         jsr     SysErr
DStat00: jsr     L22D8
         bcc     @L239B
@L2398:  jmp     IO_Error

@L239B:  lda     #$01
         jsr     L2325
         lda     #$aa
         jsr     L230A
         jsr     L22D8
         bcs     @L2398
         lda     $ca29
         sta     Var0E
         lda     #$40
         ldy     #$05
@L23B4:  lsr     Var0E
         bcc     @L23BC
         eor     @L23C2-1,y
@L23BC:  dey
         bne     @L23B4
         sta     (CSList),y
         rts

@L23C2:  .byte   $04
         .byte   $20
         .byte   $40
         .byte   $00
         .byte   $02

; 
; status code $01
; 
DStat01: ldy     #$06
@L23C9:  lda     VarTbl2,y
         sta     (CSList),y
         dey
         bpl     @L23C9
         rts

; 
; status code fe
; 
DStatFE: ldy     #$00
         lda     #$ff
         sta     (CSList),y
         iny
         sta     (CSList),y
         rts

; 
; D_REPEAT - repeat the last D_READ or D_WRITE call
; 
DRepeat: lda     VarTbl1
         cmp     SOS_Unit
         bne     @L23F4
         lda     VarTbl1+1
         sta     ReqCode
         bne     @L23ED
         jmp     DRead

@L23ED:  cmp     #$02
         bcs     @L23F4
         jmp     DWrite

@L23F4:  jmp     BadOp

; 
; D_CONTROL call processing
; 
DControl:
         lda     CtlStat    ;CtlStat
         cmp     #$fe       ;Format the device
         beq     CtrlFE
         lda     CtlStat
         cmp     #$05
         bcs     BadCtl
         asl     A
         tay
         lda     CtrlDispatch+1,y
         pha
         lda     CtrlDispatch,y
         pha
         rts

BadCtl:  lda     #XCTLCODE
         jsr     SysErr
; 
CtrlDispatch:
         .word   DCtrl00-1
         .word   DCtrl01-1
         .word   DCtrl02-1
         .word   DCtrl03-1
         .word   DCtrl04-1
; 
; control code $fe
; format disk??
CtrlFE:  jsr     L22D8
         bcc     @L2425
@L2422:  jmp     IO_Error

@L2425:  ldy     #$00
@L2427:  lda     @L246D,y
         sta     $cb00,y
         lda     @L256D,y
         sta     $cc00,y
         lda     @L266D,y
         sta     $cd00,y
         lda     L276D,y
         sta     $ce00,y
         iny
         bne     @L2427
         lda     #$06
         jsr     L2325
         lda     #$aa
         jsr     L230A
         lda     #$00
         jsr     L22DB
         bcc     @L2460
         lda     #$00
         jsr     L22DB
         bcc     @L2460
         jsr     DCtrl00
         jmp     @L2422

@L2460:  lda     $ca29
         bmi     @L2466
         rts

@L2466:  cmp     #$90
         bcc     @L2422
         jmp     L2150

; maybe Z8 code to do the formatting of the disk
@L246D:  .byte   $8b,$04,$97,$01,$02,$00,$8f,$e6
         .byte   $39,$80,$e6,$04,$10,$e6,$05,$4b
         .byte   $d6,$00,$18,$31,$10,$b0,$12,$56
         .byte   $33,$01,$e6,$31,$3f,$e6,$04,$10
         .byte   $e6,$05,$4e,$d6,$00,$18,$d6,$13
         .byte   $ed,$d6,$13,$bd,$7b,$2a,$d6,$13
         .byte   $f3,$6b,$05,$d6,$13,$bc,$7b,$20
         .byte   $d6,$13,$59,$7b,$1b,$20,$31,$56
         .byte   $31,$3f,$a6,$31,$2e,$eb,$e7,$06
         .byte   $33,$02,$20,$12,$a6,$12,$02,$1b
         .byte   $c9,$e4,$1a,$3b,$b0,$39,$b0,$3a
         .byte   $af,$31,$10,$0c,$05,$e4,$1a,$0d
         .byte   $d6,$14,$0a,$d6,$13,$76,$fb,$0d
         .byte   $d6,$13,$76,$fb,$08,$d6,$13,$ec
         .byte   $31,$10,$0a,$ec,$df,$af,$31,$10
         .byte   $8c,$16,$38,$eb,$4c,$ff,$d6,$13
         .byte   $b3,$31,$10,$1c,$81,$7b,$1b,$98
         .byte   $0e,$92,$48,$31,$10,$3a,$ef,$9c
         .byte   $00,$38,$eb,$1c,$82,$82,$48,$a6
         .byte   $e4,$ff,$eb,$06,$a0,$e8,$3a,$f5
         .byte   $cf,$af,$31,$10,$38,$12,$e0,$e3
         .byte   $44,$31,$e3,$e4,$11,$39,$e4,$13
         .byte   $3a,$df,$af,$e6,$04,$10,$e6,$05
         .byte   $33,$8d,$00,$18,$af,$e6,$1a,$38
         .byte   $d6,$14,$0a,$d6,$13,$76,$31,$00
         .byte   $fb,$15,$d6,$13,$76,$31,$00,$fb
         .byte   $0e,$26,$1a,$02,$a6,$1a,$18,$9b
         .byte   $e7,$e6,$11,$85,$8d,$13,$a2,$26
         .byte   $1a,$02,$ac,$13,$bc,$05,$c8,$1a
         .byte   $92,$ca,$cf,$af,$af,$e4,$00,$1e
         .byte   $e6,$1b,$17,$31,$20,$e6,$04,$10
         .byte   $e6,$05,$3c,$d6,$00,$18,$a4,$1e
@L256D:  .byte   $45,$6b,$05,$e4,$45,$1e,$00,$1b
         .byte   $af,$af,$e4,$1a,$0d,$d6,$15,$02
         .byte   $d6,$15,$17,$31,$20,$e4,$1b,$07
         .byte   $56,$e4,$7f,$92,$48,$31,$00,$b0
         .byte   $f6,$46,$e3,$40,$46,$e0,$c0,$4c
         .byte   $01,$5c,$f4,$e6,$0a,$0a,$f0,$0c
         .byte   $2c,$ff,$d6,$14,$09,$80,$e4,$eb
         .byte   $f2,$a0,$e4,$80,$e4,$2c,$a9,$00
         .byte   $ea,$6b,$05,$44,$33,$33,$8b,$f1
         .byte   $ff,$f0,$0a,$f0,$0a,$4c,$0b,$82
         .byte   $ce,$28,$ec,$a0,$ee,$00,$e4,$6b
         .byte   $04,$f0,$e5,$8b,$f2,$e6,$04,$05
         .byte   $d6,$14,$fd,$44,$33,$33,$ff,$4c
         .byte   $04,$82,$ce,$28,$ec,$00,$e4,$6b
         .byte   $06,$a0,$ee,$f0,$05,$8b,$f2,$4c
         .byte   $02,$5c,$bf,$ff,$ff,$44,$33,$33
         .byte   $ff,$2c,$96,$e6,$0f,$82,$80,$e4
         .byte   $6b,$02,$8b,$f1,$f0,$05,$4c,$02
         .byte   $82,$ce,$28,$ec,$a0,$ee,$00,$e4
         .byte   $6b,$04,$f0,$05,$8b,$f2,$a0,$e8
         .byte   $82,$68,$e4,$06,$0b,$b4,$1f,$eb
         .byte   $b4,$12,$eb,$ac,$01,$c2,$ca,$ac
         .byte   $15,$bc,$7a,$92,$ca,$2c,$ff,$e6
         .byte   $0a,$01,$b8,$e6,$c2,$ca,$e6,$0a
         .byte   $15,$bc,$77,$92,$ca,$bc,$81,$92
         .byte   $ca,$2c,$ff,$48,$ed,$fc,$73,$d6
         .byte   $14,$f1,$f0,$ea,$d6,$14,$fd,$f0
         .byte   $04,$ff,$00,$e7,$ed,$14,$4d,$44
         .byte   $33,$33,$e6,$f6,$ff,$56,$e0,$3f
         .byte   $31,$20,$46,$e4,$80,$92,$48,$31
         .byte   $00,$af,$44,$33,$33,$44,$33,$33
         .byte   $f0,$05,$d6,$15,$01,$2c,$ff,$4a
@L266D:  .byte   $f1,$af,$e6,$04,$10,$e6,$05,$2a
         .byte   $d6,$00,$18,$e6,$04,$10,$e6,$05
         .byte   $5a,$d6,$00,$18,$31,$10,$af,$31
         .byte   $00,$78,$1b,$d6,$15,$4f,$ec,$15
         .byte   $fc,$76,$58,$31,$4c,$01,$c2,$c4
         .byte   $92,$ce,$fe,$ac,$96,$92,$ae,$fe
         .byte   $58,$12,$c2,$c4,$92,$ce,$58,$31
         .byte   $b6,$e5,$00,$59,$1f,$b4,$12,$e5
         .byte   $c2,$c4,$fc,$7a,$92,$ce,$fc,$81
         .byte   $92,$ae,$fc,$73,$ec,$15,$af,$a8
         .byte   $e7,$b8,$e7,$be,$d0,$eb,$9c,$00
         .byte   $8c,$16,$b0,$e5,$92,$58,$a0,$e8
         .byte   $02,$5b,$92,$58,$a0,$e8,$22,$5b
         .byte   $5e,$26,$ea,$02,$ab,$ee,$9c,$00
         .byte   $8c,$16,$af,$d5,$aa,$96,$ab,$ab
         .byte   $ab,$96,$ab,$de,$aa,$ff,$d5,$aa ;these look familiar for low level disk nibbles
         .byte   $ad,$ab,$de,$aa
; 
; control code $01
; 
DCtrl01: jsr     L22D8
         bcc     DCtrlOk
DCtrlErr:
         jmp     IO_Error

DCtrlOk: lda     #$03
         jsr     L2325
         lda     #$aa
         jsr     L230A
         jsr     L22D8
         bcs     DCtrlErr
         lda     $ca2a
         bmi     DCtrlErr
         rts

; 
; control code $04
; 
DCtrl04: jsr     L22D8
         bcs     DCtrlErr
         lda     #$02
         jsr     L2325
         ldy     #$00
         jsr     L278D
         stx     $d8
         sta     $d9
         lda     (CSList),y
         sta     $14d9
         iny
         jsr     L278D
         pha
         txa
         clc
         adc     $d8
         sta     VarTbl1+2
         pla
         adc     $d9
         sta     VarTbl1+3
         jsr     L278D
         stx     $da
         stx     $ca27
         sta     $ca26
         clc
         adc     #$b8
         sta     $db
         jsr     @L2756
         lda     #$aa
         jsr     L230A
@L2750:  jsr     L22D8
         bcs     @L2750
         rts

@L2756:  ldy     #$00
L2758:   lda     ($d8),y
         sta     ($da),y
         inc     $d8
         bne     @L2762
         inc     $d9
@L2762:  inc     $da
         bne     @L2768
         inc     $db
@L2768:  lda     $d8
         cmp     VarTbl1+2
L276D:   lda     $d9
         sbc     VarTbl1+3
         bcc     L2758
         rts

; 
; control code $02
; 
DCtrl02: ldy     #$00
         jsr     L278D
         stx     Var20
         sta     Var21
         rts

DCtrl03: ldy     #$00
         jsr     L278D
         stx     Var1E
         sta     Var1F
         rts

L278D:   lda     (CSList),y
         iny
         tax
         lda     (CSList),y
         iny
         rts

         .byte   $00
