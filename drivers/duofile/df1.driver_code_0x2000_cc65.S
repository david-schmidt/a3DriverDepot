; Target assembler: cc65 v2.18.0 [--target none -C df1.driver_code_0x2000_cc65.cfg]
; 6502bench SourceGen v1.8.1
         .setcpu "6502"
XREQCODE =       $20
XCTLCODE =       $21
XNORESRC =       $25
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
;   Device identification Block (DIB) #1
;  
         .org    $2000
DIB1:    .word   DIB2       ;Link pointer to next DIB
         .word   Entry      ;Entry pointer
         .byte   $04        ;Name length byte
         .byte   ".DF1"     ;Device name
         .res    11,$00
         .byte   $80        ;Active, no page alignment
DIB1_Slot:
         .byte   $01        ;Slot number
         .byte   $00        ;Unit number
         .byte   $f1        ;Type
         .byte   $03        ;Subtype
         .byte   $00        ;Filler
DIB1_Blks:
         .word   $06a6      ;# Blocks in device
         .word   $0001      ;Manufacturer
         .word   $101a      ;Driver version
         .word   $0005      ;DCB length followed by DCB
L2022:   .byte   $00
L2023:   .byte   $ff
L2024:   .byte   $02
L2025:   .byte   $06
         .byte   $00
; 
;   Device identification Block (DIB) #2
; 
DIB2:    .word   $0000      ;Link pointer to next DIB
         .word   Entry      ;Entry pointer
         .byte   $04        ;Name length byte
         .byte   ".DF2"     ;Device name
         .res    11,$00
         .byte   $80        ;Active, no page alignment
         .byte   $01        ;Slot number
         .byte   $01        ;Unit number
         .byte   $f1        ;Type
         .byte   $03        ;Subtype
         .byte   $00        ;Filler
         .word   $06a6      ;# Blocks in device
         .word   $0001      ;Manufacturer
         .word   $101a      ;Driver version
         .word   $0000      ;DCB length followed by DCB
; 
         .byte   "(C) APPLE COMPUTER INC. 1982"
; 
SIR_Addr:
         .byte   $67
         .byte   $20
         .byte   $10
         .byte   $00
         .byte   $00
         .byte   $00
         .byte   $00
L206C:   .byte   $00
L206D:   .byte   $00
L206E:   .byte   $00
L206F:   .byte   $00
L2070:   .byte   $00
L2071:   .byte   $00
L2072:   .byte   $00
L2073:   .byte   $00
L2074:   .byte   $00
L2075:   .byte   $00
L2076:   .byte   $00
L2077:   .byte   $00
L2078:   .byte   $00
L2079:   .byte   $00
         .byte   $00
L207B:   .res    8,$00
L2083:   .byte   $00
L2084:   .byte   $00
L2085:   .byte   $00
L2086:   .byte   $00
         .byte   $17

Entry:   lda     ReqCode
         cmp     #$08
         beq     @L2093
         lda     L206C
         bne     L20BD
@L2093:  lda     ReqCode
         cmp     #$09
         beq     @L209C
         sta     L2077
@L209C:  jsr     @L20AA
         lda     #$96
         jsr     L230F
         lda     #$00
         jsr     $1922
         rts

@L20AA:  lda     ReqCode
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
L20BD:   jsr     SysErr
; 
BadOp:   lda     #XBADOP    ;Invalid operation!
         jsr     SysErr
; 
         lda     #XNODRIVE  ;Drive not connected
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
DInit:   bit     L2083
         bmi     @L2123
         dec     L2083
         lda     DIB1_Slot
         clc
         adc     SIR_Addr+2
         sta     SIR_Addr+2
         lda     #SIR_Len
         ldx     SIR_Addr
         ldy     SIR_Addr+1
         jsr     AllocSIR
         bcs     @L212B
         lda     DIB1_Slot
         asl     A
         asl     A
         asl     A
         asl     A
         sta     L206D
         lda     #$01
         jsr     L22E0
         bcc     @L2118
         jsr     L2323
         lda     #$01
         jsr     L22E0
         bcs     @L2127
@L2118:  lda     $ca28
         cmp     #$d5
         beq     @L2123
         cmp     #$ad
         bne     @L2127
@L2123:  jsr     L277D
         rts

@L2127:  lda     #XNODRIVE  ;Drive not connected
         bne     @L212D

@L212B:  lda     #XNORESRC  ;Resource not available
@L212D:  sta     L206C
         jsr     SysErr
; 
; D_READ call processing
; 
DRead:   lda     #$00       ;Zero # bytes read
         tay
         sta     (QtyRead),y
         iny
         sta     (QtyRead),y
         jsr     L21E4
L213E:   jsr     L2229
         jsr     L22DD
         bcs     IO_Error
L2146:   lda     #$08
         jsr     L232A
         lda     #$aa
         jsr     L230F
         jsr     L22DD
         bcs     IO_Error
L2155:   jsr     L2246
         beq     @L216A
         bcc     @L215F
         jsr     SysErr
@L215F:  dec     L206E
         ldx     L206E
         bpl     L2146
         jsr     SysErr
@L216A:  ldx     #$7f
         ldy     #$00
@L216E:  lda     $c980,x
         sta     ($ca),y
         lda     $c900,x
         sta     ($cc),y
         lda     $c880,x
         sta     ($ce),y
         lda     $c800,x
         sta     ($d0),y
         dex
         iny
         bpl     @L216E
         lda     #$02
         ldy     #$01
         adc     (QtyRead),y
         sta     (QtyRead),y
         jsr     L2210
         bne     L213E
         rts

IO_Error:
         lda     #$27       ;I/O error
         jsr     SysErr
; 
; D_WRITE call processing
; 
DWrite:  jsr     L21E4
@L219C:  jsr     L22DD
         bcs     IO_Error
         lda     #$05
         jsr     L232A
         ldy     #$7f
@L21A8:  lda     ($ca),y
         sta     $c80c,y
         lda     ($cc),y
         sta     $c88c,y
         lda     ($ce),y
         sta     $c90c,y
         lda     ($d0),y
         sta     $c98c,y
         dey
         bpl     @L21A8
         lda     #$aa
         jsr     L230F
         jsr     L22DD
         bcs     IO_Error
         jsr     L2246
         beq     @L21DE
         bcc     @L21D3
         jsr     SysErr
@L21D3:  dec     L206E
         ldx     L206E
         bpl     @L219C
         jsr     SysErr
@L21DE:  jsr     L2210
         bne     @L219C
         rts

L21E4:   lda     SOS_Unit
         sta     L2076
         lda     L2022
         sta     L206E
         lda     ReqCnt
         bne     @L21FA
         lda     ReqCnt+1
         bne     @L21FA
@L21F7:  jmp     SysErr

@L21FA:  lda     SosBlk
         cmp     DIB1_Blks
         lda     SosBlk+1
         sbc     DIB1_Blks+1
         lda     #XBLKNUM   ;Block number to large
         bcs     @L21F7
         jsr     L2281
         lda     #XBYTECNT  ;Byte count not a multiple of 512
         bcc     @L21F7
         rts

L2210:   ldx     #$06
@L2212:  inc     $cb,x
         inc     $cb,x
         dex
         dex
         bpl     @L2212
         inc     L2072
         bne     @L2222
         inc     L2073
@L2222:  jsr     L2229
         dec     L206F
         rts

L2229:   lda     $cb
         cmp     #$82
         bcc     @L2245
         ldx     $14cb
         bpl     @L2245
         cpx     #$8f
         bcs     @L2245
         ldx     #$08
@L223A:  inc     $14c9,x
         asl     $c9,x
         lsr     $c9,x
         dex
         dex
         bne     @L223A
@L2245:  rts

L2246:   ldx     #$06
@L2248:  lda     $ca29,x
         sta     L207B,x
         dex
         bpl     @L2248
         sec
         lda     $ca29
         bpl     @L2272
         and     #$70
         sta     L2074
         ldx     #$05
@L225E:  lda     @L227B,x
         bit     L2074
         bne     @L226D
         dex
         dex
         bpl     @L225E
@L226A:  lda     #$27
         rts

@L226D:  dex
         lda     @L227B,x
         rts

@L2272:  lda     $ca2a
         clc
         bmi     @L226A
         lda     #$00
         rts

@L227B:  .byte   $2e,$10,$27,$20,$2b,$40

L2281:   lda     ReqCnt
         bne     @L22DB
         lda     ReqCnt+1
         lsr     A
         bcs     @L22DB
         sta     L206F
         clc
         adc     SosBlk
         sta     L2074
         lda     #$00
         adc     SosBlk+1
         sta     L2075
         lda     DIB1_Blks
         cmp     L2074
         lda     DIB1_Blks+1
         sbc     L2075
         bcc     @L22DB
         lda     SosBlk
         sta     L2072
         lda     SosBlk+1
         sta     L2073
; Sets up 4 zero page pointers ca, cc, ce, d0 to be able to copy the 512 bytes
; of data in four 128 byte chunks
         lda     SosBuf
         ldx     SosBuf+1
         sta     $ca        ;setup buffer pointer one
         stx     $cb
         ldy     #$02       ;now setup 3 more pointers, in $80 increments
@L22BC:  clc
         adc     #$80
         sta     $00ca,y
         bcc     @L22C5
         inx
@L22C5:  stx     $cb,y
         iny
         iny
         cpy     #$08
         bne     @L22BC
         lda     $14c3      ;setup the xbyte for the pointers
         ldx     #$06
@L22D2:  sta     $14cb,x
         dex
         dex
         bpl     @L22D2
         sec
         rts

@L22DB:  clc
         rts

L22DD:   lda     L2023
L22E0:   sta     L2071
@L22E3:  jsr     @L22EE
         bcc     @L22ED
         dec     L2071
         bne     @L22E3
@L22ED:  rts

@L22EE:  lda     #$a0
         sta     L2070
         ldy     #$00
         ldx     L206D
@L22F8:  lda     Q6L,x
         bmi     @L2307
         dey
         bne     @L22F8
         dec     L2070
         bne     @L22F8
         sec
         rts

@L2307:  clc
         lda     DIB1_Slot
         jsr     $1922
         rts

L230F:   sta     $ca28
         ldx     L206D
         ldy     #$00
@L2317:  lda     MOTOROFF,x
         bpl     @L2321
         dey
         bne     @L2317
         sec
         rts

@L2321:  clc
         rts

L2323:   ldx     L206D
         lda     $c084,x
         rts

L232A:   tay
         sta     $ca20
         lda     L2084
         sta     $ca24
         lda     L2085
         sta     $ca25
         lda     SOS_Unit
         sta     $ca23
         lda     L2072
         sta     $ca22
         lda     L2073
         sta     $ca21
         lda     Entry-1
         sta     $ca26
         lda     L2086
         sta     $ca27
         cpy     #$06
         bne     @L2370
         lda     #$00
         sta     $ca24
         lda     #$13
         sta     $ca26
         lda     #$00
         sta     $ca27
         ldx     L2024
         stx     $ca21
@L2370:  cpy     #$03
         bne     @L2381
         ldy     #$00
         lda     (CSList),y
         sta     $ca26
         iny
         lda     (CSList),y
         sta     $ca27
@L2381:  rts

; 
; D_STATUS call processing
; 
DStatus: ldx     #$01
         cpx     CtlStat
         beq     @L23CC
         dex
         cpx     CtlStat
         beq     @L2398
         dex
         dex
         cpx     CtlStat
         beq     @L23D7
         lda     #XCTLCODE  ;Invalid control/status code
         jsr     SysErr
@L2398:  jsr     L22DD
         bcc     @L23A0
@L239D:  jmp     IO_Error

@L23A0:  lda     #$01
         jsr     L232A
         lda     #$aa
         jsr     L230F
         jsr     L22DD
         bcs     @L239D
         lda     $ca29
         sta     L2074
         lda     #$40
         ldy     #$05
@L23B9:  lsr     L2074
         bcc     @L23C1
         eor     @L23C7-1,y
@L23C1:  dey
         bne     @L23B9
         sta     (CSList),y
         rts

@L23C7:  .byte   $04
         .byte   $20
         .byte   $40
         .byte   $00
         .byte   $02

; 
; status code $01
; 
@L23CC:  ldy     #$06
@L23CE:  lda     L207B,y
         sta     (CSList),y
         dey
         bpl     @L23CE
         rts

@L23D7:  ldy     #$00
         lda     #$ff
         sta     (CSList),y
         iny
         sta     (CSList),y
         rts

; 
; D_REPEAT - repeat the last D_READ or D_WRITE call
; 
DRepeat: lda     L2076
         cmp     SOS_Unit
         bne     @L23F9
         lda     L2077
         sta     ReqCode
         bne     @L23F2
         jmp     DRead

@L23F2:  cmp     #$02
         bcs     @L23F9
         jmp     DWrite

@L23F9:  jmp     BadOp

; 
; D_CONTROL call processing
; 
DControl:
         lda     CtlStat    ;CtlStat
         cmp     #$fe       ;Format the device
         beq     @L2422
         lda     CtlStat
         cmp     #$05
         bcs     @L2413
         asl     A
         tay
         lda     @L2418+1,y
         pha
         lda     @L2418,y
         pha
         rts

@L2413:  lda     #$21
         jsr     SysErr
; 
@L2418:  .word   T277A-1    ;Reset device?
         .word   T26F6-1
         .word   T278A-1
         .word   T2796-1
         .word   T2713-1

; control code $fe
; format disk??
@L2422:  jsr     L22DD
         bcc     @L242A
@L2427:  jmp     IO_Error

@L242A:  ldy     #$00
@L242C:  lda     @L2472,y
         sta     $cb00,y
         lda     @L2572,y
         sta     $cc00,y
         lda     @L2672,y
         sta     $cd00,y
         lda     L2772,y
         sta     $ce00,y
         iny
         bne     @L242C
         lda     #$06
         jsr     L232A
         lda     #$aa
         jsr     L230F
         lda     #$00
         jsr     L22E0
         bcc     @L2465
         lda     #$00
         jsr     L22E0
         bcc     @L2465
         jsr     L2323
         jmp     @L2427

@L2465:  lda     $ca29
         bmi     @L246B
         rts

@L246B:  cmp     #$90
         bcc     @L2427
         jmp     L2155

; maybe Z8 code to do the formatting of the disk
@L2472:  .byte   $8b,$04,$97,$01,$02,$00,$8f,$e6
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
@L2572:  .byte   $45,$6b,$05,$e4,$45,$1e,$00,$1b
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
@L2672:  .byte   $f1,$af,$e6,$04,$10,$e6,$05,$2a
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
T26F6:   jsr     L22DD
         bcc     L26FE
L26FB:   jmp     IO_Error

L26FE:   lda     #$03
         jsr     L232A
         lda     #$aa
         jsr     L230F
         jsr     L22DD
         bcs     L26FB
         lda     $ca2a
         bmi     L26FB
         rts

; 
; control code $04
; 
T2713:   jsr     L22DD
         bcs     L26FB
         lda     #$02
         jsr     L232A
         ldy     #$00
         jsr     L27A2
         stx     $d8
         sta     $d9
         lda     (CSList),y
         sta     $14d9
         iny
         jsr     L27A2
         pha
         txa
         clc
         adc     $d8
         sta     L2078
         pla
         adc     $d9
         sta     L2079
         jsr     L27A2
         stx     $da
         stx     $ca27
         sta     $ca26
         clc
         adc     #$b8
         sta     $db
         jsr     @L275B
         lda     #$aa
         jsr     L230F
@L2755:  jsr     L22DD
         bcs     @L2755
         rts

@L275B:  ldy     #$00
L275D:   lda     ($d8),y
         sta     ($da),y
         inc     $d8
         bne     @L2767
         inc     $d9
@L2767:  inc     $da
         bne     @L276D
         inc     $db
@L276D:  lda     $d8
         cmp     L2078
L2772:   lda     $d9
         sbc     L2079
         bcc     L275D
         rts

; 
; control code $00
; 
T277A:   jsr     L2323
L277D:   lda     L2025
         sta     L2084
         lda     DIB2-1
         sta     L2085
         rts

; 
; control code $02
; 
T278A:   ldy     #$00
         jsr     L27A2
         stx     L2086
         sta     Entry-1
         rts

; 
; control code $03
; 
T2796:   ldy     #$00
         jsr     L27A2
         stx     L2084
         sta     L2085
         rts

L27A2:   lda     (CSList),y
         iny
         tax
         lda     (CSList),y
         iny
         rts

