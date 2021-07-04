;CF140A
;         .TITLE    "Apple /// CompactFlash/IDE Driver Ver 1.40A"
                    ;Limit Maximum Volume Size to 32767 Blocks

;         .PROC     CFIDE
;
; SOS Equates
;
ExtPG     =         $1401             ;driver extended bank address offset
AllocSIR  =         $1913             ;allocate system internal resource
SELC800   =         $1922             ;Enable Expansion Rom Space
DeAlcSIR  =         $1916             ;de-allocate system internal resource
SysErr    =         $1928             ;report error to system
EReg      =         $FFDF            ;environment register
ReqCode   =         $C0              ;request code
SOS_Unit  =         $C1              ;unit number
SosBuf    =         $C2              ;SOS buffer pointer
ReqCnt    =         $C4              ;requested byte count
CtlStat   =         $C2              ;control/status code
CSList    =         $C3              ;control/status list pointer
SosBlk    =         $C6              ;starting block number
QtyRead   =         $C8              ;bytes read return by D_READ
;
; Our temps in zero page
;
CurPart   =         $CC              ;1 byte
Count     =         $CD              ;2 bytes
Validate  =         $CE              ;1 byte
;
; Parameter block specific to current SOS request
;
ATA_Cmd   =         $CF
Drv_Parm  =         $D0
Sect_HB   =         $D1
Sect_MB   =         $D2
Sect_LB   =         $D3
Num_Blks  =         $D4             ;2 bytes lb,hb
DataBuf   =         $D6             ;2 bytes
CurDrive  =         $D8             ;Current IDE drive number of SOS_Unit #
CurDrvNo  =         $DA             ;Current DIB drive number of SOS_Unit #
;
; SOS Error Codes
;
XREQCODE  =         $20              ;Invalid request code
XCTLCODE  =         $21              ;Invalid control/status code
XCTLPARAM =         $22              ;Invalid control/status parameter
XNORESRC  =         $25              ;Resource not available
XBADOP    =         $26              ;Invalid operation
XIOERROR  =         $27              ;I/O error
XNODRIVE  =         $28              ;Drive not connected
XBYTECNT  =         $2C              ;Byte count not a multiple of 512
XBLKNUM   =         $2D              ;Block number to large
XDCMDERR  =         $31              ;device command ABORTED error occurred
XCKDEVER  =         $32              ;Check device readiness routine failed
XNORESET  =         $33              ;Device reset failed
XNODEVIC  =         $38              ;Device not connected
;
; IFC I/O locations
;
IFC_ID    =         $C820
IOBase    =         $C080
ATdataHB  =         IOBase+$00
SetCSMsk  =         IOBase+$01       ;Two special strobe locations to set and
                                     ;clear MASK bit that is used to disable
                                     ;CS0 line to the CompactFlash during the
                                     ;CPU read cycles.
ClrCSMsk  =         IOBase+$02       ;that occur before every CPU write cycle.
                                     ;The normally innocuous read cycles were
                                     ;causing the SanDisk CF to double
                                     ;increment during sector writes commands.
Alt_Stat  =         IOBase+$06       ;when reading (not used)
ATAdCtrl  =         IOBase+$06       ;when writing
ATdataLB  =         IOBase+$08 
ATAError  =         IOBase+$09 
Features  =         IOBase+$09 
ATSectCt  =         IOBase+$0a
ATSector  =         IOBase+$0b       ;11=LB,12=MB,13=HB
ATAHead   =         IOBase+$0e
ATCmdReg  =         IOBase+$0f       ;when writing
ATA_Stat  =         IOBase+$0f       ;when reading
;
; ATA/CF Commands Codes
;
ATA_XErr  =         $03
ATACRead  =         $20
ATACWrit  =         $30
ATA_Vrfy  =         $40
ATA_Frmt  =         $50
ATA_Diag  =         $90
ATAIdent  =         $EC
SetFeatr  =         $EF
;
; Constants for Wait
;
Wait5ms   =         $2a ; 42.
Wait150ms =         $f2 ; 242.
;
; Switch Macro
;
          .MACRO    SWITCH index,bounds,adrs_table,noexec
          .IFNBLANK index            ; If PARM1 is present,
          lda       index            ; load A with switch index
          .ENDIF
          .IFNBLANK bounds           ; If PARM2 is present,
          cmp       #bounds+1        ; perform bounds checking
          bcs       @110             ; on switch index
          .ENDIF
          asl       A                ; Multiply by 2 for table index
          tay
          lda       adrs_table+1,y   ; Get switch address from table
          pha                        ; and push onto Stack
          lda       adrs_table,y
          pha
          .IFBLANK noexec
          rts                        ; Exit to code
          .ENDIF
@110:
          .ENDMACRO

          .SEGMENT "TEXT"

;
; Comment Field of driver
;
          .WORD     $FFFF
          .WORD     $53
          .byte     "Apple /// CFFA Driver - "
          .byte     "written by Dale S. Jackson 8/08"
          .byte     ", modified by D Schmenk 8/11"

          .SEGMENT  "DATA"

;
; DIB Values for Map & Drive No.
;
; Map No.   IDE $0   IDE $1     Map No.   IDE $0    IDE $1
;   0       $00       $01        32        $80        $81
;   1       $04       $05        33        $84        $85
;   2       $08       $09        34        $88        $89
;   3       $0C       $0D        35        $8C        $8D
;   4       $10       $11        36        $90        $91
;   5       $14       $15        37        $94        $95
;   6       $18       $19        38        $98        $99
;   7       $1C       $1D        39        $9C        $9D
;   8       $20       $21        40        $A0        $A1
;   9       $24       $25        41        $A4        $A5
;  10       $28       $29        42        $A8        $A9
;  11       $2C       $2D        43        $AC        $AD
;  12       $30       $31        44        $B0        $B1
;  13       $34       $35        45        $B4        $B5
;  14       $38       $39        46        $B8        $B9
;  15       $3C       $3D        47        $BC        $BD
;  16       $40       $41        48        $C0        $C1
;  17       $44       $45        49        $C4        $C5
;  18       $48       $49        50        $C8        $C9
;  19       $4C       $4D        51        $CC        $CD
;  20       $50       $51        52        $D0        $D1
;  21       $54       $55        53        $D4        $D5
;  22       $58       $59        54        $D8        $D9
;  23       $5C       $5D        55        $DC        $DD
;  24       $60       $61        56        $E0        $E1
;  25       $64       $65        57        $E4        $E5
;  26       $68       $69        58        $E8        $E9
;  27       $6C       $6D        59        $EC        $ED
;  28       $70       $71        60        $F0        $F1
;  29       $74       $75        61        $F4        $F5
;  30       $78       $79        62        $F8        $F9
;  31       $7C       $7D        63        $FC        $FD

;------------------------------------
;
; Device identification Block (DIB) - Volume #0
;
;------------------------------------

DIB_0:    .WORD     DIB_1            ;link pointer
          .WORD     Entry            ;entry pointer
          .BYTE     7                ;name length byte
          .byte     ".CFIDE1        ";device name
;          .BYTE     0C0              ;active, page aligned
          .BYTE     $80              ;active, no page alignment
DIB0_Slot:
          .BYTE     $FF              ;slot number
          .BYTE     $00              ;unit number
          .BYTE     $D1              ;type
          .BYTE     $10              ;subtype
          .BYTE     $00              ;filler
DIB0_Blks:
          .WORD     $0000            ;# blocks in device
          .WORD     $444A            ;manufacturer - DJ
          .WORD     $140A            ;Version 1.40A
          .WORD     0003             ;DCB length followed by DCB
Driv_No0: .BYTE     $00              ;Drive #, Bit 0=$0 (master) or $1 (slave)
                                     ;Upper 6 bits = Partition address.
                                     ;64 different partition addresses avail.
Part_No0: .BYTE     $00              ;Partition number on the drive = $00-07
FastXfer: .BYTE     $00              ;Fast transfer flag
;
; Device identification Block (DIB) - Volume #1
; Page alignment begins here
;
DIB_1:    .WORD     DIB_2            ;link pointer
          .WORD     Entry            ;entry pointer
          .BYTE     7                ;name length byte
          .byte     ".CFIDE2        ";device name
          .BYTE     $80              ;active
          .BYTE     $FF              ;slot number
          .BYTE     $01              ;unit number
          .BYTE     $D1              ;type
          .BYTE     $10              ;subtype
          .BYTE     $00              ;filler
DIB1_Blks:
          .WORD     $0000            ;# blocks in device
          .WORD     $444A            ;manufacturer - DJ
          .WORD     $140A            ;Version 1.40A
          .WORD     $0002            ;DCB length followed by DCB
          .BYTE     $00              ;Drive # = $00 (master) or $01 (slave)
                                     ;Upper 6 bits = Partition address.
                                     ;64 different partition addresses avail.
          .BYTE     $01              ;Partition number on the drive = $00-07
;
; Device identification Block (DIB) - Volume #2
;
DIB_2:    .WORD     DIB_3            ;link pointer
          .WORD     Entry            ;entry pointer
          .BYTE     7                ;name length byte
          .byte     ".CFIDE3        ";device name
          .BYTE     $80              ;active
          .BYTE     $FF              ;slot number
          .BYTE     002              ;unit number
          .BYTE     $D1              ;type
          .BYTE     $10              ;subtype
          .BYTE     $00              ;filler
DIB2_Blks:
          .WORD     $0000            ;# blocks in device
          .WORD     $444A            ;manufacturer - DJ
          .WORD     $140A            ;Version 1.40A
          .WORD     $0002            ;DCB length followed by DCB
          .BYTE     $00              ;Drive # = $00 (master) or $01 (slave)
                                     ;Upper 6 bits = Partition address.
                                     ;64 different partition addresses avail.
          .BYTE     002              ;Partition number on the drive = $00-07
;
; Device identification Block (DIB) - Volume #3
;
DIB_3:    .WORD     DIB_4            ;link pointer
          .WORD     Entry            ;entry pointer
          .BYTE     7                ;name length byte
          .byte     ".CFIDE4        ";device name
          .BYTE     $80              ;active
          .BYTE     $FF              ;slot number
          .BYTE     003              ;unit number
          .BYTE     $D1              ;type
          .BYTE     $10              ;subtype
          .BYTE     $00              ;filler
DIB3_Blks:
          .WORD     $0000            ;# blocks in device
          .WORD     $444A            ;manufacturer - DJ
          .WORD     $140A            ;Version 1.40A
          .WORD     $0002            ;DCB length followed by DCB
          .BYTE     $00              ;Drive # = $00 (master) or $01 (slave)
                                     ;Upper 6 bits = Partition address.
                                     ;64 different partition addresses avail.
          .BYTE     003              ;Partition number on the drive = $00-07
;
; Device identification Block (DIB) - Volume #4
;
DIB_4:    .WORD     DIB_5            ;link pointer
          .WORD     Entry            ;entry pointer
          .BYTE     7                ;name length byte
          .byte     ".CFIDE5        ";device name
          .BYTE     $80              ;active
          .BYTE     $FF              ;slot number
          .BYTE     004              ;unit number
          .BYTE     $D1              ;type
          .BYTE     $10              ;subtype
          .BYTE     $00              ;filler
DIB4_Blks:
          .WORD     $0000            ;# blocks in device
          .WORD     $444A            ;manufacturer - DJ
          .WORD     $140A            ;Version 1.40A
          .WORD     $0002            ;DCB length followed by DCB
          .BYTE     $00              ;Drive # = $00 (master) or $01 (slave)
                                     ;Upper 6 bits = Partition address.
                                     ;64 different partition addresses avail.
          .BYTE     004              ;Partition number on the drive = $00-07
;
; Device identification Block (DIB) - Volume #5
;
DIB_5:    .WORD     DIB_6            ;link pointer
          .WORD     Entry            ;entry pointer
          .BYTE     7                ;name length byte
          .byte     ".CFIDE6        ";device name
          .BYTE     $80              ;active
          .BYTE     $FF              ;slot number
          .BYTE     005              ;unit number
          .BYTE     $D1              ;type
          .BYTE     $10              ;subtype
          .BYTE     $00              ;filler
DIB5_Blks:
          .WORD     $0000            ;# blocks in device
          .WORD     $444A            ;manufacturer - DJ
          .WORD     $140A            ;Version 1.40A
          .WORD     $0002            ;DCB length followed by DCB
          .BYTE     $00              ;Drive # = $00 (master) or $01 (slave)
                                     ;Upper 6 bits = Partition address.
                                     ;64 different partition addresses avail.
          .BYTE     005              ;Partition number on the drive = $00-07
;
; Device identification Block (DIB) - Volume #6
;
DIB_6:    .WORD     DIB_7            ;link pointer
          .WORD     Entry            ;entry pointer
          .BYTE     7                ;name length byte
          .byte     ".CFIDE7        ";device name
          .BYTE     $80              ;active
          .BYTE     $FF              ;slot number
          .BYTE     $06              ;unit number
          .BYTE     $D1              ;type
          .BYTE     $10              ;subtype
          .BYTE     $00              ;filler
DIB6_Blks:
          .WORD     $0000            ;# blocks in device
          .WORD     $444A            ;manufacturer - DJ
          .WORD     $140A            ;Version 1.40A
          .WORD     $0002            ;DCB length followed by DCB
          .BYTE     $00              ;Drive # = $00 (master) or $01 (slave)
                                     ;Upper 6 bits = Partition address.
                                     ;64 different partition addresses avail.
          .BYTE     $06              ;Partition number on the drive = $00-07
;
; Device identification Block (DIB) - Volume #7
;
DIB_7:    .WORD     $0000            ;link pointer
          .WORD     Entry            ;entry pointer
          .BYTE     7                ;name length byte
          .byte     ".CFIDE8        ";device name
          .BYTE     $80              ;active
          .BYTE     $FF              ;slot number
          .BYTE     $07              ;unit number
          .BYTE     $D1              ;type
          .BYTE     $10              ;subtype
          .BYTE     $00              ;filler
DIB7_Blks:
          .WORD     $0000            ;# blocks in device
          .WORD     $444A            ;manufacturer - DJ
          .WORD     $140A            ;Version 1.40A
          .WORD     $0002            ;DCB length followed by DCB
          .BYTE     $00              ;Drive # = $00 (master) or $01 (slave)
                                     ;Upper 6 bits = Partition address.
                                     ;64 different partition addresses avail.
          .BYTE     $07              ;Partition number on the drive = $00-07


;------------------------------------
;
; Local storage locations
;
;------------------------------------

SlotCX:   .BYTE     $00              ;compute C0X0 and store on init
PtBlkIdx: .BYTE     $A3,$A8,$B3,$B8,$C3,$C8,$D3,$D8  ;Offsets to 8 block
PtVolIdx: .BYTE     $A6,$AB,$B6,$BB,$C6,$CB,$D6,$DB  ;segment for ea partition
UnitStat: .RES      $08,$FF          ;if UnitStat,X = $FF then partition info
                                                ;not initialized
                                     ;if UnitStat,X > $0F then UnitStat = error code
                                     ;else UnitStat,X = partition number
                                     ;if UnitStat,0 = #XNODRIVE then driver is
                                                ;nonfunctional
LastOP:   .RES      $08,$FF          ;last op for D_REPEAT calls
StBlk_HB: .RES      $08,$00          ;Starting block number for SOS/ProDos
StBlk_MB: .RES      $08,$00          ;vol #0, vol #1, vol #2, vol #3
StBlk_LB: .RES      $08,$00          ;vol #4, vol #5, vol #6, vol #7
Block_LB: .RES      $08,$00          ;Total blocks for each volume #
Block_HB: .RES      $08,$00
DCB_Idx:  .BYTE     $00
          .BYTE     DIB1_Blks-DIB0_Blks
          .BYTE     DIB2_Blks-DIB0_Blks
          .BYTE     DIB3_Blks-DIB0_Blks
          .BYTE     DIB4_Blks-DIB0_Blks
          .BYTE     DIB5_Blks-DIB0_Blks
          .BYTE     DIB6_Blks-DIB0_Blks
          .BYTE     DIB7_Blks-DIB0_Blks
SIR_Addr: .WORD     SIR_Tbl
SIR_Tbl:  .RES      $05,$00
SIR_Len   =         *-SIR_Tbl
PmapCall: .RES      $03,$00          ;Block $hb mb lb for partition record
          .BYTE     $01,$00          ;Read 1 ATA sector (block) lb, hb
          .WORD     PmapBuf          ;Buffer address to place partition record
PmapBuf:  .RES      $0100,$00
Err_Data: .RES      $0100,$00
RdBlk1Pr: .WORD     RdBlk1
WrBlk1Pr: .WORD     WrBlk1
RdBlk2Pr: .WORD     RdBlk2
WrBlk2Pr: .WORD     WrBlk2
RdBlk_Proc:
          .WORD    $0000
WrBlk_Proc:
          .WORD    $0000

;          .byte     "Written By Dale S. Jackson, initial writing 12/12/02"
;          .byte     "v1.40a revised 8/20/11 By Dave Schmenk"

;------------------------------------
;
; Driver request handlers
;
;------------------------------------

Entry:    LDA       UnitStat+0
          CMP       #XNODEVIC
          BEQ       No_Drive
          LDX       SOS_Unit         ;get drive number for this unit
          LDY       DCB_Idx,X
          LDA       Driv_No0,Y
          STA       CurDrvNo
          AND       #$01             ;only bit 0 counts
          STA       CurDrive         ;Set device to LBA mode
          ORA       #$0E             ;device mode bits   %00001(LBA)1(Drive#)
          ASL       A                ;shift left 4 bits to high order nibble
          ASL       A
          ASL       A
          ASL       A
          STA       Drv_Parm
          LDA       ReqCode
          CMP       #$02             ;Status Call
          BCS       @2
          LDA       UnitStat,X       ;Check if partition table is current
          CMP       #$FF
          BNE       @1
          JSR       GetPmap          ;fetch it if not.
          JSR       PInit
          LDX       SOS_Unit
          LDA       UnitStat,X
@1:       CMP       #$10             ;Check if SOS_Unit driver is good.
          BCS       Err_Out1
@2:       JSR       Dispatch         ;Now call the dispatcher
          LDX       SOS_Unit         ;Save current operation
          LDA       ReqCode          ;for D_REPEAT processing
          STA       LastOP,X
          RTS
;
; The Dispatcher.  Does it depending on ReqCode.  Note
; that if we came in on a D_INIT call, we do a branch to
; Dispatch, normally.  Dispatch is called as a subroutine!
; We copy the buffer pointer and block # from the parameter
; area into our own temps, as the system seems to want them
; left ALONE.
;
DoTable:  .WORD     DRead-1          ;0 read request
          .WORD     DWrite-1         ;1 write request
          .WORD     DStatus-1        ;2 status request
          .WORD     DControl-1       ;3 control request
          .WORD     BadReq-1         ;4 unused
          .WORD     BadReq-1         ;5 unused
          .WORD     BadOp-1          ;6 open - invalid request
          .WORD     BadOp-1          ;7 close - invalid request
          .WORD     DInit-1          ;8 init request
          .WORD     DRepeat-1        ;9 repeat request
Dispatch: SWITCH    ReqCode,9,DoTable ;go do it.
;
; Errors
;
BadReq:   LDA       #XREQCODE        ;bad request code!
Err_Out1: JSR       SysErr           ;doesn't return
Slot_Err: LDA       #XNODEVIC        ;#XNORESRC        SIR not available
          STA       UnitStat+0       ;no! it didn't go ok.
No_Drive: LDA       #XNODRIVE        ;Flag this driver not useable
          JSR       SysErr           ;doesn't return
;
; D_INIT call processing for all Volumes.
; Called at system init time only.  Check DIB0_Slot to
; make sure that the user set a valid slot number for our
; interface.  Allocate it by calling AllocSIR. If slot not
; available then set UnitStat+0 to XNORESRC error code.
;
; Compute the system internal resource number (SIR) and
; call AllocSIR to try and grab that for us.  It performs
; slot checking as a side effect.
;
DInit:    LDA       SIR_Tbl
          BNE       Norm_Out
          LDA       DIB0_Slot
          AND       #$07
          ORA       #$10             ;SIR = 16+slot#
          STA       SIR_Tbl
          LDA       #SIR_Len
          LDX       SIR_Addr
          LDY       SIR_Addr+1
          JSR       AllocSIR         ;this one's mine!
          BCS       Slot_Err
          LDA       DIB0_Slot        ;Compute C0X0 for this slot
          ASL       A
          ASL       A
          ASL       A
          ASL       A
          STA       SlotCX
          JSR       ResetIFC         ;Initialization of Device
          BCS       Slot_Err
          LDY       #$07             ;Reset drive status to initial startup
          LDA       #$FF
@1:       STA       UnitStat,Y
          DEY
          BPL       @1
Norm_Out: CLC
          RTS
;
; D_REPEAT - repeat the last D_READ or D_WRITE call
;
DRepeat:  LDX       SOS_Unit
          LDA       LastOP,X         ;look at the last thing we did
          CMP       #$02
          BCS       BadOp
          STA       ReqCode
          JMP       Dispatch
BadOp:    LDA       #XBADOP          ;invalid operation!
          JSR       SysErr           ;doesn't return
;
; D_READ call processing
;
DRead:    JSR       CkCnt
          LDA       #ATACRead
          STA       ATA_Cmd
CRead:    LDA       #$00             ;Zero # bytes read
          STA       Count
          STA       Count+1
          TAY
          STA       (QtyRead),Y      ;bytes read
          INY
          STA       (QtyRead),Y      ;msb of bytes read
          LDA       Num_Blks         ;check for Num_Blks greater than zero
          ORA       Num_Blks+1
          BEQ       ReadExit
@1:       JSR       FixUp
          JSR       Read_Blk         ;Transfer a block to/from the disk
          LDY       #$00
          LDA       Count
          STA       (QtyRead),y      ;Update # of bytes actually read
          INY
          LDA       Count+1
          STA       (QtyRead),y
          BCS       IO_Error         ;An error occurred
ReadExit: RTS                        ;exit read routines
SRead:    JSR       Read_Blk         ;Transfer a block to/from the disk
          BCC       ReadExit
IO_Error: LDA       #XIOERROR        ;I/O error
          JSR       SysErr           ;doesn't return
;
; D_WRITE call processing
;
DWrite:   JSR       CkCnt
          LDA       #ATACWrit
          STA       ATA_Cmd
CWrite:   LDA       Num_Blks         ;check for Num_Blks greater than zero
          ORA       Num_Blks+1
          BEQ       WriteExit        ;quantity to write is zero
          JSR       FixUp
          JSR       Write_Blk
          BCS       IO_Error
WriteExit:
          RTS
;
; D_STATUS call processing
;  $00   Drivers Status - always $0
;  $01   Return device identification - $0200 bytes long
;  $02   Return most recent device error information/data
;  $03   Return partition table data - $0100 bytes long
;  $04   Return DIB configuration bytes - 2 bytes long
;  $FE   Return preferrred bitmap location ($FFFF)
;
DStatus:  LDA       CtlStat          ;status command
          BNE       @1
          TAY                        ;Driver Status = $0 always
          STA       (CSList),Y
          RTS
@1:       CMP       #$01
          BEQ       S_Ident
          CMP       #$02
          BEQ       ErrStat
          CMP       #$03
          BEQ       ParTable
          CMP       #$04
          BEQ       DIBinfo
          CMP       #$FE
          BEQ       BitMap
CS_Bad:   LDA       #XCTLCODE        ;control/status code no good
Err_Out3: JSR       SysErr           ;doesn't return
ParTable: JSR       GetPmap          ;Return partn table of current SOS_Unit
          BNE       @2               ;Partition map is not valid
          LDY       #$00
@1:       LDA       PmapBuf,Y
          STA       (CSList),Y
          INY
          BNE       @1
          RTS
@2:       JMP       No_Drive
ErrStat:  LDY       #$05            ;Return most recent error data.
@1:       LDA       Err_Data,y      ; Byte 0:      Device Status Code
          STA       (CSList),Y      ; Byte 1:      Device Error Code 
          DEY                       ; (if status ERR bit is 0, error code = 0)
          BPL       @1              ; Byte 2,3,4:  Sector# (LB,MB,HB) of error
          CLC                       ; Byte 5:      # of sectors left to xfer
          RTS
DIBinfo:  LDX       SOS_Unit         ;Return DIB configuration bytes
          LDY       DCB_Idx,X
          LDA       Part_No0,Y       ;Get assigned partition number this driver
          PHA
          LDA       Driv_No0,Y       ;Get assigned partition map/IDE device
          LDY       #$00             ;for this driver.
          STA       (CSList),Y
          INY
          PLA
          STA       (CSList),Y
          CLC
          RTS
BitMap:   LDY       #$00             ;Return preferred bit map locations.
          LDA       #$FF             ;We return FFFF, don't care
          STA       (CSList),Y
          INY
          STA       (CSList),Y       
          CLC
          RTS
S_Ident:  LDA       CSList            ;Device Identification
          STA       DataBuf
          LDA       CSList+1
          STA       DataBuf+1
          LDA       CSList+ExtPG
          STA       DataBuf+ExtPG
          LDA       #ATAIdent
          STA       ATA_Cmd
C_Ident:  LDA       #$01
          STA       Num_Blks
          LDA       #$00
          STA       Num_Blks+1
          JMP       SRead

;
; D_CONTROL call processing
;  $00     Reset device
;  $01     Perform device I/O function with user supplied
;          call block.
;  $04     Set DIB configuration bytes
;  $FE     Perform media formatting
;
DControl: LDA       CtlStat          ;control command
          BEQ       CReset
          CMP       #$01
          BEQ       UserIO
          CMP       #$04
          BEQ       New_DIB
          CMP       #$FE             ;formatting?
          BEQ       MFormat
          JMP       CS_Bad           ;Control code no good!
CReset:   JSR       ResetIFC         ;Reset CFFA card
          BCS       @1
          RTS
@1:       LDA       #XNORESET
          JSR       SysErr           ;doesn't return
MFormat:  LDX       #$07             ;Execute media formatting call.
@1:       LDY       DCB_Idx,X
          LDA       Driv_No0,Y
          CMP       CurDrvNo
          BNE       @2
          LDA       #$FF             ;Invalidate partition table status
          STA       UnitStat,X       ;so subsequent read/writes
@2:       DEX                        ;will re-initialize the partition info
          BPL       @1               ;for each driver designated for this
          CLC                        ;partiton table.
          RTS
New_DIB:  LDX       SOS_Unit         ;Save new DIB configuration bytes
          LDA       #$FF             ;Invalidate partition table status of driver
          STA       UnitStat,X
          LDY       #$00
          LDA       (CSList),Y
          PHA
          INY
          LDA       (CSList),Y
          LDY       DCB_Idx,X
          STA       Part_No0,Y      ;Get assigned partition number this driver
          PLA
          STA       Driv_No0,Y      ;Get assigned partition map/IDE device
          CLC                       ;for this driver.
          RTS
UserIO:   LDY       #$04
@1:       LDA       (CSList),Y
          STA       ATA_Cmd+1,Y
          DEY
          BNE       @1
          LDA       Num_Blks         ;if zero then 256 blocks is requested
          BNE       @2
          INY
@2:       STY       Num_Blks+1
          CLC                        ;Setup data addresses
          LDA       CSList
          ADC       #$05
          STA       QtyRead
          LDA       CSList+1
          ADC       #$00
          STA       QtyRead+1
          LDA       QtyRead
          ADC       #$02
          STA       DataBuf
          LDA       QtyRead+1
          ADC       #$00
          STA       DataBuf+1
          LDA       CSList+ExtPG
          STA       QtyRead+ExtPG
          STA       DataBuf+ExtPG
          LDY       #$00
          LDA       (CSList),Y
          LDY       #$06
@3:       CMP       CtrlCmds,y
          BEQ       @4
          DEY
          BPL       @3
          JMP       CS_Bad
@4:       STA       ATA_Cmd
          TYA
          ASL       A
          TAY
          LDA       Cmd_Tbl+1,Y
          PHA
          LDA       Cmd_Tbl,Y
          PHA
          RTS
;
; Perform I/O with user supplied call block
; Call Block Organization:
;     Byte 0:        ATA Command Code
;     Byte 1,2,3:    Sector# (HB,MB,LB absolute sector)
;     Byte 4:        # of sectors
;     Byte 5-6:      Bytes returned to buffer
;     Byte 7...      Data Buffer
;
CtrlCmds: .BYTE     ATA_XErr
          .BYTE     ATACRead
          .BYTE     ATACWrit
          .BYTE     ATA_Vrfy
          .BYTE     ATA_Frmt
          .BYTE     ATA_Diag
          .BYTE     ATAIdent

Cmd_Tbl:  .WORD     Send_Cmd-1       ;Extended Error Info  $03
          .WORD     CRead-1          ;Sector Read  $20
          .WORD     CWrite-1         ;Sector Write  $30
          .WORD     Verify-1         ;Read verify  $40
          .WORD     CWrite-1         ;Sector Format  $50
          .WORD     Send_Cmd-1       ;Internal Diagnostic Test  $90
          .WORD     C_Ident-1        ;Device Identity  $EC

Send_Cmd: JSR       CkDevice
          LDA       #$00
          STA       ATdataHB,x       ;Clear high byte data latch
          LDA       ATA_Cmd
          STA       ATCmdReg,x       ;Issue the ATA command to the drive
@1:       LDA       ATA_Stat,x
          BMI       @1
          LDA       ATAError,x
          LDY       #$00
          STA       (DataBuf),y
          JMP       CSet2Mhz
;
; Verify - Verify requested blocks
;
Verify:   LDA       #ATA_Vrfy
          JSR       SetupLBA         ;Program the device's task file registers
@1:       LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       @1
          LSR       A
          BCS       @2
          JMP       Set2Mhz
@2:       JMP       Save_Err

;------------------------------------
;
; Partition Table routines
;
;------------------------------------

;
; Get Partition Map from drive
;
GetPmap:  LDA       CurDrvNo
          AND       #$FC             ;get upper 6 bits of drive number for
          STA       PmapCall         ;partition address.
          LDY       #$06
@1:       LDA       PmapCall,Y
          STA       Sect_HB,Y
          DEY
          BPL       @1
          LDA       #$00
          STA       DataBuf+ExtPG
          LDA       #ATACRead
          STA       ATA_Cmd
          JSR       SRead
          LDY       #$02             ;Compute partition table checksum
          LDA       #$A5             ;Carry is initially clear
@2:       EOR       PmapBuf,Y
          INY
          ADC       PmapBuf,Y
          INY
          BNE       @2
          EOR       PmapBuf,Y
          STA       Validate
          RTS                        ;Return Validate=$0 if valid partition
;
; Initialize partition info for each partition in the current table.
;
PInit:    LDX       #$07
Next_DIB: LDY       DCB_Idx,X
          LDA       CurDrvNo
          CMP       Driv_No0,Y
          BNE       IncDIB_x
          LDA       Validate
          BNE       Bad_Drv
          LDA       Part_No0,Y       ;Get assigned partition number for this
          CMP       #$08             ;driver.
          BCS       Bad_Drv          ;Partition number is out of range
          STA       CurPart
          JSR       GVolParm
          BCS       Bad_Drv
          LDY       DCB_Idx,X
          LDA       Block_HB,X
          STA       DIB0_Blks+1,Y
          LDA       Block_LB,X
          STA       DIB0_Blks,Y
          LDA       CurPart
          BPL       Good_Drv
Bad_Drv:  LDA       #XNODRIVE        ;Flag this driver not useable
Good_Drv: STA       UnitStat,X
IncDIB_x: DEX
          BPL       Next_DIB
          RTS
;
; Get the volume start block in this partition segment
;
GVolParm: LDY       CurPart          ;Xreg contains DIB unit number
          LDA       PtBlkIdx,Y
          TAY
          LDA       PmapBuf+2,Y      ;Block HB
          CMP       #$04             ;Test if beginning track is valid
          BCS       @1               ;Nope!  Beginning block larger than 16 mb
          ORA       PmapCall         ;set upper 6 bits for partition address
          STA       StBlk_HB,X
          LDA       PmapBuf+1,Y      ;Block MB
          STA       StBlk_MB,X
          ORA       PmapBuf+2,Y      ;Check if beginning track is zero
          ORA       PmapBuf,Y
          BEQ       Bad_Vol          ;Volume start block can't be zero
          LDA       PmapBuf,Y        ;Block LB
          STA       StBlk_LB,X
          LDY       CurPart          ;Xreg contains DIB unit number
          LDA       PtVolIdx,Y       ;Get the volume size
          TAY
          LDA       PmapBuf,Y
          STA       Block_LB,X
          ORA       PmapBuf+1,Y
          BEQ       Bad_Vol          ;Volume size cannot be zero
          LDA       PmapBuf+1,Y
          STA       Block_HB,X
          BMI       Bad_Vol          ;Volume size is greater than 32767 blocks
          CLC
@1:       RTS
Bad_Vol:  SEC
          RTS

;------------------------------------
;
; Utility routines
;
;------------------------------------

;
; Check ReqCnt to insure it's a multiple of 512.
;
CkCnt:    LDA       ReqCnt           ;look at the lsb of bytes to do
          BNE       @1               ;no good!  lsb should be 00
          STA       Num_Blks+1       ;zero high byte of number of blocks
          LDA       ReqCnt+1         ;look at the msb
          LSR       A                ;put bottom bit into carry, 0 into top
          STA       Num_Blks         ;save as number of blocks to transfer
          BCC       CvtBlk           ;Carry is set from LSR to mark error.
@1:       LDA       #XBYTECNT
          JSR       SysErr           ;doesn't return
;
; Test for valid block number. Carry clear on return means
; no error.  Carry set means block number bad.  X register
; contains volume number.
;
CvtBlk:   LDA       SosBuf
          STA       DataBuf
          LDA       SosBuf+1
          STA       DataBuf+1
          LDA       SosBuf+ExtPG
          STA       DataBuf+ExtPG
          LDY       SOS_Unit
          LDA       SosBlk
          CMP       Block_LB,y
          LDA       SosBlk+1
          SBC       Block_HB,y
          BCS       BlkErr
          LDA       SosBlk
          ADC       StBlk_LB,y
          STA       Sect_LB
          LDA       SosBlk+1
          ADC       StBlk_MB,y
          STA       Sect_MB
          LDA       StBlk_HB,y
          ADC       #$00
          STA       Sect_HB
          RTS
BlkErr:   LDA       #XBLKNUM
          JSR       SysErr             ;doesn't return
IncrAdr:  INC       Count+1
          INC       Count+1
BumpAdr:  INC       DataBuf+1         ;increment DataBuf msb
;
; Fix up the buffer pointer to correct for an addressing
; anomalies!  We just need to do the initial checking
; for two cases:
; 00xx bank N -> 80xx bank N-1
; 20xx bank 8F if N was 0
; FDxx bank N -> 7Dxx bank N+1
; If pointer is adjusted, return with carry set
;
FixUp:    LDA       DataBuf+1         ;look at msb
          BEQ       @1                 ;that's one!
          CMP       #$FD               ;is it the other one?
          BCS       @2                 ;yep. fix it!
          RTS                          ;Pointer unchanged, return carry clear.
@1:       LDA       #$80               ;00xx -> 80xx
          STA       DataBuf+1
          DEC       DataBuf+ExtPG     ;bank N -> band N-1
          LDA       DataBuf+ExtPG     ;see if it was bank 0
          CMP       #$7F               ;(80) before the DEC.
          BNE       @3                 ;nope! all fixed.
          LDA       #$20               ;if it was, change both
          STA       DataBuf+1         ;msb of address and
          LDA       #$8F
          STA       DataBuf+ExtPG     ;bank number for bank 8F
          RTS                          ;return carry set
@2:       AND       #$7F               ;strip off high bit
          STA       DataBuf+1         ;FDxx ->7Dxx
          INC       DataBuf+ExtPG     ;bank N -> bank N+1
@3:       RTS                          ;return carry set
;
; Wait - Copy of Apple's wait routine.
;  Input:
;   A =  delay time, where Delay(us) = 2.5A^2 + 13.5A + 36
;        including JSR to this routine.
;        or more typically A = (Delay[in uS]/2.5 - 7.11)^.5 - 2.7
Wait:     PHP
          SEI
          SEC
@1:       PHA
@2:       SBC       #$01
          BNE       @2
          PLA
          SBC       #$01
          BNE       @1
          PLP
          RTS
;
; Throttle back to 1 MHz
;
Set1Mhz:  PHP
          SEI
          LDA       EReg
          ORA       #$80
          STA       EReg
          PLP
          RTS
;
; Throttle up to 2 MHz
;
CSet2Mhz: CLC
Set2Mhz:  PHP
          SEI
          LDA       EReg
          AND       #$7F
          STA       EReg
          PLP
          RTS

;----------------------------------
;
; Supplemental Device Subroutines
;
; Device Internal Diagnostic Routine  ATA Command $90
;  Returns 1 byte of diagnostic code in Buffer
;  $01 = No Error Detected
;  $02 = Formatter Device Error
;  $03 = Sector Buffer Error
;  $04 = ECC Circuitry Error
;  $05 = Controlling Microprocessor Error
;  $8x = Slave Failed (true IDE mode)
;
; Extended Error Code Request
;  Returns 1 byte of exteded error code in Buffer
;  $00 = No Error Detected
;  $01 = Self test OK (No error)
;  $09 = Miscellaneous Error
;  $20 = Invalid Command
;  $21 = Invalid address (requested head or sector invalid)
;  $2F = Address Overflow (address too large)
;  $35,$36 = Supply voltage out of tolerance
;  $11 = Uncorrectable ECC error
;  $18 = Corrected ECC Error
;  $05,$30-34,$37,$3E = Self test or diagnostic failed
;  $10,$14 = ID not found
;  $3A = Spare sectors exhausted
;  $1F = Data transfer error/Aborted command
;  $0C,$38,$3B,$3C,$3F = Corrupted Media Format
;  $03 = Write/Erase failed
;
;----------------------------------

;
; Execute reset call to ATA device
;
ResetIFC: JSR       Set1Mhz
          LDA       RdBlk1Pr
          STA       RdBlk_Proc
          LDA       RdBlk1Pr+1
          STA       RdBlk_Proc+1
          LDA       WrBlk1Pr
          STA       WrBlk_Proc
          LDA       WrBlk1Pr+1
          STA       WrBlk_Proc+1
;          LDA       DIB0_Slot
;          JSR       SELC800         ;Turn on ROM
;          LDA       IFC_ID+2
;          AND       #0F0            ;Check major ROM version is 2.xx
;          EOR       IFC_ID+1
;          EOR       IFC_ID
;          TAX
;          LDA       #000
;          JSR       SELC800         ;Turn off ROM
;          CPX       #015
;          BNE       No_Ver2
          LDA       FastXfer
          BEQ       No_Ver2
          LDA       RdBlk2Pr
          STA       RdBlk_Proc
          LDA       RdBlk2Pr+1
          STA       RdBlk_Proc+1
          LDA       WrBlk2Pr
          STA       WrBlk_Proc
          LDA       WrBlk2Pr+1
          STA       WrBlk_Proc+1
No_Ver2:  LDX       SlotCX
          LDA       ClrCSMsk,x      ;reset MASK bit in PLD for normal CS0
          LDA       #$00            ;signaling.
          STA       ATdataHB,x      ;Clear high byte data latch
          LDA       #$06            ;Reset bit=1, Disable INTRQ=1
          STA       ATAdCtrl,x
          JSR       Norm_Out        ;Per ATA-6 spec, need to wait 5us minimum.
          LDA       #$02            ;Reset bit=0, Disable INTRQ=1
          STA       ATAdCtrl,x
          LDY       #$d0 ; 208.     ;Per ATA-6 spec, need to wait 2ms minimum
          LDA       #Wait5ms        ;Use a initial delay of 5ms.
@1:       JSR       Wait
          LDA       ATA_Stat,x      ;Per ATA-6 spec, wait up to 31 sec for
          BMI       @2              ;busy to clear if a slave is attached.
          JMP       CSet2Mhz
@2:       LDA       #Wait150ms      ;Wait for up to 31 secs if a slave is
          DEY                       ;attached.  After 31 secs pass, the card
          BNE       @1              ;is not installed in slot.
          SEC                       ;Drive(s) Not Ready
          JMP       Set2Mhz
;
; Check Device - test drive status register is readable
; and equal to $40
;
CkDevice: JSR       Set1Mhz
          LDY       #$c8 ;200.
          LDX       SlotCX
          LDA       ClrCSMsk,x       ;reset MASK bit in PLD for normal CS0
@1:       LDA       ATA_Stat,x       ;signaling.
          BMI       @1
          AND       #$08             ;%00001000  Check bus can receive a
          BEQ       @2               ;device register setting DRQ=0
          LDA       #Wait5ms
          JSR       Wait             ;Wait 5ms to try again
          DEY
          BNE       @1              ;Wait up to 1 second for drive to be ready
          BEQ       NotReady        ;always taken
@2:       LDA       Drv_Parm        ;Select drive to check
          STA       ATAHead,x
          LDY       #$c8 ;200.
@3:       LDA       ATA_Stat,x
          BMI       @3
          AND       #$E9             ;%11101001  Check if device is ready to
          CMP       #$40             ;receive a command.  If BSY=0, RDY=1,
          BNE       @4               ;DF=0, DRQ=0, and ERR=0
          RTS                        ;We're good to go.  Returns @ 1 Mhz clock
                                     ;Xreg = SlotCX, and carry set.
@4:       LDA       #Wait5ms
          JSR       Wait             ;Wait 5ms to try again
          DEY
          BNE       @3               ;Wait up to 1 seconds for drive readiness
NotReady: JSR       Set2Mhz
          LDA       #XCKDEVER        ;DEVICE NOT READY error
          JSR       SysErr
;
; SetupLBA - Programs devices task registers with LBA data
; Input:
;        partition data Sect_HB, MB, & LB
;        A =  command
;        X =  requested slot number in form $n0 where
;             n =  slot 1 to 7
; This function programs the device registers with
; the ATA Logical Block Address (LBA) to be accessed.
; A SOS block and a ATA sector are both 512 bytes.
; Logical Block Mode, the Logical Block Address is
; interpreted as follows:
;  LBA07-LBA00: Sector Number Register D7-D0.
;  LBA15-LBA08: Cylinder Low Register D7-D0.
;  LBA23-LBA16: Cylinder High Register D7-D0.
;  LBA27-LBA24: Drive/Head Register bits HS3-HS0.
;
SetupLBA: PHA
          JSR       CkDevice         ;returns with carry set
          LDA       Sect_LB
          STA       ATSector,x       ;store low block # into LBA 0-7
          LDA       Sect_MB
          STA       ATSector+1,x     ;store mean block # into LBA 15-8
          LDA       Sect_HB
          STA       ATSector+2,x     ;store high block # LBA bits 23-16
          LDA       Num_Blks
          STA       ATSectCt,x       ;store # of blocks to be read/written
          LDA       #$00
          STA       ATdataHB,x
          TAY
          PLA
          STA       ATCmdReg,x       ;Issue the command
          RTS
;
; Save device error status
;
Save_Err: LDA       ATA_Stat,x
          STA       Err_Data
          LSR       A
          BCS       @1               ;if status ERR bit is one then get error
          LDA       #$00             ;register data,else return error data
          BEQ       @2               ;byte = zero.
@1:       LDA       ATAError,x
@2:       STA       Err_Data+1
          LDA       ATSector,x       ;retrieve sector # error occurred
          STA       Err_Data+2       ;LB
          LDA       ATSector+1,x
          STA       Err_Data+3       ;MB
          LDA       ATSector+2,x
          STA       Err_Data+4       ;HB
          LDA       ATSectCt,x
          STA       Err_Data+5       ;retrieve # of sectors left
          STY       Count
          SEC
          JMP       Set2Mhz
;
; Read_Blk - Read requested blocks from device into memory
;
;
Read_Blk: LDA       ATA_Cmd
          JSR       SetupLBA         ;Program the device's task file registers
          JMP       (RdBlk_Proc)
;
; CFFA Ver 1.x
;
RdBlk1:   LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       RdBlk1
          AND       #$09             ;Check for error response from device
          CMP       #$08             ;If DRQ=0 or ERR=1 a device error
          BNE       Read_Err
@1:       LDA       ATdataLB,x
          STA       (DataBuf),y
          INY
          LDA       ATdataHB,x
          STA       (DataBuf),y
@2:       LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       @2
          INY
          BNE       @1
          INC       DataBuf+1
@3:       LDA       ATdataLB,x
          STA       (DataBuf),y
          INY
          LDA       ATdataHB,x
          STA       (DataBuf),y
@4:       LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       @4
          INY
          BNE       @3
          JSR       IncrAdr
          DEC       Num_Blks         ;did we get what was asked for
          BNE       RdBlk1
          DEC       Num_Blks+1
          BPL       RdBlk1
          JMP       CSet2Mhz
Read_Err: JMP       Save_Err
;
; CFFA Ver 2.x
;
RdBlk2:   LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       RdBlk2
          AND       #$09             ;Check for error response from device
          CMP       #$08             ;If DRQ=0 or ERR=1 a device error
          BNE       Read_Err
@1:       LDA       ATdataLB,x
          STA       (DataBuf),y
          INY
          LDA       ATdataHB,x
          STA       (DataBuf),y
          INY
          BNE       @1
          INC       DataBuf+1
@2:       LDA       ATdataLB,x
          STA       (DataBuf),y
          INY
          LDA       ATdataHB,x
          STA       (DataBuf),y
          INY
          BNE       @2
          JSR       IncrAdr
          DEC       Num_Blks         ;did we get what was asked for
          BNE       RdBlk2
          DEC       Num_Blks+1
          BPL       RdBlk2
          JMP       CSet2Mhz
;
; Write_Blk - write requested blocks to device from memory
;
Write_Blk:
          LDA       ATA_Cmd
          JSR       SetupLBA         ;Program the device's task file registers
          JMP       (WrBlk_Proc)
;
; CFFA Ver 1.x
;
WrBlk1:   LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       WrBlk1
          AND       #$09             ;Check for error response from device
          CMP       #$01             ;If DRQ=0 and ERR=1 a device error
          BEQ       Write_Err
@1:       LDA       SetCSMsk,x       ;any access sets mask bit to block
                                     ;IDE -CS0 on I/O read to drive
          INY
          LDA       (DataBuf),y
          STA       ATdataHB,x
          DEY
          LDA       (DataBuf),y
          STA       ATdataLB,x
          LDA       ClrCSMsk,x
@2:       LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       @2
          INY
          INY
          BNE       @1
          INC       DataBuf+1
@3:       LDA       SetCSMsk,x
          INY
          LDA       (DataBuf),y
          STA       ATdataHB,x
          DEY
          LDA       (DataBuf),y
          STA       ATdataLB,x
          LDA       ClrCSMsk,x       ;Set back to normal, allow CS0 assertions
                                     ;on read cycles
@4:       LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       @4
          INY
          INY
          BNE       @3
          JSR       BumpAdr
          DEC       Num_Blks         ;did we do what was asked for
          BNE       WrBlk1
          DEC       Num_Blks+1       ;we might have to do this one more time
          BPL       WrBlk1
          LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          AND       #$09             ;Check for error response from device
          CMP       #$01             ;If DF=1 or DRQ=0 or ERR=1 a device error
          BEQ       Write_Err
          JMP       CSet2Mhz         ;exit write routines
Write_Err:
          JMP       Save_Err
;
; CFFA Ver 2.x
;
WrBlk2:   LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       WrBlk2
          AND       #$09             ;Check for error response from device
          CMP       #$01             ;If DRQ=0 or ERR=1 a device error
          BEQ       Write_Err
          LDA       SetCSMsk,x       ;any access sets mask bit to block
                                     ;IDE -CS0 on I/O read to drive
@1:       INY
          LDA       (DataBuf),y
          STA       ATdataHB,x
          DEY
          LDA       (DataBuf),y
          STA       ATdataLB,x
          INY
          INY
          BNE       @1
          INC       DataBuf+1
@2:       INY
          LDA       (DataBuf),y
          STA       ATdataHB,x
          DEY
          LDA       (DataBuf),y
          STA       ATdataLB,x
          INY
          INY
          BNE       @2
          LDA       ClrCSMsk,x       ;Set back to normal, allow CS0 assertions
                                     ;on read cycles
          JSR       BumpAdr
          DEC       Num_Blks         ;did we do what was asked for
          BNE       WrBlk2
          DEC       Num_Blks+1       ;we might have to do this one more time
          BPL       WrBlk2
@3:       LDA       ATA_Stat,x       ;Wait for BUSY flag to clear
          BMI       @3
          AND       #$09             ;Check for error response from device
          CMP       #$01             ;If DF=1 or DRQ=0 or ERR=1 a device error
          BEQ       Write_Err
          JMP       CSet2Mhz         ;exit write routines
