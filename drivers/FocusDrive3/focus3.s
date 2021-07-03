; Focus3

;			.TITLE "Apple /// Focus Drive Driver"
			.PROC  Focus3

			.setcpu "6502"
			.reloc

DriverVersion	= $005B		; Version number
DriverMfgr		= $4453		; Driver Manufacturer - DS
DriverType		= $E1		; No formatter present for the time being
DriverSubtype	= $02		;
InitialSlot		= $02		; Slot number to assume we're in

;
; SOS Equates
;
ExtPG		= $1401			; Driver extended bank address offset
AllocSIR	= $1913			; Allocate system internal resource
SELC800		= $1922			; Enable Expansion Rom Space
DeAlcSIR	= $1916			; De-allocate system internal resource
SysErr		= $1928			; Report error to system
EReg		= $FFDF			; Environment register

;
; SOS Zero page parameters
;
ReqCode		= $C0			; Request code
SOS_Unit	= $C1			; Unit number
SosBuf		= $C2			; SOS buffer pointer
ReqCnt		= $C4			; Requested byte count
CtlStat		= $C2			; Control/status code
CSList		= $C3			; Control/status list pointer
SosBlk		= $C6			; Starting block number
QtyRead		= $C8			; Bytes read return by D_READ

;
; Focus Hardware
;
ATOffset		= $8F				; Base offset for $C0FF,X addressing

ATData8			= $C080-ATOffset+8	; 8 bit data port
ATError			= $C081-ATOffset+8	; Error flags
ATSectorCount	= $C082-ATOffset+8	; Number of sectors to process
ATSectorNumber	= $C083-ATOffset+8	; Sector number requested
ATCylL			= $C084-ATOffset+8	; Cylinder # Low
ATCylH			= $C085-ATOffset+8	; Cylinder # High
ATHead			= $C086-ATOffset+8	; Head #
ATStatus		= $C087-ATOffset+8	; (R) Status of drive
ATCommand		= $C087-ATOffset+8	; (W) Issue a command
ATData16		= $C088-ATOffset-8	; 16 bit data port, accessed with MSlot16 index value
ATReset			= $C08B-ATOffset-8	; (W) Reset the drive
ATAltStatus		= $C08E-ATOffset-8	; (R) Status of drive (Don't clear)
ATDigOut		= $C08E-ATOffset-8	; (W) Reset
ATDriveAdr		= $C08F-ATOffset-8	; (R) Drive address

;
; Parameter block specific to current SOS request
;
Num_Blks	= $D2			; Number of blocks requested (we'll never ever have > 128 blocks)
Count		= $D3			; 2 bytes lb,hb

;
; Focus zero page
;
Track		= $D6			; (3) Current track / ProDOS block
Head		= $D9			; (1) Current head
Sector		= $DA			; (1) Current sector
Temp		= $DB			; (2) Timer temp
RetryCount	= $DD			; (1) Number of read retries
OkFlag		= $DE			; (1) Compare byte for return value
ProCommand	= $DF			; (1) 0=Status,1=Read,2=Write,3=Format
Ytemp		= $E1			; (1) Temp space for Y
ProBlock	= $E2			; (3) Block # to work with

;
; SOS Error Codes
;
XDNFERR		= $10			; Device not found
XBADDNUM	= $11			; Invalid device number
XREQCODE	= $20			; Invalid request code
XCTLCODE	= $21			; Invalid control/status code
XCTLPARAM	= $22			; Invalid control/status parameter
XNORESRC	= $25			; Resource not available
XBADOP  	= $26			; Invalid operation
XIOERROR	= $27			; I/O error
XNODRIVE	= $28			; Drive not connected
XBYTECNT	= $2C			; Byte count not a multiple of 512
XBLKNUM 	= $2D			; Block number to large
XDISKSW		= $2E			; Disk switched
XDCMDERR	= $31			; device command ABORTED error occurred
XCKDEVER	= $32			; Check device readiness routine failed
XNORESET	= $33			; Device reset failed
XNODEVIC	= $38			; Device not connected

;
; Switch Macro
;
.MACRO		SWITCH index,bounds,adrs_table,noexec	; See SOS Reference
.IFNBLANK index							; If PARM1 is present,
			lda		index				; load A with switch index
.ENDIF
.IFNBLANK	bounds						; If PARM2 is present,
			cmp		#bounds+1			; perform bounds checking
			bcs		@110				; on switch index
.ENDIF
			asl		A					; Multiply by 2 for table index
			tay
			lda		adrs_table+1,y		; Get switch address from table
			pha							; and push onto Stack
			lda		adrs_table,y
			pha
.IFBLANK	noexec
			rts							; Exit to code
.ENDIF
@110:
.ENDMACRO

			.SEGMENT "TEXT"

;
; Comment Field of driver
;
			.word	$FFFF ; Signal that we have a comment
			.word	COMMENT_END - COMMENT
COMMENT:	.byte	"Apple /// Focus Driver - by David Schmidt 2019"
COMMENT_END:

			.SEGMENT	"DATA"

;------------------------------------
;
; Device identification Block (DIB) - Volume #0
;
;------------------------------------

DIB_0:		.word	DIB_1			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".FOCUSD1       "; Device name
			.byte	$80				; Active, no page alignment
DIB0_Slot:	.byte	InitialSlot		; Slot number
			.byte	$00				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB0_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #1
; Page alignment begins here
;
DIB_1:		.word	DIB_2			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".FOCUSD2       "; Device name
			.byte	$80				; Active
DIB1_Slot:	.byte	InitialSlot		; Slot number
			.byte	$01				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB1_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #2
;
DIB_2:		.word	DIB_3			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".FOCUSD3       "; Device name
			.byte	$80				; Active
DIB2_Slot:	.byte	InitialSlot		; Slot number
			.byte	$02				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB2_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #3
;
DIB_3:		.word	DIB_4			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".FOCUSD4       "; Device name
			.byte	$80				; Active
DIB3_Slot:	.byte	InitialSlot		; Slot number
			.byte	$03				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB3_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #4
;
DIB_4:		.word	DIB_5			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".FOCUSD5       "; Device name
			.byte	$00				; Inactive
DIB4_Slot:	.byte	InitialSlot		; Slot number
			.byte	$04				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB4_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #5
;
DIB_5:		.word	DIB_6			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".FOCUSD6       "; Device name
			.byte	$00				; Inactive
DIB5_Slot:	.byte	InitialSlot		; Slot number
			.byte	$05				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB5_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #6
;
DIB_6:		.word	DIB_7			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".FOCUSD7       "; Device name
			.byte	$00				; Inactive
DIB6_Slot:	.byte	InitialSlot		; Slot number
			.byte	$06				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB6_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #7
;
DIB_7:		.word	$0000			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".FOCUSD8       "; Device name
			.byte	$00				; Inactive
DIB7_Slot:	.byte	InitialSlot		; Slot number
			.byte	$07				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB7_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB

;------------------------------------
;
; Local storage locations
;
;------------------------------------

LastOP:		.res	$08, $FF			; Last operation for D_REPEAT calls
SIR_Addr:	.word	SIR_Tbl
SIR_Tbl:	.res	$05, $00
SIR_Len		=		*-SIR_Tbl
RdBlk_Proc:	.word	$0000
WrBlk_Proc:	.word	$0000
MaxUnits:	.byte	$08					; The maximum number of units
DriveType:	.byte	$00					; Type of drive
DCB_Idx:	.byte	$00					; DCB 0's blocks
			.byte	DIB1_Blks-DIB0_Blks	; DCB 1's blocks
			.byte	DIB2_Blks-DIB0_Blks	; DCB 2's blocks
			.byte	DIB3_Blks-DIB0_Blks	; DCB 3's blocks
			.byte	DIB4_Blks-DIB0_Blks	; DCB 4's blocks
			.byte	DIB5_Blks-DIB0_Blks	; DCB 5's blocks
			.byte	DIB6_Blks-DIB0_Blks	; DCB 6's blocks
			.byte	DIB7_Blks-DIB0_Blks	; DCB 7's blocks
SigFocus:	.byte	"Parsons Engin."	; Focus card signature in memory
CardIsOK:	.byte	$00					; Have we found the Focus card yet?
LastError:	.byte	$00					; Recent error RC from Focus
StatusBlks:	.word	$0000				; Temp storage for number of blocks

;
; Storage items from Focus ROM
;

; Partition sizes for max of 8 partitions
PartLo:			.res	$08, $00		; Partition start block
PartMd:			.res	$08, $00		;    wait for it...
PartHi:			.res	$08, $00		;    24 bit!

; Temps
PartSizeLo1:	.byte	$00
PartSizeMid1:	.byte	$00
PartSizeHi1:	.byte	$00
SmartFlag:		.byte	$00
MSlot16:		.byte	$00				; Offset to hardware port based on slot

;------------------------------------
;
; Driver request handlers
;
;------------------------------------

Entry:
			lda		DIB0_Slot			; Slot we're in (all DIBx_Slot values are the same)
			jsr		SELC800				; Turn on C800 ROM space from our slot
			jsr		GoSlow
			jsr		Dispatch			; Call the dispatcher
			jsr		GoFast
			ldx		SOS_Unit			; Get drive number for this unit
			lda		ReqCode				; Keep request around for D_REPEAT
			sta		LastOP,x			; Keep track of last operation
			lda		#$00
			jsr		SELC800				; Unselect C800 ROM space
			rts

;
; The Dispatcher.  Note that if we came in on a D_INIT call,
; we do a branch to Dispatch normally.  
; Dispatch is called as a subroutine!
;
DoTable:	.word	DRead-1				; 0 Read request
			.word	DWrite-1			; 1 Write request
			.word	DStatus-1			; 2 Status request
			.word	DControl-1			; 3 Control request
			.word	BadReq-1			; 4 Unused
			.word	BadReq-1			; 5 Unused
			.word	BadOp-1				; 6 Open - valid for character devices
			.word	BadOp-1				; 7 Close - valid for character devices
			.word	DInit-1				; 8 Init request
			.word	DRepeat-1			; 9 Repeat last read or write request
Dispatch:	SWITCH	ReqCode,9,DoTable	; Serve the request

;
; Dispatch errors
;
BadReq:		lda		#XREQCODE			; Bad request code!
			jsr		SysErr				; Return to SOS with error in A
BadOp:		lda		#XBADOP				; Invalid operation!
			jsr		SysErr				; Return to SOS with error in A

;
; D_REPEAT - repeat the last D_READ or D_WRITE call
;
DRepeat:	ldx		SOS_Unit
			lda		LastOP,x			; Recall the last thing we did
			cmp		#$02				; Looking for operation < 2
			bcs		BadOp				; Can only repeat a read or write
			sta		ReqCode
			jmp		Dispatch

NoDevice:	lda		#XDNFERR			; Device not found
			jsr		SysErr				; Return to SOS with error in A

;
; D_INIT call processing - called once each for all volumes.
;
DInit:
			lda		SOS_Unit			; Check if we're initting the zeroeth unit
			cmp		#$00
			bne		DInitDone			; No - then skip the signature check

CheckSig:	lda		#$08				; Prepare MSlot16 slot address calculation
			clc
			adc		DIB0_Slot			; A is now $08+slot#
			asl
			asl
			asl
			asl							; A is now $80+(slot#*16) in the high nibble, $0 in low
			clc
			adc		#$0F				; A is now $9F through $CF depending on slot
			sta		MSlot16				; Hang on to this for MSlot16 math
			lda		#$C0				; Form a $CsED address, where s = slot #
			ora		DIB0_Slot			; Add in slot number
			sta		Count+1
			lda		#$ED				; $CxED is where "Parsons Engin." string lives
			sta		Count
			ldy		#$0D
@1:			lda		(Count),y
			cmp		SigFocus,y			; Check for 'Parsons Engin.' signature in our slot
			bne		NoDevice			; No device if all bytes don't match
			dey
			bpl		@1

			lda		DIB0_Slot			; Found a Focus!
			ora		#$10				; SIR = $10+slot#
			sta		SIR_Tbl
			sta		CardIsOK			; Remember that we found the card
			lda		#SIR_Len
			ldx		SIR_Addr
			ldy		SIR_Addr+1
			jsr		AllocSIR			; Claim SIR
			bcs		NoDevice
			lda		#$00				; 0=status
			sta		ProCommand			; Command for Focus
			jsr		InitDrive			; Hit the hardware, load up defaults
			bcs		NoDevice
DInitDone:
			lda		CardIsOK			; Did we previously find a card?
			bne		:+
			jmp		NoDevice			; If not... then bail
:			clc
			rts

;
; D_READ call processing
;
DRead:
			lda		CardIsOK			; Did we previously find a card?
			bne		DReadGo
			jmp		NoDevice			; If not... then bail
DReadGo:
			jsr		CkCnt				; Checks for validity, aborts if not
			jsr		CkUnit				; Checks for unit below unit max
			lda		#$00				; Zero # bytes read
			sta		Count				; Local count of bytes read
			sta		Count+1
			sta		ProBlock+2
			sta		QtyRead				; Userland count of bytes read
			sta		QtyRead+1			; Msb of userland bytes read
			lda		Num_Blks			; Check for block count greater than zero
			beq		ReadExit
			jsr		FixUp				; Correct for addressing anomalies
			lda		#$01				; 1=read
			sta		ProCommand			; Prepare to read a block
			lda		SosBlk
			sta		ProBlock
			lda		SosBlk+1
			sta		ProBlock+1
ReadOne:	jsr		ReadBlock
			bcs		IO_Error
			inc		ProBlock
			bne		SkipReadMSBIncrement
			inc		ProBlock+1
SkipReadMSBIncrement:
			inc		SosBuf+1			; Go get the next block in the buffer
			jsr		BumpSosBuf			;   ...512 bytes in, and check the pointer
			dec		Num_Blks
			bne		ReadOne
			lda		Count				; Local count of bytes read
			sta		QtyRead				; Update # of bytes actually read
			lda		Count+1
			sta		QtyRead+1
			clc
ReadExit:
			rts							; Exit read routines
IO_Error:	lda		#XIOERROR			; I/O error
			jsr		SysErr				; Return to SOS with error in A

;
; D_WRITE call processing
;
DWrite:
			lda		CardIsOK			; Did we previously find a card?
			bne		DWriteGo
			jmp		NoDevice			; If not... then bail

DWriteGo:
			jsr		CkCnt				; Checks for validity, aborts if not
			jsr		CkUnit				; Checks for unit below unit max
			lda		#$00				; 0=Status
			lda		Num_Blks			; Check for block count greater than zero
			beq		WriteExit
			jsr		FixUp				; Correct for addressing anomalies
			lda		#$02				; 2=write
			sta		ProCommand
			lda		SosBlk
			sta		ProBlock
			lda		SosBlk+1
			sta		ProBlock+1
			lda		#$00
			sta		ProBlock+2
WriteOne:	jsr		WriteBlock
			bcs		IO_Error
			inc		ProBlock			; Bump the block number
			bne		SkipWriteMSBIncrement
			inc		ProBlock+1
SkipWriteMSBIncrement:
			inc		SosBuf+1			; Go get the next block in the buffer
			jsr		BumpSosBuf			;   ...512 bytes in, and check the pointer
			dec		Num_Blks
			bne		WriteOne
			clc
WriteExit:
			rts

;
; D_STATUS call processing
;  $00 = Driver Status
;  $FE = Return preferred bitmap location ($FFFF)
;
DStatus:
			lda		CardIsOK			; Did we previously find a card?
			bne		DStatusGo
			jmp		NoDevice			; If not... then bail

DStatusGo:
			lda		CtlStat				; Which status code to run?
			bne		DS0
			clc
			rts
DS0:		cmp		#$FE
			bne		DSWhat

			ldy		#$00				; Return preferred bit map locations.
			lda		#$FF				; We return FFFF, don't care
			sta		(CSList),Y
			iny
			sta		(CSList),Y       
			clc
			rts

DSWhat:		lda		#XCTLCODE			; Control/status code no good
			jsr		SysErr				; Return to SOS with error in A

;
; D_CONTROL call processing
;  $00 = Reset device
;
DControl:
			LDA		CardIsOK			; Did we previously find a card?
			BNE		DContGo
			JMP		NoDevice			; If not... then bail
			
DContGo:	LDA		CtlStat				; Control command
			BEQ		CReset
			JMP		DCWhat				; Control code no good!
CReset:		clc							; No-op
DCDone:		rts          
DCWhat:		lda		#XCTLCODE			; Control/status code no good
			jsr		SysErr				; Return to SOS with error in A

;------------------------------------
;
; Utility routines
;
;------------------------------------

;
; Check ReqCnt to ensure it is a multiple of 512.
;
CkCnt:		lda		ReqCnt				; Look at the lsb of bytes requested
			bne		@1					; No good!  lsb should be 00
			lda		ReqCnt+1			; Look at the msb
			lsr		A					; Put bottom bit into carry, 0 into top
			sta		Num_Blks			; Convert bytes to number of blks to xfer
			bcc		CvtBlk				; Carry is set from LSR to mark error.
@1:			lda		#XBYTECNT
			jsr		SysErr				; Return to SOS with error in A

;
; Test for valid block number; abort on error
;
CvtBlk:
			ldx		SOS_Unit
			ldy		DCB_Idx,x
			sec
			lda		DIB0_Blks+1,y		; Blocks on unit msb
			sbc		SosBlk+1			; User requested block number msb
			bvs		BlkErr				; Not enough blocks on device for request
			beq		@1					; Equal msb; check lsb.
			rts							; Greater msb; we're ok.
@1:			lda		DIB0_Blks,y			; Blocks on unit lsb
			sbc		SosBlk				; User requested block number lsb
			bvs		BlkErr				; Not enough blocks on device for request
			rts							; Equal or greater msb; we're ok.
BlkErr:		lda		#XBLKNUM
			jsr		SysErr				; Return to SOS with error in A

BumpSosBuf:	inc		SosBuf+1			; Increment SosBuf MSB
			; fallthrough to FixUp

;
; Fix up the buffer pointer to correct for addressing
; anomalies.  We just need to do the initial checking
; for two cases:
; 00xx bank N -> 80xx bank N-1
; 20xx bank 8F if N was 0
; FDxx bank N -> 7Dxx bank N+1
; If pointer is adjusted, return with carry set
;
FixUp:		lda		SosBuf+1			; Look at msb
			beq		@1					; That's one!
			cmp		#$FD				; Is it the other one?
			bcs		@2					; Yep. fix it!
			rts							; Pointer unchanged, return carry clear.
@1:			lda		#$80				; 00xx -> 80xx
			sta		SosBuf+1
			dec		SosBuf+ExtPG		; Bank N -> band N-1
			lda		SosBuf+ExtPG		; See if it was bank 0
			cmp		#$7F				; (80) before the DEC.
			bne		@3					; Nope! all fixed.
			lda		#$20				; If it was, change both
			sta		SosBuf+1			; Msb of address and
			lda		#$8F
			sta		SosBuf+ExtPG		; Bank number for bank 8F
			rts							; Return carry set
@2:			and		#$7F				; Strip off high bit
			sta		SosBuf+1			; FDxx ->7Dxx
			INC		SosBuf+ExtPG		; Bank N -> bank N+1
@3:			rts							; Return carry set

CkUnit:		lda		SOS_Unit			; Checks for unit below unit max
			cmp		MaxUnits
			bmi		UnitOk
NoUnit:		lda		XBADDNUM			; Report no unit to SOS
			jsr		SysErr
UnitOk:		clc
			rts

;
; Throttle back to 1 MHz
;
GoSlow:		pha
			php
			lda		EReg
			ora		#$80
			sta		EReg
			plp
			pla
			rts

;
; Throttle up to 2 MHz
;
CGoFast:	clc
GoFast:		pha
			php
			lda		EReg
			and		#$7F
			sta		EReg
			plp
			pla
			rts

;
; Gorp copied from ROM
;

DriveCount:
			.word   $0012
DSectors:
			.word   $001B,$001B,$001B,$0021
			.word   $0021,$0021,$0021,$001F
			.word   $002B,$0011,$0011,$0018
			.word   $0011,$0026,$002B,$002B
			.word   $0023,$0026
DHeads: .word   $0004,$0003,$0002,$0002
			.word   $0004,$0006,$0008,$0004
			.word   $0002,$0004,$0005,$0004
			.word   $0005,$0004,$0004,$0004
			.word   $0003,$0008
DCylinders:
			.word   $030E,$030E,$030E,$0536
			.word   $0536,$0536,$0536,$029E
			.word   $03D1,$0266,$FFFF,$0368
			.word   $03D1,$0223,$03D1,$03CD
			.word   $0317,$0224
DPark:  .word   $035D,$035D,$035D,$0537
			.word   $0537,$0537,$0537,$029E
			.word   $03DE,$0276,$FFFF,$0369
			.word   $03D4,$0228,$03DE,$03DE
			.word   $0320,$0224
DHeadCyl:
			.word   $006C,$0051,$0036,$0042
			.word   $0084,$00C6,$0108,$007C
			.word   $0056,$0044,$0055,$0060
			.word   $0055,$0098,$00AC,$00AC
			.word   $0069,$0130

InvalidCommand:
			lda		#$01
			rts

WaitDataOut:
WaitDataIn:
			lda		#$08
			bne		DoCheck
			lda		#$00
DoCheck:	sta		OkFlag
			lda		#$00
			sta		Temp
			lda		#$F1
			sta		Temp+1
			ldx		MSlot16
@B:			ldy		#$00
@A:			lda		ATStatus,x
			and		#$88
			cmp		OkFlag
			beq		OK1
			iny
			bne		@A
			inc		Temp
			bne		@B
			inc		Temp+1
			bne		@B
			sec
			rts

OK1:		clc
			rts

InitDrive:
			jsr		SetUpDefaults
			bcs		Err2

OK2:
			lda		#$00
			clc
			rts

Err2:
			sec
LocalRTS:
			rts

SetUpDefaults:
			jsr		ReadPartition			; Read partition data
			lda		#$27
			bcs		LocalRTS				; Didn't work
			ldx		MSlot16
			lda		ATData16,x				; 0x00
			sta		Track
			lda		ATData16+1,x			; 0x01 'Pa' (Parsons Tech)
			sta		Track+1
			ldy		#$06					; Index to sectors/Heads
			jsr		KillYWords				; 0x02..0x0d killed 
			lda		ATData16,x				; 0x0e wasteage
			lda		ATData16+1,x			; 0x0f get # active partitions
			and		#$7f					; Sanitize: strip off high bit
			cmp		#$08					; We support at most 8
			bmi		:+
			lda		#$08					; If they have > 8... they have 8
:			sta		MaxUnits
			ldy		#$06					; Index to sectors/Heads
			jsr		KillYWords				; 0x10..0x1b killed
			lda		ATData16,x				; 0x1c Get drive type (ignore 0x1d)
			asl		a
			sta		DriveType
			ldy		#$01					; Index to zeroeth partition start
@A:			jsr		KillYWords				; 0x1e..0x1f killed
											; Note: y is now 0x00
PartStart:
			lda		ATData16,x				; (Part*0x10)+0x20 get start block 1 of 4
			sta		PartLo,y				; Save Volume size data
			lda		ATData16+1,x			; (Part*0x10)+0x21 get start block 2 of 4
			sta		PartMd,y
			lda		ATData16,x				; (Part*0x10)+0x22 get start block 3 of 4
			sta		PartHi,y
			lda		ATData16+1,x			; (Part*0x10)+0x23 discard start block 4 of 4
			lda		ATData16,x				; (Part*0x10)+0x24 get block count 1 of 4
			sta		PartSizeLo1
			lda		ATData16+1,x			; (Part*0x10)+0x25 get block count 2 of 4
			sta		PartSizeMid1
			lda		ATData16,x				; (Part*0x10)+0x26 get block count 3 of 4 (ignore 4 of 4)
			sta		PartSizeHi1
			lda		ATData16,x				; (Part*0x10)+0x28 get write protect flag (ignore 2 of 2)
			sty		Ytemp					; Stash current partition number
			ldy		#$03					; Kill File system ID, end of part
			jsr		KillYWords				; (Part*0x10)+0x2a..0x2f killed

			ldx		Ytemp					; X now holds partition number
			ldy		DCB_Idx,x				; Y now holds offset to DIBx_Blks of this partition/unit
			lda		PartSizeLo1
			sta		DIB0_Blks,y
			lda		PartSizeMid1
			sta		DIB0_Blks+1,y
			lda		PartSizeHi1
			cmp		#$01					; Do we have 65536 (or more) blocks?
			bne		:+
			lda		#$FF					; Then we really only have 65535 blocks.
			sta		DIB0_Blks,y
			sta		DIB0_Blks+1,y
:			ldx		MSlot16					; Reload X with MSlot16
			ldy		Ytemp
			iny
			cpy		MaxUnits
			bne		PartStart
; Now we need to burn the rest of the partition table
			jsr		KillToDRQ				; Kill the rest of the sector
			jsr		WaitStatus				; Exit (Return Carry Clear/Set)
			bcs		Err1
			ldx		Track
			cpx		#$50					; 'P'
			bne		Invalid
			ldx		Track+1
			cpx		#$61					; 'a'
			bne		Invalid
			ldy		DriveType
			ldx		MSlot16
			lda		DHeads,y
			sec
			sbc		#$01
			ora		#$A0
			sta		ATHead,x
			lda		DSectors,y
			sta		ATSectorCount,x
			lda		#$91					; Setup size command
			sta		ATCommand,x
			jsr		WaitStatus				; Wait for ack
			bcc		NoErr2
Err1:		lda		#$28
			rts

NoErr2:		lda		#$00
			clc
			rts

Invalid:
			lda		#$00
			sta		MaxUnits
			lda		#$28
			sec
			rts

ReadPartition:
			lda		#$02
			sta		RetryCount
TryRead:	lda		#$00
			sta		Track
			sta		Track+1
			sta		Head
			sta		Sector
			jsr		TRYME
			bcc		VERR
			dec		RetryCount
			beq		VERR
			jsr		RecalDrive
			jmp		TryRead

TRYME:		lda		#$20					; Send a read command (But don't actually read yet)
			jsr		SendPacket
			jmp		WaitDataIn				; Wait for readiness

VERR:		rts

RecalDrive:
			ldx		MSlot16
			lda		#$00
			sta		ATReset,x
			lda		#$80
			sta		ATReset,x
			lda		#$10
			jsr		SendPacket
			jmp		WaitStatus

WaitStatus:
			ldx		MSlot16
			ldy		#$00
			sty		Temp
			lda		#$F1
			sta		Temp+1
@A:			lda		ATStatus,x
			bpl		NotBusy
			iny
			bne		@A
			inc		Temp
			bne		@A
			inc		Temp+1
			bne		@A
IOErr:		lda		#$27
			sec
			rts

NotBusy:	and		#$01
			bne		IOErr
			clc
			rts

;
; Remove y*2 bytes (y words) from port (Note that this will only retain the low 8 bits!)
;
KillYWords:
			lda		ATStatus,x
			and		#$08
			beq		KillYWords
			lda		ATData16,x
			dey
			bne		KillYWords
			rts

;
; Remove all bytes from port until the Data Transfer Requested bit is off
;
KillToDRQ:
			lda		ATData16,x
			lda		ATStatus,x
			and		#$08
			bne		KillToDRQ
			rts

ReadBlock:
WriteBlock:	jsr		FindBlock
			lda		#$04
			sta		RetryCount
OneTry:		lda		ProCommand
			lsr		a
			bcs		DoRead
			jsr		Write1Block
			bcs		Retry
NoErr1:		lda		#$00
			tax
			rts

DoRead:		jsr		Read1Block
			bcc		NoErr1
Retry:		jsr		RecalDrive
			dec		RetryCount
			bne		OneTry
IOError:	sec
			lda		#$27
			rts

WriteErr:
			lda		#$2B
			ldx		#$00
			ldy		#$00
			rts

Read1Block:
			lda		#$20
			jsr		SendPacket
			jsr		WaitDataIn
			bcs		OhWell
			ldx		MSlot16
			ldy		#$00
@1:			lda		ATData16,x
			sta		(SosBuf),y
			iny
			lda		ATData16+1,x
			sta		(SosBuf),y
			iny
			bne		@1
			jsr		BumpSosBuf
@2:			lda		ATData16,x
			sta		(SosBuf),y
			iny
			lda		ATData16+1,x
			sta		(SosBuf),y
			iny
			bne		@2
			dec		SosBuf+1
			jmp		WaitStatus

OhWell:		sec
			rts

Write1Block:
			lda		#$30
			jsr		SendPacket
			jsr		WaitDataOut
			ldy		#$00
WriteSector:
			ldx		MSlot16
			lda		(SosBuf),y
			sta		ATData16,x				; C090=S1,C0A0=S2,C0B0=S3,C0C0=S4
			iny
			lda		(SosBuf),y
			sta		ATData16+1,x
			iny
			bne		WriteSector
			jsr		BumpSosBuf
Local:
			lda		(SosBuf),y
			sta		ATData16,x
			iny
			lda		(SosBuf),y
			sta		ATData16+1,x
			iny
			bne		Local
			dec		SosBuf+1
			jmp		WaitStatus				; Return through WaitStatus

SendPacket:
			pha
			ldx		MSlot16
			lda		Head
			ora		#$A0
			sta		ATHead,x
			lda		#$01
			sta		ATSectorCount,x
			clc
			adc		Sector
			sta		ATSectorNumber,x
			lda		Track
			sta		ATCylL,x
			lda		Track+1
			sta		ATCylH,x
			pla
			sta		ATCommand,x
			rts

;
; Locate the drive block number given SOS_Unit, ProBlock(+1+2)
; Sets Track, Sector, Head
;
FindBlock:
			ldy		SOS_Unit
			clc
			lda		PartLo,y
			adc		ProBlock
			sta		Track
			lda		PartMd,y
			adc		ProBlock+1
			sta		Track+1
			lda		PartHi,y
			adc		ProBlock+2
			sta		Track+2
			ldx		DriveType
			lda		DHeadCyl,x
			sta		Head
			lda		DHeadCyl+1,x
			sta		Sector
			ldx		#$00
			stx		Temp
			stx		Temp+1
			ldy		#$18
			lda		Track+2
			bne		Do24Bit
			lda		Track+1
			beq		Do8Bit
			sta		Track+2
			lda		Track
			sta		Track+1
			ldy		#$10
			bne		Do16Bit
Do8Bit:		lda		Track
			sta		Track+2
			ldy		#$08
			stx		Track+1
Do16Bit:	stx		Track
Do24Bit:	asl		Track
			rol		Track+1
			rol		Track+2
			rol		Temp
			rol		Temp+1
			sec
			lda		Temp
			sbc		Head
			tax
			lda		Temp+1
			sbc		Sector
			bcc		@A
			stx		Temp
			sta		Temp+1
			inc		Track
@A:			dey
			bne		Do24Bit
			sty		Head
			ldx		DriveType
@B:			sec
			lda		Temp
			sbc		DSectors,x
			tay
			lda		Temp+1
			sbc		DSectors+1,x
			bcc		CF
			sty		Temp
			sta		Temp+1
			inc		Head
			bne		@B
CF:			lda		Temp
			sta		Sector
			rts

			.ENDPROC
			.END