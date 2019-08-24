;----------------------------------------------------------------
; Constants
;----------------------------------------------------------------
  PPUCTRL	       = $2000
  PPUMASK	       = $2001
  PPUSTATUS      = $2002
  OAMADDR        = $2003
  PPUSCROLL	     = $2005
  PPUADDR        = $2006
  PPUDATA        = $2007
  OAMDMA         = $4014

  CTRL1          = $4016

  TIMESECONDS1   = $0221
  TIMESECONDS2   = $0225
  TIMEMINUTES1   = $022D
  TIMEMINUTES2   = $0231

	STATETITLE     = $00  ; Displaying title screen
	STATEPLAYING   = $01  ; Move player and tooly
	STATEGAMEOVER  = $02  ; Displaying game over screen

;----------------------------------------------------------------
; Variables
;----------------------------------------------------------------

  .enum $0000

  ; A pointer
  pointer         .dsw 1

  ; For function LoadToPPU
  datasize        .dsb 1

  ; Tooly state
  turn            .dsb 1
  toolyx          .dsb 1
  toolyy          .dsb 1

  ; Player state
  playerx         .dsb 1
  playery         .dsb 1
  health          .dsb 1

  ; For function CheckCollision
  coordx          .dsb 1
  coordy          .dsb 1

  ; Time
  ticks           .dsb 1

  gamestate       .dsb 1

  buttons         .dsb 1

  .ende

;----------------------------------------------------------------
; Macros
;----------------------------------------------------------------

  MACRO setPointer addr
  LDA #<addr            ; Get low byte of Address
  STA pointer           ; Store in pointer
  LDA #>addr            ; Get high byte of Address
  STA pointer+1         ; Store in pointer+1
  ENDM

  MACRO setPpuAddr addr
  LDA #>addr            ; Get high byte of Address
  STA PPUADDR
  LDA #<addr            ; Get low byte of Address
  STA PPUADDR
  ENDM

;----------------------------------------------------------------
; iNES header
;----------------------------------------------------------------

	.inesprg 1            ; 1x 16KB PRG code
	.ineschr 1            ; 1x  8KB CHR data
	.inesmap 0            ; Mapper 0 = NROM, no bank swapping
	.inesmir 1            ; Background mirroring

;----------------------------------------------------------------
; PGR
;----------------------------------------------------------------

  .org $C000

vblankwait:             ; First wait for vblank to make sure PPU is ready
  BIT PPUSTATUS
  BPL vblankwait
  RTS

LoadToPPU:
  LDY #$00              ; Start out at 0
LoadToPPULoop:
  LDA (pointer), y      ; Load data from address
  STA PPUDATA           ; Write to PPU
  INY
  DEC datasize
  BNE LoadToPPULoop
  RTS

Reset:
	SEI                   ; Disable IRQs
  CLD                   ; Disable decimal mode
  LDX #$40
  STX $4017             ; Disable APU frame IRQ
  LDX #$FF
  TXS                   ; Set up stack
  INX                   ; Now X = 0
  STX $2000             ; Disable NMI
  STX $2001             ; Disable rendering
  STX $4010             ; Disable DMC IRQs
  JSR vblankwait

clrmem:
	LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem
  JSR vblankwait

LoadPalettes:
  LDA PPUSTATUS         ; Read PPU status to reset the high/low latch
  setPpuAddr #$3F00
  setPointer palette
  LDA #$20
  STA datasize
  JSR LoadToPPU

  ; $0200-$02FF	contains 256 bytes to be copied to OAM during next vertical blank
LoadSprites:
	LDX #$00              ; Start at 0
LoadSpritesLoop:
	LDA sprites, x        ; Load data from address (sprites +  x)
  STA $0200, x          ; Store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$34
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero

LoadBackground:
  LDA PPUSTATUS         ; Read PPU status to reset the high/low latch
  setPpuAddr #$2000
  setPointer background ; Low byte is 00
  LDA #$00
  STA datasize
  LDX #$00
LoadBackgroundLoop:
  JSR LoadToPPU
  INC pointer+1         ; Low byte went 0 to 256, so high byte needs to be changed now
  INX
  CPX #$04
  BNE LoadBackgroundLoop

LoadAttribute:
  LDA PPUSTATUS         ; Read PPU status to reset the high/low latch
  setPpuAddr #$23C0
  setPointer attribute
  LDA #$3C
  STA datasize
  JSR LoadToPPU

  ; Set initial positions
  LDA #$20
  STA toolyy
  LDA #$20
  STA toolyx

  LDA #$40
  STA playery
  LDA #$80
  STA playerx

  ; Set initial health value
  LDA #$64
  STA health

  ; Set initial time value
  LDA #$00
  STA ticks

  ; Set starting game state
  LDA #STATEPLAYING
  STA gamestate

  LDA #%10010000        ; Enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110        ; Enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
	JMP Forever           ; Initialization is done, loop forever

NMI:
  ; Sprites have been loaded into $0200
	LDA #$00
  STA OAMADDR           ; Set the low byte (00) of the RAM address
  LDA #$02
  STA OAMDMA            ; Set the high byte (02) of the RAM address, start the transfer

  ; This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10000000        ; Enable NMI, background and sprites from Pattern Table 0
  STA PPUCTRL
  LDA #%00011110        ; Enable sprites, enable background, no clipping on left side
  STA PPUMASK
  LDA #$00              ; Tell the ppu there is no background scrolling
  STA PPUSCROLL
  STA PPUSCROLL

  ; All graphics updates done by here, run game engine
  JSR ReadController     ; Get the current button data

GameEngine:
	LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle

  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver

  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying

GameEngineDone:
	JSR UpdateSprites     ; Set sprites from positions
  RTI

EngineTitle:
  JMP GameEngineDone

EngineGameOver:
  JMP GameEngineDone

EnginePlaying:
  JSR CheckHealthDown

MoveTooly:
  JMP MoveToolyDone
  LDA #$01
  EOR turn
  STA turn
  BEQ MoveToolyDone

MoveToolyX:
  LDA playerx
  CMP toolyx
  BEQ MoveToolyY
  JSR LoadToolyCoords
  BCC MoveToolyLeft
MoveToolyRight:
  INC coordx
  JSR CheckCollision
  CMP #$01
  BEQ MoveToolyY
  INC toolyx
  JMP MoveToolyY
MoveToolyLeft:
  DEC coordx
  JSR CheckCollision
  CMP #$01
  BEQ MoveToolyY
  DEC toolyx

MoveToolyY:
  LDA playery
  CMP toolyy
  BEQ MoveToolyDone
  JSR LoadToolyCoords
  BCC MoveToolyUp
MoveToolyDown:
  INC coordy
  JSR LoadToolyCoords
  JSR CheckCollision
  CMP #$01
  BEQ MoveToolyDone
  INC toolyy
  JMP MoveToolyDone
MoveToolyUp:
  DEC coordy
  JSR CheckCollision
  CMP #$01
  BEQ MoveToolyDone
  DEC toolyy
MoveToolyDone:

MovePlayerUp:
	LDA buttons
  AND #%00001000
  BEQ MovePlayerUpDone

  JSR LoadPlayerCoords
  DEC coordy
  JSR CheckCollision
  CMP #$01
  BEQ MovePlayerUpDone
  DEC playery
MovePlayerUpDone:

MovePlayerDown:
	LDA buttons
  AND #%00000100
  BEQ MovePlayerDownDone

  JSR LoadPlayerCoords
  INC coordy
  JSR CheckCollision
  CMP #$01
  BEQ MovePlayerDownDone
  INC playery
MovePlayerDownDone:

MovePlayerLeft:
	LDA buttons
  AND #%00000010
  BEQ MovePlayerLeftDone

  JSR LoadPlayerCoords
  DEC coordx
  JSR CheckCollision
  CMP #$01
  BEQ MovePlayerLeftDone
  DEC playerx
MovePlayerLeftDone:

MovePlayerRight:
	LDA buttons
  AND #%00000001
  BEQ MovePlayerRightDone

  JSR LoadPlayerCoords
  INC coordx
  JSR CheckCollision
  CMP #$01
  BEQ MovePlayerRightDone
  INC playerx
MovePlayerRightDone:

  JSR CheckHealthUp
  JSR IncrementTime

  JMP GameEngineDone

CheckHealthDown:
  LDA playerx
  CMP toolyx
  BNE CheckHealthDownDone
  LDA playery
  CMP toolyy
  BNE CheckHealthDownDone
  DEC health
  BNE CheckHealthDownDone
  LDA #STATEGAMEOVER
  STA gamestate
  JMP GameEngineDone
CheckHealthDownDone:
  RTS

CheckHealthUp:
  setPointer waterandfood
  JSR CheckOverlap
  ADC health
  STA health
  RTS

  ; Convert pixels to blocks
NormalizeCoords:
  LSR coordx
  LSR coordx
  LSR coordx
  LSR coordy
  LSR coordy
  LSR coordy
  RTS

LoadPlayerCoords:
  LDA playerx
  STA coordx
  LDA playery
  STA coordy
  RTS

LoadToolyCoords:
  LDA toolyx
  STA coordx
  LDA toolyy
  STA coordy
  RTS

CheckCollision:
  setPointer collision
  JMP CheckOverlap

  ; Takes in (coordx,coordy), returns Z = 1 if there's a collision
CheckOverlap:
  JSR NormalizeCoords
  LDX #$00
  LDY #$00
CheckCollisionLoop:
  LDA (pointer), y
  INY
  CMP coordy
  BNE Continue
  LDA (pointer), y
  CMP coordx
  BNE Continue
  LDA #$01
  RTS
Continue:
  INY
  BNE CheckCollisionLoop
  INC pointer+1
  INX
  CPX #$01
  BNE CheckCollisionLoop
  LDA #$00
  RTS

UpdateSprites:
	; Player
  LDA playery
  STA $0200
  STA $0204
  ADC #$08
  STA $0208
  STA $020C
  LDA playerx
  STA $0203
  STA $020B
  ADC #$08
  STA $0207
  STA $020F

  ; Tooly
  LDA toolyy
  STA $0210
  STA $0214
  ADC #$08
  STA $0218
  STA $021C
  LDA toolyx
  STA $0213
  STA $021B
  ADC #$08
  STA $0217
  STA $021F

  RTS

IncrementTime:
  INC ticks
  LDA #$07
  AND ticks
  BNE IncDone
  INC TIMESECONDS1
  LDA TIMESECONDS1
  CMP #$FA              ; Check if it overflowed, now equals 10
  BNE IncDone
	LDA #$F0
  STA TIMESECONDS1      ; Wrap digit to 0
  INC TIMESECONDS2
  LDA TIMESECONDS2
  CMP #$F6              ; Check if it overflowed, now equals 6
  BNE IncDone
	LDA #$F0
  STA TIMESECONDS2      ; Wrap digit to 0
  INC TIMEMINUTES1
  LDA TIMEMINUTES1
  CMP #$FA              ; Check if it overflowed, now equals 10
  BNE IncDone
	LDA #$F0
  STA TIMEMINUTES1      ; Wrap digit to 0
  INC TIMEMINUTES2
  LDA TIMESECONDS2
  CMP #$F6              ; Check if it overflowed, now equals 6
  BNE IncDone
	LDA #$F0
  STA TIMEMINUTES2      ; Wrap digit to 0
IncDone:
  RTS

ReadController:
	LDA #$01
  STA CTRL1
  LDA #$00
  STA CTRL1
  LDX #$08
ReadControllerLoop:
	LDA CTRL1
  LSR A                 ; Bit 0 -> Carry
  ROL buttons           ; Carry -> bit 0; bit 7 -> Carry
  DEX
  BNE ReadControllerLoop
  RTS

;----------------------------------------------------------------
; Data
;----------------------------------------------------------------

.org $E000
palette:
  ; Background
  .db $2c,$00,$1a,$3d, $2c,$30,$19,$1d, $2c,$30,$14,$19, $2c,$2e,$30,$3f
  ; Sprites
  .db $18,$37,$01,$1c, $3c,$15,$01,$1c, $19,$3c,$09,$21, $29,$3a,$01,$1c

sprites:
	   ;vert tile attr horiz
  .db $80, $32, $00, $80   ; sprite 0
  .db $80, $33, $00, $88   ; sprite 1
  .db $88, $34, $00, $80   ; sprite 2
  .db $88, $35, $00, $88   ; sprite 3

	   ;vert tile attr horiz
  .db $80, $32, $00, $80   ; sprite 4
  .db $80, $33, $00, $88   ; sprite 5
  .db $88, $34, $00, $80   ; sprite 6
  .db $88, $35, $00, $88   ; sprite 7

background:
  .db $0e,$3c,$2d,$28,$29,$3a,$a4,$2b,$0b,$2d,$2d,$1d,$28,$29,$3a,$08
  .db $09,$0a,$2d,$3d,$1d,$3c,$1d,$2f,$1d,$28,$29,$3a,$a4,$2b,$0b,$1d
  .db $0e,$3c,$2d,$28,$29,$3a,$a4,$2b,$0b,$2d,$0d,$1d,$28,$29,$3a,$08
  .db $09,$0a,$1d,$3d,$84,$3c,$1d,$2f,$1d,$28,$29,$3a,$a4,$2b,$0b,$1d
  .db $0e,$3c,$2d,$28,$29,$3a,$a4,$2b,$0b,$2d,$2d,$2d,$28,$29,$3a,$08
  .db $09,$0a,$1d,$3d,$94,$3c,$1d,$2f,$1d,$28,$29,$3a,$a4,$2b,$0b,$1d
  .db $0e,$3c,$2d,$38,$39,$3a,$a4,$3b,$1b,$2d,$2d,$1d,$38,$39,$3a,$18
  .db $19,$1a,$1d,$3d,$1d,$3c,$1d,$2f,$1d,$38,$39,$3a,$a4,$3b,$1b,$1d
  .db $0e,$3c,$40,$41,$42,$43,$42,$43,$44,$45,$40,$43,$42,$43,$44,$41
  .db $46,$47,$44,$3d,$0e,$3c,$1d,$0f,$42,$43,$44,$43,$40,$41,$42,$43
  .db $0e,$3c,$50,$51,$52,$53,$54,$55,$50,$51,$52,$53,$54,$55,$50,$51
  .db $56,$57,$54,$3d,$0e,$3c,$1e,$1f,$52,$53,$54,$55,$80,$81,$82,$53
  .db $0e,$3c,$60,$61,$62,$63,$64,$65,$66,$67,$60,$61,$62,$63,$64,$65
  .db $66,$67,$60,$3d,$0e,$3c,$2e,$61,$62,$63,$64,$65,$90,$91,$92,$61
  .db $04,$04,$04,$05,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05,$05
  .db $16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$25,$16,$16,$16
  .db $af,$af,$af,$af,$a8,$af,$af,$af,$af,$af,$af,$9f,$af,$af,$af,$af
  .db $16,$16,$16,$16,$25,$16,$16,$16,$34,$35,$16,$16,$16,$16,$16,$16
  .db $25,$48,$49,$4a,$4b,$4c,$4d,$4e,$4e,$4f,$af,$af,$af,$af,$af,$9f
  .db $16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$25,$16,$16,$16,$16
  .db $af,$58,$59,$5a,$5b,$5c,$5d,$5e,$5e,$5f,$af,$af,$af,$26,$af,$af
  .db $16,$16,$16,$16,$16,$16,$36,$37,$26,$16,$16,$16,$16,$16,$16,$16
  .db $af,$68,$69,$6a,$6b,$6c,$6d,$6e,$6e,$6f,$af,$af,$af,$af,$af,$af
  .db $16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$25,$16
  .db $af,$78,$6b,$79,$7a,$7b,$7c,$7d,$7e,$7f,$af,$25,$af,$af,$af,$af
  .db $16,$16,$16,$16,$16,$16,$16,$34,$35,$16,$16,$16,$25,$16,$16,$16
  .db $af,$88,$89,$8a,$8a,$8b,$8c,$8d,$8e,$af,$af,$af,$26,$af,$25,$af
  .db $16,$16,$24,$25,$16,$16,$16,$16,$16,$25,$16,$16,$16,$16,$25,$af
  .db $af,$af,$99,$9a,$9b,$9b,$9b,$9b,$9e,$af,$af,$af,$af,$af,$af,$af
  .db $af,$af,$af,$af,$af,$34,$35,$16,$af,$98,$af,$af,$af,$af,$af,$af
  .db $af,$25,$a9,$aa,$ab,$ac,$ad,$ad,$ae,$af,$af,$af,$af,$af,$af,$af
  .db $af,$af,$af,$af,$af,$16,$16,$16,$af,$af,$af,$af,$98,$af,$98,$af
  .db $98,$af,$af,$af,$af,$af,$af,$bd,$af,$14,$26,$af,$25,$af,$af,$26
  .db $af,$16,$16,$16,$16,$af,$16,$16,$24,$af,$25,$26,$16,$af,$af,$af
  .db $af,$af,$98,$af,$a8,$af,$af,$af,$af,$af,$af,$af,$af,$af,$af,$af
  .db $16,$16,$24,$25,$16,$34,$35,$16,$af,$af,$af,$af,$af,$98,$98,$25
  .db $98,$27,$af,$af,$af,$af,$af,$24,$25,$af,$af,$9f,$af,$af,$af,$af
  .db $16,$16,$16,$af,$16,$16,$16,$16,$af,$16,$24,$af,$af,$af,$af,$af
  .db $98,$af,$14,$a8,$a8,$af,$af,$af,$af,$af,$af,$af,$af,$25,$af,$af
  .db $24,$25,$af,$a8,$af,$9f,$16,$af,$af,$af,$af,$af,$a8,$af,$98,$af
  .db $af,$af,$98,$af,$af,$af,$af,$af,$98,$26,$af,$af,$af,$af,$af,$af
  .db $af,$af,$af,$16,$34,$35,$af,$98,$af,$af,$a8,$af,$af,$a8,$af,$a8
  .db $24,$25,$14,$af,$af,$24,$14,$af,$af,$af,$af,$25,$af,$af,$af,$af
  .db $af,$af,$14,$1d,$1d,$af,$af,$af,$af,$14,$af,$af,$af,$af,$af,$af
  .db $14,$af,$af,$14,$af,$af,$af,$af,$14,$af,$af,$af,$af,$26,$af,$af
  .db $af,$af,$af,$af,$1d,$af,$af,$af,$af,$af,$af,$af,$af,$af,$af,$af
  .db $af,$af,$af,$af,$af,$af,$af,$af,$af,$af,$26,$af,$af,$af,$af,$25
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e8,$a0,$a0,$a0,$a0,$a0,$a0,$a0
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e8,$e8,$e8,$e8,$e8,$e8
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e8,$e8,$e8,$e8,$e8,$e8
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e8,$e8,$e8,$e8,$e8,$e8
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e8,$e8,$e8,$e8,$e8,$e8
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e8,$e8,$e8,$e8,$e8,$e8
  .db $a0,$a0,$a0,$e8,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
  .db $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e8,$e8,$e8,$e8,$e8,$e8

attribute:
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101

collision: ; (x,y) coordinates, from (0,0) to (1F,1D)
  ; Top
  .dw $0000,$0100,$0200,$0300,$0400,$0500,$0600,$0700,$0800,$0900,$0a00,$0b00,$0c00,$0d00,$0e00,$0f00
  .dw $1000,$1100,$1200,$1300,$1400,$1500,$1600,$1700,$1800,$1900,$1a00,$1b00,$1c00,$1d00,$1e00,$1f00
  ; Bottom
  .dw $001C,$011C,$021C,$031C,$041C,$051C,$061C,$071C,$081C,$091C,$0a1C,$0b1C,$0c1C,$0d1C,$0e1C,$0f1C
  .dw $101C,$111C,$121C,$131C,$141C,$151C,$161C,$171C,$181C,$191C,$1a1C,$1b1C,$1c1C,$1d1C,$1e1C,$1f1C
  ; Left
  .dw $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,$0008,$0009,$000A,$000B,$000C,$000D,$000E,$000F
  .dw $0010,$0011,$0012,$0013,$0014,$0015,$0016,$0017,$0018,$0019,$001A,$001B,$001C,$001D
  ; Right
  .dw $1E00,$1E01,$1E02,$1E03,$1E04,$1E05,$1E06,$1E07,$1E08,$1E09,$1E0A,$1E0B,$1E0C,$1E0D,$1E0E,$1E0F
  .dw $1E10,$1E11,$1E12,$1E13,$1E14,$1E15,$1E16,$1E17,$1E18,$1E19,$1E1A,$1E1B,$1E1C,$1E1D
  ; Pad to become multiple of 256
  .dsb 8

waterandfood:
  .dsb 256

;----------------------------------------------------------------
; Interrupt vectors
;----------------------------------------------------------------

IRQ:
  ; No IRQs

  .org $fffa

  .dw NMI
  .dw Reset
	.dw IRQ

;----------------------------------------------------------------
; CHR-ROM
;----------------------------------------------------------------

  .incbin "tooly.chr"
