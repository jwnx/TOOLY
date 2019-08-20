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

	STATETITLE     = $00  ; Displaying title screen
	STATEPLAYING   = $01  ; Move paddles/ball, check for collisions
	STATEGAMEOVER  = $02  ; Displaying game over screen

	RIGHTWALL      = $F4  ; When ball reaches one of these, do something
	TOPWALL        = $20
	BOTTOMWALL     = $E0
	LEFTWALL       = $04

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

  ; For function CheckCollision
  coordx          .dsb 1
  coordy          .dsb 1

  ; Score
  score           .dsb 1
  scoreOnes       .dsb 1   ; Byte for each digit in the decimal score
  scoreTens       .dsb 1
  scoreHundreds   .dsb 1

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
  BIT $2002
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
  CPX #$20              ; compare X to hex $20, decimal 32
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
  LDA #$08
  STA datasize
  JSR LoadToPPU

  ; Set initial state
  LDA #$20
  STA toolyy

  LDA #$20
  STA toolyx

  LDA #$FF
  STA playery

  LDA #$FF
  STA playerx

  ; Set initial score value
  LDA #$00
  STA scoreOnes
  STA scoreTens
  STA scoreHundreds

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

  JSR DrawScore

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
  BEQ EngineTitle    ;;game is displaying title screen

  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver  ;;game is displaying ending screen

  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying   ;;game is playing

GameEngineDone:
	JSR UpdateSprites  ;;set ball/paddle sprites from positions
  RTI             ; return from interrupt

;;;;;;;;

EngineTitle:
	;;if start button pressed
  ;;  turn screen off
  ;;  load game screen
  ;;  set starting paddle/ball position
  ;;  go to Playing State
  ;;  turn screen on
  JMP GameEngineDone

;;;;;;;;;

EngineGameOver:
	;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on
  JMP GameEngineDone

;;;;;;;;;;;

EnginePlaying:

MoveTooly:
  LDA #$01
  EOR turn
  STA turn
  BEQ MoveToolyYDone
  LDA playerx
  CMP toolyx
  BEQ MoveToolyXDone
  BCC MoveToolyLeft
  INC toolyx
  JMP MoveToolyXDone
MoveToolyLeft:
  DEC toolyx
MoveToolyXDone:
  LDA playery
  CMP toolyy
  BEQ MoveToolyYDone
  BCC MoveToolyUp
  INC toolyy
  JMP MoveToolyYDone
MoveToolyUp:
  DEC toolyy
MoveToolyYDone:

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

JMP GameEngineDone

  ; Convert pixels to blocks
NormalizeCoord:
  LSR A
  LSR A
  LSR A
  RTS

LoadPlayerCoords:
  LDA playerx
  JSR NormalizeCoord
  STA coordx
  LDA playery
  JSR NormalizeCoord
  STA coordy
  RTS

LoadToolyCoords:
  LDA toolyx
  JSR NormalizeCoord
  STA coordx
  LDA toolyy
  JSR NormalizeCoord
  STA coordy
  RTS

  ; Takes in (coordx,coordy), returns Z = 1 if there's a collision
CheckCollision:
  setPointer collision
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

  ADC #$04
  STA $0208
  STA $020C

  ;LDA player_tile_1
  ;STA $0201
  ;STA $0205
  ;STA $0209
  ;STA $020D

  ;LDA #$00
  ;STA $0202
  ;STA $0206
  ;STA $020A
  ;STA $020E

  LDA playerx
  STA $0203
  STA $020B

  ADC #$04
  STA $0207
  STA $020F

  ;; Tooly

  LDA toolyy
  STA $0210
  STA $0214

  ADC #$04
  STA $0218
  STA $021C

  LDA toolyx
  STA $0213
  STA $021B

  ADC #$04
  STA $0217
  STA $021F

  RTS

DrawScore:
	LDA $2002
  LDA #$20
  STA $2006
  LDA #$20
  STA $2006          ; start drawing the score at PPU $2020

  LDA scoreHundreds  ; get first digit
;  CLC
;  ADC #$30           ; add ascii offset  (this is UNUSED because the tiles for digits start at 0)
  STA $2007          ; draw to background
  LDA scoreTens      ; next digit
;  CLC
;  ADC #$30           ; add ascii offset
  STA $2007
  LDA scoreOnes      ; last digit
;  CLC
;  ADC #$30           ; add ascii offset
  STA $2007
  RTS

IncrementScore:
IncOnes:
	LDA scoreOnes      ; load the lowest digit of the number
  CLC
  ADC #$01           ; add one
  STA scoreOnes
  CMP #$0A           ; check if it overflowed, now equals 10
  BNE IncDone        ; if there was no overflow, all done
IncTens:
	LDA #$00
  STA scoreOnes      ; wrap digit to 0
  LDA scoreTens      ; load the next digit
  CLC
  ADC #$01           ; add one, the carry from previous digit
  STA scoreTens
  CMP #$0A           ; check if it overflowed, now equals 10
  BNE IncDone        ; if there was no overflow, all done
IncHundreds:
	LDA #$00
  STA scoreTens      ; wrap digit to 0
  LDA scoreHundreds  ; load the next digit
  CLC
  ADC #$01           ; add one, the carry from previous digit
  STA scoreHundreds
IncDone:

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
	.db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ; background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ; sprite palette

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
	.db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 7
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 8
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 9
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 11
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 12
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 13
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 14
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 15
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 16
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 17
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 18
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 19
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 20
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 21
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 22
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 23
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 25
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 26
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 27
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 28
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 29
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 30
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

attribute:
	.db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000

  .db $24,$24,$24,$24, $47,$47,$24,$24
  .db $47,$47,$47,$47, $47,$47,$24,$24
  .db $24,$24,$24,$24 ,$24,$24,$24,$24
  .db $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms
  .db $47,$47,$47,$47, $47,$47,$24,$24
	.db $24,$24,$24,$24 ,$24,$24,$24,$24
  .db $24,$24,$24,$24, $55,$56,$24,$24

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

  .incbin "mario.chr"
