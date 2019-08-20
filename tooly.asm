;----------------------------------------------------------------
; constants
;----------------------------------------------------------------
  PPUSTATUS      = $2002
  PPUADDR        = $2006
  PPUDATA        = $2007

	STATETITLE     = $00  ; displaying title screen
	STATEPLAYING   = $01  ; move paddles/ball, check for collisions
	STATEGAMEOVER  = $02  ; displaying game over screen

	RIGHTWALL      = $F4  ; when ball reaches one of these, do something
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

  gamestate       .dsb 1

  toolyx          .dsb 1
  toolyy          .dsb 1
  toolyup         .dsb 1
  toolydown       .dsb 1
  toolyleft       .dsb 1
  toolyright      .dsb 1
  toolyspeed      .dsb 1

  playerx         .dsb 1
  playery         .dsb 1

  buttons         .dsb 1
  score           .dsb 1
  pointerLo       .dsb 1   ; pointer variables are declared in RAM
  pointerHi       .dsb 1   ; low byte first, high byte immediately after
  scoreOnes       .dsb 1   ; byte for each digit in the decimal score
  scoreTens       .dsb 1
  scoreHundreds   .dsb 1

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

	.inesprg 1   ; 1x 16KB PRG code
	.ineschr 1   ; 1x  8KB CHR data
	.inesmap 0   ; mapper 0 = NROM, no bank swapping
	.inesmir 1   ; background mirroring

;----------------------------------------------------------------
; PGR
;----------------------------------------------------------------

  .org $C000

vblankwait:    ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait
  RTS

LoadToPPU:
  LDY #$00              ; start out at 0
LoadToPPULoop:
  LDA (pointer), y      ; load data from address
  STA PPUDATA           ; write to PPU
  INY
  DEC datasize
  BNE LoadToPPULoop
  RTS

Reset:
	SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs
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
  LDA PPUSTATUS         ; read PPU status to reset the high/low latch
  setPpuAddr #$3F00
  setPointer palette
  LDA #$20
  STA datasize
  JSR LoadToPPU

  ; $0200-$02FF	contains 256 bytes to be copied to OAM during next vertical blank
LoadSprites:
	LDX #$00              ; start at 0
LoadSpritesLoop:
	LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero

LoadBackground:
  LDA PPUSTATUS         ; read PPU status to reset the high/low latch
  setPpuAddr #$2000
  setPointer background
  LDA #$00
  STA datasize
  LDX #$00
LoadBackgroundLoop:
  JSR LoadToPPU
  INC pointer+1         ; low byte went 0 to 256, so high byte needs to be changed now
  INX
  CPX #$04
  BNE LoadBackgroundLoop

LoadAttribute:
  LDA PPUSTATUS         ; read PPU status to reset the high/low latch
  setPpuAddr #$23C0
  setPointer attribute
  LDA #$08
  STA datasize
  JSR LoadToPPU

;;;Set some initial ball stats
  LDA #$00
  STA toolyup
  STA toolydown
  STA toolyleft
  STA toolyright

  LDA #$20
  STA toolyy

  LDA #$20
  STA toolyx

  LDA #$03
  STA toolyspeed

  LDA #$50
  STA playery

  LDA #$80
  STA playerx

;;;Set initial score value
  LDA #$00
  STA scoreOnes
  STA scoreTens
  STA scoreHundreds

;;:Set starting game state
  LDA #STATEPLAYING
  STA gamestate


  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001


Forever:
	JMP Forever     ;jump back to Forever, infinite loop



NMI:
	LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  JSR DrawScore

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  ;;;all graphics updates done by here, run game engine

  JSR ReadController  ;;get the current button data for player 1

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

	MoveToolyRight:
  LDA toolyright
  BEQ MoveToolyRightDone   ;;if toolyright=0, skip this section

MoveToolyRightDone:


	MoveToolyLeft:
		LDA toolyleft
  BEQ MoveToolyLeftDone   ;;if ballleft=0, skip this section

MoveToolyLeftDone:


	MoveToolyUp:
		LDA toolyup
  BEQ MoveToolyUpDone   ;;if ballup=0, skip this section

MoveToolyUpDone:


	MoveToolyDown:
		LDA toolydown
  BEQ MoveToolyDownDone   ;;if ballup=0, skip this section

MoveToolyDownDone:


	MovePlayerUp:
		LDA buttons
  AND #%00001000
  BEQ MovePlayerUpDone

  LDA playery
  SBC #$01
  STA playery
  JSR IncrementScore
  ;;if up button pressed
  ;;  if paddle top > top wall
  ;;    move paddle top and bottom up
MovePlayerUpDone:


	MovePlayerDown:
		LDA buttons
  AND #%00000100
  BEQ MovePlayerDownDone

  LDA playery
  ADC #$01
  STA playery
  ;;if down button pressed
  ;;  if paddle bottom < bottom wall
  ;;    move paddle top and bottom down
MovePlayerDownDone:


	MovePlayerLeft:
		LDA buttons
  AND #%00000010
  BEQ MovePlayerLeftDone

  LDA playerx
  SBC #$01
  STA playerx
  ;; JSR IncrementScore
  ;;if up button pressed
  ;;  if paddle top > top wall
  ;;    move paddle top and bottom up
MovePlayerLeftDone:

	MovePlayerRight:
		LDA buttons
  AND #%00000001
  BEQ MovePlayerRightDone

  LDA playerx
  ADC #$01
  STA playerx
  ;;if down button pressed
  ;;  if paddle bottom < bottom wall
  ;;    move paddle top and bottom down
MovePlayerRightDone:


	CheckCollision:
		;;if ball x < paddle1x
  ;;  if ball y > paddle y top
  ;;    if ball y < paddle y bottom
  ;;      bounce, ball now moving left
CheckCollisionDone:

	JMP GameEngineDone


UpdateSprites:
	;; Player
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


  ;;update tooly sprites
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
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadControllerLoop:
	LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons      ; bit0 <- Carry
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
