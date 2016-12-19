;NES KEYBOARD
;written by Zack Nelson

  .macro ld_point	;loads a destination to pointer
    lda #low(\1)
    sta <pointer
    lda #high(\1)
    sta <pointer+1
  .endm
  
  .macro ld_2006	;puts an address into $2006
    lda $2002
    lda #high(\1)
    sta $2006
    lda #low(\1)
    sta $2006
  .endm
	
sprite .func sprites +((\1) *4)		; Take a sprite number, returns address
	
  .inesprg 2   ; 2x 16KB PRG code = 32KB
  .ineschr 0   ; 0x  8KB CHR data = CHR RAM
  .inesmap 1   ; mapper 1 = MMC1
  .inesmir %10   ; bottom bit for mirroring is completely ignored, mirroring must be set in your code
                 ; however the top bit is used to enable the WRAM battery

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0

pointer		.rs 2		;a general pointer

temp		.rs 1 ;pass values/do math/stort time store
count .rs 1 ;a general counter
caps_flag	.rs 1 ;caps lock/shift flag
escape_flag	.rs 1
scroll_val	.rs 1
scroll_flag .rs 1

nt_pointer_H .rs 1
nt_pointer_L .rs 1

i_b_len = 16 ;constant power of 2
in_buffer	.rs 16 ; holds text from keyboard
in_r_pointer .rs 1 ;text read position
in_w_pointer .rs 1 ;text write position
in_count .rs 1 ;# of bytes in the buffer

o_b_len = 32 ;constant power of 2
out_buffer .rs 32 ; holds text to be written to screen
out_r_pointer .rs 1 ;text read position
out_w_pointer .rs 1 ;text write position
out_count .rs 1 ;# of bytes in the buffer

pc .rs 1 ;brainf prog count
prog_offset .rs 1 ;brainf prog offset

com_offset .rs 1 ;com_log offset
read_p .rs 1
start_p .rs 1

jmp_NMI_flag .rs 1
NMI_point_l .rs 1
NMI_point_h .rs 1

pointer2 .rs 2
pointer3 .rs 2
pointer4 .rs 2
  .rsset $0100
stack		.rs 256 ;;not actually needed, just a place holder

  .rsset $0200
sprites		.rs 256  ;sprite ram area

  .rsset $0300
ram			.rs 256 ;ram for apps

  .rsset $0400
;program		.rs 256 ;space from brainf prog

  .rsset $0500
com_log		.rs 256

;;;;These variables have been moved to the WRAM area at $6000-7FFF
;; they are set up normally just like the console RAM, but cannot be used until the WRAM is enabled

  .rsset $6000
program		.rs 256 ;space from brainf prog

;resetSignature  .rs 2    ;;signature in WRAM to detect how many times the console was reset
;resetCount      .rs 1    ;;if the battery is enabled, this will count up ever time the ROM is loaded or reset
                         ;;if the battery is disabled, this will clear when the ROM is loaded but count up when it is reset

;;;;;;;;;;;;;;;;;;
  .bank 0
  .org $8000

  .bank 1
  .org $A000

  .bank 2
  .org $C000   ;8KB graphics in this fixed bank

  .bank 3    ;all code will go in the last 8KB, which is not a swappable bank
  .org $E000    
Graphics:
  .incbin "ascii.chr"
;;;;first some subroutines
LoadSpritePalette:      ;;set correct address, then jump to the palette loading loop
  ld_2006 $3F10
  jmp LoadPalette


LoadBGPalette:          ;;set correct address, then jump to the palette loading loop
  ld_2006 $3F00

LoadPalette:
  LDA <temp        ;load which palette number to use
  CLC
  ASL A
  ASL A                 ;each palette is 16 bytes
  ASL A                 ;so shift 4 times to multiply by 16
  ASL A
  TAY                   ;transfer A to Y, because Y will use used as index below
   
  LDX #$00              ; byte count starts at 0
LoadPalettesLoop:
  ;TODO: change to zp,x instead of abs,y
  LDA palettes, y       ; load data from address (palette + the value in y)
  STA $2007             ; write to PPU
  INY
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10 = 16 bytes
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, all done
  RTS
;;;;;;;;;;;;;;;;;

  
LoadCHRRAM:            ;;copies 8KB of graphics from PRG to CHR RAM
  ld_2006 $1000
  ldy #$00
  ldx #$80             ;8 x 128 bytes = 1 KB
  lda #LOW(Graphics)
  sta <pointer
  lda #HIGH(Graphics)  ;get the address of the graphics data ($C000)
  sta <pointer+1         ;put into our source pointer
LoadCHRRamLoop:
  lda [pointer],y    ;copy from source pointer
  sta $2007            ;to PPU CHR RAM area
  iny
  cpy #08
  bne LoadCHRRamLoop   ;;loop 256 times
  ;inc sourceHi         ;;then increment the high address byte
  clc
  lda <pointer
  adc #08
  sta <pointer
  lda <pointer+1
  adc #0
  sta <pointer+1

  lda #$0
blank_loop: ;store blank 8 times and clear y
  sta $2007
  dey
  bne blank_loop

  dex                  ;do that 128 times
  bne LoadCHRRamLoop   
LoadCHRRamDone:
  rts
    
;;;;;;;;;;;;;;;;;;;;;;;

ConfigWrite:     ; make sure this is in the last PRG bank so the RTS doesn't get swapped away
  LDA #$80
  STA $8000      ; reset the shift register
  ldy #$5
  LDA #%00001111 ; 8KB CHR, 16KB PRG, $8000-BFFF swappable, hor mirroring
conf_loop: 
  STA $8000      ; first data bit
  LSR A          ; shift to next bit
  dey
  bne conf_loop
  ;STA $8000      ; second data bit
  ;LSR A          ; etc
  ;STA $8000
  ;LSR A
  ;STA $8000
  ;LSR A
  ;STA $8000
  RTS

PRGBankWrite:     ; make sure this is in last bank so it doesnt get swapped away
  LDA <temp  ; load bank number into A
  
  AND #%01111111  ; clear the WRAM bit so it is always enabled
  ldy #$5
pg_w_loop:  
  STA $E000       ; first data bit
  LSR A           ; shift to next bit
  dey
  bne pg_w_loop
  ;STA $E000
  ;LSR A
  ;STA $E000
  ;LSR A
  ;STA $E000
  ;LSR A
  ;STA $E000
  RTS

vblankwait:   ;;manually wait for a vblank to happen
  bit $2002
  bpl vblankwait
  rts
;;;;;;;;;;;;;;;;;;;;;;

RESET:
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

  jsr vblankwait    ; First wait for vblank to make sure PPU is ready

  jsr ConfigWrite   ; Set up the MMC1 banking and config

  lda #$00
  sta <temp
  jsr PRGBankWrite  ; do this switch to enable WRAM

  ;;8KB CHR RAM is used, so no banking or switching is needed
  ;;those registers are left alone
  LDA #$00
clrmem:
  
  ;STA <$00, x
  STA $0100, x
  ;STA $0200, x
  ;STA $0300, x
  ;STA $0400, x
  ;STA $0500, x
  ;STA $0600, x
  ;STA $0700, x
  
  ;STA $6000, x
  ;STA $6100, x
  ;STA $6200, x
  ;STA $6300, x
  ;STA $6400, x  ;;clear out the WRAM we use too
    
  INX
  BNE clrmem
  jsr clear_ram
   
  jsr vblankwait      ; Second wait for vblank, PPU is ready after this
  jsr vblankwait
;;load all the graphics information
  lda #$00
  sta <temp
  jsr LoadCHRRAM        
  jsr LoadBGPalette
  jsr LoadSpritePalette    

  ;make cursor
  ld_2006 $0010
  ldy #$0f
  lda #%00000000
sprite_loop:
  sta $2007            ;to PPU CHR RAM area  
  dey
  bne sprite_loop
  lda #$FF
  sta $2007
  
  jsr clear_screen

  txa
  sta <out_w_pointer
  sta <out_count
  
  lda #$01
  sta sprites+1
  lda #$80
  sta sprites
  sta sprites+3

  jsr vblankwait   ;;wait for vblank so screen isnt turned on while rendering is happening
  
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta $2000
  sta <scroll_flag

  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta $2001

  lda #$00        ;;tell the ppu there is no background scrolling
  ;sta $2006
  ;sta $2006
  sta $2005
  sta $2005
  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  ;lda #%11		;allow ps2, float clock
  ;sta $4800
  lda #1
  sta $4800
  sta $4801
  
  ;lda #$01
  ;sta $4016
  
  lda #high($2040)
  sta <nt_pointer_H ;setup name table pointers
  lda #low($2040)
  sta <nt_pointer_L
  
  ld_point welcome
  jsr print_str
  jmp welcome_over
;welcome: .db "NES Keyboard v0 - Skidlz\n>",0
;welcome: .db "0123456789abcdef0123456789abcde\n>",0
welcome: .db "NES OS v0 - Zack Nelson\n>",0
welcome_over:
  inc <com_offset
  lda #$0a
  sta com_log
Forever:
  jsr vblankwait
  
  ;ld_point program
  lda <com_offset
  sta <count
  ld_point com_log
  lda #$00 ;clear newline flag
  sta <temp
  jsr buffer_copy
  pha
  lda <count
  sta <com_offset
  pla
  lda <temp ;check nl flag
  beq controller
  jsr com_match
  ld_point prompt
  jsr print_str
  
  
controller:
  lda #$01
  sta $4016
  lsr a ;clear a
  sta $4016

CHECK_A:  
  lda $4016
  and #$01
  beq CHECK_B
  
  lda #'A'
  ldx <in_w_pointer ;if we did get a character
  sta <in_buffer, x ;store it in our buffer
  inx
  txa
  and #i_b_len-1 ;roll over at 16
  sta <in_w_pointer
  inc <in_count
  
CHECK_B:
  lda $4016
  and #$01
  beq CHECK_SELECT
  lda #'B'

  ldx <in_w_pointer ;if we did get a character
  sta <in_buffer, x ;store it in our buffer
  inx
  txa
  and #i_b_len-1 ;roll over at 16
  sta <in_w_pointer
  inc <in_count
  
CHECK_SELECT:
  lda $4016
  and #$01
  beq CHECK_START
  
CHECK_START:
  lda $4016
  and #$01
  beq CHECK_UP
  ;jsr brainf
  ;jmp Forever
  
CHECK_UP:
  lda $4016
  and #$01
  beq CHECK_DOWN
  dec <scroll_val
  lda <scroll_val
  cmp #$F0
  bne CHECK_DOWN
  lda #$E0
  sta <scroll_val
  
  lda #%10010010   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta <scroll_flag

CHECK_DOWN:
  lda $4016
  and #$01
  beq CHECK_LEFT
  inc <scroll_val
  lda <scroll_val
  cmp #$F0
  bne CHECK_LEFT
  lda #$00
  sta <scroll_val
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta <scroll_flag
  
CHECK_LEFT:
  lda $4016
  and #$01
  beq CHECK_RIGHT
  ;load last command
  ;buggy
  ldx <com_offset
  inx
  jsr com_start_init
  
;NOT WORKING
;  stx <temp
;  lda <com_offset
;  sec
;  sbc <temp
;  beq skip_cl
;  tax
;  sta <count
;  lda #0
;  sec
;  sbc <count
;  sta <count
;  jsr print_char
;  lda #0
;clear_line:
;  jsr print_char
;  dex
;  bne clear_line
;  ldx <temp
;  lda <count

;  jsr print_char
;skip_cl:
;END

  dex
  jsr com_start_init
  ldy <com_offset
  lda com_log, x
com_load_loop:
  sta com_log, y
  jsr print_char
  inx
  iny
  lda com_log, x
  cmp #$0a ;nl
  bne com_load_loop
com_load_end:
  tya
  sta <com_offset
  
  
CHECK_RIGHT:
  lda $4016
  and #$01
  beq CONTROLLER_DONE
  
CONTROLLER_DONE:

  ;move cursor
  lda nt_pointer_L
  sta $203
  lda nt_pointer_H
  asl $203
  rol a
  asl $203
  rol a
  asl $203
  rol a
  asl a
  asl a
  asl a
  sec
  sbc scroll_val
  sta $200
    
jmp_Forever:
  jmp Forever     ;jump back to Forever, infinite loop, waiting for NMI
  
palettes:
  .db $1D,$30,$30,$30, $1D,$30,$30,$30, $1D,$30,$30,$30, $1D,$30,$30,$30   ;; palette 0
  ;.db $1D,$20,$1D,$1D, $1D,$20,$1D,$1D, $1D,$20,$1D,$1D, $1D,$20,$1D,$1D   ;; palette 1
  ;.db $1D,$10,$1D,$1D, $1D,$10,$1D,$1D, $1D,$10,$1D,$1D, $1D,$10,$1D,$1D   ;; palette 2
  ;.db $1D,$00,$1D,$1D, $1D,$00,$1D,$1D, $1D,$00,$1D,$1D, $1D,$00,$1D,$1D   ;; palette 3
  ;.db $1D,$03,$1D,$1D, $1D,$03,$1D,$1D, $1D,$03,$1D,$1D, $1D,$03,$1D,$1D   ;; palette 4
;test:
;  .incbin "test.txt"

NMI:
  ;todo: fix too many newline problem
  ;if too many nls are in buffer, we overshoot vblank
  
  ;pha
  ;tya
  ;pha
  ;txa
  ;pha
  ;lda <temp
  ;pha
  ;LDA #$00
  ;STA $2003       ; set the low byte (00) of the RAM address
  ;LDA #$02
  ;STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  ;hijack NMI
  ;lda <jmp_NMI_flag
  ;beq no_NMI_jmp
  
  ;pla
  ;sta <temp
  ;pla
  ;tax
  ;pla
  ;tay
  ;pla
  
  ;jmp [NMI_point_l]
  pha
  ;hijack NMI
  lda <jmp_NMI_flag
  beq no_NMI_jmp
  pla
  jmp [NMI_point_l]
  
no_NMI_jmp:
  tya
  pha
  txa
  pha
  lda <temp
  pha
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  lda <out_count
  beq buffer_empty ;if zero, buffer empty
  ldx <out_r_pointer ;load pointer
  lda $2002
  lda nt_pointer_H
  sta $2006
  lda nt_pointer_L
  sta $2006
  
  ldy #$00 ;y is write count 
  
write_loop:
  lda <out_buffer, x ;load from buffer
  bpl check_NL
  jsr add_offset
  jmp new_line
check_NL:
  cmp #10 ;newline
  bne no_newline
  clc ;handling newline
  tya
  adc #$20
  adc nt_pointer_L
  and #%11100000
  pha
  lda #$00
  adc nt_pointer_H
  sta $2006
  pla
  sta $2006
  sec
  sbc nt_pointer_L
  tay
  jmp new_line
no_newline:
  sta $2007
  iny
new_line:
  inx
  txa
  and #o_b_len-1 ;roll over at 16
  tax
  dec <out_count
  bne write_loop
  
  sty <temp
  clc
  lda nt_pointer_L
  adc <temp
  sta nt_pointer_L
  lda nt_pointer_H
  bit <temp
  bmi add_num
  adc #0
  jmp *+5
add_num:
  sbc #0

  sta nt_pointer_H
  
  ;todo change name tables
  cmp #$23
  bne write_exit
  lda nt_pointer_L
  cmp #$C0
  ;bne write_exit
  bcc write_exit
  lda #$20
  sta nt_pointer_H
  sta nt_pointer_L
  
write_exit:  
  txa
  sta <out_r_pointer
buffer_empty:

  lda $2002
  lda #$00        ;;tell the ppu there is no background scrolling
  sta $2006
  sta $2006
  sta $2005
  lda <scroll_val
  sta $2005
  
  lda <scroll_flag
  sta $2000
  
  
  ;#############################################################
  
  lda $4800
  and #1
  bne read_data ;if clear, no data
  jmp no_data
read_data:
  ldy #%00 ;block ps2, pull low
  ldx #%01 ;float
  lda #$80 ;use bit in c_flag as exit condition
  ;added
  sty $4801
  ;end
  sta <temp

read_keyb:
  sty $4800 ;clock low
  lda $4800
  lsr A
  ror <temp
  stx $4800 ;float
  bcc read_keyb

  lda <temp
  eor #$FF ;invert
  sta <temp
  
  ;remove parity
  sty $4800 ;clock low
  stx $4800 ;float
  
  ;remove stop bit
  sty $4800 ;clock low
  stx $4800 ;float
    
  ;lda #%11		;allow ps2, float clock
  ;sta $4800
  ;added
  stx $4800
  stx $4801
  ;end
  
  lda <escape_flag ;see if we are in an escape sequence
  beq not_escape
escape:
  lda #0
  sta <escape_flag
  jmp no_data
not_escape:
  lda <temp
  cmp #$66+1 ;largest value that is a character
  bcs not_character ;if greater than, branch
  
  sec
  sbc #13 ;<13 = is not a character
  tax ;byte becomes offset
  lda <caps_flag ;lda sets/clears zero
  beq no_caps
  lda ascii_table_high, x ;lda sets/clears zero flag
  jmp check_char
no_caps:
  lda ascii_table_low, x ;lda sets/clears zero flag
check_char:
  beq not_character ;check if we got back a character
  ldx <in_w_pointer ;if we did get a character
  sta <in_buffer, x ;store it in our buffer
  inx
  txa
  and #i_b_len-1 ;roll over at 16
  sta <in_w_pointer
  inc <in_count
  jmp no_data
  
not_character:
  lda <temp
  ldx #2
special_data_loop:
  cmp special_signals, x
  beq load_offset
  dex
  bpl special_data_loop
  jmp no_data
  
load_offset:
  txa
  asl A
  tax
  inx
  lda offset_table, x
  pha
  dex
  lda offset_table, x
  pha
  rts
  
handle_escape:
  inc <escape_flag ;make non zero
  jmp no_data
handle_special:
  jmp no_data
handle_caps:
  lda <caps_flag
  eor #$FF
  sta <caps_flag

no_data:

  pla
  sta <temp
  pla
  tax
  pla
  tay
  pla
  rti
 
special_signals:
  .db $58, $E0, $F0
offset_table:
  .dw handle_caps-1, handle_special-1, handle_escape-1
  
;SUBROUTINE##########
add_offset:
  sta <temp
  bit <temp
  bvs sub_offset
  
  and #%01111111
  sta <temp
  tya
  clc
  adc <temp
add_offset1:
  tay
  clc
  adc nt_pointer_L
  pha
  lda nt_pointer_H

  adc #0
  sta $2006
  pla
  sta $2006
  rts
sub_offset:
  tya
  clc
  adc <temp
  bpl add_offset1
  tay
  clc
  adc nt_pointer_L
  pha
  lda nt_pointer_H
  sbc #$00
  sta $2006
  pla
  sta $2006
  rts

;-----------------------
print_str:
  ldy #$0
  ldx <out_w_pointer
  lda [pointer], y ;load from buffer
print_loop:
  sta <out_buffer, x ;store it in our buffer
  inx
  txa
  and #o_b_len-1 ;roll over at 16
  sta <out_w_pointer
  tax
  iny
  inc <out_count
  jsr wait_buffer
  
  lda [pointer], y ;load from buffer
  bne print_loop
  rts
;-----------------------
wait_buffer: ;spin lock
  lda <out_count
  cmp #o_b_len-1
  beq wait_buffer
  rts
;-----------------------
print_char:
  pha
  jsr wait_buffer
  pla
  stx <temp
  ldx <out_w_pointer
  sta <out_buffer, x ;store it in our buffer
  inx
  txa
  and #o_b_len-1 ;roll over at 16
  sta <out_w_pointer
  inc <out_count
  ldx <temp
  rts
;-----------------------
buffer_copy:
  ;move from in buffer to out buffer
  lda <in_count
  ;beq no_transfer ;if zero, buffer empty
  beq transfer_exit
  ldx <in_r_pointer ;load pointer
  ldy <out_w_pointer ;if we did get a character
  
transfer_loop:
  lda <temp
  bne transfer_end

  lda <in_buffer, x ;load from buffer
  cmp #08
  bne no_bs
  lda #$FF
  sta out_buffer, y
  jsr inc_offset
  lda #$20
  sta out_buffer, y
  jsr inc_offset
  lda #$FF
no_bs:
  sta out_buffer, y ;store it in our buffer
  
  jsr inc_offset
  pha
  ldy <count
  inc <count
  lda <in_buffer, x ;load from buffer
  cmp #08
  bne no_bs_prog
  dec <count
  dec <count
  dey
  lda #' '
no_bs_prog:
  cmp #$0A
  bne transfer
  sta <temp
transfer:
  ;sta program, y ;also store for brainf
  sta [pointer],y

  pla
  tay
    
  inx
  txa
  and #i_b_len-1 ;roll over at 16
  tax
  dec <in_count
  bne transfer_loop
transfer_end:
  txa
  sta <in_r_pointer
  tya
  sta <out_w_pointer
  ;jmp no_transfer
transfer_exit:
  rts
inc_offset:
  ;tya
  iny
  tya
  and #o_b_len-1 ;roll over at 16
  tay
  inc <out_count
  rts
;-----------------------
clear_screen:
  ld_2006 $2000
  
  ldx #192 ;32*30=960 tiles - (256*3)= 192
  ldy #4
  lda #' ' ;space for clear
clear_loop:
  sta $2007
  dex
  bne clear_loop
  dey
  bne clear_loop
  rts
  
clear_ram:
  lda #$0
  tax
clear_ram0:
  STA <$00, x
  ;STA $0100, x
  STA $0200, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  
  STA $6000, x
  STA $6100, x
  STA $6200, x
  STA $6300, x
  STA $6400, x  ;;clear out the WRAM we use too
  STA $6500, x
  STA $6600, x
  STA $6700, x 
  
  INX
  BNE clear_ram0
  rts
  
  .include "commands.asm"
  .include "keyb_snake.asm"
  .include "keyb_life.asm"
  
prompt: .db $0A, '>', 0
  
ascii_table_low:
  ;.db 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,'`',  0
  .db 9,'`',  0
  .db 0,  0,  0,  0,  0,'q','1',  0,  0,  0,'z','s','a','w','2',  0
  .db 0,'c','x','d','e','4','3',  0,  0,' ','v','f','t','r','5',  0
  .db 0,'n','b','h','g','y','6',  0,  0,  0,'m','j','u','7','8',  0
  .db 0,',','k','i','o','0','9',  0,  0,'.','/','l',';','p','-',  0
  .db 0,  0,''',  0,'[','=',  0,  0,  0,  0, 10,']',  0,'\',  0,  0;10=newline
  .db 0,  0,  0,  0,  0,  0,  8
;TODO, case conversion by sbc 32
; lookup symbols only in a table?
ascii_table_high:
  ;.db 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,'~',  0
  .db 9,'~',  0
  .db 0,  0,  0,  0,  0,'Q','!',  0,  0,  0,'Z','S','A','W','@',  0
  .db 0,'C','X','D','E','$','#',  0,  0,' ','V','F','T','R','%',  0
  .db 0,'N','B','H','G','Y','^',  0,  0,  0,'M','J','U','&','*',  0
  .db 0,'<','K','I','O',')','(',  0,  0,'>','?','L',':','P','_',  0
  .db 0,  0,'"',  0,'{','+',  0,  0,  0,  0, 10,'}',  0,'|',  0,  0;10=newline
  .db 0,  0,  0,  0,  0,  0,  8
  ;.org $EA00 ;brainF code
  ;.incbin "brainf.bin"
  ;jmp no_transfer
  
  .org $FE00
  .include "brainf.asm" ;cannot span pages or functions will break
  
  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
