  .rsset $0300
sn_log .rs 256 ;ring buffer of old pos
sn_len = 256

  .rsset $0400
sn_count .rs 1 ;number of segments
sn_point .rs 1 ;pointer in ring buffer
dir_new .rs 1 ;direction buffer
dir .rs 1 ;old firection
timer_max .rs 1 ;speed
start_speed = 14
timer_count .rs 1 ;count for speed
timer_sub .rs 1 ;count for inc in speed
timer_sub_speed = 4
mouse_x .rs 1
mouse_y .rs 1

mod_flag .rs 1
erase_L .rs 1
erase_H .rs 1

old_L .rs 1
old_H .rs 1

write_L .rs 1
write_H .rs 1

rand_gen_h .rs 1
rand_gen_l .rs 1

NMI_flag .rs 1

snake:
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  jsr clear_ram
  
  ld_2006 $0020
  ldy #0
  ldx #16
load_sprites:
  lda spritebin,y    ;copy from source pointer
  sta $2007            ;to PPU CHR RAM area
  iny
  dex
  bne load_sprites
  
  lda #2
  sta sprite(1)+1
  
  jsr clear_screen

  ;todo?
  ;type welcome to snake
  ;type press start
  
  ;SETUP
  lda #start_speed
  sta timer_max
  sta sn_log
  sta sn_log+1
  lda #timer_sub_speed
  sta timer_sub

  jsr RAND_ROL_INIT
  jsr gen_mouse
  
  ;set up border
  ;ld_2006 $2030
  ;ldx #0
  ;lda mice, x
write_mice
  ;sta $2007
  ;inx 
  ;lda mice, x
  ;bne write_mice
  
mice_done:
  ld_2006 $2040
  ldy #1
border0:
  lda #8
  ldx #32
border1:
  sta $2007
  dex
  bne border1
  ld_2006 $2380
  dey
  beq border0
  
  lda #%100;draw down
  sta $2000
  lda $2000
  ld_2006 $2060
  ldy #1
border2:
  lda #9
  ldx #25
border3:
  sta $2007
  dex
  bne border3
  ld_2006 $207F
  dey
  beq border2
  
    
  sec ;draw first segment of snake
  rol NMI_flag
  
  lda #low(snake_NMI)
  sta NMI_point_l
  lda #high(snake_NMI)
  sta NMI_point_h
  
  sec
  rol jmp_NMI_flag ;set flag
  
  lda #0
  sta $2005
  sta $2005
  jsr vblankwait
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta $2000

  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta $2001
  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  
SN_Forever:
  jsr vblankwait
  
SN_controller:
  lda #$01
  sta $4016
  lsr a ;clear a
  sta $4016

SN_CHECK_A:  
  lda $4016
  and #$01
  beq SN_CHECK_B
  ;test
  inc sn_count
  lda sn_count
  and #sn_len-1
  sta sn_count
  
SN_CHECK_B:
  lda $4016
  and #$01
  beq SN_CHECK_SELECT
  jsr gen_mouse
  
SN_CHECK_SELECT:
  lda $4016
  and #$01
  beq SN_CHECK_START
  ;exit
  
SN_CHECK_START:
  lda $4016
  and #$01
  beq SN_CHECK_UP
  ;wait for start
  
SN_CHECK_UP:
  lda $4016
  and #$01
  beq SN_CHECK_DOWN
  lda dir
  cmp #2
  beq SN_CHECK_DOWN
  lda #0
  sta dir_new
  jmp SN_controller_DONE
SN_CHECK_DOWN:
  lda $4016
  and #$01
  beq SN_CHECK_LEFT
  lda dir
  cmp #0
  beq SN_CHECK_DOWN
  lda #2
  sta dir_new
  jmp SN_controller_DONE
SN_CHECK_LEFT:
  lda $4016
  and #$01
  beq SN_CHECK_RIGHT
  lda dir
  cmp #1
  beq SN_CHECK_DOWN
  lda #3
  sta dir_new
  jmp SN_controller_DONE
SN_CHECK_RIGHT:
  lda $4016
  and #$01
  beq SN_controller_DONE
  lda dir
  cmp #3
  beq SN_CHECK_DOWN
  lda #1
  sta dir_new
  
SN_controller_DONE:

  inc timer_count
  lda timer_count
  cmp timer_max  ;see if it's time to move
  beq timer_equ
  jmp SN_Forever
timer_equ:
  jsr RAND_ROL0
  lda #0
  sta timer_count
  ldy sn_point
  lda sn_log,y
  pha
  iny
  iny
  tya
  and #sn_len-1
  tay
  pla
  sty sn_point
  sta sn_log,y
  dey
  tya
  and #sn_len-1
  tay
  lda sn_log,y
  pha
  iny
  iny
  tya
  and #sn_len-1
  tay
  pla
  sta sn_log,y
  
  lda sn_point
  and #sn_len-1
  sta sn_point
  tax
  
  ;move snake
  ;ldx sn_point
  lda dir_new
  sta dir
  cmp #0
  bne sn_right
  ;move up
  inx 
  dec sn_log,x
  jmp sn_move_done
sn_right:
  cmp #1
  bne sn_down
  inc sn_log,x
  jmp sn_move_done
sn_down:
  cmp #2
  bne sn_left
  inx 
  inc sn_log,x
  jmp sn_move_done
sn_left:
  dec sn_log,x
sn_move_done:

  ;check mouse collision
  ldx sn_point
  lda sn_log,x
  cmp mouse_x
  bne no_mouse
  inx
  lda sn_log,x
  cmp mouse_y
  bne no_mouse
  jsr gen_mouse;move mouse
no_mouse: ;no mouse collision

  ;check snake collision
  ldx sn_count
  cpx #1
  beq *+3
  dex
  ldy sn_point
  lda sn_log,y
  sta temp
  cmp #0 ;if in left wall die
  beq game_end
  cmp #31
  beq game_end
  iny
  lda sn_log,y
  dey
  sta count
  cmp #2 ;if in wall die
  beq game_end
  cmp #28
  beq game_end
collision0:
  
  dey
  dey
  tya
  and #sn_len-1
  tay
  lda temp
  cmp sn_log,y
  bne collision1
  iny
  lda sn_log,y
  dey
  cmp count
  bne collision1
  ;handle collision
game_end:
  lda #$00
  sta $2000				; Disable NMI & sprites pattern table
  sta $2001				; Disable background/sprite rendering
  ld_2006 $220b
  ldx #0
  lda game_over,x
write_go:
  sta $2007
  inx 
  lda game_over, x
  bne write_go

  lda #$00
  sta $2005
  sta $2005
  jsr vblankwait   ;;wait for vblank so screen isnt turned on while rendering is happening
  
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta $2000
  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta $2001
self:
  lda #$01
  sta $4016
  lsr a ;clear a
  sta $4016
  lda $4016 ;a
  lda $4016 ;b
  lda $4016 ;sel
  and #1
  beq self_start
  jmp snake
self_start:
  lda $4016 ;start
  and #1
  beq self
  
  jmp RESET
game_over: .db "Game Over",0  
collision1:
  dex
  bne collision0
  
  ;prune sprites
  lda sn_point
  sec
  sbc sn_count
  sec 
  sbc sn_count
  and #sn_len-1
  ;copy to erase buffer
  tax
  jsr gen_nto
  sta erase_H
  dex
  lda sn_log,x
  ora temp
  sta erase_L
  
  ;copy to write buffer
  ldx sn_point
  jsr gen_nto
  sta write_H
  dex
  lda sn_log,x
  ora temp
  sta write_L
  
  ;copy to old buffer
  ldx sn_point
  dex
  dex
  jsr gen_nto
  sta old_H
  dex
  lda sn_log,x
  ora temp
  sta old_L
  
  sec
  rol NMI_flag
  
jmp_SN_Forever:
  jmp SN_Forever     ;jump back to SN_Forever, infinite loop, waiting for NMI
;----- Generate NameTable Offset -----;
gen_nto:
  lda #0
  sta temp
  inx
  lda sn_log,x
  lsr A;2
  ror temp
  lsr A;4
  ror temp
  lsr A;8
  ror temp
  clc
  adc #$20
  rts
;----- Random Number Generator -----;
RAND_ROL_INIT:
	lda #$01
	sta rand_gen_l
	
RAND_ROL0:
	lda rand_gen_h
	asl rand_gen_l
	rol rand_gen_h
	eor rand_gen_l
	and #$20
	beq RAND_ROL1
	inc rand_gen_l ;asl clears last bit, inc sets to 1
RAND_ROL1:
	rts
	
gen_mouse:
  jsr RAND_ROL0
  lda rand_gen_h		;combine low and high values
  asl A
  eor rand_gen_l
  ;lsr A
  and #$F8;#%11111000
  sta sprite(1)+3
  lsr A
  lsr A
  lsr A
  cmp #0
  beq gen_mouse
  cmp #31
  beq gen_mouse
  sta mouse_x
gen_mouse0:
  ;jsr RAND_ROL0
  ;jsr RAND_ROL0
  jsr RAND_ROL0
  jsr RAND_ROL0
  lda rand_gen_h		;combine low and high values
  asl A
  eor rand_gen_l
  ;lsr A
  and #$F8;#%11111000
  sta sprite(1)
  lsr A
  lsr A
  lsr A
  cmp #3
  bcc gen_mouse0
  cmp #28
  bcs gen_mouse0
  sta mouse_y

  lda sn_count
  cmp #(sn_len-1)/2
  beq sn_count_max
  inc sn_count
sn_count_max:
  dec timer_sub
  bne no_timer_sub
  lda #timer_sub_speed
  sta timer_sub
  lda timer_max
  cmp #3
  beq no_timer_sub
  dec timer_max
no_timer_sub:
  rts


snake_NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  lda NMI_flag
  beq snake_NMI_end
  lda old_H
  sta $2006
  lda old_L
  sta $2006
  lda #1
  sta $2007
  
  lda erase_H
  sta $2006
  lda erase_L
  sta $2006
  lda #0
  sta $2007
  
  lda write_H
  sta $2006
  lda write_L
  sta $2006
  lda #2
  sta $2007
  
  lda #0
  sta NMI_flag
snake_NMI_end:
  lda $2002
  lda #$00        ;;tell the ppu there is no background scrolling
  sta $2006
  sta $2006
  sta $2005
  sta $2005
  
  rti
;-----------------------

spritebin: .db $66,$99,$DB,$7E,$5A,$7E,$24,$18,0,0,$5a,$3C,$18,$3C,0,0
mice: .db "Mice:", 0
