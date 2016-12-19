;Conway's Game of Life
;written by Zack Nelson

  .rsset $0300
lf_running .rs 1

lf_cursor_x .rs 1
lf_cursor_y .rs 1

lf_NT_L .rs 1
lf_NT_H .rs 1
lf_sprite_number .rs 1
NMI_write_flag .rs 1

loop_counter .rs 1

render_flag .rs 1

lf_r_count .rs 1
lf_l_count .rs 1
lf_u_count .rs 1
lf_d_count .rs 1
lf_start_old .rs 1

  .rsset $6000
life_buf .rs 32*30
  .rsset $6400
life_buf2 .rs 32*30


life:
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  jsr clear_ram

  ld_2006 $1010
  lda #%00000000
  sta $2007
  sta $2007
  lda #%00011000
  sta $2007
  lda #%00111100
  sta $2007
  sta $2007
  lda #%00011000
  sta $2007
  lda #%00000000
  ldy #10
lf_sprite_load:
  sta $2007
  dey
  bne lf_sprite_load

  jsr clear_screen
  jsr clear_screen2

  ;lda #$01 
  lda #9 ;circle from pattern table 2
  sta sprite(0)+1
  lda #$78
  sta sprite(0)
  sta sprite(0)+3
  lda #$f
  sta lf_cursor_x
  sta lf_cursor_y
  

  lda #0
  sta lf_running
  
  lda #$28
  sta nt_pointer_H
  lda #$20
  sta nt_pointer_L
  
  lda #low(lf_NMI)
  sta NMI_point_l
  lda #high(lf_NMI)
  sta NMI_point_h
  
  sec
  rol jmp_NMI_flag ;set flag
  
  lda #0
  sta $2005
  sta $2005
  jsr vblankwait
  ;lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  lda #%10011000
  sta $2000
  sta <scroll_flag

  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta $2001
  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  
  
lf_Forever:
  jsr vblankwait

  lda #$01
  sta $4016
  lsr a ;clear a
  sta $4016

lf_CHECK_A:  
  lda $4016
  lsr A
  bcc lf_CHECK_B
  
  lda $4016 ;throw away B
  lda #1 ;face
  ;lda #7 ;dot

  jmp lf_nt_calc
  
lf_CHECK_B:
  lda $4016
  lsr A
  bcc lf_CHECK_SELECT

  lda #0 ;blank
lf_nt_calc:
  sta lf_sprite_number
  lda #0
  sta temp
  lda lf_cursor_y
  lsr A;2
  ror temp
  lsr A;4
  ror temp
  lsr A;8
  ror temp
  clc
  adc #$20
  sta lf_NT_H
  clc
  adc #$40
  sta <pointer+1
  lda temp
  ora lf_cursor_x
  sta lf_NT_L
  sta <pointer
  ldy #0
  lda lf_sprite_number
  sta [pointer],y

  sec
  rol NMI_write_flag
  
lf_CHECK_SELECT:
  lda $4016
  lsr A
  bcc lf_CHECK_START
  jmp life

  
lf_CHECK_START:
  lda $4016
  and #1 ;if 0 use for clear
  beq clear_start
  
  lda lf_start_old
  bne lf_CHECK_UP ;start must be released to register again
  lda #1
  sta lf_start_old
  lda lf_running
  eor #$1 ;toggle
  sta lf_running
  jmp lf_CHECK_UP
clear_start:
  sta lf_start_old
lf_cnt_speed = 3
  
lf_CHECK_UP:
  lda $4016
  and #1 ;if 0 use for clear
  beq clear_up
  
  lda lf_u_count
  cmp #lf_cnt_speed
  beq lf_dec_y  
  inc lf_u_count
  cmp #0
  bne lf_CHECK_DOWN
lf_dec_y:
  lda lf_cursor_y
  cmp #1
  beq lf_CHECK_DOWN
  dec lf_cursor_y
  lda lf_cursor_y
  asl A
  asl A
  asl A
  sta sprite(0)
  jmp lf_CHECK_DOWN
clear_up:
  sta lf_u_count

lf_CHECK_DOWN:
  lda $4016
  and #1 ;if 0 use for clear
  beq clear_down ;lf_CHECK_LEFT
  
  lda lf_d_count
  cmp #lf_cnt_speed
  beq lf_inc_y
  inc lf_d_count
  cmp #0
  bne lf_CHECK_LEFT
lf_inc_y:
  lda lf_cursor_y
  cmp #28
  beq lf_CHECK_LEFT
  inc lf_cursor_y
  lda lf_cursor_y
  asl A
  asl A
  asl A
  sta sprite(0)
  jmp lf_CHECK_LEFT
clear_down:
  sta lf_d_count

lf_CHECK_LEFT:
  lda $4016
  and #1 ;if 0 use for clear
  beq clear_left ;lf_CHECK_RIGHT
  
  lda lf_l_count
  cmp #lf_cnt_speed
  beq lf_dec_x
  inc lf_l_count
  cmp #0
  bne lf_CHECK_RIGHT
lf_dec_x:
  lda lf_cursor_x
  beq lf_CHECK_RIGHT
  dec lf_cursor_x
  lda lf_cursor_x
  asl A
  asl A
  asl A
  sta sprite(0)+3
  jmp lf_CHECK_RIGHT
clear_left:
  sta lf_l_count

lf_CHECK_RIGHT:
  lda $4016
  and #1 ;if 0 use for clear
  beq clear_right ;CONTROLLER_DONE
  
  lda lf_r_count
  cmp #lf_cnt_speed
  beq lf_inc_x
  inc lf_r_count
  cmp #0
  bne lf_CONTROLLER_DONE
lf_inc_x:
  lda lf_cursor_x
  cmp #31
  beq lf_CONTROLLER_DONE
  inc lf_cursor_x
  lda lf_cursor_x
  asl A
  asl A
  asl A
  sta sprite(0)+3
  jmp lf_CONTROLLER_DONE
clear_right:
  sta lf_r_count
  
lf_CONTROLLER_DONE:
  
  lda lf_running
  bne lf_not_run
  jmp lf_Forever
lf_not_run:    
  lda #72 ;30*28-(256*3)=72
  sta loop_counter
  ;ldy #0
  ldy #1 ;no left
  ldx #4
  ld_point life_buf+$20
  
  lda #low(life_buf)
  sta <pointer2
  lda #high(life_buf)
  sta <pointer2+1
  
  lda #low(life_buf+$40)
  sta <pointer3
  lda #high(life_buf+$40)
  sta <pointer3+1
  
  lda #low(life_buf2+$20)
  sta <pointer4
  lda #high(life_buf2+$20)
  sta <pointer4+1
  
lf_loop:
  lda #0
  clc
  adc [pointer2],y ;top
  dey ;top left
  adc [pointer2],y
  iny
  iny ;top right
  adc [pointer2],y
  dey
  dey ;left
  adc [pointer],y
  iny
  iny ;right
  adc [pointer],y
  dey
  adc [pointer3],y ;bot
  dey ;bot left
  adc [pointer3],y
  iny
  iny ;bot right
  adc [pointer3],y
  dey
  
  cmp #3 ;if 3
  bne lf_loop1
  lda #1
  ;lda #7 ;dot
  jmp lf_loop_write
lf_loop1:
  cmp #2 ;if 2
  bne lf_loop2
  lda [pointer],y ;buf
  jmp lf_loop_write
lf_loop2:
  ;others
  lda #0
lf_loop_write:
  sta [pointer4],y ;buf2
  iny
  ;cpy #$20 ;every 32 inc pointer
  cpy #$1f ;every 32 inc pointer
  ;bne lf_loop4
  bne lf_point3
  ;ldy #0
  ldy #1
  lda <pointer
  clc
  adc #$20
  sta <pointer
  sta <pointer4
  bcc lf_loop4
  inc <pointer+1
  inc <pointer4+1
lf_loop4:
  lda <pointer2 ;top pointer
  clc
  adc #$20
  sta <pointer2
  bcc lf_point2
  inc <pointer2+1
lf_point2:
  lda <pointer3 ;bot pointer
  clc
  adc #$20
  sta <pointer3
  bcc lf_point3
  inc <pointer3+1
lf_point3:
  dec loop_counter
  bne lf_loop
  dex
  bne lf_loop
  
render_loop: ;synch to vblank
  lda render_flag
  beq render_loop
  lda #0
  sta render_flag
  ;copy buf2 back to buf
  
  lda #128 ;32*28-(256*3)=128
  sta loop_counter
  ldy #0
  ldx #4
  ld_point life_buf2+$20
  lda #low(life_buf+$20)
  sta <pointer3
  lda #high(life_buf+$20)
  sta <pointer3+1
copy_buf_loop:
  lda [pointer],y ;buf2
  sta [pointer3],y ;buf
  iny
  bne copy_buf_loop1
  inc <pointer+1
  inc <pointer3+1
copy_buf_loop1:
  dec loop_counter
  bne copy_buf_loop
  dex
  bne copy_buf_loop
  
jmp_lf_Forever:
  jmp lf_Forever ;jump back to lf_Forever, infinite loop, waiting for NMI

lf_NMI:
  pha
  tya
  pha
  txa
  pha
  lda <temp
  pha
  lda <pointer+1
  pha
  lda <pointer
  pha
  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  
  ldy #0
  lda <scroll_flag
  and #%00000010
  bne no_mod
  jmp NMI_NT1
  
no_mod:
  lda $2002
  lda nt_pointer_H
  sta $2006
  clc
  adc #$40 ;$20 to $60
  sta <pointer+1
  lda nt_pointer_L
  sta $2006
  sta <pointer
  
sprites_count = 128

NMI_draw:
  lda [pointer],y
  sta $2007
  iny
  lda [pointer],y
  sta $2007
  iny
  lda [pointer],y
  sta $2007
  iny
  lda [pointer],y
  sta $2007
  iny

  cpy #sprites_count
  bne NMI_draw

  lda nt_pointer_L
  clc
  adc #sprites_count
  sta nt_pointer_L
  lda #0
  adc nt_pointer_H
  sta nt_pointer_H
  cmp #$23
  ;bne NMI_done
  beq NMI_swap0
  jmp NMI_done
NMI_swap0:
  lda nt_pointer_L
  cmp #$A0

  beq NMI_swap1
  jmp NMI_done
NMI_swap1:
  ;swap NT
  lda #$28
  sta nt_pointer_H
  lda #$20
  sta nt_pointer_L

  sec
  rol render_flag

  lda <scroll_flag 
  and #%11111101
  sta <scroll_flag
  
  jmp NMI_done
  
NMI_NT1:
  lda $2002
  lda nt_pointer_H
  sta $2006
  clc
  adc #$38 ;$28 to $60
  sta <pointer+1
  lda nt_pointer_L
  sta $2006
  sta <pointer
  
NMI_draw1:
  lda [pointer],y
  sta $2007
  iny
  lda [pointer],y
  sta $2007
  iny
  lda [pointer],y
  sta $2007
  iny
  lda [pointer],y
  sta $2007
  iny
  cpy #sprites_count
  bne NMI_draw1

  lda nt_pointer_L
  clc
  adc #sprites_count
  sta nt_pointer_L
  lda #0
  adc nt_pointer_H
  sta nt_pointer_H
  cmp #$2b
  bne NMI_done
  lda nt_pointer_L
  cmp #$A0
  bne NMI_done
  
  ;reset
  lda #$20
  sta nt_pointer_H
  sta nt_pointer_L
  
  sec
  rol render_flag
  
  lda <scroll_flag
  ora #%000000010
  sta <scroll_flag
NMI_done:
  lda $2002
  lda #$00        ;;tell the ppu there is no background scrolling
  sta $2006
  sta $2006
  sta $2005
  lda #$FF
  sta $2005
  
  lda <scroll_flag
  sta $2000
NMI_exit:
  pla
  sta <pointer
  pla
  sta <pointer+1
  pla
  sta <temp
  pla
  tax
  pla
  tay
  pla
  rti

;-----------------------
clear_screen2:
  ld_2006 $2800
  ldx #192 ;32*30=960 tiles - (256*3)= 192
  ldy #4
  lda #' ' ;space for clear
  jmp clear_loop
