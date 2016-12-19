commands:
  .db "echo", 0
  .db "pause", 0
  .db "clear", 0

  .db "edit", 0
  .db "brainf", 0
  .db "peek", 0
  .db "poke", 0
  .db "snake", 0
  .db "life", 0, 0 ;double zero terminates list
  
com_table:
  .dw echo-1
  .dw pause-1
  .dw clear-1
  .dw edit-1
  .dw brainf_launch-1
  .dw peek-1
  .dw poke-1
  .dw snake-1
  .dw life-1
;ECHO############
echo:
  ;nop
  lda #$00
  ldx com_offset
  inx
  sta com_log,x
  
  ld_point com_log+6
	
  ;jsr com_start_init ;returns offset in a
  lda <count ;com offset
  clc
  adc <pointer
  sta <pointer
  lda #0
  adc <pointer+1
  sta <pointer+1
  
  jsr print_str
  rts
;PAUSE###########
pause:
  ;nop
  ld_point pause_msg
  jsr print_str
  jsr vblankwait
  lda <in_count
  sta <temp
pause_loop:
  lda <temp
  cmp in_count
  beq pause_loop
  
  dec <in_count
  dec <in_w_pointer
  lda <in_w_pointer
  and #%1111
  sta <in_w_pointer
  rts
pause_msg: .db "press key...", 0
;CLEAR###########
clear:
  ;jsr vblankwait   ;;wait for vblank so screen isnt turned on while rendering is happening
  lda #$00
  sta $2000    ; disable NMI
  sta $2001    ; disable rendering
  sta scroll_val
  
  jsr clear_screen

  lda #high($2020)
  sta <nt_pointer_H ;setup name table pointers
  lda #low($2020)
  sta <nt_pointer_L
  
  lda #0
  sta $2006
  sta $2006
  
  jsr vblankwait   ;;wait for vblank so screen isnt turned on while rendering is happening
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta $2000

  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta $2001
  
  rts
;EDIT############
edit:
  ld_point edit_splash
  jsr print_str
  lda program ;if empty
  beq edit_loop ;skip print
  
  ldx #$00
find_prog_end:
  inx
  beq found_prog_end
  lda program,x
  bne find_prog_end
found_prog_end:
  txa
  sta prog_offset
  
  ld_point program
  jsr print_str
edit_loop:
  lda <prog_offset
  sta <count
  ld_point program
  lda #$00 ;clear newline flag
  sta <temp
  jsr buffer_copy
  pha
  lda <count
  sta <prog_offset
  pla
  lda <temp ;check nl flag
  beq edit_loop
  
  dec <prog_offset
  ldx <prog_offset
  lda #0
  sta program, x
  rts
edit_splash: .db "Now editing program space:\n",0
;PEEK############
peek:
  lda <com_offset
  sec
  sbc #$05
  tay
  ldx #$04
peek_adr:
  lda com_log,y
  jsr print_char
  iny
  dex
  bne peek_adr
  
  ldy <com_offset
  dey
  dey
  ldx #$00
text_bin_loop:
  lda com_log,y
  jsr text_to_bin
  sta <pointer,x
  dey

  lda com_log,y
  jsr text_to_bin
  asl a
  asl a
  asl a
  asl a
  ora <pointer,x
  sta <pointer,x
  dey
  inx
  cpx #$2
  bne text_bin_loop

  lda #':'
  jsr print_char
  
  ldx #9
bin_text_loop:
  ldy #$0
  lda [pointer],y
  lsr a
  lsr a
  lsr a
  lsr a
  jsr bin_to_text
  jsr print_char
  
  ;sta out_buffer
  lda [pointer],y
  and #%1111
  jsr bin_to_text
  jsr print_char
  lda #' '
  jsr print_char
  inc pointer
  bne b_t_l_no_carry
  inc pointer+1
b_t_l_no_carry:
  dex
  bne bin_text_loop
  lda #$FF ;remove last space
  jsr print_char
  rts
;POKE############
poke:
  nop
  rts
;SNAKE###########
;snake:

;BRAINF##########
brainf_launch:
  lda #$0
  tax
  sta <pc
  sta <pointer
bf_clear:
  STA ram, x
    
  INX
  BNE bf_clear
  jmp brainf
  
;Command Interpreter######
com_match:
  ldx <com_offset
  jsr com_start_init
  sta <count
  dec <count
  ldx #$00
  stx <temp
  
match_init:
  ;ldy #$FF
  
  ldy <count
match:
  iny
  lda commands,x
  beq com_end ;end of com name
  inx
  
  cmp com_log,y ;needs to point to newset command
  beq match
  ;jsr find_end
find_end:
  inx
  lda commands,x
  bne find_end
  inx ;start on next char
  
  inc <temp
  lda commands,x
  bne match_init
exit:
  ;not a valid command
  ld_point not_com
  jsr print_str
  rts
not_com: .db "Error:not a valid command",0
  
com_end:
  ;make sure our match is followed by a space or newline
  ;otherwise commands with same starting characters
  ;would conflict
  
  lda com_log,y
  cmp #' '
  beq com_ok
  cmp #$0a ;newline
  beq com_ok
  inc <temp
  inx
  jmp match_init
com_ok:
  lda <temp ;use the count of 0s as offset
  asl a
  tax
  inx
  lda com_table,x
  pha
  dex
  lda com_table,x
  pha
  rts
  
;SUBROUTINES###
com_start_init:  
  dex
find_com_start:
  dex
  lda com_log,x
  cmp #$0a
  bne find_com_start
  
  inx
  txa
  rts
  
text_to_bin:
  cmp #$3A
  bmi text_num
  sec
  sbc #$27
text_num:
  sec
  sbc #$30
  rts
  
bin_to_text:
  cmp #$0a
  bmi bin_num
  clc
  adc #$27
bin_num:
  clc
  adc #$30
  rts