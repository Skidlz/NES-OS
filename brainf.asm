;BrainF interpreter
;reference:
;http://groups.google.com/group/comp.emulators.apple2/browse_thread/thread/3a6dc92aa0d9a040?pli=1

;written by Zack Nelson
;Aug 24, 2011

brainf:
  ;ram is assumed to be zero
  lda #high(ram) ;set pointer to ram start
  sta <pointer+1
main:
  ldy #$00 ;clear y for startup + undo [] changes
  jsr get_code
  inc <pc
  bne main
  rts

get_code:
  ldx <pc ;brainf program counter
  lda program,x
  ldx #7
find_code:
  cmp codes, x
  beq jump_offset
  dex
  bpl find_code ;bpl loops on 0
  rts
codes:
  .db '>', '<', '+', '-', '.', ',', '[', ']'
code_offsets:
  .db low(inc_pointer)-1, low(dec_pointer)-1, low(inc_val)-1, low(dec_val)-1
  .db low(output)-1, low(input)-1, low(start_brac)-1, low(end_brac)-1
  
jump_offset:
  lda #high(brainf) ;high byte
  pha
  lda code_offsets, x
  pha
  rts
  
inc_pointer: 						;>
  .db $e6,$00,$d0,$02,$e6,$01
  rts
dec_pointer: 						;<
  .db $a5,$00,$d0,$02,$c6,$01,$c6,$00
  rts
inc_val: 						;+
  .db $18,$a9 ,$01 ;add 01
  .db $71,$00,$91,$00
  rts
dec_val: 						;-
  .db $38,$b1,$00,$e9 ,$01 ;sub 1
  .db $91,$00
  rts
output: 						;.
  ldx <out_w_pointer
  lda [$00],Y
  sta <out_buffer,X
  inx 
  txa 
  and #o_b_len-1
  sta <out_w_pointer
  inc <out_count
  rts
  
input: 							;,
  lda <in_count
  beq input
  ldx <in_r_pointer
  lda <in_buffer,X
  sta [$00],Y
  inx 
  txa 
  and #$0f
  sta <in_r_pointer
  dec <in_count
  rts
  
start_brac:
  lda [$00],y
  bne start_brac_con
  
  ldx <pc
  iny ;use y as brac count
find_end_brac:
  inx
  lda program,x
  cmp #'['
  bne not_start
  iny ;brac counter
not_start:
  cmp #']'
  bne find_end_brac
  dey ;found matching brac?
  bne find_end_brac
  txa 
  sta <pc ;store pc of matching brac
start_brac_con:
  rts
  
end_brac:
  lda [$00],y
  beq end_brac_con
  
  ldx <pc
  iny ;use y as brac count
find_start_brac:
  dex
  lda program,x
  cmp #']'
  bne not_end
  iny ;brac counter
not_end:
  cmp #'['
  bne find_start_brac
  dey ;found matching brac?
  bne find_start_brac
  txa 
  sta <pc ;store pc of matching brac
end_brac_con:
  rts