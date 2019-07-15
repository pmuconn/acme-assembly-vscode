;============================================================
; BASIC loader with start address $c000
;============================================================

* = $0801                               ; BASIC start address (#2049)
!byte $0d,$08,$dc,$07,$9e,$20,$34,$39   ; BASIC loader to start at $c000...
!byte $31,$35,$32,$00,$00,$00           ; puts BASIC line 2012 SYS 49152
* = $c000     				            ; start address for 6502 code

;============================================================
;  Main routine with IRQ setup and custom IRQ routine
;============================================================


        sei         ; set interrupt disable flag
            
        jsr init_screen     ; clear the screen
        jsr init_text       ; write lines of text
           ;jsr sid_init     ; init music routine now

           ldy #$7f    ; $7f = %01111111
           sty $dc0d   ; Turn off CIAs Timer interrupts ($7f = %01111111)
           sty $dd0d   ; Turn off CIAs Timer interrupts ($7f = %01111111)
           lda $dc0d   ; by reading $dc0d and $dd0d we cancel all CIA-IRQs in queue/unprocessed
           lda $dd0d   ; by reading $dc0d and $dd0d we cancel all CIA-IRQs in queue/unprocessed
          
           lda #$01    ; Set Interrupt Request Mask...
           sta $d01a   ; ...we want IRQ by Rasterbeam (%00000001)

           lda $d011   ; Bit#0 of $d011 indicates if we have passed line 255 on the screen
           and #$7f    ; it is basically the 9th Bit for $d012
           sta $d011   ; we need to make sure it is set to zero for our intro.

           lda #<irq   ; point IRQ Vector to our custom irq routine
           ldx #>irq 
           sta $314    ; store in $314/$315
           stx $315   

           lda #$00    ; trigger first interrupt at row zero
           sta $d012

           cli                  ; clear interrupt disable flag
           jmp *                ; infinite loop


;============================================================
;    custom interrupt routine
;============================================================

irq        dec $d019        ; acknowledge IRQ / clear register for next interrupt

           jsr colwash      ; jump to color cycling routine
           ;jsr play_music	  ; jump to play music routine


           jmp $ea81        ; return to kernel interrupt routine

;============================================================
;    setup and init symbols we use in the code
;============================================================

address_music = $1000 ; loading address for sid tune
sid_init = $1000      ; init routine for music
sid_play = $1006      ; play music routine

;============================================================
; tables and strings of data 
;============================================================

line1   !scr "     actraiser in 2013 presents...      "
line2   !scr "example effect for dustlayer tutorials  " 

; color data table
; first 9 rows (40 bytes) are used for the color washer
; on start the gradient is done by byte 40 is mirroed in byte 1, byte 39 in byte 2 etc... 

color        !byte $09,$09,$02,$02,$08 
             !byte $08,$0a,$0a,$0f,$0f 
             !byte $07,$07,$01,$01,$01 
             !byte $01,$01,$01,$01,$01 
             !byte $01,$01,$01,$01,$01 
             !byte $01,$01,$01,$07,$07 
             !byte $0f,$0f,$0a,$0a,$08 
             !byte $08,$02,$02,$09,$09 

color2       !byte $09,$09,$02,$02,$08 
             !byte $08,$0a,$0a,$0f,$0f 
             !byte $07,$07,$01,$01,$01 
             !byte $01,$01,$01,$01,$01 
             !byte $01,$01,$01,$01,$01 
             !byte $01,$01,$01,$07,$07 
             !byte $0f,$0f,$0a,$0a,$08 
             !byte $08,$02,$02,$09,$09 

;============================================================
; one-time initialization routines
;============================================================

;============================================================
; clear screen
; a loop instead of kernal routine to save cycles
;============================================================

init_screen      ldx #$00     ; set X to zero (black color code)
                 stx $d021    ; set background color
                 stx $d020    ; set border color

clear            lda #$20     ; #$20 is the spacebar Screen Code
                 sta $0400,x  ; fill four areas with 256 spacebar characters
                 sta $0500,x 
                 sta $0600,x 
                 sta $06e8,x 
                 lda #$00     ; set foreground to black in Color Ram 
                 sta $d800,x  
                 sta $d900,x
                 sta $da00,x
                 sta $dae8,x
                 inx           ; increment X
                 bne clear     ; did X turn to zero yet?
                               ; if not, continue with the loop
                 rts           ; return from this subroutine

;============================================================
; write the two line of text to screen center
;============================================================


init_text  ldx #$00          ; init X-Register with $00
loop_text  lda line1,x      ; read characters from line1 table of text...
           sta $0590,x      ; ...and store in screen ram near the center
           lda line2,x      ; read characters from line1 table of text...
           sta $05e0,x      ; ...and put 2 rows below line1

           inx 
           cpx #$28         ; finished when all 40 cols of a line are processed
           bne loop_text    ; loop if we are not done yet
           rts

;============================================================
;    subroutines called during custom IRQ
;============================================================

colwash   ldx #$27        ; load x-register with #$27 to work through 0-39 iterations
          lda color+$27   ; init accumulator with the last color from first color table

cycle1    ldy color-1,x   ; remember the current color in color table in this iteration
          sta color-1,x   ; overwrite that location with color from accumulator
          sta $d990,x     ; put it into Color Ram into column x
          tya             ; transfer our remembered color back to accumulator
          dex             ; decrement x-register to go to next iteration
          bne cycle1      ; repeat if there are iterations left
          sta color+$27   ; otherwise store te last color from accu into color table
          sta $d990       ; ... and into Color Ram
                          
colwash2  ldx #$00        ; load x-register with #$00
          lda color2+$27  ; load the last color from the second color table

cycle2    ldy color2,x    ; remember color at currently looked color2 table location
          sta color2,x    ; overwrite location with color from accumulator
          sta $d9e0,x     ; ... and write it to Color Ram
          tya             ; transfer our remembered color back to accumulator 
          inx             ; increment x-register to go to next iteraton
          cpx #$26        ; have we gone through 39 iterations yet?
          bne cycle2      ; if no, repeat
          sta color2+$27  ; if yes, store the final color from accu into color2 table
          sta $d9e0+$27   ; and write it into Color Ram
 
          rts             ; return from subroutine

