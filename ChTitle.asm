
DrawTitle:
    ; ---- First window
    
    ; show spectrum logo
    
    ld ix, Tit_prp_data
    
    call A_by_IX
    call DrawZX128logo

WHBLU = %01001111       ; white on blue
WHBLK = %01000111       ; white on black
YEBLK = %01000110       ; yellow on black
RDBLK = %01000010       ; red on black
LBBLK = %01000101       ; light blue on black
LGBLK = %01000100       ; light green on black

    call A_by_IX
    ld b,22
    ld c,5
    call A_ClearRect
    
    ld b,2          ; two half-chars
TitC05loop:
    call A_by_IX
    ld a,32
    ld de,Char05
    call A_PrepHL
    call DrawChar
    call A_PrepHLAttrib ; attribute + position
    ld a, %01000001
    ld (hl),a
    djnz TitC05loop
    
    ld iy, TitSCR0
    call TitDspText
    
    
    ; ---- Second window
    
    ld iy, TitSCR1
    call TitDspText
    
    ; display waiting
    ;ld a,0
    ;call ClearScreen
    
    call A_by_IX
    ld de, Tit_W0
    call A_DrawString
    
    ret

TitDspText:

    ld b,(iy)   ; read count of texts
    inc iy

TitDspTloop:    
    ld de,(iy)  ; read address of text
    inc iy
    inc iy
    call A_by_IX
    call A_DrawString
    djnz TitDspTloop    ; print all texts

    call PressKey
    ret

Tit_00:
    .byte "Project ChessZX",0
Tit_01:
    .byte " by Chessforeva,2016",0
Tit_T0:
    .byte "    Chess puzzles to solve      "    
    .byte "       1687  M1-move            "
    .byte "       4103  M2-moves           "
    .byte "       2626  M3-moves           "
    .byte "   (generated, not played)      "
    .byte "                                "
    .byte "  There is no other first move. "      
    .byte "  The last move is checkmate#.  "
    .byte "                                "
    .byte 0
Tit_P0:    
    .byte "        (press a key)           "
    .byte 0

 
Tit_T2:
    .byte "                                "
    .byte "  Cursor:                       "
    .byte "  Kempston,                     "
    .byte "                                "
    .byte "      - redefine controls       "
    .byte "      - skip to next puzzle     "
    .byte "                                "
    .byte "                                " 
    .byte 0
    
Tit_T2a:
    .byte " 1.Try to speed up the emulator."
    .byte "  Disable same keypress twice   "
    .byte "  by key     and use            "
    .byte "  to repeat last keys.          "
    .byte "                                "
    .byte 0
    
Tit_T2b:
    .byte "    Or (for slow emulation):    "
    .byte " 2.Press     to turn off slow   "
    .byte "  verification on loading.      "
    .byte "                                "
    .byte 0
    
Tit_T2c:
    .byte "                                "
    .byte " Thanks to SjASMPlus compiler,  "
    .byte " Assembly Studio 8x editor,     "
    .byte " disassembled ZX 128 ROM,       "       
    .byte " Speccy,EmuZWin,ZXSpin,RealSpec "       
    .byte " and other sources on the Web!  "  
    .byte 0

Tit_K_WASD:
    .byte "[WASD],Space,Enter",0
Tit_K_O:
    .byte "[O]",0
Tit_K_N:
    .byte "[N]",0
Tit_K_T:
    .byte "[T]",0    
Tit_K_FV:
    .byte "[F,V]",0
Tit_K_P:
    .byte "[P]",0

Tit_W0:
    .byte " Let's start, wait while "    
    .byte 0
    
TitSCR0:
    .byte 4
    .word Tit_00, Tit_01, Tit_T0, Tit_P0

TitSCR1:
    .byte 10
    .word Tit_T2, Tit_T2a, Tit_T2b, Tit_K_WASD, Tit_K_O
    .word Tit_K_N, Tit_K_T, Tit_K_FV, Tit_K_P, Tit_T2c
    
Tit_prp_data:
    .byte 1, 12, 0          ; ZX spectrum logo
    .byte 7, 5,  WHBLU      ; Blue rectangle
    .byte 6, 26, WHBLU      ; half-chars
    .byte 11, 4, WHBLU      ;
    .byte 8, 6,  WHBLU      ; Tit_00
    .byte 10, 6, WHBLU      ; Tit_01
    .byte 13, 0, WHBLK      ; Tit_T0, white on black
    
    .byte 22, 0, YEBLK      ; Tit_P0, yellow on black
    
    .byte 0, 0, WHBLK       ; Tit_T2, white on black
    .byte 7, 0, LBBLK      ; Tit_T2a
    .byte 12, 0, LGBLK      ; Tit_T2b
    
    .byte 2, 11, RDBLK      ; Tit_K_WASD, red on black
    .byte 4, 2, RDBLK       ; Tit_K_O
    .byte 5, 2, RDBLK       ; Tit_K_N
    .byte 9, 9, RDBLK      ; Tit_K_T
    .byte 9, 21, RDBLK     ; Tit_K_FV
    .byte 13, 9, RDBLK      ; Tit_K_P
    .byte 15, 0, WHBLK      ; Tit_K_T2c
    .byte 22, 0, YEBLK      ; Tit_W0, yellow on black
