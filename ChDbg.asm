;-------------------------------------------------------
;
;  If should find exact position in memory
;   by Bank,Seg,GameI
;

dbgRdBank:          ; 0,1,3,4,6,7
    ;ld a,1
    ret
dbgRdSeg:           ; Seg C0..CF
    ;ld a,$EE
    ret
dbgRdGi:            ; Game 0..GC-1
    ;ld a,1
    ret
;----


;-------------------------------------------------------
; dbgA - displays code on screen (for debug)
; Regs: a - code
;-------------------------------------------------------

dbgA:
    push af
    push bc
    push de
    ld de, dspCdStr
    call Str_itoa
    call DrawString
    pop de
    pop bc
    pop af
    ret
dspCdStr:
    .ds 4

;---- Saves board to compare later
dbgBOARD_s:
    push af
    push bc
    push de
    push hl
    
    ld de,dbgBoSave
    ld hl,BOARD
    ld bc,64+6
    ldir            ; save board
    
    pop hl
    pop de
    pop bc
    pop af
    ret    

;---- Verifies board for bugs
dbgBOARD_v:
    
    push af
    push bc
    push de
    push hl
    push ix
    
    ld de,dbgBoSave
    ld hl,BOARD
    ld b,64+6
D1gLoop:
    ld a,(hl)
D1g1:
    ld c,a
    ld a,(de)
    cp c
    jr z,D1g2
    
        ; if board not the same, then tell it
    call DrawBoard
    ld ix,dbgBodt
    ld a,64+6
    sub b
    ld de,dbgBoSq           ; display square info     
    call DbgcD
    ld a,c
    ld de,dbgBoCd           ; display code on it     
    call DbgcD    
    ld de,dbgBoError        ; display info that wrong data  
    call DrawString
    call PressEnter
    call DrawBoard
    
    jr D1g3
D1g2:
    inc de
    inc hl
    djnz D1gLoop
D1g3:
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ret
    
dbgBoError:
    .byte "BOARD error",0
dbgBodt:    
    .byte 2,0,  BKWH
    .byte 2,10, BKWH    
dbgBoSq:
    .byte "Sq=   ",0
dbgBoCd:
    .byte "Cd=   ",0
    
;---- Simply restores board pieces
dbgBOARD_r:
    push af
    push bc
    push de
    push hl
    
    ld de,BOARD
    ld hl,dbgBoSave
    ld bc,64+6
    ldi         ; copy from saved
    
    pop hl
    pop de
    pop bc
    pop af
    ret
    
dbgBoSave: .ds 64+6     ; BOARD,SIDE,CASTLES,ENPSQ,WKSQ,BKSQ
 
;--------------------------
;
; Useful tool on key "V" in Board GUI
; Displays main variables.
;
DebugMovesMemory:
    push af
    push bc
    push de
    push ix
    push iy
    
    call LocateIYbyDepth
    call NoVerLoopForMv     ; finds move in IX
     
        ; to see next move
    ld a,(ix)
    inc ix
    ld (DrwC0),a       ; to see from-square
    call DrawSquare
    ld a,(ix)
    inc ix
    ld (DrwC2),a       ; to see to-square
    call DrawSquare
 
    call MovesToStr    ; to print moves
    call DrawMx

BKWH = %01111000

    ld ix,dbgmvdt

    ld de,dbgmvI    ; display move index from data
    ld a,(iy)
    call DbgcD

    call A_by_IX
    ld de,NEXT_MVSTR
    call A_DrawString
    
    ; read-chess-data variables
    ld de,dbgmvBK       ; display BANK 0..7
    ld a,(RdBnkSet)
    call DbgcD
    
    call A_by_IX
    ld de,dbgmvSG+3       ; display segment
    ld a,(RdSeg)
    call Str_itoa_hex
    ld de,dbgmvSG
    call A_DrawString
    
    ld de,dbgmvGi       ; random to skip game
    ld a,(RdGi)
    call DbgcD
    
    ld de,dbgmvGC       ; count of games in data bank
    ld a,(RdGC)
    call DbgcD


    ld de,dbgmvB0       ; B0 count of bits
    ld a,(RdB0)
    call DbgcD
    
    ld de,dbgmvBY       ; count of bytes to be sure
    ld a,(RdBY)
    call DbgcD
    
    ld de,dbgmvWW       ; white-width in bits
    ld a,(RdMvW)
    call DbgcD
    ld de,dbgmvBW       ; black-width in bits
    ld a,(RdMvB)
    call DbgcD

    call A_by_IX
    ld de,RdFEN
        ; remove $ sign at the end
    push af
    push de
dbgStDl:
    ld a,(de)
    or a
    jr z,dbgExDl
    cp '$'
    jr nz,dbgLpDl
    xor a
    ld (de),a
    jr dbgExDl
dbgLpDl:
    inc de
    jr dbgStDl
dbgExDl:
    pop de
    pop af
    call A_DrawString   ; FEN as we got it
    
    call A_by_IX
    ld de,dbgmv_P0        ; draw "press Enter"
    call A_DrawString
    
    call PressEnter
    
    pop iy
    pop ix
    pop de
    pop bc
    pop af
    ret

; calc and draw value
DbgcD:
    push de
    push af
    call A_by_IX
    pop af
    inc de
    inc de
    inc de
    call Str_itoa
    pop de
    call A_DrawString
    ret
    
dbgmvI:
    .byte "I=    ",0
dbgmvBK:
    .byte "Bk=   ",0
dbgmvSG:
    .byte "SG=   ",0
dbgmvGi:
    .byte "Gi=   ",0
dbgmvGC:
    .byte "GC=   ",0    
dbgmvB0:
    .byte "B0=   ",0
dbgmvBY:
    .byte "By=   ",0  
dbgmvWW:
    .byte "WW=   ",0
dbgmvBW:
    .byte "BW=   ",0  

dbgmv_P0:    
    .byte "(press Enter)", 0
    
dbgmvdt:
    .byte 0,0, BKWH      ; row,col,black on white
    .byte 1,0, BKWH      ; possible chess moves
    
    .byte 17,0, BKWH
    .byte 17,8, BKWH
    .byte 17,20, BKWH
    .byte 18,0, BKWH
    .byte 18,8, BKWH
    .byte 18,20, BKWH
    .byte 19,0, BKWH
    .byte 19,8, BKWH
    .byte 20,0, BKWH
    .byte 22, 8, YEBLK
