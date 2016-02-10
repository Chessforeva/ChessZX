;----------------------------------
;
; Draws chess board
;
;
DrawBoard:
        push af
        push bc  
        ld b,64             ; draw all 64 squares
DrwBoLoop:
        ld a,b
        dec a               ; sq-1 =  0..63
        call DrawSquare
        djnz DrwBoLoop
        pop bc
        pop af
        ret
        
;-----------
; a = square 0..63
DrawSquare:

        push af
        push de
        push bc
        push hl
        
        ld (DrwSq),a
        
        push af
        
        ld b,a
        and 7
        ld c,a        
        rla     ;x4 chars per piece
        rla
        ld (A_c8),a         ; pos X

        ld a,b
        rra     ;/8
        rra
        rra
        and 7
        ld b,a
        
        add c
        and 1
        jr z, DrwSq2x
        ld c,7         ; white background for square
        jr DrwSqOv
DrwSq2x:
        ld c,0         ; black background for square
DrwSqOv:
        ld a,c
        rla             ; set paper color bits
        rla
        rla
        
        or 64           ; bright color
        ; or 128 for blinking    

        push bc
        
        ld b,a      ; save attrib
        ld a,(DrwSq)
        ld c,a      ; c = square to draw
        
        ld a,(DrwCB)
        sub c       ; should blink
        jr nz,DrwNoBlink
        ld a,128
        or b        ; attrib |= 128
        jr DrwBlnkEx
DrwNoBlink:
        ld a,b      ; restore attrib
DrwBlnkEx:

        ld b,a      ; save attrib
        ld a,(DrwC1)
        sub c       ; should set green
        jr nz,DrwNoC1
        ld a,b
        and %11000111      
        or  %00101000       ; attrib color of paper=5    
        jr DrwC1Ex
DrwNoC1:
        ld a,b      ; restore attrib
DrwC1Ex:

        ld b,a      ; save attrib
        ld a,(DrwC0)
        sub c       ; should set pink
        jr nz,DrwNoC0
        ld a,b
        and %11000111
        or  %00011000       ; attrib color of paper=3        
       
        jr DrwC0Ex
DrwNoC0:
        ld a,b      ; restore attrib
DrwC0Ex:

        ld b,a      ; save attrib
        ld a,(DrwC2)
        sub c       ; should set yellow
        jr nz,DrwNoC2
        ld a,b
        and %11000111      
        or  %00110000       ; attrib color of paper=6        
        jr DrwC2Ex
DrwNoC2:
        ld a,b      ; restore attrib
DrwC2Ex:
        pop bc

        ld (A_attr), a
                      
        ld a,b
        add b   ;x3 rows per piece
        add b
        ld b,a
        ld a,21
        sub b               ;inverted top-down 21-vert
        ld (A_r8),a         ; pos Y
        
        ; prepare address of charset in de
        pop af      
                      
        ld de, BOARD        ; de = de + a
        ld l, a
        ld h, 0
        add hl,de
        ex de,hl

        ld a,1
        ld (DrwBl),a        ; next chara
        xor a
        ld (Drwfb),a        ; clear flag
                
        ld a,(de)           ; BOARD[x]
        
        or  a               ; is empty square?
        jr z, DwChSqEmpty
        
        cp ' '              ; if 0,' '
        jr nz, DwPcCs
        
DwChSqEmpty:        
        xor a               ; a=0
        ld (DrwBl),a        ; the same char
               
        call CharSetTableDE
        jr DwPcC2
DwPcCs:              
        cp 'a'
        jr nc, DwBlackP
        ld c,2              ; white piece, red color
        jr DwPcC0
DwBlackP:
        sub 32              ; to uppercase
        ld c,1              ; black piece, blue color
DwPcC0:
        push af
        ld a,(A_attr)
        and %11111000
        or a,c
        ld (A_attr), a      ; add ink color
        pop af

        cp 'P'
        jr z, DwPpc
        cp 'N'
        jr z, DwNpc     
        cp 'B'
        jr z, DwBpc
        cp 'R'
        jr z, DwRpc
        cp 'Q'
        jr z, DwQpc
        cp 'K'
        jr z, DwKpc
        jr DwPcEx       ; wtf
        
DwPpc:  ld de, Pawn_img
        jr DwPcC2
DwNpc:  ld de, Knight_img
        jr DwPcC2
DwBpc:  ld de, Bishop_img
        jr DwPcC2
DwRpc:  ld de, Rook_img
        jr DwPcC2
DwQpc:  ld de, Queen_img
        jr DwPcC2
DwKpc:  ld de, King_img
        
DwPcC2:
        push af
        push bc
        
        ld a,(DrwC1)        ; cursor with frame
        ld b,a
        ld a,(DrwSq)
        sub b
        jr nz,DwNoCursDrw
        
            ; combine 96 bytes = 3*4*8
        push hl
        push ix
        ld ix,DwPcBuf       ; write to buffer
        ld hl,cursor_img    ; hl=cursor address
                
        ld b,96
DwCursSqLoop:
        ld a,(de)           ; de=piece address
        ld c,a
        ld a,(hl)
        or c               ; |"or" all 96 bytes
        ld (ix),a
        
        ld a,(DrwBl)
        or a
        jr z,DwCrs0Sq
        inc de
DwCrs0Sq:        
        inc hl
        inc ix
        djnz DwCursSqLoop
        
        pop ix
        pop hl
        ld de,DwPcBuf       ; draw from buffer
        
        ld a,1
        ld (Drwfb),a
        
DwNoCursDrw:
        pop bc
        pop af
                                
        call DrawPiece
DwPcEx:      
        pop hl
        pop bc
        pop de
        pop af
        ret

DwPcBuf: ds 96      ; generates bitmap here for cursor

;------------
; Draws Chess pieces
; de = piece charset address
DrawPiece:
        push bc
        push hl
        ld a,32             ; first char from this table
        ld b,3              ; 3 rows x 4 seq. chars
DrwPLp0:
        ld c,4
DrwPLp1:
        ld (DrwChA),a       ; save this char to print
        push af
        
  ;labels for empty square
        ld a,(DrwBl)
        or a
        jr nz, DrwLbNo
        
        ; draw labels if empty square and 1st line or 1st column
        push de
        push bc
         
        ; Draw 1-8
        ld a,(A_c8)
        or a
        jr nz,DrwLbNotV
        
        ld a,b
        cp 2
        jr nz,DrwLbNotV
        
        ld a,(DrwSq)
        and %11111000
        rra  ;/8
        rra
        rra
        add '1'
        call DrwLabel
DrwLbNotV:
        ; Draw A-H
        ld a,(A_r8)
        cp 23
        jr nz,DrwLbNotH
        
        ld a,c
        cp 2
        jr nz,DrwLbNotH
        
        ld a,(DrwSq)
        and %00000111
        add 'A'
        call DrwLabel

        
DrwLbNotH:    
        pop bc
        pop de
        
DrwLbNo:

        ld a,(DrwBl)
        or a
        jr nz, DrwNoESq
            ; empty square colors        
        call DrwSetEmptySqAttribute
DrwNoESq:

        ld a,(DrwChA)
        or a
        jr z,DrwSkipDrawCh         
        
        call A_PrepHL       ; screen + position
        call DrawChar       ; display 1,2,...,12 chars
        
        call A_PrepHLAttrib ; attribute + position
        ld a,(A_attr)
        ld (hl),a           ; set attribute

DrwSkipDrawCh:

        ld a,(A_c8)
        inc a
        ld (A_c8),a         ; column++

        pop af
        
        push bc
        ld c,a
        ld a,(Drwfb)        ; if cursor on blank then +1 anyway
        ld b,a        
        ld a,(DrwBl)
        or b
        add a,c             ; a=a+(1 or 0,if blank square)
        pop bc

        dec c
        jr nz,DrwPLp1       ; till 4 chars printed
        
        push af
        ld a,(A_c8)
        sub 4               ; store back column 
        ld (A_c8),a
        ld a,(A_r8)
        inc a
        ld (A_r8),a
        pop af

        dec b
        jr z,DrwPLpOk
        jp DrwPLp0
DrwPLpOk:        
        ld a,(A_r8)
        sub 3               ; store back row
        ld (A_r8),a
        
DrwPLpEx:
        pop hl
        pop bc
        ret

; Set attribute color of empty square, to see cursor
DrwSetEmptySqAttribute:

        push af
        push bc
        ld a,(SIDE)
        or a
        jr nz,DrwEsqB2move
            ; white to move
        ld a,2              ; red
        jr DrwEsq0 
DrwEsqB2move:
            ; black to move
        ld a,1              ; blue
DrwEsq0:
        ld b,a     
        ld a,(A_attr)
        and %11111000
        or b
        ld (A_attr),a       ; set attribute color
        pop bc
        pop af
        ret
        
; sub draws label of A char     
DrwLabel:
        call CharSetTableDE
        
        call A_PrepHL       ; screen + position
        call DrawChar       ; display labels
        
        call A_PrepHLAttrib ; attribute + position
        ld a,(A_attr)
        and %11111000
        or 4                ; set color=4 light green        
        ld (hl),a           ; set attribute
        
        xor a
        ld (DrwChA),a       ; do not print again
        ret

; clears cursor DrwC0, redraws square
ClearC0:
        push af
        ld a,(DrwC0)
        cp $ff
        jr z,ClrC0ov
        
        push af
        ld a,$ff
        ld (DrwC0),a
        pop af
        call DrawSquare     ; redraw cursor square
ClrC0ov:
        pop af
        ret

; clears cursor DrwC1, redraws square
ClearC1:
        push af
        ld a,(DrwC1)
        cp $ff
        jr z,ClrC1ov
        
        push af
        ld a,$ff
        ld (DrwC1),a
        pop af
        call DrawSquare     ; redraw cursor square
ClrC1ov:
        pop af
        ret

; draws chess puzzle type M1,M2,M3
DrawMx:
    push af
    push bc
    push de
    push ix
    
    ld ix,MxText
    
    ld a,(RdMvDp)
    inc a
    sra a
    add '0'
    ld (ix+1),a         ; M"1" or "2" or "3"
    
    ld a,(RdMoveTo)
    or a
    jr z,DrwMxW
        ; color as side to move
    ld a,'b'
    ld c,1              ; blue color
    jr DrwMx0
DrwMxW:
    ld c,2              ; red color
    ld a,'w'
DrwMx0:
    ld (ix+2),a         ; "w" or "b" -side to move
        
    ld de,ix
    ld a,28                 ; column on H8
    
    ld b,a
    ld a,(BOARD+63)         ; put text on H8
    or a
    jr z,DrwMxOnH8
        
    ld c,6                  ; yellow color

DrwMxOnH8:

    ld a,(A_attr)
    and %11111000
    or c
    ld (A_attr),a              ; save attribute color

    ld a,b
    ld (A_c8),a
    xor a
    ld (A_r8),a
            
    call A_DrawString
    
    pop ix
    pop de
    pop bc
    pop af
    ret

MxText: .byte "Mxc",0

; display loading
DrawLoading:
    push af
    push de
    ld a,%01000110      ; yellow on black
    ld (A_attr),a       ; save attribute color
    ld a,25
    ld (A_c8),a
    ld a,22
    ld (A_r8),a
    ld de,LoadingStr
    call A_DrawString
    pop de
    pop af
    ret
    
LoadingStr:
    .byte "loading",0

DrwBl:  .byte 0     ; blank square flag
Drwfb:  .byte 0     ; flag for cursor blank square
DrwChA: .byte 0     ; to pass char to print
DrwSq:  .byte 0     ; current square 0..63
DrwCB:  .byte $ff   ; square of blinking
DrwC0:  .byte $ff   ; square of cursor 0 (pink)
DrwC1:  .byte 12    ; square (E2) of cursor 1 (green), frame around
DrwC2:  .byte $ff   ; square of cursor 2 (yellow)
