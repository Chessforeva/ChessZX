; ---------------------------
;
; Reads chess positions
; HL is at C000
;
ReadRndGame:
    di
    push af
    push bc
    push de
    push hl
    push ix
    push iy
    
    call DrawLoading
    
    ld a,(BANKVALUE)
    and 7
    ld (RdBnkSet),a     ; save bank value
    
RdGameRead:
    
Rnd62pz:
    call Random0
    and 63              ; 62 segments C0xx-FExx
    cp 63
    jr nz, Rnd62ok
    xor a               ; 63->0
Rnd62ok:
    add a,h
    
    ; debug
    call dbgRdSeg
    
    ld (RdSeg),a
    ld h,a              ; 0xC0+62 <=0xFE
    call Read256bytes
    
    dec a
    ld c,a              ; c = games-1 in these 256 bytes

    call Random0
    and $1f             ; all under 32, it's faster
    ld b,a              ; b=random number 0..255
RdLoopB:
    ccf   
    sub c
    jr c, RdPozQ        ; find b inside [0..c] (mod)
    ld b,a              ; b -= c   
    jr RdLoopB
RdPozQ:
    ld a,b
    
    ; debug
    call dbgRdGi
        
    ld (RdGi),a
    ld b,a
  
    ; b = count of games to skip
RdLoopTillPos:

    call ReadB0ofGame   ; set c,e
  
    ld a,b
    or a
    jr z, RdGameFound
    dec b               ; skip next game
    
    push bc
    ld b,0
    add hl,bc       ; skip to next chess position
    pop bc
           
    jr RdLoopTillPos

        
RdGameFound:
    
    call ReadGame
    
    ld a,(RdCkMt)
    or a
    jr z,RdGameRead     ; if not checkmate then read again (on bug-case)
    
    ld de,RdFEN
    call GetFEN     ; read fen to see later
    
    pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ei
    ret

Read256bytes:

    ld a,(hl)       ; read count of games
    ld (RdGC),a     ; save
    inc hl
    ret

ReadB0ofGame:

; read counter of bits
; at address HL
; 1)sets total bit counter for a game in E
; 2)returns count of game bytes in C
    ld a,(hl)
    inc hl
    ld e,a
    and %11111000    
    rra         ;bytes= /8
    rra
    rra
    ld c,a
    ld a,e
    and %00000111
    jr z,RdB0ex
    inc c
RdB0ex:
    ld a,e
    ld (RdB0),a
    ld a,c
    ld (RdBY),a
    ret
    
ReadGame:
    ld d,128
    
ReadChessPosition:

    xor a
    ld iy,BOARD
    ld b,64                 ; clear all 64 squares
RdPosBo0Loop:
    ld (iy),a
    inc iy
    djnz RdPosBo0Loop
    
    ld a,56                 ; A8
    ld (RdBoI),a            ; BOARD[56]
    
RdPosLineLoop:    
    ld a,1                  ; read "is piece" flag
    call ReadBits
    
    or a
    jr nz, RdNxtBoardLine
    ld a,4                  ; read piece number 1..12
    call ReadBits

    ld ix,RdPiecesList    
RdPosPcLp:
    inc ix                  ; ix - pointer to piece
    dec a
    jr nz,RdPosPcLp

    ld a,3                  ; read A-H of piece squre
    call ReadBits

    ld b,0    
    ld iy,RdBoI
    add a,(iy)
    ld c,a                  ; c=   line pointer+column
    ld iy,BOARD
    add iy,bc
    ld a,(ix)
    ld (iy),a               ; BOARD[bc]=piece
    
    ; update WKSQ, BKSQ for chess logic
    cp 'K'
    jr nz, RdIsBking
    ld a,c
    ld (WKSQ),a
    jr RdNoBking
RdIsBking:
    cp 'k'
    jr nz, RdNoBking
    ld a,c
    ld (BKSQ),a
RdNoBking:

    jr RdPosLineLoop        ; read next piece

RdNxtBoardLine:
    ld iy,RdBoI
    ld a,(iy)
    sub 8
    ld (iy),a
    jr nc,RdPosLineLoop

    ; when last line reached
    ld a,0
    ld (CASTLES),a
    ld (MOVE_CNT),a
    ld (RdMvDp),a
    ld (RdCkMt),a
       
    ld a,$ff
    ld (ENPSQ),a
            
    ; read "to move" bit
    ld a,1
    call ReadBits
    ld (RdMoveTo),a
    ld (SIDE),a
    
    ; read code for table of moves
    ld a,3                  ; code in 3 bits
    call ReadBits
    
    ld (RdMvTbCode),a       ; how to read info

    ; obtain bit-width for sides
    rla         ;*2
    ld b,0
    ld c,a
    ld ix,RdMvTable
    add ix,bc
    ld b,(ix)       ; Side to move bit-width
    inc ix
    ld c,(ix)       ; Other side bit-width
    ld a,(RdMoveTo)
    or a            ; if black to move then swap
    jr z,RdMvW2mv
    ld a,b
    ld b,c          ; swap width for sides
    ld c,a
RdMvW2mv:
    ld a,b
    ld (RdMvW),a
    ld a,c
    ld (RdMvB),a
    
    ld iy, MOVES_N      ; move numbers, list
    ld ix, MOVES_I      ; extended with all move datas
    
    ld a,(RdMoveTo)
    ld c,a              ; c=side to move
RdMvListLoop:
            ; loop last E bits
    ld a,(RdMvDp)
    inc a
    ld (RdMvDp),a       ; depth++

    ld b,CDEPTH+1       ; If can read more moves then data error
    ld a,c
    or a
    jr nz,RdMvBlck
    ld a,(RdMvW)        ; white bit-width
    jr RdMvRd
RdMvBlck:
    ld a,(RdMvB)        ; black bit-width
RdMvRd:

    call ReadBits       ; A=move number
    
    push bc
    ld b,a
    ld (iy),a
    inc iy
    
    ld a,(fSlow)
    or a
    jr z,RdMvNoSlow
        ; if no-verification
    ld a,1    
    ld (RdCkMt),a

    jr RdMvOkMv
    
RdMvNoSlow:

    ld a,b        
    ; ix - points to last record of moves data buffer
    call VerifyAndSaveChessMove

RdMvOkMv: 
    pop bc
    
    dec b
    jr nz,RdNxtC
    
    ; if reads after depth, then tell it
    push de
    ld de,RdeError        ; display info that wrong data
    call DrawString
    call PressEnter
    pop de
    jr RdDataEx

RdNxtC:
      
    ld a,1
    sub c
    ld c,a                  ; c=1-c
    
    ld a,e
    or a                    ; loop until E=0
    jr nz,RdMvListLoop

RdDataEx:
    call UndoAllMoves
    
    ld a,$ff
    ld (iy),a        ; end of list
    
    ret


RdeError:
        .byte "Read error MVs",0

; generate moves, find in list, save
VerifyAndSaveChessMove:

        push de
        push hl
        
        ld c,a      ; c = which move by number
    
        call GenMoves   ; generate chess moves now

        ; Look for move in the list of possible moves

        ld hl, NEXT_MVCNT
        ld b,(hl)
        ld hl, NEXT_MOVES

        ld a,b
        or a
        jr z,LoKmEnd
        sub c
        jr nc,LoKmSt
            ; if move impossible (N>count), then tell it
        call LoKmDispError
        jr LoKmEnd

LoKmSt:
        inc c
LoKmLoop:
        ld d,0          ; not saved
        
        ld a,(RdMvDp)
        ld (ix),a      ; depth
        inc ix
                 
        push bc
        ld b,4          ; copy piece, sq.from,to, flags
LoKmLp4bytes:
        ld a,(hl)
        ld (ix),a
        inc ix
        inc hl
        djnz LoKmLp4bytes
        pop bc
        
        dec hl          ; on flags
        
        dec c            
        jr nz,LoKmNotN  ; is this move?
        
        ; make move now
        
        dec hl
        dec hl
        ld a,(hl)
        ld (SQi),a      ; from sq.
        inc hl
        ld a,(hl)
        ld (SQt),a      ; to sq.
        inc hl
        ld a,(hl)       ; flags
      
        bit 1,a
        jr z, LoKmNoProm

        ; piece promoted too
        ; which piece?
        srl a
        srl a
        and 3         ; 0..3 piece Id
        inc a           ; 4=Q,3=R,2=B,1=N
        jr LoKmPromPc
LoKmNoProm:
        ld a,0          ; not promoted
LoKmPromPc:    
        ld (PromoPcId),a
        call MakeMove
        ld d,1          ; moved and saved flag

LoKmNotN:
        ld a,(hl)
        inc hl
        bit 7,a         ; if checkmate then anyway good move
        jr z,LoKmNotSave
        
        ld a,1          ; there is checkmate#
        ld (RdCkMt),a
        jr LoKmLoopMv      ; save this in list (not remove)
          
LoKmNotSave:
        ld a,d          ; if already saved
        or a
        jr nz,LoKmLoopMv
 
        dec ix     ; if not this move then remove from list
        dec ix
        dec ix
        dec ix
        dec ix
         
LoKmLoopMv:
        dec b
        jr nz,LoKmLoop      ; otherwise look for other move

        xor a
        ld (ix),a          ; the last
LoKmEnd:       
        pop hl
        pop de
        ret
        
LoKmDispError:
        push de
        ld de,LoKmError        ; display info that wrong data
        call DrawString
        call PressEnter
        pop de
        ret
           
LoKmError:
        .byte "MoveGen,B0 error",0
        
RdMvDp: .byte 0     ; depth now 0..5


; --------------
;
; Read sequenced bits at address
; A - count of bits to read
; Returns:
;  A=value [0..255]
;  E is 0 if the last bit reached

ReadBits:
    push bc
    ld b,a          ; b=count of bits to read
    
    ld a,e
    sub b
    ld e,a           ; counter-=b
    
    ld c,0          ; c = Value = 0
RdBtLoop:           ; loop and add bits to Value
    rl c            ; c<<=1
    
    ld a,(hl)
    and d
    jr z,RdBt1      ; if no bit then do nothing
    inc c           ; c|=1

RdBt1:
    ld a,d
    dec a           ; is d==1 ?
    jr nz,RdBt2
    inc hl          ; next byte if is
    ld d,128
    jr RdBt3
RdBt2:
    rr d            ; d to next bit, or to 128
RdBt3:
    djnz RdBtLoop   ; read all b bits
    ld a,c          ; A = Value
    pop bc
    ret
    
RdBoI:
    .byte 0

RdPiecesList:
    .byte " PNBRQKpnbrqk",0

RdMoveTo:
    .byte 0         ; "side to move" 0-white,1-black
    
RdMvTbCode:
    .byte 0         ; code of move encoding [0..7]

RdMvTable:          ; width in bits (side to move, opponent)
    .byte 4,3,  5,3,  5,4,  5,5,  6,3,  6,4,  6,5,  6,6

RdMvW:
    .byte 0         ; width of move for side to move
RdMvB:
    .byte 0         ; width of move for other side
    
RdBnkSet:
    .byte 0         ; memory bank set
RdSeg:
    .byte 0         ; segment C0-FE
    
RdGC:
    .byte 0         ; count of games in 256 bytes
RdGi:
    .byte 0         ; which game of these in 256 bytes
RdB0:
    .byte 0         ; count of bits of this game
RdBY:
    .byte 0         ; game bytes
RdFEN:
    .ds 100         ; FEN of game from data
RdCkMt:
    .byte 0         ; flag, 1 there is checkmate


; Move numbers by depth, ends with $ff
; CDEPTH is 5 ply for 3 moves
MOVES_N:
    .ds (CDEPTH+3)


;
; Also a buffer of moves, verification case.
; Plus checkmate cases, for the last move.
;
; 5 bytes record:
;   1. depth, started from 1, or 0 if end of list
;   2.-5. move data as 4 bytes in MOVE_HIST

MOVES_I:
    .ds 50*(CDEPTH+3)

