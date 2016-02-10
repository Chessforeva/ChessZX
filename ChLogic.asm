  
startFEN:       
        .byte "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        .byte 0                       


; STATUSES OF CURRENT CHESS GAME 

; Current chess position

BOARD:  .byte "a1    h1"        ;64 bytes of board
        .byte "a2    h2"
        .byte "        "
        .byte "        "
        .byte "        "
        .byte "        "
        .byte "        "
        .byte "a8    h8"
        .byte 0
        
SIDE:     .byte 0       ; side to move 0-white, 1-black   
CASTLES:  .byte 0       ; castling allowance flags
                        ; if bits are set: 0 wK, 1 wQ, 2 bK, 3 bQ sides
ENPSQ:    .byte $ff     ; en-passant square none
WKSQ:     .byte 0       ; square of white king
BKSQ:     .byte 0       ; square of black king
            
NEXT_MVCNT: .byte 0      ;count, values, and string to output          
NEXT_MOVES: .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      
; list of moves from the root
; bytes:
;  square from,
;  square to,
;  promoted piece with flags
;  en-passant square move before

MOVE_CNT   .byte 0   ; count of moves (below 127 chess both-side-moves) 

MOVE_HIST  .ds 1024; Kb for history of chess moves (4bytes per 1-side-move)
          
; Container for UCI-string-of-moves in form "e2e4 e7e5 ... e1g1 ... b7b8q ... "
; (set before MOVE_byString usage), it is for data passing
MOVE_str: .byte "                                                                "          
          .byte "                                                                " 
          .byte "                                                                "
          .byte "                                                                "
          .byte "                                                                "
          .ds $700
          ;above 2Kb of moves

;
; variables and arrays for next movement generation
; (for printing and displaying)          
NEXT_MVSTR: .byte "                                                    "
            .byte "                                                    "
            .byte "                                                    "
            .byte "                                                    "
            .byte "                                                    "                      
            .byte "                                                    "
            .byte "                                                    "
            .byte "                                                    "
            .byte "                                                    "
            .byte "                                                    "
            .byte "                                                    "                      
            .byte "                                                    "
            .byte "                                                    "
            .byte "                                                    "
            
; We use own memory of .COM file till now
;  
;  So, we are able to generate next chess moves, detect check and checkmate in 1.
;  And print it out. No reasonable chess evaluation searches possible here!

GenMovesAndString:
            call GenMoves            
            call MovesToStr    ; to print after 
            ret
                                    
;BEGIN of ========== MAKE MOVES BY UCI-MOVE-STRING          
; use UndoAllMoves before or setFEN
;
                                        
MOVE_byString:
        push af
        push bc
        push de

        ld de, MOVE_str 
        
MbyStrLoop:
        call GenMoves 
                                     
        ld a,(de)          ; find separator
        cp ' '
        jr z,MbySover
        cp 0
        jr z,MbySover
        
        call MbyStrMove    ; single move
        
        inc de
        inc de             ; skip promoted and blank too
        jr MbyStrLoop      ; and process next move   
MbySover:        
        pop de
        pop bc
        pop af
        ret

; "move by string" in DE register
; restore registers after
MbyStrMove:
        call MbySgetSQ
        ld (SQi),a
        call MbySgetSQ
        ld (SQt),a
        ld a,(de)
        cp ' '
        jr nz,MbyPromPiece
        dec de
        jr MbyNoN        
MbyPromPiece:
        cp 'q'
        jr nz,MbyNoQ
        ld a,4
        jr MbyProm
MbyNoQ: cp 'r'       
        jr nz,MbyNoR
        ld a,3
        jr MbyProm
MbyNoR: cp 'b'       
        jr nz,MbyNoB
        ld a,2
        jr MbyProm
MbyNoB: cp 'n'       
        jr nz,MbyNoN
        ld a,1
        jr MbyProm
MbyNoN: ld a,0             ; not promoted       
MbyProm:
        ld (PromoPcId),a
        call MakeMove
        ret

MbySgetSQ:
        ld a,(de)
        inc de
        sub 'a'
        ld b,a
        ld a,(de)
        inc de
        sub '1'
        sla a
        sla a
        sla a
        or b
        ret                ; a-reg contains square on return

;END


;BEGIN of ========== SIMPLY UNDO ALL MOVES

UndoAllMoves:
         push af
UndoAllLoop:
         ld a,(MOVE_CNT)
         cp 0
         jr z,UndoAllBrk
         call UnMakeMove
         jr UndoAllLoop
UndoAllBrk:         
         pop af
         ret
;END
      
                                                  
;BEGIN of ========== MOVE GEN - movement generation routine
; Very proper - according to chess rules.
; Scanning all board squares 0..63, trying to move pieces by type, then by directions.
; If move seems good then adds to the list with verification on own king attacks after movement
; to avoid illegal cases.

SQi:         .byte 0    ;current square
SQt:         .byte 0    ;square to move to

MateDetect:  .byte 0    ; if 1 then just find the first legal move and return

GenMoves:
         push af
         push bc
         push de
         push hl
         push ix
         push iy
         
         call BOsave
                  
         ld a,0    
         ld (CastlMove),a
         ld (PromoPcId),a
         ld (CaptPiece),a 
         
         ld a, (MateDetect)
         cp 1
         jr z,GenMvNoClr    ; if we are not gona save
                  
         ld ix, NEXT_MVCNT
         ld (ix), 0
         
GenMvNoClr:         
         ld a, 0            ; square counter 0..63
         ld (SQi), a
         ld hl, BOARD
GenMvSqScan:
         ld a, (hl)          ; piece
         
         push hl
         pop iy    ; set to remember square-from
         
         ld a, (hl)          ; get piece standing there
         cp 0
         jr z, OverMGSq      ; empty square
         
         cp 'a'              ; pieces
         jr nc, BlackPiece

WhitePiece:                  ; A-Z                    
         ld a,(SIDE)
         cp 0        
         jr nz, OverMGSq     ; not turn
         ld a, (hl)
         jr WhichPiece        
BlackPiece:       
         ld a,(SIDE)
         cp 1 
         jr nz, OverMGSq     ; not turn
         ld a, (hl)
         sub 32            ; to uppercase
WhichPiece: 
         cp 'P'
         jr z, Pmoves
         cp 'N'
         jr z, Knmoves       
         cp 'B'
         jr z, Bsmoves
         cp 'R'
         jr z, Rkmoves
         cp 'Q'
         jr z, Qnmoves
         cp 'K'
         jr z, Kgmoves                                          
         jr OverMGSq                
Pmoves:
         ld a,(SIDE)
         cp 1
         jr z, blackPmoves
         call MoveGenWPawn
         jr OverMGSq
blackPmoves:
         call MoveGenBPawn
         jr OverMGSq
Knmoves:
         call MoveGenKnight
         jr OverMGSq
Bsmoves:
         call MoveGenBishop
         jr OverMGSq
Rkmoves:
         call MoveGenRook
         jr OverMGSq                                                           
Qnmoves:
         call MoveGenBishop   ; simply
         call MoveGenRook     ; the same
         jr OverMGSq
Kgmoves:
         call MoveGenKing
         jr OverMGSq
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
OverMGSq:
         inc hl
         
         ld a, (MateDetect)
         cp 2
         jr nz, GenMVscn
         jr GenMVexit
GenMVscn:         
         ld a, (SQi)       
         inc a
         ld (SQi),a
         
         call MoveGen_Dbg   

         sub 64   ; Loop 0-63
         jr nz, GenMvSqScan
GenMVexit:

         call LookForCheckmates   ; verifies checkmates
         
         call BOrest
                                   
         pop iy
         pop ix
         pop hl
         pop de
         pop bc
         pop af
         ret

MoveGen_Dbg:
         cp 16
         jr nz,MoveGen_Dbg_e
         nop
MoveGen_Dbg_e:
         ret       
                  
; helping procedure to obtain current X,Y on board and store to d,e regs.
StoreXYtoDE:
         ld a, (SQi)
         and 7
         ld d,a      ; X- a..h
         ld a, (SQi)
         srl a
         srl a
         srl a
         ld e,a      ; Y- 1..8
         ret

; sets new SQt to move and calculates new hl position          
StoreSqToMove:                 
         ld a,e
         sla a
         sla a
         sla a
         or d
         ld (SQt),a              ;a-reg. contains the new square to jump
         
         ld b,0
         ld a,(SQi)
         ld c,a
         ld a,(SQt)
         sub c
         jr c, StDistNeg
         ld c,a
         add hl,bc              ;new hl position sq-forward
         jr StNSqOver
StDistNeg:
         ld a,(SQt)
         ld c,a
         ld a,(SQi)         
         sub c
         ld c,a                         
         scf
         ccf
         sbc hl,bc              ;new hl position sq-backward
StNSqOver:         
         ret
;END


;BEGIN of ========== MOVE GEN for WHITE PAWN goes up

MoveGenWPawn:

         push af
         push de
         
         ld a,0
         ld (CaptPiece),a

         ld a, (ENPSQ)
         ld d,a         
                  
         ld a,(SQi)                  
         add a, 8
         ld (SQt),a
         ld bc,8         ;1-7 vert. try next sq forward        
         add hl, bc      ;new square-to
         
         call IsEmpty
         jr nc, wpNoFWmoves
         call AddMovPromo
         
         ld a, (SQi)
         cp 16          ;below row 3
         jr nc, wpNoFWmoves
         add a, 8+8
         ld (SQt),a 
         ld bc, 8        ;another (double) move
         add hl, bc      ;new square-to
         
         call IsEmpty
         jr nc, wpNoFWmoves
         call AddMove

wpNoFWmoves:         
         push iy
         pop hl    ; restore 
         
         ld a,1
         ld (CaptPiece),a
         
         ld a, (SQi)
         and 7
         cp 0
         jr z, wpTryCaptRight

         ld a, (SQi)                  
         add a, 8-1 ; try capture left
         ld (SQt),a
         ld bc, 8-1
         add hl, bc

         call IsBlack
         jr nc, wpTryLeftEnPs
         
         call AddMovPromo
         jr wpTryCaptRight
         
wpTryLeftEnPs:              
         ld a,(SQt)
         cp d
         jr nz, wpTryCaptRight
                                
         call AddMovPromo
         jr wpTryCaptRight
                                                                               
wpTryCaptRight:
         push iy
         pop hl    ; restore 
         
         ld a, (SQi)
         and 7
         cp 7
         jr z, wpMGend
                  
         ld a, (SQi)
         add a, 8+1 ; try capture right
         ld (SQt),a
         ld bc, 8+1
         add hl, bc

         call IsBlack
         jr nc, wpTryRightEnPs
         
         call AddMovPromo
         jr wpMGend
         
wpTryRightEnPs:              
         ld a,(SQt)
         cp d
         jr nz, wpMGend
                     
         call AddMovPromo
         jr wpMGend
                                                                                                                                               
wpMGend:
         push iy
         pop hl    ; restore 

         ld a,0
         ld (PromoPcId),a
         
         pop de
         pop af
         ret
     
;END

;BEGIN of ========== MOVE GEN for BLACK PAWN goes down

MoveGenBPawn:

         push af
         push de
         
         ld a,0
         ld (CaptPiece),a

         ld a, (ENPSQ)
         ld d,a         
                  
         ld a,(SQi)                  
         sub 8
         ld (SQt),a
         scf
         ccf
         ld bc,8         ;1-7 vert. try next sq forward        
         sbc hl, bc      ;new square-to
         
         call IsEmpty
         jr nc, bpNoFWmoves
         call AddMovPromo
         
         ld a, (SQi)
         cp 48           ;above row 6
         jr c, bpNoFWmoves
         sub 8+8
         ld (SQt),a
         scf
         ccf 
         ld bc, 8        ;another (double) move
         sbc hl, bc      ;new square-to
         
         call IsEmpty
         jr nc, bpNoFWmoves
         call AddMove

bpNoFWmoves:         
         push iy
         pop hl    ; restore 
         
         ld a,1
         ld (CaptPiece),a
         
         ld a, (SQi)
         and 7
         cp 7
         jr z, bpTryCaptRight

         ld a, (SQi)                  
         sub 8-1 ; try capture left
         ld (SQt),a
         scf
         ccf
         ld bc, 8-1
         sbc hl, bc

         call IsWhite
         jr nc, bpTryLeftEnPs
         
         call AddMovPromo
         jr bpTryCaptRight
         
bpTryLeftEnPs:              
         ld a,(SQt)
         cp d
         jr nz, bpTryCaptRight
                                
         call AddMovPromo
         jr bpTryCaptRight
                                                                               
bpTryCaptRight:
         push iy
         pop hl    ; restore 
         
         ld a, (SQi)
         and 7
         cp 0
         jr z, bpMGend
                  
         ld a, (SQi)
         sub 8+1 ; try capture right
         ld (SQt),a
         scf
         ccf
         ld bc, 8+1
         sbc hl, bc

         call IsWhite
         jr nc, bpTryRightEnPs
         
         call AddMovPromo      
         jr bpMGend
         
bpTryRightEnPs:              
         ld a,(SQt)
         cp d
         jr nz, bpMGend
                                
         call AddMovPromo
         jr bpMGend
                                                                                                                                               
bpMGend:
         push iy
         pop hl    ; restore 

         ld a,0
         ld (PromoPcId),a
         
         pop de
         pop af
         ret
     
;END


;BEGIN of ========== MOVE GEN for ROOKS

rookMoves: .byte '+',1,'=',0 ,'-',1,'=',0 ,'=',0,'+',1 ,'=',0,'-',1
           .byte 0
                          
MoveGenRook:

         push af
         push bc
         push ix
         
         call StoreXYtoDE   ; d= X [a..h]   e= Y [1..8]
         
         ld ix,rookMoves
         ld b, 4
         
RookScan4directions:

         push de
RookLoop:
         call RookTryMove
         cp 0
         jr z,RookLoop
         pop de
                 
         inc ix
         inc ix
         inc ix
         inc ix
         
         djnz RookScan4directions
       
         pop ix
         pop bc
         pop af
         ret
         
; try move to that direction
RookTryMove:

         push bc
         push ix
         
         ld a,0
         ld (CaptPiece),a
                  
         ld a, (ix)     ;given directions for a..h  -1,+1,=0
         inc ix
         cp '='
         jr z, RkZero1
         cp '-'
         jr z, RkNegX      
         ld a,d
         inc a           ;a=direction +1 of X 
         cp 8
         jr nc, RkNo
         jr RkCanOk1  
RkNegX:
         ld a,0          ;a=direction -1 of X
         cp d
         jr c, RkCanSubtrX
         jr RkNo
RkCanSubtrX:         
         ld a,d
         dec a 
RkCanOk1:
         ld d,a         
RkZero1: inc ix
         
         ld a, (ix)     ;given directions for 1..8  -1,+1,=0
         inc ix
         cp '='
         jr z, RkZero2        
         cp '-'
         jr z, RkNegY
         ld a,e
         inc a          ;a=direction +1 of Y  
         cp 8
         jr nc, RkNo
         jr RkCanOk2   
RkNegY:
         ld a,0         ;a=direction -1 of Y
         cp e
         jr c, RkCanSubtrY
         jr RkNo
RkCanSubtrY:         
         ld a, e
         dec a
RkCanOk2:
         ld e,a
RkZero2: inc ix         
         
         call StoreSqToMove    ;new SQt,hl        

         ld a, (SIDE)
         cp 1
         jr z,RkThisBlack
         call IsBlack           ; enemy?
         jr c,RkCaptMove
         call IsWhite           ; our?
         jr c,RkNo
         ld a,0                 ; continue after to this direction
         jr RkAdd        
RkThisBlack:         
         call IsWhite           ; enemy?
         jr c,RkCaptMove
         call IsBlack           ; our?                        
         jr c,RkNo
         ld a,0                 ; continue after to this direction
         jr RkAdd
RkCaptMove:
         ld a,1
         ld (CaptPiece),a       ; and stop to this direction  
RkAdd:
         call AddMove
         jr RkOver        

RkNo:    ld a,1                 ; stop to this direction

RkOver:
         push iy
         pop hl
         
         pop ix
         pop bc
         ret 

;END



;BEGIN of ========== MOVE GEN for BISHOPS

bishopMoves: .byte '+',1,'+',1 ,'+',1,'-',1 ,'-',1,'+',1 ,'-',1,'-',1
             .byte 0
                          
MoveGenBishop:

         push af
         push bc
         push ix
         
         call StoreXYtoDE   ; d= X [a..h]   e= Y [1..8]
         
         ld ix,bishopMoves
         ld b, 4
         
BishopScan4directions:

         push de
BishopLoop:
         call BishopTryMove
         cp 0
         jr z,BishopLoop
         pop de
                 
         inc ix
         inc ix
         inc ix
         inc ix
         
         djnz BishopScan4directions
       
         pop ix
         pop bc
         pop af
         ret
         
; try move to that direction
BishopTryMove:

         push bc
         push ix
                  
         ld a,0
         ld (CaptPiece),a
         
         ld a, (ix)     ;given directions for a..h  -1,+1
         inc ix
         cp '-'
         jr z, BsNegX      
         ld a,d
         inc a           ;a=direction +1 of X 
         cp 8
         jr nc, BsNo
         jr BsCanOk1  
BsNegX:
         ld a,0          ;a=direction -1 of X
         cp d
         jr c, BsCanSubtrX
         jr BsNo
BsCanSubtrX:         
         ld a,d
         dec a 
BsCanOk1:
         ld d,a         
         inc ix
         
         ld a, (ix)     ;given directions for 1..8  -1,+1
         inc ix
         cp '-'
         jr z, BsNegY
         ld a,e
         inc a          ;a=direction +1 of Y  
         cp 8
         jr nc, BsNo
         jr BsCanOk2   
BsNegY:
         ld a,0         ;a=direction -1 of Y
         cp e
         jr c, BsCanSubtrY
         jr BsNo
BsCanSubtrY:         
         ld a, e
         dec a
BsCanOk2:
         ld e,a
         inc ix         
         
         call StoreSqToMove    ;new SQt,hl        

         ld a, (SIDE)
         cp 1
         jr z,BsThisBlack
         call IsBlack           ; enemy?
         jr c,BsCaptMove
         call IsWhite           ; our?
         jr c,BsNo
         ld a,0                 ; continue after to this direction
         jr BsAdd        
BsThisBlack:         
         call IsWhite           ; enemy?
         jr c,BsCaptMove
         call IsBlack           ; our?                        
         jr c,BsNo
         ld a,0                 ; continue after to this direction
         jr BsAdd
BsCaptMove:
         ld a,1
         ld (CaptPiece),a       ; and stop to this direction  
BsAdd:
         call AddMove
         jr BsOver        

BsNo:    ld a,1                 ; stop to this direction

BsOver:
         push iy
         pop hl
         
         pop ix
         pop bc
         ret 

;END




;BEGIN of ========== MOVE GEN for KNIGHTS

knightMoves: .byte '+',1,'+',2 ,'+',2,'+',1 ,'+',2,'-',1 ,'+',1,'-',2
             .byte '-',1,'-',2 ,'-',2,'-',1 ,'-',2,'+',1 ,'-',1,'+',2
             .byte 0
             

MoveGenKnight:

         push af
         push bc
         push de
         push ix
         
         call StoreXYtoDE   ; d= X [a..h]   e= Y [1..8]
         
         ld ix, knightMoves
         ld b, 8
KnightScan8moves:
         call KnightTryMove
         inc ix
         inc ix
         inc ix
         inc ix
         djnz KnightScan8moves
       
         pop ix
         pop de
         pop bc
         pop af
         ret

; try move to that square
KnightTryMove:
         push bc
         push de
         push ix
         
         ld a,0
         ld (CaptPiece),a
         
         ld a, (ix)     ;given directions for a..h  -1,+1,-2,+2
         inc ix
         cp '-'
         jr z, KnNegX
         ld a, (ix)     ; a=direction +1 or +2 of X      
         add a,d
         cp 8
         jr nc, KnNo
         jr KnCanOk1  
KnNegX:
         ld a, (ix)     ; a=direction -1 or -2 of X
         dec a
         cp d
         jr c, KnCanSubtrX
         jr KnNo
KnCanSubtrX:         
         ld a, d
         sub (ix)    
KnCanOk1:
         ld d,a         
         inc ix
         
         ld a, (ix)     ;given directions for 1..8  -1,+1,-2,+2
         inc ix
         cp '-'
         jr z, KnNegY
         ld a, (ix)     ; a=direction +1 or +2 of Y  
         add a,e
         cp 8
         jr nc, KnNo
         jr KnCanOk2   
KnNegY:
         ld a, (ix)     ; a=direction -1 or -2 of Y
         dec a 
         cp e
         jr c, KnCanSubtrY
         jr KnNo
KnCanSubtrY:         
         ld a, e
         sub (ix)
KnCanOk2:
         ld e, a
         inc ix         
         
         call StoreSqToMove    ;new SQt,hl        

         ld a, (SIDE)
         cp 1
         jr z, KnThisBlack
         call IsBlack           ; enemy?
         jr c,KnCaptMove
         call IsWhite           ; our?
         jr c,KnNo
         jr KnAdd        
KnThisBlack:         
         call IsWhite           ; enemy?
         jr c,KnCaptMove
         call IsBlack           ; our?                        
         jr c,KnNo
         jr KnAdd
KnCaptMove:
         ld a,1
         ld (CaptPiece),a           
KnAdd:
         call AddMove         

KnNo:
         push iy
         pop hl
         
         pop ix
         pop de
         pop bc
         ret 

;END

;BEGIN of ========== MOVE GEN for KINGS

kingMoves: .byte '-',1,'=',0 ,'-',1,'+',1 ,'=',0,'+',1 ,'+',1,'+',1
           .byte '+',1,'=',0 ,'+',1,'-',1 ,'=',0,'-',1 ,'-',1,'-',1
           .byte 0
                          
MoveGenKing:

         push af
         push bc
         push ix
         
         call StoreXYtoDE   ; d= X [a..h]   e= Y [1..8]
         
         ld a,4       ; on E?
         cp d
         jr nz, CastleCantBe
         call CastleMOVES   ; add castling moves without check-verification
         
CastleCantBe:
         
         ld ix,kingMoves
         ld b, 8
         
KingScan8moves:
         call KingTryMove
         inc ix
         inc ix
         inc ix
         inc ix
         djnz KingScan8moves
                       
         pop ix
         pop bc
         pop af
         ret
         
; try move to that square
KingTryMove:
         push bc
         push de
         push ix
         
         ld a,0
         ld (CaptPiece),a
                  
         ld a, (ix)      ;given directions for a..h  -1,+1,=0
         inc ix
         cp '='
         jr z, KgZero1
         cp '-'
         jr z, KgNegX      
         ld a,d
         inc a           ;a=direction +1 of X 
         cp 8
         jr nc, KgOver
         jr KgCanOk1  
KgNegX:
         ld a,0          ;a=direction -1 of X
         cp d
         jr c, KgCanSubtrX
         jr KgOver
KgCanSubtrX:         
         ld a,d
         dec a 
KgCanOk1:
         ld d,a         
KgZero1: inc ix
         
         ld a, (ix)     ;given directions for 1..8  -1,+1,=0
         inc ix
         cp '='
         jr z, KgZero2        
         cp '-'
         jr z, KgNegY
         ld a,e
         inc a          ;a=direction +1 of Y  
         cp 8
         jr nc, KgOver
         jr KgCanOk2   
KgNegY:
         ld a,0         ;a=direction -1 of Y
         cp e
         jr c, KgCanSubtrY
         jr KgOver
KgCanSubtrY:         
         ld a, e
         dec a
KgCanOk2:
         ld e,a
KgZero2: inc ix         
         
         call StoreSqToMove    ;new SQt,hl        

         ld a, (SIDE)
         cp 1
         jr z,KgThisBlack
         call IsBlack           ; enemy?
         jr c,KgCaptMove
         call IsWhite           ; our?
         jr c,KgOver
         jr KgAdd        
KgThisBlack:         
         call IsWhite           ; enemy?
         jr c,KgCaptMove
         call IsBlack           ; our?                        
         jr c,KgOver
         jr KgAdd
KgCaptMove:
         ld a,1
         ld (CaptPiece),a
KgAdd:
         call AddMove
KgOver:
         push iy
         pop hl
         
         pop ix
         pop de
         pop bc
         ret 

; Add castling moves to list
CastleMOVES:
                                 
         ld a,0
         ld (CaptPiece),a      
         ld a,1
         ld (CastlMove),a
         
         ld c,1       ; directions 0-King side, 1-Queen side
CastleDirScan:       
         ld a, (CASTLES)
         ld b,a       ; b contains castlings flags
         ld a,e
         cp 7         ; detect king
         jr nz, CstlWhite
         srl b        ; simply roll castling flags for black
         srl b
         jr CstlBegin
CstlWhite:
         cp 0
         jr nz, CastleBrk    
CstlBegin:
         push de
         ld a,0
         cp c
         jr z, CstlQside       
         ld a,b
         bit 0,a
         jr z,CstlLoop
CstEmpty1:
         inc d
         inc hl
         ld a,d
         cp 7
         jr z, CstlGood1
         call IsEmpty
         jr nc, CstlLoop
         jr CstEmpty1
CstlGood1:
         dec d              ; back h1->g1  or h8->g8
         dec hl
         ld a,(SQi)
         inc a
         inc a
         ld (SQt),a
         call AddMove
         jr CstlLoop
CstlQside:
         ld a,b
         bit 1,b
         jr z,CstlLoop
CstEmpty2:          
         dec d
         dec hl
         ld a,d
         cp 0
         jr z, CstlGood2
         call IsEmpty
         jr nc, CstlLoop
         jr CstEmpty2
CstlGood2:
         inc d
         inc d              ; back a1->c1  or a8->c8
         inc hl
         inc hl
         ld a,(SQi)
         dec a
         dec a
         ld (SQt),a
         call AddMove         
CstlLoop:
         pop de
         ld a,0
         cp c
         jr z,CastleBrk
         dec c
         
         push iy
         pop hl   ; restore hl
         
         jr CastleDirScan    ;Next direction
         
CastleBrk:         
         push iy
         pop hl   ; restore hl
         
         ld a,0
         ld (CastlMove),a                    

         ret

;END

;BEGIN of ========== Is empty square?

IsEmpty:
         ld a, (hl)
         cp 0
         jr z, EmptySqIs
         scf
         ccf
         jr EmptySqOver
EmptySqIs:
         scf
EmptySqOver:
         ret           
;END

;BEGIN of ========== Is square occupied?

IsOccupied:
         ld a, (hl)
         cp 0
         jr z, OccupiedSqIs
         scf
         ccf
         jr OccupiedSqOver
OccupiedSqIs:
         scf
OccupiedSqOver:
         ret           
;END


;BEGIN of ========== Is piece white?

IsWhite:
         ld a, (hl)
         cp 'P'
         jr z, WhitePcIs
         cp 'N'
         jr z, WhitePcIs
         cp 'B'
         jr z, WhitePcIs
         cp 'R'
         jr z, WhitePcIs
         cp 'Q'
         jr z, WhitePcIs
         cp 'K'
         jr z, WhitePcIs
         scf
         ccf
         jr WhitePcOver
WhitePcIs:
         scf
WhitePcOver:
         ret    

;END

;BEGIN of ========== Is piece black?

IsBlack:
         ld a, (hl)
         cp 'p'
         jr z, BlackPcIs
         cp 'n'
         jr z, BlackPcIs
         cp 'b'
         jr z, BlackPcIs
         cp 'r'
         jr z, BlackPcIs
         cp 'q'
         jr z, BlackPcIs
         cp 'k'
         jr z, BlackPcIs
         scf
         ccf
         jr BlackPcOver
BlackPcIs:
         scf
BlackPcOver:
         ret    

;END

;BEGIN of ========== ADD NEW MOVE to the list with verification

;various flags
CaptPiece:  .byte 0
PromoPcId:  .byte 0
CastlMove:  .byte 0

AddMovPromo:
         ld a,4
         ld (PromoPcId),a        ; pawns may promote, so this counter is like promoted-piece-id of Q,R.B.N
AddMove:
         push af
         
         call ValidateKingCheck  ; this verifies if king is attacked after movement, or can not castle
         cp 0
         jr z,AddMLegal          ; if valid move
         
         jr fAddMVover
         
AddMLegal:
         ld a, (MateDetect)
         cp 0
         jr z, AddMNormalCase
        
         ld a,2                  ; we found a legal move, so not checkmate
         ld (MateDetect),a
         jr fAddMVover   
         
AddMNormalCase:         
                  
         push bc
         push ix
         ld ix, NEXT_MVCNT
         ld c, (ix)     ;n
         ld b, 0
         inc (ix)       ;n=n+1
         ld ix, NEXT_MOVES
         sla c
         sla c
         add ix, bc     ;n*(piece,from,to,flags)
         ld a, (iy)     ;piece
         cp 'a'
         jr c, addMVupperAlready
         sub 32         ;to uppercase
         
addMVupperAlready:         
         ld (ix), a
         inc ix
                 
         ld a, (SQi)    ;from square
         ld (ix), a
         inc ix
                 
         ld a, (SQt)    ;to square
         ld (ix), a
         inc ix
         
         ; and add one byte for flags
         
         ld a, (CaptPiece)
         ld b, a                ; bit 0 - captured a piece

         ld a, (PromoPcId)
         cp 0
         jr z, addMVnoprom
                  
         ld a, (SQt)    ;to
         cp 64-8        ;row 8?
         jr c, addMVq
         jr addMVpromos
         
fAddMVover:
         jr AddMVover 
         
addMVq:
         cp 8           ;row 1?
         jr nc, addMVnoprom

addMVpromos:         
         set 1,b                ; bit 1 - promotion, bits 2,3 - promoted piece Nr. Q=3,R=2,B=1,N=0   
         ld a, (PromoPcId)
         dec a
         ld (PromoPcId),a
         cp 0
         jr z, addMVp2
         
         call AddMove          ; promotion move for all other pieces, check detection lags here     
         
addMVp2:        
         sla a
         sla a
         or b
         ld b,a       
   
addMVnoprom:
         ld a, (CastlMove)
         cp 0
         jr z, addMVnoCastle
         set 4,b                ; bit 4 - castling indicator
addMVnoCastle:

         ld a, (CaptPiece)
         cp 0
         jr z,addMVnoEP
         ld a, (ENPSQ)
         cp $ff
         jr z,addMVnoEP 
         ld c,a
         ld a, (SQt)
         cp c
         jr nz,addMVnoEP
         set 5,b                ; bit 5 - en-passant capture
                                ; bit 6 - later for check+ detection
                                ; bit 7 - later for checkmate detection
addMVnoEP:
                                    
         ld (ix), b
         inc ix
         
         pop ix
         pop bc        
                                                                            
         call MakeMove         ; check+ detection action, with checkmate detection later
         call UnMakeMove
                         
AddMVover:
         pop af         
         ret

;END


;BEGIN of ========== Composes notation string for output for current list of generated moves
;                   (last by MOVE GENs) 

MovesToStr:
         push af
         push bc
         push de
         push ix
         
         ld de, NEXT_MVSTR
         ld ix, NEXT_MVCNT

         ld b,(ix)
         ld ix, NEXT_MOVES
         ld a, b
         cp 0
         jr z, MV2sEndSymb 
         
MV2sToLoop:

         ld a, (ix)   ; piece
         cp 'P'
         jr z, MV2sNoPieceNotate
         ld (de),a
         inc de
MV2sNoPieceNotate:
         inc ix
                
         call MC2getSQ   ; from square
         inc de    
         inc de
         call MC2getSQ   ; to square
         dec de    
         dec de
                           
         ld a, (ix)    ; flags
         bit 4,a
         jr z,MV2sNoCastle
         call MC2sCASTLEs   ; redraws castling 0-0 or 0-0-0      
         jr MV2sNoProm    
MV2sNoCastle:
         ld a, (ix)    ; flags again
         bit 0,a
         jr z, MV2sNoCapt
         ld a, 'x'
         jr MV2xTo
MV2sNoCapt:
         ld a, '-'
MV2xTo:         
         ld (de),a
         inc de        
         inc de
         inc de
                
         ld a, (ix)    ; possibly promotion
         bit 1,a
         jr z, MV2sNoProm
         
         call MV2sPromotion ; add piece promotion text =Q..N 

MV2sNoProm:
         ld a, (ix)    ; possibly check+
         bit 6,a
         jr z, MV2sNoCheck
         
         ld a, (ix)    ; possibly checkmate
         bit 7,a
         jr z, MV2sJustCheck

         ld a, '#'
         jr MV2sCk1
         
MV2sJustCheck:         
         ld a, '+'
MV2sCk1: ld (de),a        
         inc de         
         
MV2sNoCheck:                     
         inc ix
                 
         ld a, ','
         ld (de),a        
         inc de
         
         djnz MV2sToLoop
         dec de
         
MV2sEndSymb:         
         ld a, '$'
         ld (de),a         
         inc de
         ld a, 0
         ld (de),a 
                 
         pop ix
         pop de
         pop bc
         pop af
         ret

MV2sPromotion:

         ld a, '='
         ld (de),a
         inc de
                  
         ld a, (ix)    ; which piece?
         srl a
         srl a
         and 3         ; 0..3 piece Id
         cp 3          ; Queen?
         jr nz, MC2sNoQ
         ld a, 'Q'
         jr MC2sPrId         
MC2sNoQ: cp 2          ; Rook?
         jr nz, MC2sNoR
         ld a, 'R'
         jr MC2sPrId        
MC2sNoR: cp 1          ; Bishop?
         jr nz, MC2sNoB
         ld a, 'B'
         jr MC2sPrId
MC2sNoB: ld a, 'N'     ; Knight
MC2sPrId:
         ld (de),a
         inc de
         ret
          
MC2sCASTLEs:           ; simply rewrites characters in buffer 

         inc de
         ld a,(de)
         dec de
         dec de
         dec de
         dec de
         cp 'c'
         jr nz,MC2sShort       
         ld a, '0'     ; 0-0-0
         ld (de),a
         inc de
         ld a, '-'
         ld (de),a
         inc de
MC2sShort:
         ld a, '0'     ; just 0-0 part
         ld (de),a
         inc de
         ld a, '-'
         ld (de),a
         inc de
         ld a, '0'
         ld (de),a
         inc de                           

         ret
; get square and store to text string
MC2getSQ:
         ld a, (ix)
         and 7
         add a, 'a'
         ld (de),a        
         inc de
         ld a, (ix)
         srl a
         srl a
         srl a
         add a, '1'
         ld (de),a
         inc ix
         ret

; printing of current legal moves to output
MovesOut:
         push de
         ld de, NEXT_MVSTR
         call PrintString
         pop de
         ret

;END        

;BEGIN of ========== SET FEN position on BOARD and variables
;
; parameter: a-reg
;  0 - set by using startFEN
;  1 - set fen provided in IX register
;
SetFEN:
         push af
         push bc
         push de
         push hl

         cp 0
         jr z, SetFENstartpos
         push ix
         pop de   ;de contains FEN address now
         jr SetFENp
SetFENstartpos:         
         ld de, startFEN
SetFENp: ld a,$ff
         ld (WKSQ),a
         ld (BKSQ),a
         call SetSqScan
         call ScanSqNxt
         ld b, 64
setBoardSquare:
         ld a, (de)
         inc de
         cp '/'
         jr z, setBoardSquare
         ld c, a
         cp '1'
         jr c, notFenDigit  ;ifA<'1' then jump
         cp '9'
         jr nc, notFenDigit ;ifA>='9' then jump
         sub '0'
         ld c,a
setFenEmptyPiece:
         ld (hl), 0
         call ScanSqNxt
         dec c
         dec b
         ld a,c
         cp 0
         jr nz, setFenEmptyPiece
         inc b
         jr nextFenPiece
notFenDigit:
         ld (hl),a
         cp 'K'
         jr nz, setFenNoWK
         call GetScanSQ
         ld (WKSQ),a
         ld a,(hl)
setFenNoWK:
         cp 'k'
         jr nz, setFenNoBK
         call GetScanSQ
         ld (BKSQ),a
         ld a,(hl)
setFenNoBK:         
         call ScanSqNxt
nextFenPiece:
         djnz setBoardSquare
         
         inc de                 ; detects which side to move
         ld a, (de)
         cp 'w'
         jr nz,setFenBlackMv
         ld a,0
         jr setFenCol2Mv
setFenBlackMv:
         ld a,1
setFenCol2Mv:
         ld (SIDE),a
         inc de
         inc de

         ld b,0           
setFenCastlings:
         ld a, (de)
         inc de
         cp ' '
         jr z,setFenCsOver ; break loop
 
         cp '-'
         jr z,setFenCs     ; just skip

         cp 'K'
         jr nz,setFenC2
         set 0,b
         jr setFenCs
setFenC2:
         cp 'Q'
         jr nz,setFenC3
         set 1,b
         jr setFenCs
setFenC3:
         cp 'k'
         jr nz,setFenC4
         set 2,b
         jr setFenCs
setFenC4:
         cp 'q'
         jr nz,setFenCs
         set 3,b     
setFenCs:
         jr setFenCastlings ;just go next      
setFenCsOver:
         ld a,b
         ld (CASTLES),a
         
         ld a, (de)           ; is en-passant square provided?
         inc de
         cp '-'
         jr z,setFenNoEnp     ; just skip
         sub 'a'
         ld b,a         
         ld a, (de)
         inc de
         sub '1'
         sla a
         sla a
         sla a       
         or b                 ; reg-a = given en-passant square
         jr setFenENPsq     
setFenNoEnp:
         ld a,$ff
setFenENPsq:
         ld (ENPSQ),a         ; save en-passant square

         inc de
         ld a,0
         ld (MOVE_CNT),a
                                                                                                                                                                                                   
         pop hl
         pop de
         pop bc
         pop af
         ret

;END

;BEGIN of ========== GET FEN position from BOARD and variables
;
; parameter:  address in DE register for resulting string,
;   ends with $, can be printed right after
;
GetFEN:
         push af
         push bc
         push de
         push hl
         push ix
         
         push de
         pop ix        ; ix = de
      
         call SetSqScan
         ld b,64
         ld d,0        ; current column
getBoardSquare:
                  
         ld a,d
         cp 8           ; if slash for new line
         jr nz,getBoNsl
         
         ld a,'/'
         ld (ix),a
         inc ix
         ld d,0
getBoNsl:
         call ScanSqNxt
         ld a, (hl)

         cp 0
         jr z,getBo0
         cp ' '
         jr z,getBo0  ; if empty square or space
         jr getBoNo0
getBo0:         
         ld a,d
         cp 0
         jr nz,getBoC2
getBo1:  ld a,'1'          ; first space
         jr getBoNo0
getBoC2:
         dec ix         
         ld a,(ix)
         inc ix
         sub '1'
         jr c,getBo1         
         cp 8
         jr nc,getBo1
         dec ix         
         ld a,(ix)
         inc a            ; char+1 counter of empty squares
getBoNo0:
         ld (ix),a
         inc ix
         
         inc d
         djnz getBoardSquare  ; scan all 64 squares    
         
         call getBo_      ; space
         
         ld a,(SIDE)      ; side to move
         cp 0
         jr nz,getBoB
         ld a,'w'
         jr getBoSd
getBoB:  ld a,'b'
getBoSd: ld (ix),a
         inc ix
                  
         call getBo_

         ld a,(CASTLES)
         ld b,a
         and %00001111
         cp 0
         jr z,getBoNoCst
         bit 0,b
         jr z,getBnWK
         ld a,'K'
         ld (ix),a
         inc ix
getBnWK:  
         bit 1,b
         jr z,getBnWQ
         ld a,'Q'
         ld (ix),a
         inc ix
getBnWQ:  
         bit 2,b
         jr z,getBnBK
         ld a,'k'
         ld (ix),a
         inc ix
getBnBK:  
         bit 3,b
         jr z,getBoCsOver
         ld a,'q'
         ld (ix),a
         inc ix         
         jr getBoCsOver
                  
getBoNoCst:
         call getBoM    ; sign - 
getBoCsOver:

         call getBo_

         ld a,(ENPSQ)   ; add en-passant info
         cp $ff
         jr z,getBoNoEp
         ld b,a
         and 7
         add a,'a'
         ld (ix),a
         inc ix
         ld a,b
         srl a
         srl a
         srl a
         add a,'1'
         ld (ix),a
         inc ix
         jr getBoEpOver
getBoNoEp:
         call getBoM    ; sign - 
getBoEpOver:
         call getBo_

         ld a,'0'       ; ignore halfmoves
         ld (ix),a
         inc ix
         
         call getBo_
         
         ld a,(MOVE_CNT)
         srl a          ; =1+int(/2)
         add a,'1'
         ld (ix),a
         inc ix
         
         ld a,'$'
         ld (ix),a
         
         pop ix                                                                                                                                                                                           
         pop hl
         pop de
         pop bc
         pop af
         ret
         
getBo_:  ld a,' '
         ld (ix),a
         inc ix
         ret         
getBoM:  ld a,'-'
         ld (ix),a
         inc ix
         ret         
         

;END

;BEGIN of ========== BOARD printing out

BoardOut:
        .byte "........", 13, 10   ;output string
        .byte "........", 13, 10
        .byte "........", 13, 10
        .byte "........", 13, 10
        .byte "........", 13, 10
        .byte "........", 13, 10
        .byte "........", 13, 10
        .byte "........", 13, 10
        .byte "$", 0   
        
printBOARD:
         push af
         push bc
         push de
         push hl
         ld b, 8
         call SetSqScan
         ld de, BoardOut
printBoardLine:
         ld c, 8
printCharsInLine:         
         call ScanSqNxt
         ld a, (hl)
         cp 0
         jr nz, printCh1
         ld a, ' '            ; Empty square     
printCh1:
         ld (de), a
         inc de
         dec c
         ld a,c
         cp 0
         jr nz, printCharsInLine   ; 8x8
         inc de                    ; chr(13)+chr(10)
         inc de
         djnz printBoardLine

         ld de, BoardOut
         call PrintString

         pop hl
         pop de
         pop bc
         pop af
         ret
;END


;BEGIN of ========== BOARD scanning A1,B1,...,H1,A2,B2...,H8

; board order on screen or FEN notation is A8,B8,...,H8,A7,B7.... H1
; so we go to square 57 then scan to 63, then jump to 49 ... till last square is 7 

ScanSqC:  .byte 0   ;counter 1..8, reg HL contains pointer to BOARD square 

SetSqScan:
         ld hl, ScanSqC
         ld (hl),0              ; +1 for first loop 
         ld hl, BOARD+64-8-1
         ret
ScanSqNxt:
         push af         
         push bc
         push ix
         ld ix, ScanSqC
         inc (ix)
         ld a,(ix)
         cp 8+1
         jr z, ScanSqNxtRow
         inc hl                 ;just next square
retSqScan:         
         pop ix
         pop bc
         pop af
         ret
ScanSqNxtRow:
         ld a,1
         ld (ix),a
         ld bc, 8+7           ;square of previous row, -7
         scf
         ccf
         sbc hl, bc           ;there is no sub oper., so we use sbc                          
         jr retSqScan
GetScanSQ:                   ; returns current square I
         push bc
         push hl
         ld bc, BOARD
         scf
         ccf
         sbc hl, bc          ; difference BOARD - hl-reg
          
         push hl
         pop bc
         ld a,c              ; will contain square Nr.
         pop hl
         pop bc
         ret

;END


;BEGIN of ========== KING CHECK+ DETECTION
;
; on return a-reg 1-is check, 0-not check
;
SQ_ck_i:      .byte 0   ; storing to restore later  
SQ_ck_t:      .byte 0
CastlMove_ck: .byte 0
PromoPcId_ck: .byte 0
CaptPiece_ck: .byte 0

IsCheck:
        push iy
        push hl
        
        ld a,(CastlMove)
        ld (CastlMove_ck),a
        ld a,(PromoPcId)
        ld (PromoPcId_ck),a
        ld a,(CaptPiece)
        ld (CaptPiece_ck),a
                
        ld a,0    
        ld (CastlMove),a
        ld (PromoPcId),a
        ld (CaptPiece),a 
                      
        ld a, (SQi)      ; save
        ld (SQ_ck_i),a
        ld a, (SQt)
        ld (SQ_ck_t),a
        ld a, (SIDE)
        cp 0
        jr nz,IsCkBlack
        ld a,(WKSQ)
        jr IsChSt
IsCkBlack:
        ld a,(BKSQ)        
IsChSt: 
        ld (SQt),a      
        ld (SQi),a
        call MM_hl        ; position hl to the square of king
                        
        push hl
        pop iy            ; contains square of king
        
        call ValidateKingCheck
        push af
        
        ld a, (SQ_ck_i)  ;restore
        ld (SQi),a
        ld a, (SQ_ck_t)
        ld (SQt),a
        
        ld a,(CastlMove_ck)
        ld (CastlMove),a
        ld a,(PromoPcId_ck)
        ld (PromoPcId),a
        ld a,(CaptPiece_ck)
        ld (CaptPiece),a
        
        pop af
        
        pop hl
        pop iy
        ret        
        
;END


;BEGIN of ========== VALIDATE KING CHECK
;
; Usually king validation is done by opposit movement generation with king capture detection.
; As it takes long to try all moves, we better verify square-attackers.
; This gives advantage that only really legal moves are in the list without further iterations.
;
; on return a=0 on valid move, 1-not valid

SQ_i:     .byte 0   
SQ_t:     .byte 0
SQ1:      .byte 0   ;contains flag that this square is nearby (+-1 sq)
SQk:      .byte 0   ;king square after movement
SQc:      .byte 0   ;square for castle checking

          
ValidateKingCheck:
                      
         push bc
         push de
         push hl
         push ix
         
         push iy
         pop hl              ;set back pointer to square of piece
         
         ld a,(SQt)          ;save values
         ld (SQ_t),a
         ld a,(SQi)
         ld (SQ_i),a
         
         ld a,(SQi)
         ld b,a
         ld a,(WKSQ)
         cp b
         jr nz,V_notWKing
         ld (SQk),a                                  
         jr V_ifCastl       ; castling of white king?
V_notWKing:
         ld a,(BKSQ)
         cp b
         jr nz,V_notKing
         ld (SQk),a
         jr V_ifCastl       ; castling of black king?
V_notKing:
         ld a,(SIDE)
         cp 0
         jr nz, V_blackPc
         ld a,(WKSQ)
         ld (SQk),a 
         jr V_Pc1
V_blackPc:
         ld a,(BKSQ)
         ld (SQk),a
V_Pc1:   ld b,a
         call V_hl_setpos   ; new position of hl at square of king
         call ValidateKSq   ; if check after movement to?        
         jr nc,KgNotValid  
         jr KgIsValid                  
V_ifCastl:                         
         ld a, (CastlMove)
         cp 0
         jr z, V_noCastl
         
         ld a,b
         ld (SQc),a
                           ; castling, b-reg contains king position already
         call ValidateKSq  ; if check?
         jr nc,KgNotValid

         ld a,(SQc)
         ld b,a                
         ld a,(SQ_t)
         cp b
         jr c, V_decWCstl
                           
         call V_CstlInc
         call ValidateKSq  ; f1,f8 under check?
         jr nc,KgNotValid
         
         call V_CstlInc
         call ValidateKSq  ; g1,g8 under check?
         jr nc,KgNotValid
         
         jr KgIsValid
         
V_CstlInc:
         ld a,(SQc)
         inc a
         ld (SQc),a
         ld b,a
         inc hl
         ret   
                  
V_decWCstl:
         call V_CstlDec
         call ValidateKSq  ; d1,d8 under check?
         jr nc,KgNotValid
         
         call V_CstlDec
         call ValidateKSq  ; c1,c8 under check?
         jr nc,KgNotValid
         
         jr KgIsValid

V_CstlDec:
         ld a,(SQc)
         dec a
         ld (SQc),a
         ld b,a
         dec hl
         ret 
                  
V_noCastl:
         ld a,(SQt)        ; move to under check?
         ld b,a
         call V_hl_setpos  ; new position of hl at square of king         
         call ValidateKSq
         jr nc,KgNotValid                          

KgIsValid:         
         ld a,0
         jr KgVret         
KgNotValid:
         ld a,1
KgVret:

         push af
         ld a,(SQ_t)          ;restore values
         ld (SQt),a
         ld a,(SQ_i)
         ld (SQi),a
         pop af
         
         pop ix
         pop hl
         pop de
         pop bc
         ret
                                                                                                                                 
; reposition of hl pointer to square, by calculation
V_hl_setpos:
         push bc
         ld a,b
         ld (SQi),a
         
         ld b,0
         ld a,(SQ_i)
         ld c,a
         ld a,(SQi)
         sub c
         jr c, V_StDistNeg
         ld c,a
         add hl,bc              ;new hl position sq-forward
         jr V_hl_rdy1
V_StDistNeg:
         ld a,(SQi)
         ld c,a
         ld a,(SQ_i)         
         sub c
         ld c,a                         
         scf
         ccf
         sbc hl,bc              ;new hl position sq-backward
V_hl_rdy1:
         pop bc
         ret

; detect if given square (in b) is under attack of opponent
  
ValidateKSq:
                                  
         ld a,b
         ld (SQi),a

         call StoreXYtoDE   ; d= X [a..h]   e= Y [1..8]  
                                                   
         ; try to attack this square in SQi (threat comes from SQt)

         ; diognals
         ld ix,bishopMoves
         ld b, 4
         
DiognalScan4directions:

         push de
         ld a,1
         ld (SQ1),a
DiognalLoop:
         call DiognalAttacks
         cp 0
         jr z,DiognalLoop
         pop de
         cp 2
         jr z,NotValidKgSq
                          
         inc ix
         inc ix
         inc ix
         inc ix
         
         djnz DiognalScan4directions        
         
         ; horizontals and verticals       
         ld ix,rookMoves
         ld b, 4
         
HorzVertScan4directions:

         push de
         ld a,1
         ld (SQ1),a
HorzVertLoop:
         call HorzVertAttacks
         cp 0
         jr z,HorzVertLoop
         pop de
         cp 2
         jr z,NotValidKgSq
              
         inc ix
         inc ix
         inc ix
         inc ix
         
         djnz HorzVertScan4directions

         ; knights       
         ld ix, knightMoves
         ld b, 8
VKnightScan8moves:
         call KnightAttacks
         cp 2
         jr z,NotValidKgSq
         inc ix
         inc ix
         inc ix
         inc ix
         djnz VKnightScan8moves
         
ValidKgSq:
         scf
         jr ValidKgSqRet
NotValidKgSq:
         scf
         ccf
ValidKgSqRet:
         ret


; look for diognal-attacks to that direction
DiognalAttacks:

         push bc
         push hl
         push ix
                          
         ld a, (ix)     ;given directions for a..h  -1,+1
         inc ix
         cp '-'
         jr z, V_BsNegX      
         ld a,d
         inc a           ;a=direction +1 of X 
         cp 8
         jr nc, V_BsSt
         jr V_BsCanOk1
         
V_BsNegX:
         ld a,0          ;a=direction -1 of X
         cp d
         jr c, V_BsCanSubtrX
         jr V_BsSt
V_BsCanSubtrX:         
         ld a,d
         dec a 
V_BsCanOk1:
         ld d,a         
         inc ix
         
         ld a, (ix)     ;given directions for 1..8  -1,+1
         inc ix
         cp '-'
         jr z, V_BsNegY
         ld a,e
         inc a          ;a=direction +1 of Y  
         cp 8
         jr nc, V_BsSt
         jr V_BsCanOk2   
V_BsNegY:
         ld a,0         ;a=direction -1 of Y
         cp e
         jr c, V_BsCanSubtrY
         jr V_BsSt
V_BsCanSubtrY:         
         ld a, e
         dec a
V_BsCanOk2:
         ld e,a
         inc ix         
         
         call StoreSqToMove    ;new SQt,hl        
         
         ld a, (SQt)
         ld b,a
         ld a, (SQ_i)
         cp b
         jr z,V_cont_f        ; if piece is on that square then will be empty
         ld a, (SQ_t)
         cp b
         jr z,V_IsThisK       ; if piece goes to that square
                              ; then if piece is not king then
                              ; our king is protected                             
         call IsEmpty
         jr c,V_cont       
         
         ld a, (SIDE)
         cp 1
         jr z,V_BsThisBlack
         
V_BsThisWhite:         
         call IsBlack           ; enemy?
         jr nc,V_NoBlack
         
         cp 'k'
         jr nz, V_NoBK
         
V_Sq1:   ld a, (SQ1)            ; nearby square
         cp 1
         jr nz,V_BsSt
         jr V_dont              ; king or pawn beats

V_cont_f:   jp V_cont
         
V_IsThisK:                      ; king can not protect self
         ld a,(SQk)
         ld b,a
         ld a,(SQ_i)
         cp b
         jr z, V_cont           ; our king goes to this direction, continue
                                ; stop, our piece protects anyway

V_BsSt:  ld a,1                 ; stop to this direction, our piece protects king
         jp V_BsOver
          
V_NoBK:    
         cp 'b'
         jr z, V_dont           ; bishop beats
         cp 'q'
         jr z, V_dont           ; queen beats
         cp 'p'
         jr nz, V_BsSt          ; otherwise knight and cant beat
         
         ld a,(SQk)
         ld b,a
         ld a,(SQ_i)
         cp b
         jr z, V_KuBp           ; if our king goes under pawn attack?
         
         ld a,(ENPSQ)
         ld b,a
         ld a,(SQ_t)
         cp b
         jr z,V_CapBep
         jr V_KuBp
         
V_CapBep:                       ; do we capture en-passant?
         ld a,(SQt)
         add a,8
         cp b
         jr nz,V_KuBp
         ld a,(CaptPiece)       ; we are capturing
         cp 0
         jr nz,V_BsSt           ; this is the pawn that did check+       

V_KuBp:         
         ld a,(SQi)             ; pawn direction verif. black pawn is above
         ld b,a
         ld a,(SQt)           
         cp b
         jr nc,V_Sq1
         jr V_BsSt

V_dont:  ld a,2                 ; stop and not valid move, king in check+
         jr V_BsOver 
V_NoBlack:
         call IsWhite           ; our?
         jr c,V_BsSt
         jr V_cont
V_NoWhite:
         call IsBlack           ; our?
         jr c,V_BsSt
         jr V_cont
V_cont:         
         ld a,0                 ; continue after to this direction
         jr V_BsOver
                 
V_BsThisBlack:         
         call IsWhite           ; enemy?
         jr nc, V_NoWhite

         cp 'K'
         jr nz, V_NoWK
         jr V_Sq1
V_NoWK:    
         cp 'B'
         jr z, V_dont           ; bishop beats
         cp 'Q'
         jr z, V_dont           ; queen beats
         cp 'P'
         jr nz, V_BsSt          ; otherwise knight and cant beat
         
         ld a,(SQk)
         ld b,a
         ld a,(SQ_i)
         cp b
         jr z, V_KuWp           ; if our king goes under pawn attack?
         
         ld a,(ENPSQ)
         ld b,a
         ld a,(SQ_t)
         cp b
         jr z,V_CapWep
         jr V_KuWp
         
V_CapWep:                       ; do we capture en-passant?
         ld a,(SQt)
         sub 8
         cp b
         jr nz,V_KuWp
         ld a,(CaptPiece)       ; we are capturing
         cp 0
         jr nz,V_BsSt_f         ; this is the pawn that did check+       

V_KuWp:
         
         ld a,(SQi)             ; pawn direction verif. white pawn is below
         ld b,a
         ld a,(SQt)           
         cp b
         jr c,V_Sq1_f
         jr V_BsSt_f
       
V_BsOver:

         push af
         ld a,0
         ld (SQ1),a      ;next square will be not nearby in that direction anyway
         pop af
         
         pop ix
         pop hl
         pop bc
         ret
                
V_Sq1_f:    jp V_Sq1
V_BsSt_f:   jp V_BsSt

; look for horizontal-vertical-attacks to that direction
HorzVertAttacks:

         push bc
         push hl
         push ix
                  
         ld a, (ix)     ;given directions for a..h  -1,+1,=0
         inc ix
         cp '='
         jr z, V_RkZero1
         cp '-'
         jr z, V_RkNegX      
         ld a,d
         inc a           ;a=direction +1 of X 
         cp 8
         jr nc, V_RkSt
         jr V_RkCanOk1  
V_RkNegX:
         ld a,0          ;a=direction -1 of X
         cp d
         jr c, V_RkCanSubtrX
         jr V_RkSt
V_RkCanSubtrX:         
         ld a,d
         dec a 
V_RkCanOk1:
         ld d,a         
V_RkZero1:
         inc ix 
         ld a, (ix)     ;given directions for 1..8  -1,+1,=0
         inc ix
         cp '='
         jr z, V_RkZero2        
         cp '-'
         jr z, V_RkNegY
         ld a,e
         inc a          ;a=direction +1 of Y  
         cp 8
         jr nc, V_RkSt
         jr V_RkCanOk2   
V_RkNegY:
         ld a,0         ;a=direction -1 of Y
         cp e
         jr c, V_RkCanSubtrY
         jr V_RkSt
V_RkCanSubtrY:         
         ld a, e
         dec a
V_RkCanOk2:
         ld e,a
V_RkZero2:
         inc ix         
         
         call StoreSqToMove    ;new SQt,hl
                
         ld a, (SQt)
         ld b,a
         ld a, (SQ_i)
         cp b
         jr z,V_Rkcont           ; if piece is on that square then will be empty
         ld a, (SQ_t)
         cp b
         jr z,V_RkIsThisK       ; if piece goes to that square
                                ; then if piece is not king then
                                ; our king is protected
         call IsEmpty
         jr c,V_Rkcont
         
         ld a, (SIDE)
         cp 1
         jr z,V_RkThisBlack
         
V_RkThisWhite:         
         call IsBlack           ; enemy?
         jr nc,V_RkNoBlack
         
         cp 'k'
         jr nz, V_RkNoBK
         
V_RkSq1: ld a, (SQ1)            ; nearby square
         cp 1
         jr nz,V_RkSt
         ;otherwise king beats our king
         
V_Rkdont:
         ld a,2                 ; stop and not valid move, king in check+
         jr V_RkOver
V_Rkcont:         
         ld a,0                 ; continue after to this direction
         jr V_RkOver
         
V_RkSt:  ld a,1                 ; stop to this direction, our piece protects king
         jr V_RkOver
                   
V_RkIsThisK:                    ; king can not protect itself
         ld a,(SQk)
         ld b,a
         ld a,(SQ_i)
         cp b
         jr z, V_Rkcont         ; our king goes to this direction, continue
                                ; stop, our piece protects anyway
         jr V_RkSt                       
          
V_RkNoBK:    
         cp 'r'
         jr z, V_Rkdont           ; rook beats
         cp 'q'
         jr z, V_Rkdont           ; queen beats
         jr V_RkSt                ; otherwise can not beat
V_RkNoBlack:
         call IsWhite           ; our?
         jr c,V_RkSt
         jr V_Rkcont
V_RkNoWhite:
         call IsBlack           ; our?
         jr c,V_RkSt
         jr V_Rkcont                 
V_RkThisBlack:         
         call IsWhite           ; enemy?
         jr nc,V_RkNoWhite

         cp 'K'
         jr nz, V_RkNoWK
         jr V_RkSq1
V_RkNoWK:    
         cp 'R'
         jr z, V_Rkdont           ; rook beats
         cp 'Q'
         jr z, V_Rkdont           ; queen beats
         jr V_RkSt                ; otherwise can not beat  

V_RkOver:
         push af
         ld a,0
         ld (SQ1),a      ;next square will be not nearby in that direction anyway
         pop af
         
         pop ix
         pop hl
         pop bc
         ret 

; look for knight-attack
KnightAttacks:
         push bc
         push de
         push hl
         push ix
                 
         ld a, (ix)     ;given directions for a..h  -1,+1,-2,+2
         inc ix
         cp '-'
         jr z, V_KnNegX
         ld a, (ix)     ; a=direction +1 or +2 of X      
         add a,d
         cp 8
         jr nc, V_KnNo
         jr V_KnCanOk1  
V_KnNegX:
         ld a, (ix)     ; a=direction -1 or -2 of X
         dec a
         cp d
         jr c, V_KnCanSubtrX
         jr V_KnNo
V_KnCanSubtrX:         
         ld a, d
         sub (ix)    
V_KnCanOk1:
         ld d,a         
         inc ix
         
         ld a, (ix)     ;given directions for 1..8  -1,+1,-2,+2
         inc ix
         cp '-'
         jr z, V_KnNegY
         ld a, (ix)     ; a=direction +1 or +2 of Y  
         add a,e
         cp 8
         jr nc, V_KnNo
         jr V_KnCanOk2   
V_KnNegY:
         ld a, (ix)     ; a=direction -1 or -2 of Y
         dec a 
         cp e
         jr c, V_KnCanSubtrY
         jr V_KnNo
V_KnCanSubtrY:         
         ld a, e
         sub (ix)
V_KnCanOk2:
         ld e, a
         inc ix         
         
         call StoreSqToMove    ;new SQt,hl
                
         call IsEmpty
         jr c,V_KnNo
         
         ld a, (SQt)
         ld b,a
         ld a, (SQ_t)
         cp b
         jr nz, V_KnIsKn
         jr V_KnNo              ; we beat this piece

V_KnIsKn:         
         ld a, (SIDE)
         cp 1
         jr z, V_KnThisKingBlack
V_KnThisKingWhite:         
         ld a,(hl)
         cp 'n'                 ; knight?
         jr z,V_Kndont
         jr V_KnNo              ; otherwise our
V_KnThisKingBlack:         
         ld a,(hl)
         cp 'N'                 ; knight?
         jr z,V_Kndont
         jr V_KnNo              ; otherwise our    
V_Kndont:
         ld a,2                 ; not valid move, king in check+
         jr V_KnOver
V_KnNo:         
         ld a,0                 ; not attacked
V_KnOver:
         pop ix
         pop hl
         pop de
         pop bc
         ret 
;END


;BEGIN of ========== MAKE MOVE
;
; Make move on BOARD
; SQi - from square
; SQt - to square
; PromoPcId - id of piece to promote = 4-queen,3-rook,2-bishop,1-knight, 0-no promotion
;

MakeMove:

         push af
         push bc
         push de
         push hl
         push ix
         push iy
         
         ld ix, NEXT_MVCNT
         ld b,(ix)
         ld ix, NEXT_MOVES
         ld a, b
         cp 0
         jr nz, MKMthereAre

         jr fMKMEnd
         
MKMthereAre: 
         ld e,0       ; flag that found

MKMToLoop:
         push bc
         push ix

         ld a, (ix)   ; notated piece
         ld d,a
         inc ix

         ld a, (SQi)
         ld b,a
         ld a, (ix)   ; from square
         cp b
         jr nz, MKMnotMV
         
         inc ix            
         ld a, (SQt)
         ld c,a
         ld a, (ix)   ; to square
         cp c
         jr nz, MKMnotMV
         
         call MKifPromoTheSame ; verifies if promoted piece the same in list
         cp 1                  ; actually we can ignore it, because undo just places
         jr nz, MKMnotMV      ; back a pawn
         
         ; ok, this move. Let's do it
         
         call MKMcastlesDO   ; saves castling flags and verifies rooks
                                     
         call MKMsaveEPSQ
         
         dec ix         
         ld a, (ix)   ; from square
         call MM_hl
         ld a,(hl)
         ld d,a       ; save piece
         
         call UpdateKingSQ   ; and castling flags too
                  
         call MM_0    ; empty square now
         
         inc ix
         ld a, (ix)   ; to square
         call MM_hl
         
         push de
         ld d,(hl)    ; save captured piece
         
         ld a,(hl)
         cp 0
         jr z, MM_noCapture  ;en-passant is not in list
         
         call MM_capt2list   ;should save last captured piece in list   
         
MM_noCapture:         
         
         pop de       ; restore our piece
         
         ld a,d
         ld (hl),a    ; just place our piece here
                  
         inc ix
         ld a, (ix)     ; flags
         bit 4,a        ; castling
         jr z,MMNoCastle
         
         call  MMcastles          
                            
MMNoCastle:
         ld a, (ix)     ; flags    
         bit 1,a        ; possibly promotion
         jr z, MMNoProm
         
         call MMPromotion ; put new piece on board         
MMNoProm:
         ld a, (ix)     ; flags
         bit 5,a        ; en-passant 
         jr z,MMNoEP

         call MMEnPass  ; en-passant capture
         jr MMNoEP                                          

fMKMEnd: jr MKMEnd

fMKMToLoop: jr MKMToLoop      ; jump far                                            
                                                                                                                                       
MMNoEP:                                         
         ld e,1         ; we found already
                 
         call Swap_Sides   ; Swap
         call IsCheck
         cp 0
         jr z, MKMnoCheck
         
         ld a, (ix)     ; flags
         set 6,a        ; bit 6 - check
         ld (ix),a      ; save to flags
MKMnoCheck:         
         
         call MKMsave2Hist ; Save history
MKMnotMV:
         pop ix
         
         inc ix        ; piece
         inc ix        ; from
         inc ix        ; to
         inc ix        ; flags

         pop bc
         
         ld a,e
         cp 1
         jr z, MKMEnd
         djnz fMKMToLoop

MKMEnd:

         ld a,e
         cp 0
         jr nz, MKMfoundmove
MKMfoundmove:
         pop iy                                                                                                   
         pop ix
         pop hl
         pop de
         pop bc
         pop af
         ret
         
MM_hl:                        ; reposition of hl to square provided in a-reg
         push bc
         ld b,0
         ld c,a
         ld hl, BOARD
         add hl,bc            ; points to piece to move
         pop bc
         ret

MM_0:    ld a,0               ; empty square now 
         ld (hl),a 
         ret

; verifies promoted piece
MKifPromoTheSame:
         push bc
         ld a, (PromoPcId)
         cp 0
         jr z, MKsamePromo
         dec a
         ld b,a
         inc ix
         ld a, (ix)       ; flags
         ld c,a
         dec ix
         bit 1,a
         jr z, MKsamePromo
         ld a,c
         srl a
         srl a
         and 3
         cp b
         jr z, MKsamePromo
         ld a,0
         jr MKsamePover                           
MKsamePromo:
         ld a,1
MKsamePover:
         pop bc
         ret

; works on castling flags          
MKMcastlesDO:
         push af
         ld a,(CASTLES)
         ld (Cstl2hist),a   ; save for history
         cp 0
         jr z,MKMNoRvf
         call MKMUpdateRookMvCapt
MKMNoRvf:
         pop af         
         ret

; Saves move into history of moves made
MKMsave2Hist:
         push hl
         push bc
         ld b,0
         ld a,(MOVE_CNT)
         ld c,a
         inc a
         ld (MOVE_CNT),a
         ld hl, MOVE_HIST

         and %11000000
         jr nz,slowBcAdd1
         sla c
         sla c      ; 4*c
          ;1.byte = from sq
          ;2.byte = to sq
          ;3.byte = flags and promoted to
          ;4.byte = castling flags, en-passant square before
         add hl,bc
         jr overBcAdd1         
slowBcAdd1:         
         add hl,bc
         add hl,bc
         add hl,bc
         add hl,bc
overBcAdd1:         
         
                  
         ld a,(SQi)
         ld (hl),a  ; from
         inc hl
         ld a,(SQt) ; to
         ld (hl),a
         inc hl
         ld a, (ix) ; flags with piece promoted
         ld (hl),a  ;
         inc hl
         ld a,(EnPs2hist)
         cp $ff
         jr z,EnP0hs
         cp 32
         jr nc,EnPBlackHs
         sub 15    ;a3=1...h3=8
         jr EnPSvhs 
EnPBlackHs:
         sub 31    ;a6=9...h6=16
         jr EnPSvhs
EnP0hs:  ld a,0
EnPSvhs: ld b,a
         ld a,(Cstl2hist)
         sla a           ; 4 shifts
         sla a
         sla a
         sla a
         or b
         ld (hl),a  ; save en-passant square and castling info that was before
         pop bc
         pop hl
         ret

; Just swap both sides
Swap_Sides:
         push af
         ld a,(SIDE)       ; swap sides
         cp 0
         jr z,SwSdBl
         ld a,0
         jr SwSdSt
SwSdBl:  ld a,1
SwSdSt:  ld (SIDE),a
         pop af
         ret
         
; save king square         
UpdateKingSQ:
         ld a,d
         cp 'K'
         jr nz,MKMnoWK1
         ld a,c
         ld (WKSQ),a
         cp 4
         jr z,KingStillE1
         push af
         ld a,(CASTLES)
         and %11111100
         ld (CASTLES),a
         pop af         
         jr MKMkingOut         
KingStillE1:         
         jr MKMkingOut         
MKMnoWK1:          
         cp 'k'
         jr nz,MKMkingOut
         ld a,c
         ld (BKSQ),a
         cp 60
         jr z,KingStillE8
         push af
         ld a,(CASTLES)
         and %11110011
         ld (CASTLES),a         
         pop af
KingStillE8:         
         
MKMkingOut:
         ret

; if rook has moved or has been captured, clear and save castling flags
MKMUpdateRookMvCapt:
         push de
         ld a,(CASTLES)
         ld d,a
         ld a,b        ;from
         call MKMiWKK
         call MKMiWKQ
         call MKMiBKK
         call MKMiBKQ
         ld a,c        ;to
         call MKMiWKK
         call MKMiWKQ
         call MKMiBKK
         call MKMiBKQ
         ld a,d
         ld (CASTLES),a
         pop de
         ret
         
MKMiWKK: cp 7          ; is H1?
         jr nz,MKMoWKK
         res 0,d       
MKMoWKK: ret
MKMiWKQ: cp 0          ; is A1?
         jr nz,MKMoWKQ
         res 1,d       
MKMoWKQ: ret
MKMiBKK: cp 63         ; is H8?
         jr nz,MKMoBKK
         res 2,d       
MKMoBKK: ret
MKMiBKQ: cp 56         ; is A8?
         jr nz,MKMoBKQ
         res 3,d       
MKMoBKQ: ret

          

                  
; save new en-passant square
MKMsaveEPSQ:
         ld a,(ENPSQ)
         ld (EnPs2hist),a   ; save for history
         ld a,$ff
         ld (ENPSQ),a
         ld a,d
         cp 'P'
         jr nz, MKMnoeps
         ld a,c
         sub 16           ; are there 16 squares inbetween
         cp b
         jr z, MKMwEp     ; double move of white pawn
         ; try for black pawn
         ld a,c
         add a,16
         cp b
         jr z, MKMbEp      ; double move of black pawn 
         jr MKMnoeps
MKMwEp:  add a,8
         jr MKMsaveNewEp
MKMbEp:  sub 8
MKMsaveNewEp:
         ld (ENPSQ),a
MKMnoeps:
         ret                                    

EnPs2hist: .byte 0        ; to save en-passant square for history later

Cstl2hist: .byte 0        ; to save castling flags for history later
                                                                
; saves captured piece in list         
MM_capt2list:                      
         push hl
         ld hl, UnMakeList
MKMloop1:         
         ld a,(hl)
         cp ' '
         jr z,MKMblankL1
         cp 0
         jr z,MKMblankL1
         inc hl
         jr MKMloop1
MKMblankL1:
         ld a,d
         ld (hl),a       ; should save for next UnMakeMove      
         pop hl
         ret
         
; castling moves, should move rook too                 
MMcastles:
         push hl
         ld a,c
         cp 6         ; g1?
         jr nz,MMNoG1
         ld a, 7      ; rook on h1
         call MM_hl
         call MM_0    ; empty square now
         ld a, 5      ; f1
         call MM_hl
         ld a,'R'
         ld (hl),a
         jr MMoverC         
MMNoG1:
         cp 2         ; c1?
         jr nz,MMNoC1
         ld a, 0      ; rook on a1
         call MM_hl
         call MM_0    ; empty square now
         ld a, 3      ; d1
         call MM_hl
         ld a,'R'
         ld (hl),a
         jr MMoverC
MMNoC1:
         cp 62        ; g8?
         jr nz,MMNoG8
         ld a, 63     ; rook on h8
         call MM_hl
         call MM_0    ; empty square now
         ld a, 61     ; f8
         call MM_hl
         ld a,'r'
         ld (hl),a
         jr MMoverC
MMNoG8:
         cp 58        ; c8?
         jr nz,MMoverC
         ld a, 56     ; rook on a8
         call MM_hl
         call MM_0    ; empty square now
         ld a, 59     ; d8
         call MM_hl
         ld a,'r'
         ld (hl),a
         jr MMoverC
MMoverC: pop hl
         ret

; promoted pieces
MMPromotion:
         ; which piece?
         ld a, (PromoPcId)
         cp 4          ; Queen?
         jr nz, MMNoQ
         ld a, 'Q'
         jr MMPcPut        
MMNoQ:   cp 3          ; Rook?
         jr nz, MMNoR
         ld a, 'R'
         jr MMPcPut        
MMNoR:   cp 2          ; Bishop?
         jr nz, MMNoB
         ld a, 'B'
         jr MMPcPut
MMNoB:   cp 1          ; Knight?  
         jr nz, MMNoN
         ld a, 'N'
         jr MMPcPut
MMNoN:   ld a, 'Q'     ; actually, should provide 3

MMPcPut:
         ld c,a        ; save
         ld a,d
         cp 'P'
         jr z,MMPcWProm
         ld a,32
         add a,c
         ld c,a        ; to lowercase
MMPcWProm:
         ld (hl),c
         ret
         
; en-passant capture         
MMEnPass:
         push hl
         ld a,d
         cp 'P'
         jr nz,MMPcBEnp         
         ld a,(SQt)
         sub 8
         jr MMEnp1
MMPcBEnp:
         ld a,(SQt)
         add a,8
MMEnp1:  call MM_hl
         call MM_0    ; empty square now
         pop hl
         ret
;END

;BEGIN of ========== UNMAKE MOVE
;
; UnMake move on BOARD (undo)
; uses own history recordings in MOVE_HIST
; which sets
;  SQi - from square
;  SQt - to square
;   +also uses flags from history

             ; we keep list of captured pieces to unmake later as stack
UnMakeList:
         .byte "                                                                  "
         .byte "                                                                  "
         .byte "                                                                  "
         .byte "                                                                  "
flag_ix:       .byte 0      ; temporary flag of move made
        
UnMakeMove:
         push af
         push bc
         push de
         push hl
         push ix
         push iy
         
         call UnMKrestoreFromHist       ; Restores from history
               
         ld a, (SQi)   ; from square
         ld b,a
                   
         ld a, (SQt)   ; to square
         ld c,a
                 
         ; Let's undo it
         call Swap_Sides   ; Swap sides
         
         call MM_hl
         ld d,(hl)
         call MM_0      ; empty square now
                  
         ld a, (flag_ix)     ; flags
         bit 0,a
         jr z,UnMKnoCapture
   
         ld a, (flag_ix)     ; flags
         bit 5,a             ; ignore en-passant case
         jr nz,UnMKnoCapture
 
         call UnMKCapturedPiece

UnMKnoCapture:
                  
         ld a,(SQi)
         call MM_hl
         ld a,d
         ld (hl),a       ; just place our piece back

         push bc
         ld c,b          ; square-from
         call UpdateKingSQ
         pop bc
         
         ld a, (flag_ix)      ; flags
         bit 4,a              ; castling      
         jr z,UnMKNoCastle
         
         call  UnMKcastles          
                                      
UnMKNoCastle:
         ld a, (flag_ix)     ; flags    
         bit 1,a             ; possibly promotion
         jr z, UnMKNoProm
         
         call UnMKPromotion  ; put pawn back on board         
UnMKNoProm:
         ld a, (flag_ix)     ; flags
         bit 5,a             ; en-passant         
         jr z,UnMKNoEP
         
         call UnMKEnPass  ; en-passant capture
UnMKNoEP:                                     
         ; en-passant square is updated before

UnMKEnd:

         pop iy                           
         pop ix
         pop hl
         pop de
         pop bc
         pop af
         ret
         
; Get SQi,SQt, promoted piece from history
UnMKrestoreFromHist:
         push hl
         push bc
         ld b,0
         ld a,(MOVE_CNT)
         dec a
         ld c,a
         ld (MOVE_CNT),a
         ld hl, MOVE_HIST
         
         and %11000000
         jr nz,slowBcAdd2
         sla c
         sla c      ; 4*c
          ;1.byte = from sq
          ;2.byte = to sq
          ;3.byte = flags and promoted to
          ;4.byte = en-passant square and castling info before
         add hl,bc
         jr overBcAdd2         
slowBcAdd2:         
         add hl,bc
         add hl,bc
         add hl,bc
         add hl,bc
overBcAdd2:         

         ld a,(hl)
         ld (SQi),a
         inc hl
         ld a,(hl)
         ld (SQt),a
         inc hl
         ld a,(hl)
         ld (flag_ix),a
         inc hl
         ld a,(hl)
         ld b,a
         and %00001111
         cp 0
         jr z,unmkEp0
         cp 9
         jr nc,unmkBlackEp
         add a,15            ; a3=16...
         jr unmkEpS
unmkBlackEp:         
         add a,31            ; a6=40...
         jr unmkEpS         
unmkEp0: ld a,$ff
unmkEpS: ld (ENPSQ),a      ; restore ENPSQ
         ld a,b
         srl a
         srl a
         srl a    ; 4 shifts
         srl a
         ld (CASTLES),a    ; restore castling info    
         pop bc
         pop hl
         ret

; Get captured piece from list and put back
UnMKCapturedPiece:                      

         push de
         ld de, UnMakeList
UnMKloop1:        
         ld a,(de)
         cp ' '
         jr z,UnMKblankL1
         cp 0
         jr z,UnMKblankL1
         inc de
         jr UnMKloop1
UnMKblankL1:
         dec de
         ld a,(de)       ; should be a previous MakeMove
         push af
         ld a,' '
         ld (de),a       ; clear this
         pop af
         pop de
         ld (hl),a       ; place back captured piece   
         ret     
                                                      
; castling moves, should move rook back too                 
UnMKcastles:
         push hl
         ld a,c
         cp 6         ; g1?
         jr nz,UnMKNoG1
         ld a, 5      ; rook on f1
         call MM_hl
         call MM_0    ; empty square now
         ld a, 7      ; h1
         call MM_hl
         ld a,'R'
         ld (hl),a
         jr UnMKoverC         
UnMKNoG1:
         cp 2         ; c1?
         jr nz,UnMKNoC1
         ld a, 3      ; rook on d1
         call MM_hl
         call MM_0    ; empty square now
         ld a, 0      ; a1
         call MM_hl
         ld a,'R'
         ld (hl),a
         jr UnMKoverC
UnMKNoC1:
         cp 62        ; g8?
         jr nz,UnMKNoG8
         ld a, 61     ; rook on f8
         call MM_hl
         call MM_0    ; empty square now
         ld a, 63     ; h8
         call MM_hl
         ld a,'r'
         ld (hl),a
         jr UnMKoverC
UnMKNoG8:
         cp 58        ; c8?
         jr nz,UnMKoverC
         ld a, 59     ; rook on d8
         call MM_hl
         call MM_0    ; empty square now
         ld a, 56     ; a8
         call MM_hl
         ld a,'r'
         ld (hl),a
         jr UnMKoverC
UnMKoverC:
         pop hl
         ret

; promoted pieces
UnMKPromotion:
         push hl
         ld a,7
         cp c
         jr c,UnMKwhitePw
         ld a,'p'
         jr UnMKPw
UnMKwhitePw:
         ld a,'P'
UnMKPw:  ld d,a
         ld a,(SQi)
         call MM_hl         
         ld (hl),d
         pop hl
         ret
          
; en-passant capture         
UnMKEnPass:
         push hl
         ld a,d
         cp 'P'
         jr nz,UnMKPcBEnp         
         ld a,(SQt)
         sub 8
         call MM_hl
         ld (hl),'p'
         jr UnMKEnp1
UnMKPcBEnp:
         ld a,(SQt)
         add a,8
         call MM_hl
         ld (hl),'P'
UnMKEnp1:
         pop hl
         ret
                 
;END
      

;BEGIN of ========== KING CHECKMATE DETECTION
;;
; It verifies check+ moves of the list for possible movement.
; Makes it slower, anyway correct for complete notation.
; Sets bit-7 for checkmate moves.
;
LookForCheckmates:

         ld a, (MateDetect)
         cp 0
         jr nz, CKMEnd
                 
         ld ix, NEXT_MVCNT
         ld b,(ix)
         ld ix, NEXT_MOVES
         ld a, b
         cp 0
         jr z,CKMEnd
         
CKMToLoop:
         inc ix        ; piece
         inc ix        ; from
         inc ix        ; to
         ld a,(ix)     ; flags
         bit 6,a
         jr z, CKMnoCheck
         dec ix
         dec ix
         ld a,(ix)
         ld (SQi),a
         inc ix
         ld a,(ix)
         ld (SQt),a
         inc ix
         ld a,(ix)     ; flags
         bit 1,a       
         jr z, CKMnoPromo
         ld a,(ix)     ; flags again       
         srl a
         srl a
         and 3
         inc a
         jr CKMpcProm
CKMnoPromo:
         ld a,0
CKMpcProm:         
         ld (PromoPcId),a
         call MakeMove  ; make this move again
         
         ld a,1
         ld (MateDetect),a            
         call GenMoves  ; finds any legal move
         
         call UnMakeMove  ; unmake
         
         ld a,(MateDetect)
         cp 2           ; is there any?
         jr z, CKMnoMate
         
         ld a,(ix)     ; flags again
         set 7,a       ; bit 7 - checkmate
         ld (ix),a     ; save flag   
         
CKMnoMate:
         ld a,0
         ld (MateDetect),a
                                                                                                                
CKMnoCheck:
         inc ix
         djnz CKMToLoop

CKMEnd:                           
         ret
         
;END 



                   
;BEGIN of ========== COUNTER for large loop of 65536 maximum value
;
; just imitates 2byte integer in 8-bit registers

Il:      .byte 0
Ih:      .byte 0


LD_Ix:   ld (Ih),a  ; sets a-reg x 256 value to counter
         ld a,0
         ld (Il),a
         ret

DEC_I:   push bc
         ld a,(Il)
         ld c,a
         ld a,(Ih)
         ld b,a
         dec bc
         ld a,c
         ld (Il),a
         ld a,b
         ld (Ih),a
         ld a,0
         cp b
         jr nz, IsNo0_I
         cp c
         jr nz, IsNo0_I
         scf       ; sets CF on empty counter
         jr Is0ret
IsNo0_I: scf
         ccf
Is0ret:  pop bc
         ret
;END

;----- Safe MoveGen++
BOsave:
    ; save board before heavy movements
        ld a,(BO_sv_0)
        inc a
        ld (BO_sv_0),a
        dec a
        jr nz,BoSvOk
        ld hl,BOARD             ; save board
        ld de,BO_savedata
        ld bc,64+6
        ldir
BoSvOk:
        ret
;-----
BOrest:        
        ld a,(BO_sv_0)
        dec a
        ld (BO_sv_0),a
        jr nz,BoReOk            ; if top
        ld de,BOARD             ; restore board
        ld hl,BO_savedata
        ld bc,64+6
        ldir
BoReOk:
        ret
         
BO_sv_0:
        .byte 0     ; 0 if top
BO_savedata:
        .ds 64+6    ; to save BOARD,SIDE,CASTLES,ENPSQ,WKSQ,BKSQ

;BEGIN of ========== DEBUG tool

; Put call DEBUGs in ASM to see that I got here
         
debugstr: .byte "DEBUG!$", 0  ; outputs this when reaches

DEBUGs:

         push de
         ld de, debugstr
         call PrintString
         pop de
         ret            

        .byte "DEBUGZ",0    ; put in code to find via debugger
;END

      
PrintString:
    ret