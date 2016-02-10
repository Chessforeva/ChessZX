;-----------------------------------
; The loop of keyboard when
; chess board is on screen
;

ChessBoardGUI:
    
    push af
    push bc    
                    ; clear cursors, except
                    ; visible green with frame DrwC1
    ld a,$ff
    ld (DrwCB),a    ; blinking
    ld (DrwC0),a    ; pink
    ld (DrwC2),a    ; yellow

    di
    call dbgBOARD_s
    call GenMoves       ; generate chess moves now
    call dbgBOARD_v
    ei
    
    xor a
    ld (KeyNflg),a      ; clear N flag
    ld (MvDepth),a      ; depth
    ld (PKLast),a       ; clear last key
    
    call DrawBoard
    call DrawMx
    
    ld a,(PKNow)
    cp 'O'
    jr z,KeyChSt
    cp 'P'
    jr z,KeyChSt

KeyChBoLoop:
    call Slw_pause
        
    ; obtain key pressed
    call GetJoyOrKey

KeyChSt:
    ld b,a
    call Random0                ; randomize counters
  
    ld a,b   
    or a
    jr nz,KeyNot0
    jr KeyChBoLoop
    
KeyNot0:
    cp 'V'
    jr nz, KeyNotV
    ld a,'F'                ; V is the same F
KeyNotV:    
    cp 'F'
    jr nz, KeyNoF
    ld a,(PKLast)
    cp 'F'
    jr nz,KeyFl2
    jr KeyChBoLoop
KeyFl2:    
    ld b,a
    ld a,$ff
    ld (PKLast),a
    ld a,1
    ld (KeyFflg),a
    ld a,b
    
KeyNoF:

    ;; debug key code
    ;call dbgA
    
    ld (PKNow),a
    call KeysSubstRedefined

    call KeySubstitutes

    ld (PKNow),a
    
    call Random1                ; randomize counters
    
        ; verify for double-press
    ld a,(fTwice)
    or a
    jr z,PKAnoSlow
    
    jr KeyNoLast
    
PKAnoSlow:
    ld a,(PKLast)
    sub b                       ; b=code
    jr nz,KeyNoLast
    
    xor a           ; if the same then clear
    jp KeyLpOk
KeyNoLast:

    ld a,(DrwC1)
    ld c,a          ; c=cursor square
    
    ld a,b          ; b=key
    
    ; use WASD or joystick keys
    cp 'W'
    jr nz,KeyWov
    
    ccf
    ld a,c
    sub 56
    jr nc, KeyLpOk
    ld a,c
    
    call ClearC1
    ld a,c
    add 8               ; Up V=V+1
    ld (DrwC1),a
    call DrawSquare
    jr KeyLSv
    
KeyWov:

    cp 'D'
    jr nz,KeyDov
    
    ccf
    ld a,c
    sub 8
    jr c, KeyLpOk
    ld a,c
    
    call ClearC1    
    ld a,c
    sub 8               ; Down V=V-1
    ld (DrwC1),a
    call DrawSquare

KeyLSv:
        ; saves last key press and loops next
    call KeySaveLast
    jr KeyLpOk
    
KeyDov:

    cp 'A'
    jr nz,KeyAov
    
    ld a,c
    and 7
    jr z, KeyLpOk
    
    call ClearC1    
    ld a,c
    dec a               ; Left H=H-1
    ld (DrwC1),a
    call DrawSquare
    jr KeyLSv
    
KeyAov:

    cp 'S'
    jr nz,KeySov
    
    ld a,c
    and 7
    cp 7
    jr z, KeyLpOk
    
    call ClearC1    
    ld a,c
    inc a               ; Right H=H+1
    ld (DrwC1),a
    call DrawSquare
    jr KeyLSv   
    
KeySov:
    cp 'N'              ; on "N" key
    jr nz,KeyNov
    call KeySaveLast
    jr KeyLpExit        ; exit and find other puzzle

KeyNov:
    cp '1'              ; on "1" key
    jr nz,Key1ov
    call DebugMovesMemory   ; display memory data to debug
    call DrawBoard
    jr KeyLSv
    
KeyLpOk:                ; Loop up

    ld a,(KeyNflg)      ; is N flag
    or a
    jr z,KeyLpOk2
    jr KeyLpExit        ; exit and find other puzzle
    
KeyLpOk2    
    jp KeyChBoLoop    
    
Key1ov:

    cp 13              ; on Enter
    jr nz,Key13ov
    call KeyEnterPress
    jr KeyLpOk
Key13ov:
    cp 32              ; on Space, the same as on Enter
    jr nz,Key32ov
    call KeyEnterPress
    jr KeyLpOk
Key32ov:

    cp 'O'
    jr nz, KeyNotO
    call KeySaveLast
    call KeysRedefine   ; user can redefine keys too
    
KeyNotO:
    cp 'P'
    jr nz, KeyNotP  
    call KeySlowCase        ; Verification ON/OFF
    jr KeyLpOk
    
KeyNotP:
    cp 'T'
    jr nz, KeyNotT  
    call KeyTwiceCase       ; Keypress twice ON/OFF
    jr KeyLpOk

KeyNotT:
    
    ; all keys scanned
    jr KeyLpOk
    
KeyLpExit:    
    pop bc
    pop af
    ret

 ; saves last key press 
KeySaveLast:
    push af
    ld a,(KeyFflg)
    or a
    jr nz,KeySvR
    ld a,(PKNow)
    jr KeySvok
KeySvR:
    xor a
    ld (KeyFflg),a      ; clear F-key flag
    ld a,'F'
KeySvok:        
    ld (PKLast),a
    pop af
    ret

;---
; substitute arrow keys, joystick
;
KeySubstitutes:
    ld a,b    
    cp 38
    jr nz, KeyNoUp
    ld a,'W'
KeyNoUp:
    cp 40
    jr nz, KeyNoDown
    ld a,'D'
KeyNoDown:
    cp 37
    jr nz, KeyNoLeft
    ld a,'A'
KeyNoLeft:
    cp 39
    jr nz, KeyNoRight
    ld a,'S'
KeyNoRight:
    cp 126
    jr nz, KeyNoFire
    ld a,13
KeyNoFire:    
    ld b,a
    ret

KeyNflg: .byte 0 ; flag, should go to next game
KeyFflg: .byte 0 ; flag, last key was "F"

    
KeyEnterPress:
    di
    push af
    push bc
    push ix
    push iy
    
        ; clear blinking
    ld a,$ff
    ld (DrwCB),a
    xor a
    ld (MvOppF),a       ; clear opp.move flag
    
    ld ix,MOVES_I
    call LocateIYbyDepth
    
MvLookLoop:
    
    ld a,(fSlow)
    or a
    jr z,MvReadData
    
    ld a,(iy)
    cp $ff
    jr z, MvEntEx
    
    call NoVerFindCkMate    ; on last move look for checkmate
    or a                    ; if A=0 then we found already
    jr z,MvNoVCkm
    call NoVerLoopForMv     ; finds move by number
MvNoVCkm:

    jr MvLookInList

MvReadData:
 
    ld a,(ix)
    or a
    jr z,MnMoDt     ; is the end of list?
    
    inc ix          ; skip depth
    inc ix          ; skip piece too
    
    ; Is the depth right?
    dec a
    ld b,a
    ld a,(MvDepth)
    sub b
    jr z,MvLookInList
        
    inc ix
    inc ix
    inc ix      ; skip this record
    jr MvLookLoop

    ; Look in the list for this square
MvLookInList:
    ld a,(ix)
    inc ix
    ld b,a
    ld a,(ix)
    inc ix          ; to flags
    ld c,a

    ld a,(DrwC1)
    cp b
    jr nz,MvNotDrag
    
    call ClearC0
    
    ld (DrwC0),a        ; drag this piece
    call DrawSquare
    jr MvEntEx
    
MvNotDrag:

    cp c                ; is move to?
    jr nz,MvNotMove
    ld a,(DrwC0)
    cp b                ; is drag ok?
    jr nz,MvNotMove

    call MvMAKEMOVE
    
    jr MvEntEx
        
MvNotMove:
    ld a,(fSlow)
    or a
    jr z,MvNotMvSkpIx
    
    jr MvEntEx
    
MvNotMvSkpIx:    
    inc ix
    jr MvLookLoop     ; loop to next move in data buffer

MnMoDt:             ; end of list

MvEntEx:
    pop iy
    pop ix
    pop bc
    pop af
    ei
    ret



;---
;
;  make move in our GUI
;
MvMAKEMOVE:

    ld a,$ff
    ld (DrwC0),a
    ld (DrwC2),a
    
    ld a,b
    ld (SQi),a
    ld a,c
    ld (SQt),a
    
    ld a,(ix)
    bit 1,a
    jr z, MvPrNoProm

    ; piece promoted too
    ; which piece?
    srl a
    srl a
    and 3         ; 0..3 piece Id
    inc a           ; 4=Q,3=R,2=B,1=N
    jr MvPrPromPc
MvPrNoProm:
    ld a,0          ; not promoted
MvPrPromPc:    
    ld (PromoPcId),a
    call MakeMove
        
        ; sets blinking
    ld a,(MvOppF)
    or a
    jr z,MvNoBlnk

    ld a,c
    ld (DrwCB),a    ; set blinking of moved piece
MvNoBlnk:

    call DrawBoard
    call DrawMx
    
    ; draw chess indicator
    ld b,' '
    ld a,(ix)
    inc ix
    bit 6,a         ; bit 6 - check
    jr z, MvNoCk
    ld b,'+'
MvNoCk: 
    bit 7,a         ; bit 7 - checkmate
    jr z, MvNoCkMt
    ld b,'#'
MvNoCkMt:
    ld a,31
    ld (A_c8),a
    ld a,0
    ld (A_r8),a
    
    ld a,(A_attr)
    and %01000111   ; on black
    ld (A_attr),a
        
    ld a,b
    call A_DrawChar
    
    call GenMoves       ; generate chess moves now
    
    ld a,(MvDepth)
    inc a
    ld (MvDepth),a       ; depth++
    ld c,a

    inc iy              ; skip to next    
    
    ld a,(fSlow)
    or a
    jr z,MvRdInIXbuff

    jr MvIfShouldmove
    
MvRdInIXbuff:

    ld a,(RdMvDp)
    ld b,a

    ld a,c
    cp b                ; if not reached checkmate depth
    jr nz,MvIfShouldmove

MvWsLast:    
    ; was the last move
    ld a,1
    ld (KeyNflg),a
    
    jr MvSkpOppMv
    
MvIfShouldmove:    
        ; if opponent (AI) should move then do it
    ld a,(SIDE)
    ld b,a
    ld a,(RdMoveTo)
    sub b
    jr z, MvSkpOppMv
    
    ld a,1
    ld (MvOppF),a
    
    ld a,(fSlow)
    or a
    jr z,MvSkp03
    
    ld a,(iy)
    cp $ff
    jr z,MvWsLast       ; if this was the last move
    
    call NoVerLoopForMv     ; finds move
    
    jr MvOppMv          ; process it
    
MvSkp03:    
    
MvLoopToOppMv:    
    ld a,(ix)
    inc ix              ; skip depth
    inc ix              ; skip piece
    
    or a
    jr z,MvSkpOppMv
    
    dec a
    cp c
    jr z,MvOppMv
    
    inc ix
    inc ix
    inc ix
    jr MvLoopToOppMv
     
MvOppMv:    
    ld a,(ix)           ; from
    inc ix
    ld b,a
    ld a,(ix)           ; to
    inc ix
    ld c,a
    jp MvMAKEMOVE       ; opponent moves too
    
MvSkpOppMv:

    ld a,$ff
    ld (DrwC0),a
    ld (DrwC2),a
    ;DrwCB stays blinking
    
    ret
    
MvOppF:
    .byte 0       ; flag indicates opponent
MvDepth:
    .byte 0       ; current depth

;----- Locate IY to current move number
LocateIYbyDepth:
    ld iy,MOVES_N   ; point to list in datas
    ld a,(MvDepth)
    or a
LcIYlp:
    jr z,LcIYex
    inc iy
    dec a
    jr LcIYlp
LcIYex:
    ret
    
;------------------------------
;
; It finds a move in the list of possible moves
;  A=index-number of move, IY position in data buffer.
; On return IX is pointed to move.
    
NoVerLoopForMv:
         ld a,(iy)           ; which move
NoVerLoopForA:        
         ld c,a         
         ld ix, NEXT_MVCNT
         ld b,(ix)
         ld ix, NEXT_MOVES
         ld a,b
         or a
         jr z,Lo0KmEnd
         sub c
         jr nc,Lo0KmSt
            ; if move impossible (N>count), then tell it
         call LoKmDispError
         ld a,c
         call dbgA
         jr Lo0KmEnd
Lo0KmSt:
         ld a,c
         or a
Lo0KmLoop:
         jr z,Lo0KmEnd
         inc ix         ; skip piece
         inc ix
         inc ix
         inc ix
         dec a          ; otherwise look for other move
         jr Lo0KmLoop
Lo0KmEnd:
         inc ix             ; skip piece                      
         ret

;------
; Find for checkmate for the last move
; Returns A=0 if this move can be checkmate we look for.
;
NoVerFindCkMate:
        
    push hl
    push de

    ld e,0              ; which move counter
    ld d,$ff
    
    ld a,(MvDepth)
    inc a
    ld b,a
    ld a,(RdMvDp)
    sub b
    jr nz,NoVCkmEx      ; for the last move only
         
    ld ix, NEXT_MVCNT
    ld b,(ix)
    ld ix, NEXT_MOVES
    ld a,b
    or a
    jr z,NoVCkmEx

NoVCkLoop:
    inc ix  ; skip piece
    ld h,(ix)
    inc ix
    ld l,(ix)
    inc ix
    ld a,(ix)
    inc ix
    bit 7,a         ; bit 7 - checkmate
    jr z,NoVCkNot
        ; if checkmate then compare
    
    ld a,(DrwC0)
    cp h
    jr nz,NoVCkNoThs
        ; if drag ok
    
    ld a,(DrwC1)
    cp l
    jr nz,NoVCkNoThs
        ; if to is cursor
    ld d,e
    jr NoVCkmEx     ; yes, this is a checkmate move

NoVCkNoThs:
    
    ld a,(DrwC1)
    cp h
    jr nz, NoVCkNot
    ld d,e          ; if cursor can drag, save

NoVCkNot:
    inc e
    djnz NoVCkLoop
    
NoVCkmEx:
    ld a,d
    pop de
    pop hl
    
    cp $ff
    jr z,NoVCkmE
        ; set position of IX to move
    call NoVerLoopForA
    ld a,0    
NoVCkmE:
    ret
         
; -------
;
; some other chess code
;
;
;; set default position
;    ld a,0              
;    call SetFEN
;    call GenMovesAndString
;    ld de,NEXT_MVSTR
;    call DrawString
;    call PressKey
;
;; to debug chess board, print only
;    call printBOARD
;    ld de,BoardOut
;    call DrawString
;    call PressKey
;


;
; Here we redefine joystick and arrows
; Emulators use substitutes for NumbKeys
;  as also Joystick II interface does.
;
KeysRedefine:
    push af
    push bc
    push de
    push ix
    
    ld a,7
    call ClearScreen
    
    ld a,0
    ld (A_c8),a
    ld a,3
    ld (A_r8),a
    ld a,%01000111
    ld (A_attr),a
    
    ld de,KeysRedefine_text
    call A_DrawString

    ld a,6
    ld (A_r8),a
    
    ld ix,KRedefs+4
    ld b,5
    ld c,'O'            ; redefine hotkey
KeyRdfLoop:
    call GetJoyOrKey
    or a
    jr z,KeyRdfLoop
    
    cp c
    jr z,KeyRdfLoop
    
    ld c,a
    ld (ix),a
    dec ix
        
        ; if key pressed
    ld de, dspCdStr
    call Str_itoa
            
    ld a,18
    ld (A_c8),a
    ld a,(A_r8)
    inc a
    ld (A_r8),a
    ld a,%01000110
    ld (A_attr),a
    
    call A_DrawString
    
    djnz KeyRdfLoop

    ld a,1
    ld (KRedefFlg),a
    
    call DrawBoard  ; redraw chess board

    pop ix
    pop de
    pop bc
    pop af
    ret
    
KeysRedefine_text:
    .byte "    Redefine joystick and       "    
    .byte "      arrow control keys        "
    .byte "                                "        
    .byte "                                "        
    .byte "       Right                    "
    .byte "       Left                     "
    .byte "       Down                     "
    .byte "       Up                       "
    .byte "       Fire                     "
    .byte "                                "
    .byte "      (press key now)           "    
    .byte 0

KRedefs:
    .ds 5       ; codes redefined
KRedefFlg
    .byte 0     ; 1 if keys redefined

;-----------------------------------
; Substitute joystick and arrow keys
; A- current keypress
;
KeysSubstRedefined:

    ld b,a
    ld a,(KRedefFlg)
    or a
    jr z,KeySbstRdfEx

            ; if redefined then
    push bc
    push ix
    push de
    ld ix,KRedefs
    ld de,joy_arrowkeys_table+1
    ld c,5
KeySbstRdLoop:
    ld a,(ix)
    cp b
    jr nz,KeySbstRdov    
    ld a,(de)
    ld (PKNow),a            ; if in redefined list, substitute
KeySbstRdov:
    inc ix
    inc de
    dec c   
    jr nz,KeySbstRdLoop
    pop de
    pop ix
    pop bc
    
KeySbstRdfEx:
    ld a,(PKNow)
    ld b,a
    ret

;
; Turns off verification
;
KeySlowCase:
    push af
    push de
    ld a,(fSlow)
    inc a
    and 1
    ld (fSlow),a
    or a
    jr z,SlwKp00
    ld de,Slw_DspON     ; display ON info
    jr SlwKp01
SlwKp00:
    ld de,Slw_DspOFF    ; display OFF info
SlwKp01:
    call DrawString
    call Slw_paus_long
    call DrawBoard
    pop de
    pop af
    ret

Slw_DspON:
    .byte "Not verify.",0
Slw_DspOFF:
    .byte "Restored.  ",0
    
;
; Turns Twice On,Of
;
KeyTwiceCase:
    push af
    push de
    ld a,(fTwice)
    inc a
    and 1
    ld (fTwice),a
    or a
    jr nz,TwcKp00
    ld de,Twc_DspON     ; display ON info
    jr TwcKp01
TwcKp00:
    ld de,Twc_DspOFF    ; display OFF info
TwcKp01:
    call DrawString
    call Slw_paus_long
    call DrawBoard
    pop de
    pop af
    ret
    
Twc_DspON:
    .byte "No same key twice.",0
Twc_DspOFF:
    .byte "Restored keypress.",0
    
; Pause after keypress 
Slw_pause:
    di
    push af
    ld a,(fTwice)
    or a
    jr z,LpSkpPause
    push bc
    ld b,20
LpPause:
    call DrawSquare
    djnz LpPause
    pop bc
LpSkpPause:    
    pop af
    ei
    ret
    
; ----- wait
Slw_paus_long:
    push bc
    ld b,160
SlwLng0:    
    call DrawSquare
    djnz SlwLng0
    pop bc
    ret
