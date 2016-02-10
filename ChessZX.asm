;
;
; Thanks to sjasmplus compiler, that generates
;  .SNA snapshot file for ZX Spectrum 128.
;

    page 0
    .org $60C0                  ; loading address
    INCLUDE "ChLogic.asm"   ; include chess logic
    INCLUDE "ChSubs.asm"    ; various subs 
    INCLUDE "ChGraph.asm"   ; useful graphics code    
    INCLUDE "ChKeys.asm"    ; useful key control code
    INCLUDE "ChPieces.asm"  ; drawings of chess pieces   
    INCLUDE "ChGUI.asm"     ; chess board loops, keypress events
    INCLUDE "ChTitle.asm"   ; title screen
    
; Both sides chess M1,M2,M3 moves 1.w-b, 2-w-b., 3.w and checkmate
CDEPTH = 5;

    INCLUDE "ChRead.asm"    ; include reading from 128Kb memory
    INCLUDE "ChDbg.asm"     ; various debug-s
    INCLUDE "String.asm"    ; string functions
    
    ; here the program starts (starting address)
start:
    ; SP=0x5FFF by sjasmplus, ok, let be stack above my program
    
    ld a,0              ; clear black
    call ClearScreen
    
    ld a,1
    ld (A_bold),a       ; set Bold chars in ChGraph.asm

    ; Display title window
    call DrawTitle

LOOPMAIN:
    call ReadGameInBank     ; go to next chess game
    call ChessBoardGUI      ; and start GUI
            
    jp LOOPMAIN         ; No ESC key as on PC, so loop
     
EXIT:
    jp EXIT             ; stay there forever 

fSlow:
    .byte 0     ; 1 if user wants turn off verification
                ; by pressing "P"
fTwice:
    .byte 1     ; 0 if user wants turn off twice key-press                


;----------------
; Call this to select bank and find a chess puzzle
ReadGameInBank:

    push hl
RdWhLoop:    
    call Random1
    and 7               ; Banks 0..7
    cp 2                ; Not 2nd bank
    jr z,RdWhAdd1
    cp 5
    jr z,RdWhAdd1       ; Not 5th bank
    jr RdWhBkOk
RdWhAdd1:
    inc a               ; 2->3, 5->6
RdWhBkOk:
    
    ; debug
    call dbgRdBank
    
        ; set BANK
    ld hl,ReadCallbackSub
    call BankCall
    pop hl
    ret

; Here we set address to data in memory and read, see ChRead.asm
ReadCallbackSub:
    ld hl,$C000
    call ReadRndGame    ; Read randomized game 
    ret

;------------------------
; Various data
;------------------------

    INCLUDE "Pieces_img.asm"    ; datas of pieces
    INCLUDE "charset_bold.asm"  ; Bold charset, CHARS_BOLD

;
; sjasmplus feature: page,org,icbin and SNA is ready!
;
;
;Banks as binaries   16128 * 6 ~  94Kb
;
; Memory address (where are loaded by port 7FFD setting):
; 0xC000 - 0xFE00
;
;  1687 M1 puzzles
;  4103 M2 puzzles
;  2626 M3 puzzles
;   8416 chess puzzles with moves
;
;File:BANK0.bin, puzzles M1
;N=1687, Bytes/pos=9, pieces/pos=5
;File:BANK1.bin, puzzles M2
;N=1340, B/p=12, pc/p=6
;File:BANK3.bin, puzzles M2
;N=1362, B/p=11, pc/p=6
;File:BANK4.bin, puzzles M2
;N=1401, B/p=11, pc/p=6
;File:BANK6.bin, puzzles M3
;N=1300, B/p=12, pc/p=6
;File:BANK7.bin, puzzles M3
;N=1326, B/p=12, pc/p=5

    page 0
    ORG $C000
    INCBIN "BANK0.bin"
    
    page 1
    ORG $C000
    INCBIN "BANK1.bin"

    page 3
    ORG $C000
    INCBIN "BANK3.bin"

    page 4
    ORG $C000
    INCBIN "BANK4.bin"
          
    page 6
    ORG $C000
    INCBIN "BANK6.bin"

    page 7
    ORG $C000
    INCBIN "BANK7.bin"

    ;save snapshot
    SAVESNA "ChessZX.sna", start