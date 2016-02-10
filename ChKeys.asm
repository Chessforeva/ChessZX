
; Wait for keypress
PressKey:
    push af
LoopPK:
    call Random2
    call Random0
    call GetJoyOrKey
    or a
    jr z,LoopPK
        ; if key pressed
    ld (PKNow),a
    push bc
    ld b,a
    call Random1
    ld a,(fTwice)
    or a
    jr nz,PK0noSlow
    ld a,(PKLast)
PK0noSlow:
    sub b           ; compare to last keypress
    pop bc
    or a
    jr z,LoopPK     ; if the same, wait
    ld a,(PKNow)
    ld (PKLast),a   ; save last pressed
    pop af
    ret

 ; current key press   
PKNow:
    .byte $ff
    
; last pressed key to avoid double press on fast emulators
PKLast:
    .byte $ff

; ---- wait for Enter key
PressEnter:

    push af
PKEntLoop:
    call PressKey
    ld a,(PKLast)
    cp 13
    jr nz,PKEntLoop
    pop af


; ===========================================
; Try joystick codes at first
; then read keyboard status
; A=ascii or 0, if no key in buffer
; ===========================================
GetJoyOrKey:
    
    ; About arrow keys codes:
    ; 
    call ReadKempstonJoystick
    or a                        ; is joystick?, then ascii code of arrow keys (on Speccy)
    jr nz,GKJKex
    call GetKey                 ; get keyboard code
GKJKex:
    ret

          
; ===========================================
;  Keyboard Reading Routines. (for 48 too, very minimal)
; ===========================================
; A=0 if no key pressed,
; or "kind of" ascii key code in A
; ----
; *not developed for ESC,TAB,BS cases, do it better
;
; ZX gamers used:
;  1) Number keys 1 to 5 or 6 to 0, also emulated by
;     Sinclair Interface II dual joysticks
;     + and  ~ as fire
;  2) O – left, P – right, Q – up, A – down, M – fire
;  3) mostly keys were redefined in games options (WASD is PC)
;
; Emulators behave very oddly, when detecting joysticks,
; cursor pads or PC arrow keys to assign as defaults.
;
GetKey:

    push bc
    push de
    push hl
    ; read key     
    ld e,0
    ld hl,in_port_table
    ld b,8
GKr0:
    ld a,(hl)
    in a,(254)
    cpl
    and 31
    ld c,a
    jp nz,GKrGotkey
    ld a,e
    add a,5
    ld e,a
    inc hl  

    dec b
    jp nz,GKr0
    jr CKexit0      ; return 0

GKrGotkey:
    ld hl,ascii_table
    ld d,0
    add hl,de
GKr1:
    rra
    jp c, GKr2
    inc hl
    jp GKr1
GKr2:
    ld a,(hl)
    and a
    jr CKexit

CKexit0:    
    xor a          ; a=0 if no key pressed
CKexit:
    pop hl
    pop de
    pop bc
    ret

in_port_table:
    .byte 0xfe
    .byte 0xfd
    .byte 0xfb
    .byte 0xf7
    .byte 0xef
    .byte 0xdf
    .byte 0xbf
    .byte 0x7f

ascii_table:
    .byte 15        ; SHIFT, or other as BS
    .byte "ZXCVASDFGQWERT1234509876POIUY"
    .byte 13        ; ENTER
    .byte "LKJH"
    .byte 32        ; SPACE
    .byte 24        ; FULL-STOP, on Speccy "Ctrl", on ZXplus "LAlt"
    .byte "MNB"
;
; Joystick control
;
ReadKempstonJoystick:
;returns ascii code in A representing joystick info

    push bc
    push hl
    ld bc,5      ; calculate code by joystick bits
    in a,(31)
        ;bits = 0 if not pressed, one otherwise.
        ; 4    fire
        ; 3    up
        ; 2    down
        ; 1    left
        ; 0    right
    and %00011111
JoyCKbtloop:
    bit 0,a
    jr nz,JoyCK0
    rra
    dec c
    jr nz,JoyCKbtloop
JoyCK0:
    ld hl,joy_arrowkeys_table
    add hl,bc
    ld a,(hl)
    pop hl
    pop bc
    ret

joy_arrowkeys_table:
;
;assign known ASCII codes
; (or substitute to number keys)
;
    .byte  0
    .byte  126 ;'~' fire, Alt on Speccy,ZXspin
    .byte  38 ;up arrow
    .byte  40 ;down arrow
    .byte  37 ;left arrow
    .byte  39 ;right arrow

