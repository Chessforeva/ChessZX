
SCREEN = $4000          ; VRAM screen memory
SCRATT = $5800          ; SCREEN + 6144

CHARSET = $3D00         ; Standard charset table in ROM
    
      
;------------
; Parameters for A_DrawChar, A_DrawString
;
; Char attribute 
; Bits:
;   7    - FLASH mode
;   6    - BRIGHTNESS mode
;   3-5  - PAPER colour
;   0-2  - INK colour
A_attr: .byte 0

; For chars it is standard(0) or bold(1) charset
A_bold: .byte 0

;  where to print, pixels 8x8=1 char
A_r8:   .byte 0     ; Column (0..31)
A_c8:   .byte 0     ; Row (0..23)
;------------


; read row,column,attrib. from data at IX
;  sets A_r8,A_c8,A_attr, IX+=3
A_by_IX:
    ld a,(ix)
    inc ix
    ld (A_r8),a
    ld a,(ix)
    inc ix
    ld (A_c8),a    
    ld a,(ix)
    inc ix
    ld (A_attr),a
    ret

;------------
; Clears screen by clearing memory and setting
; colour attributes
;
; call with a = ink colour.
;
ClearScreen:

        push bc
        push de
        push hl
        ld e,a
        ld hl,SCREEN
        ld bc,6144
Cls_cs0:
        ld a,0
        ld (hl),a
        inc hl
        dec bc
        ld a,b
        or c
        jp nz, Cls_cs0

        ld bc,768
Cls_cs1:
        ld (hl),e
        inc hl
        dec bc
        ld a,b
        or c
        jp nz, Cls_cs1
        pop hl
        pop de
        pop bc
        ret 

;------------
; Draws a character
;
DrawChar:
        ;; call with a = ascii character code.
        ;; hl = screen address.
        ;; de = charset table

        push bc
        push de
        push hl

        sub 32           ;; 32 = space = first char = 0, so sub 32 !
        push hl
        ld h,0
        ld l,a
        add hl,hl        ;; multiply by 8.
        add hl,hl
        add hl,hl

        add hl,de
        ex de,hl         ;; de = address of char data.
        pop hl

        ld c,8
DrwChr_dc0:
        ld a,(de)
        ld (hl),a
        inc h
        inc de
        dec c
        jp nz, DrwChr_dc0

        pop hl
        pop de
        pop bc
        ret
;
; Prints attributed char in position, A=code
; Parameters: A_attr, A_r8, A_c8, also A_bold
A_DrawChar:
        push af
        push hl
        push de

        call CharSetTableDE                
        call A_PrepHL
        call DrawChar

        call A_PrepHLAttrib ; attribute + position
        
        ld a,(A_attr)
        ld (hl),a           ; set attribute

        pop de
        pop hl
        pop af        
        ret 
       
 
; Prepares HL address from A_r8,A_c8
A_PrepHL:
        push af
        push bc
        ld a, (A_r8)
        rla     ;*8
        rla
        rla
        ld b,a      ; x pixel
        ld a, (A_c8)
        rla     ;*8
        rla
        rla
        ld c,a      ; y pixel
        call Get_Pixel_Address
        pop bc
        pop af
        ret

; Prepares HL address of attributes from A_r8,A_c8
A_PrepHLAttrib:
        push af
        push bc
        ld a, (A_r8)
        and %00000111
        rla     ;*32
        rla
        rla
        rla
        rla
        ld l,a
        ld a, (A_r8)
        and %11111000
        rra
        rra
        rra
        or %01011000    ; + base address of screen attrib.
        ld h,a
        ld a,(A_c8)
        add a,l
        ld l,a
        pop bc
        pop af
        ret

;----
; Sets DE=charset table
;
CharSetTableDE:
        push af
        ld a,(A_bold)
        or a
        jr z,ChrsStnd
        ld de,CHARS_BOLD    ; address of our bold chars
        jr ChrsTbEx
ChrsStnd:
        ld de,CHARSET       ; charset address in ROM
ChrsTbEx:
        pop af
        ret     


;------------
; Attributed printing of string by using
; A_DrawChar with A_attr, A_r8, A_c8, also A_bold
;  de - string address
A_DrawString:

        push af
        push de
ADrwStrLoop:
        ld a,(de)
        or a
        jr z,ADrwStrEx
        call A_DrawChar ; Attributed output a to screen
        inc de
        ld a,(A_c8)
        inc a           ; column++
        cp 32           ; if should be linefeed
        jr c,ADrwNxtClmn
        ld a,(A_r8)     ; next row
        inc a
        ld (A_r8),a
        xor a           ; column=0
ADrwNxtClmn:        
        ld (A_c8),a
        jr ADrwStrLoop
ADrwStrEx:
        pop de
        pop af
        ret
        
;------------
; Prints a string (ends with 0)
; hl - screen address
; de - string address
DrawString:

        push bc
        push de
        push hl  
        ld b,d              ; de->bc
        ld c,e
        ld hl,SCREEN
        call CharSetTableDE
DrwStrLoop:
        ld a,(bc)
        or a
        jr z,DrwStrEx
        call DrawChar       ; Output a to screen
        inc bc
        inc hl
        jr DrwStrLoop
DrwStrEx:
        pop hl
        pop de
        pop bc
        ret
        

; Get screen address
; B = Y pixel position
; C = X pixel position
; Returns address in HL
Get_Pixel_Address:
        LD A,B          ; Calculate Y2,Y1,Y0
        AND %00000111   ; Mask out unwanted bits
        OR %01000000    ; Set base address of screen
        LD H,A          ; Store in H
        LD A,B          ; Calculate Y7,Y6
        RRA             ; Shift to position
        RRA
        RRA
        AND %00011000   ; Mask out unwanted bits
        OR H            ; OR with Y2,Y1,Y0
        LD H,A          ; Store in H
        LD A,B          ; Calculate Y5,Y4,Y3
        RLA             ; Shift to position
        RLA
        AND %11100000   ; Mask out unwanted bits
        LD L,A          ; Store in L
        LD A,C          ; Calculate X4,X3,X2,X1,X0
        RRA             ; Shift into position
        RRA
        RRA
        AND %00011111   ; Mask out unwanted bits
        OR L            ; OR with Y5,Y4,Y3
        LD L,A          ; Store in L
        RET

; B = Y pixel position
; C = X pixel position
; Returns address in HL
Get_Attrib_Address:
        ld a,b
        rra             ; /64 (64x3=192 lines 3x256=768 attribs) 
        rra
        rra
        rra
        rra
        rra
        and %00000111
        or %01011000    ; + base address of screen attrib.
        ld h,a
        ld a,b
        rla             ; x 4 = x 32 /8
        rla
        and %11111100
        ld l,a
        ld a,c
        rra             ; /8
        rra
        rra
        and %00011111
        add a,l
        ld l,a
        ret

;---------------------------
;
; Clears rectangle, sets attribute
;  A_c8, A_r8 - left top corner
;  A_attr - attribute to set
;  b - width in chars
;  c - height in chars
; 
A_ClearRect:

        push af
        push de
        push bc

ClrRectCLoop:
        push bc
ClrRectBLoop:
        ld a, 32        ; clear char
        call A_DrawChar ; Attributed output a to screen
        ld a,(A_c8)
        inc a           ; column++
        ld (A_c8),a
        djnz ClrRectBLoop
        pop bc

        ld a,(A_c8)
        sub b
        ld (A_c8),a
        
        ld a,(A_r8)     ; next row
        inc a
        ld (A_r8),a
        
        dec c
        jr nz,ClrRectCLoop
 
        pop bc
        pop de
        pop af
        ret

;-------------------
; Draw LOGO on black background
; Uses A_c8, A_r8
;-------------------
DrawZX128logo:

    push af
    push bc
    push de
    push ix
    
    ld c,3
DrwLgRwLoop:
    ld ix, DrwAtrbsLogo
    ld b,9
DrwLgLoop:
    ld a,32
    ld de, Char05       ; draw "half-character"
    call A_PrepHL
    call DrawChar

    call A_PrepHLAttrib ; attribute + position
        
    ld a,(ix)
    inc ix
    ld (hl),a           ; set attribute
    
    ld a,(A_c8)
    inc a               ; column++
    ld (A_c8),a
    
    djnz DrwLgLoop

    ld a,(A_c8)
    sub 10
    ld (A_c8),a
    ld a,(A_r8)
    inc a
    ld (A_r8),a
    
    dec c
    jr nz,DrwLgRwLoop
    
    ld a,(A_r8)
    inc a
    ld (A_r8),a
    
    ld a,%01000010   ; Draw red "ZX Spectrum 128"
    ld (A_attr),a
    ld de,DrwLogoStr
    call A_DrawString
    
    pop ix
    pop de
    pop bc
    pop af
    ret

DrwLogoStr:
    .byte "ZX Spectrum 128",0

        ; list of attributes for logo
DrwAtrbsLogo:
    .byte %01000010
    .byte %01010010
    .byte %01010110
    .byte %01110110   
    .byte %01110100
    .byte %01100100
    .byte %01100011
    .byte %01011011
    .byte %01011000

Char05:

    .byte %00000001 ;  half-char
    .byte %00000011
    .byte %00000111
    .byte %00001111
    .byte %00011111
    .byte %00111111
    .byte %01111111
    .byte %11111111
