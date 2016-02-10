;-------------------------------------
;
; Most common String functions
;  (MSX Z80 asm, also for ZX Spectrum)
; String can be 256 characters long maximum
; 
;
; String ends with 0, 
;  should be reserved bytes if string becomes longer,
;   of course.
;
;-------------------------------------
; 

; returns length of DE string in A, also sets Z
Str_Len:
        push bc
        push de
        ld c,0
Str_LnScn:        
        ld a,(de)
        cp 0
        jr z,Str_LnEx
        inc de
        inc c
        jr Str_LnScn
Str_LnEx:
        ld a,c
        pop de        
        pop bc
        cp 0
        ret
        
; set empty string
Str_Clear:
        push af
        ld a,0
        ld (de),a
        pop af
        ret



; Cuts string of DE at length=A [0..254]
;     (label+nn  is much faster)
;
Str_Cut:
         push af
         push bc
         push hl
         ld b,0
         ld c,a
         push de
         pop hl
         add hl,bc
         ld a,0
         ld (hl),a
         pop hl
         pop bc
         pop af
         ret
         
; compares string of DE with string of IX
; Returns: A=0 - strings are equal
;          A=1 - IX string starts with DE str.
;          A=2 - not equal
; 
Str_Compare:
         push bc
         push de
         push ix
Str_compScn:         
         ld a,(ix)
         cp 0
         jr z,Str_compL0   
         ld b,a
         ld a,(de)
         cp 0
         jr z,Str_compN1
         cp b
         jr nz,Str_compNEq
         inc de
         inc ix
         jr Str_compScn
Str_compL0:
         ld a,(de)
         cp 0
         jr z,Str_compEq
         jr Str_compNEq
Str_compEq:
         ld a,0
         jr Str_compEx
Str_compNEq:
         ld a,2
         jr Str_compEx         
Str_compN1:
         ld a,1
Str_compEx:
         pop ix
         pop de
         pop bc
         ret

; Copies string DE to IX
Str_Copy:
         push af
         push de
         push ix
Str_CpyScn:
         ld a,(de)
         ld (ix),a
         cp 0
         jr z,StrCpyEx
         inc de
         inc ix
         jr Str_CpyScn
StrCpyEx:                  
         pop ix
         pop de
         pop af
         ret

; Converts string DE to UpperCase
Str_Upper:
         push af
         push de
Str_UppScn:
         ld a,(de)
         cp 0
         jr z,Str_UppEx
         cp 'a'
         jr c,Str_UppNCnv
         cp 'z'
         jr nc,Str_UppNCnv
         sub 32
         ld (de),a
Str_UppNCnv:
         inc de         
         jr Str_UppScn
Str_UppEx:         
         pop de
         pop af
         ret

; Converts string DE to LowerCase
Str_Lower:
         push af
         push de
Str_LwrScn:
         ld a,(de)
         cp 0
         jr z,Str_LwrEx
         cp 'A'
         jr c,Str_LwrNCnv
         cp 'Z'
         jr nc,Str_LwrNCnv
         add a,32
         ld (de),a
Str_LwrNCnv:
         inc de         
         jr Str_LwrScn
Str_LwrEx:         
         pop de
         pop af
         ret

; Finds string DE in string IX first 254 bytes
; On return: A=position, starting from 0
; If not found then A=$ff                 
Str_IndexOf:
         push bc
         push de
         push ix

         ld b,0
Str_InxScn:
         ld a,(ix)
         cp 0
         jr nz,Str_InxCompr
         ld a,$ff
         jr Str_InxEx
Str_InxCompr:
         call Str_Compare
         cp 2
         jr nz,Str_InxFound
         inc b
         ld a,$ff
         cp b
         jr z,Str_InxEx
         inc ix
         jr Str_InxScn
Str_InxFound:
         ld a,b 
Str_InxEx:                  
         pop ix
         pop de
         pop bc
         ret

;returns a=0 and Z flag set if string IX contains substring DE
; (length not limited)
Str_Contains:
         push ix
Str_Contai_Scn:
         ld a,(ix)
         cp 0
         jr nz,Str_Contai_Cont
         ld a,2
         jr Str_Contai_ex
Str_Contai_Cont:      
         call Str_Compare
         cp 2
         jr nz,Str_Contai_found
         inc ix
         jr Str_Contai_Scn
Str_Contai_found:
         ld a,0
Str_Contai_ex:          
         pop ix
         cp 0
         ret

;
; Places substring of IX into DE
; c=starting char in IX, b-length in chars
;
Str_Substr:
         push af
         push bc
         push de
         push hl       

         push ix
         pop hl
         ld a,c
         cp 0
         jr z,Str_SubAt0
         push bc
         ld b,0
         add hl,bc   ; offset
         pop bc
Str_SubAt0:         
         ld c,b
         ld b,0
         ldir        ; copy bc bytes from HL to DE addrs.
         ld a,0
         ld (de),a
         
         pop hl
         pop de
         pop bc
         pop af
         ret

;
; Removes left spaces for string in DE
;
Str_Ltrim:
         push af
         push bc
         push de
         push ix
         
         push de
         pop ix
         ld c,0
Str_LtrScn:         
         ld a,(de)
         cp 32
         jr nz,Str_LtrNS
         inc de
         ld c,1
         jr Str_LtrScn
Str_LtrNS:
         ld a,c
         cp 0
         jr z,Str_LtrEx
         call Str_Copy
Str_LtrEx:
         pop ix
         pop de
         pop bc
         pop af
         ret
         
;
; Removes right spaces for string in DE
;
Str_Rtrim:
         push af
         push de         
         ld a,(de)
         cp 0
         jr z,Str_RtrEx
Str_RtrRscn:
         inc de        
         ld a,(de)
         cp 0
         jr z,Str_Rtr1
         jr Str_RtrRscn
Str_Rtr1:
         dec de
         cp 32
         jr nz,Str_RtrEx
         ld a,0
         ld (de),a
         jr Str_Rtr1
Str_RtrEx:
         pop de
         pop af
         ret

; both trims
Str_Alltrim:
         call Str_Rtrim
         call Str_Ltrim
         ret

;
; Adds DE string at the end of IX string
;
Str_Append:
         push af
         push ix         
         ld a,(ix)
         cp 0
         jr z,Str_App1
Str_AppRscn:
         inc ix        
         ld a,(ix)
         cp 0
         jr z,Str_App1
         jr Str_AppRscn
Str_App1:
         call Str_Copy
         pop ix
         pop af
         ret

;
; Replaces all DE substrings in IX string with HL string
;
Str_Replace:
         push af
         push bc
         push hl
         push ix
         
         call Str_Len
         ld c,a           ;c=Len(de-string)
         cp 0
         jr z,Str_ReplNo
Str_ReplLoop:        
         call Str_IndexOf
         cp $ff
         jr z,Str_ReplNo
         
         push de
         ld b,a
         ld de,Str_buf_
         
         push ix
Str_ReplIy0:                   ; copy till substr...
         ld a,(ix)
         ld (de),a
         ld a,b
         cp 0
         jr z,Str_ReplIx0
         inc ix
         inc de
         dec b
         jr Str_ReplIy0
Str_ReplIx0:

         push hl
Str_ReplIy1:                   ; copy new substring...         
         ld a,(hl)
         ld (de),a
         cp 0
         jr z,Str_ReplHl0
         inc hl
         inc de
         jr Str_ReplIy1
Str_ReplHl0:
         pop hl
         
         ld a,c
Str_ReplSkp1:         
         inc ix                 ; skip substr
         dec a
         jr nz,Str_ReplSkp1
Str_ReplIy3:                    ; copy all string after...
         ld a,(ix)
         ld (de),a
         cp 0
         jr z,Str_ReplIx3
         inc ix
         inc de
         jr Str_ReplIy3
Str_ReplIx3:
         pop ix
         
         ld de,Str_buf_
         call Str_Copy        ; copy new string to IX 

         pop de
         jr Str_ReplLoop        ; to replace all occurrances
Str_ReplNo:         
         pop ix
         pop hl
         pop bc
         pop af
         ret
         
Str_buf_ .ds $100
         .byte 0


;
; This truncates DE string where new line (ascii 10 or 13)
; If string is longer 254 chars then truncates at space char.
; Sets IX as pointer to next line starting character
; This supposed to get safe-as-much-possible string opeations.

Str_Line:
         push af
         push bc
         push de

         ld bc,0
Str_Ln_Scn:
         ld a,(de)       ; find end of line
         cp 0
         jr z,Str_Ln_q1
         cp 13
         jr z,Str_Ln_q1
         cp 10
         jr z,Str_Ln_q1
         inc de
         inc c
         ld a,$ff         ; reached 255?         
         cp c
         jr nz, Str_Ln_Scn
         
Str_Ln_fSpc:         
                           
         ld a,(de)         ; if string too long then finds space near 254th position
         cp ' '
         jr z,Str_Ln_q1    ; use this position and truncate string
         dec de
         dec c
         jr nz,Str_Ln_fSpc
                    
Str_Ln_cL:
         ld a,(de)          ; if cant find space then just searches for line end
         cp 0               ; to obtain new IX anyway
         jr z,Str_Ln_q1
         cp 13
         jr z,Str_Ln_q1
         cp 10
         jr z,Str_Ln_q1
         inc de
         jr Str_Ln_cL
         
Str_Ln_q1:
         ld a,0          ; set to 0 till next line
         ld (de),a
         inc de
         ld a,(de)
         cp 0          
         jr z,Str_Ln_q1
         cp 13
         jr z,Str_Ln_q1
         cp 10
         jr z,Str_Ln_q1
         
         push de
         pop ix
        
         pop de
         pop bc
         pop af
         ret

;
; This removes all non-character symbols and sets TABs as 3 spaces
; of string in DE.  Use after Str_Line not to lose line feeds.
;                  
Str_CharsOnly:

         push af
         push de
         push ix
         push de
         push de
         pop ix
         ld de,Str_buf_
Str_CharsO_loop:
         ld a,(ix)
         cp 0
         jr z, Str_CharsO_Ex
         cp 9
         jr nz, Str_CharsO_nTb
         ld a,' '
         ld (de),a
         inc de
         ld (de),a
         inc de
Str_CharsO_cpy:         
         ld (de),a
         inc de
         inc ix
         jr Str_CharsO_loop
Str_CharsO_nTb:
         cp ' '
         jr c,Str_CharsO_loop
         cp $ff
         jr z,Str_CharsO_loop
         jr Str_CharsO_cpy
Str_CharsO_Ex:
         ld (de),a
         ld de,Str_buf_
         pop ix
         call Str_Copy        ; copy new string to IX 
         pop ix
         pop de
         pop af
         ret

;
; places value of integer in reg-a into DE string
; (hexadecimal)
Str_itoa_hex:
         
         push af
         push bc
         ld b,a
         
         inc de
         inc de
         xor a          ; ends with 0
         ld (de),a
         dec de
         
          ; second char
         ld a,b
         and $f
         cp 10
         jr nc,Str_i2h1
         add a,'0'
         jr Str_i2h2
Str_i2h1:
         add a,'7'    ; 55 + code >= 65 that is code of "A"-"F"
Str_i2h2:
         ld (de),a
         dec de

          ; first char
         ld a,b          
         and $f0
         rra
         rra
         rra
         rra
         cp 10
         jr nc,Str_i2h3
         add a,'0'
         jr Str_i2h4
Str_i2h3:
         add a,'7'    ; 55 + code >= 65 that is code of "A"-"F"
Str_i2h4:
         ld (de),a
         pop bc     
         pop af
         ret

;
; places value of integer in reg-a into DE string
;
Str_itoa:
         push af
         push bc
         push de
         cp 0
         jr z,Str_i2_0
         cp 200
         jr nc,Str_i2a200
         cp 100
         jr nc,Str_i2a100
         cp 10
         jr nc,Str_i2a10
         ld c,a
         jr Str_i2q
Str_i2a10:
         ld b,0
Str_i2a_l_10:         
         cp 10
         jr c,Str_i2a1
         sub 10
         inc b
         jr Str_i2a_l_10
Str_i2a1:
         ld c,a
         ld a,'0'
         add a,b
         ld (de),a
         inc de
Str_i2q: ld a,'0'
         add a,c
         ld (de),a
         inc de
         ld a,0
         ld (de),a
         jr Str_i2ex                  
Str_i2a200:
         sub 200
         ld c,a                 
         ld a,'2'
         ld (de),a
         inc de
         ld a,c         
         jr Str_i2a10
Str_i2a100:
         sub 100
         ld c,a         
         ld a,'1'
         ld (de),a
         inc de
         ld a,c
         jr Str_i2a10
Str_i2_0:
         ld c,0
         jr Str_i2q        
Str_i2ex:
         pop de
         pop bc
         pop af
         ret
         
;
; places integer value of DE string into reg-a
;
Str_atoi:
         push bc
         push de
         push hl
         
         ex de,hl ;hl=de
         ld de,0
         ld bc,0
Str_a2iLoop:         
         ld a,(hl)
         inc hl
         cp 0
         jr z, Str_a2ex
         cp '0'
         jr c, Str_a2ex
         cp '9'+1
         jr nc, Str_a2ex
         jr Str_a2digit
Str_a2digit:
         sub '0'
         ld c,a
         ld a,b
         cp 0
         jr z,Str_a2ph1
         cp 1
         jr z,Str_a2ph2
         cp 2
         jr z,Str_a2ph2   ; the same method  *10+a
Str_a2ex:               
         ld a,d          ; store to A
         jr Str_a2iEx         
Str_a2Lp:
         jr Str_a2iLoop
         
Str_a2ph1:
         ld d,c  ; ones, save
         ld b,1
         jr Str_a2Lp

Str_a2ph2:
         ld a,d           ;d*10 = (d*8)+(d*2)
         sla a   ; *2
            
         sla d
         sla d
         sla d   ;*8
         add a,d  ;+ = become tens
         add a,c  ; +ones
         ld d,a   ;save
         ld b,2         
         jr Str_a2Lp
         
Str_a2iEx:
         pop hl         
         pop de
         pop bc
         ret

; finds tags in string IX and copies substring between them to DE address
; tags are strings addressed by hl (as left tag) and iy (as right tag)
; for example: <div>substring between</div>
; Separators should be at least 1char.
; If can't find left tag till 254th position then returns empty string.
; If can't find right tag then truncates substring at 255th position anyway. 
;
Str_TagSubstr:
         push af
         push bc
         push de
         push hl
         push ix
         
         push de
         ex de,hl   ; de=hl (left tag)
         ld c,0
Str_TagScn:
         inc c
         ld a,$ff
         cp c              ; if 255th reached
         jr z,Str_LTagNotFound
         call Str_Compare
         inc ix
         cp 2
         jr z,Str_TagScn
         call Str_Len ;calculates length of left tag
Str_TagSkp1:
         cp 1
         jr z,Str_TagBr1
         inc ix
         dec a
         jr Str_TagSkp1
         
Str_LTagNotFound:
         pop hl   ; hl = de where to store substring 
         jr Str_TagBr2
         
Str_TagBr1:                  
         
         push iy
         pop de   ; de now = iy (right tag)
         pop hl   ; hl = de where to store substring
         
         ld c,0
Str_TagSc2:
         call Str_Compare
         cp 2
         jr nz,Str_TagBr2
          
         ld a,(ix)
         ld (hl),a
         inc hl
         inc ix
         inc c
         ld a,$ff
         cp c              ; if 255th reached
         jr nz,Str_TagSc2
Str_TagBr2:
         ld a,0
         ld (hl),a   ; end of new string
         pop ix
         pop hl
         pop de
         pop bc
         pop af         
         ret
                    

;
; Faster copy from IX to DE length of A
;  (ignores 0)
;

Block_Copy_:
         cp 0
         jr z,Block_CpyExit
         push af
         push bc
         push de
         push hl       

         push ix
         pop hl
         
         ld c,a
         ld b,0
         ldir        ; copy bc bytes from HL to DE addrs.
         
         pop hl
         pop de
         pop bc
         pop af
Block_CpyExit:         
         ret
         
; sets to 0 block in DE length of A
Block_Clear_:
         cp 0
         jr z,Block_ClrExit
         push af
         push bc
         push de
         push hl       

         ld c,a
         dec c
         
         push de
         pop hl
         
         ld a,(Block_ch0)
         ld (hl),a
         inc de
         
         ld b,0
         ldir        ; copy repeating 0 from HL to DE addrs.
                 
         pop hl
         pop de
         pop bc
         pop af
Block_ClrExit:         
         ret

Block_ch0: .byte 0   ;uses this byte to set 0
                     ; (can set spaces or other)



