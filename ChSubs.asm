
BANKATMEMORY = $C000    ; where the bank memory is loaded
BANKVALUE = $5b5c       ; the last value of bank selected
BANKPORT = $7FFD

;=========================================================
; To switch bank and do something, then return
; A-bank to 1..7, 0=default (2,5 are screen banks)
; HL-address of call-back sub to call
; (data reading when switched)
;
; Stack pointer should be set before 0xC000.
; Memory 0xC000-0xFFFF will contain bank data.
; Read data in sub, then restore bank.
; Do not call inside again, then create other.
;
; Sample:
;  ld a,1
;  ld hl,myproc_data_read_at_C000
;  call BankCall
;  ld a,2
;  ld hl,myproc_data_read_at_C000
;  call BankCall
;  ...
;myproc_data_read_at_C000:
;  ...
;  ret 

BankCall:
    ld (BankClb),hl         ; Save to memory
    ld (BankStackSave),sp   ; Save stack pointer
;    ld sp, $BFFF            ; Set stack before bank memory

    push af
    ld a,(BANKVALUE)
    ld (BankValSave),a
    pop af
    
    call SETBANK            ; set new bank
    ld hl,BankRst
    push hl                 ; push return address
    ld hl,(BankClb)
    jp (hl)                 ; call my callback sub
BankRst:    
    ld a,(BankValSave)
    call SETBANK            ; restore bank
    ld  sp,(BankStackSave)  ; restore stack pointer
    ret
    
BankClb:
    .word 0         ; subroutine to call
BankStackSave:
    .word 0         ; to save stack pointer
BankValSave:
    .byte 0         ; to save bank selected


; This sets bank, A=bank 0..7
SETBANK:
    di          ; disable the interrupts.
    push bc     ; Remember BC.
    ld bc, BANKPORT
    or $10              ; Install ROM 48.
    ld (BANKVALUE),a    ; Change lane. BANK
    out (c), a          ; Output to port $7FFD values
    pop bc
    ei          ; Enable interrupts.
    ret


;=========================================================
; RANDOMIZER
; Sets A and returns value
;
Random0:
    push bc
    ld b,a
    ld a,(Rnd0)
    adc a,b
    ld b,a
    ld a,(RndI)
    inc a
    ld (RndI),a
    adc a,b
    rlc a
    ld (Rnd0),a
    pop bc
    ret
Rnd0: .byte 0
RndI: .byte 0

Random1:
    push bc
    ld b,a
    ld a,(Rnd1)
    add b
    rlc a
    xor $ff
    rlc a
    ld (Rnd1),a
    pop bc
    ret
Rnd1: .byte 0

Random2:
    push bc
    ld b,a
    ld a,(RndA)
    inc a
    ld (RndA),a
    add b
    pop bc
    ret
RndA: .byte 0