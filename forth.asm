; FORTH program for arduino
; Arduino nano is the prefered target, and so this will be programmed with that in mind
; This is my first assembly program, so comments are going to be 90% of it

.device ATmega328p

; Setting values
; RAMEND part of .inc file
.equ RAMEND = 0x08FF ; 2KB RAM. This will have to change
.equ RSPSTART = RAMEND
.equ RETURNSIZE = 0x0020
.equ PSPSTART = RAMEND - RETURNSIZE
.equ SRAM_START = 0x0100

.equ SPL = 0x3D
.equ SPH = 0x3E

.equ SMCR = 0x53
.equ SE  = 0
.equ SM0 = 1
.equ SM1 = 2
.equ SM2 = 3

.equ UDR0   = 0xC6
.equ UCSR0A = 0xC0
.equ UCSR0B = 0xC1
.equ UCSR0C = 0xC2
.equ UBRR0L = 0xC4
.equ UBRR0H = 0xC5

.equ UCSZ00 = 1
.equ UCSZ01 = 2
.equ TXEN0  = 3
.equ RXEN0  = 4
.equ UDRE0  = 5
.equ TXCIE0 = 6
.equ RXCIE0 = 7

.equ F_CPU = 16000000
.equ BAUD  = 9600
.equ UBRR_VALUE =(F_CPU / (16 * BAUD)) - 1 ; 103 for 9600 baud @ 16MHz

.macro rpush
	; load rsp into Z
	lds r30, rsp
	lds r31, rsp+1
	; store the low, then high of the given registers onto the stack
	st -Z, @0 ; low
	st -Z, @1 ; high
	; store the new rsp into Z
	sts rsp, r30
	sts rsp+1, r31
.endmacro

.macro rpop
	; load rsp into Z
	lds r30, rsp
	lds r31, rsp+1
	; pop high, then low
	ld @1, Z+
	ld @0, Z+
	; store the new rsp from Z
	sts rsp, r30
	sts rsp+1, r31
.endmacro

.dseg ; This can produce my starting words and code fields
.org SRAM_START
	inpointer:
		.byte 2
	textBuffer:
		.byte 32
	; What about some variables? These need to be stored in data
	; Good example layout of primitive
	parsepointer:
		.byte 2
	latestlink:
		.byte 2
	rsp:
		.byte 2
; First Word -- find '
	link:
		.byte 2
	length:
		.byte 1
	name:
		.byte 1
	flags:
		.byte 1
	code:
		.byte 2
; Second Word -- exec
	link2:
		.byte 2
	length2:
		.byte 1
	name2:
		.byte 4
	flags2:
		.byte 1
	code2:
		.byte 2
; Third Word -- emit
	link3:
		.byte 2
	length3:
		.byte 1
	name3:
		.byte 4
	flags3:
		.byte 1
	code3:
		.byte 2
; Fourth Word -- hello
	link4:
		.byte 2
	length4:
		.byte 1
	name4:
		.byte 5
	flags4:
		.byte 1
	code4:
		.byte 2
; Fifth Word -- line
	link5:
		.byte 2
	length5:
		.byte 1
	name5:
		.byte 4
	flags5:
		.byte 1
	code5:
		.byte 2
; Sixth Word -- interpreter loop. This is the first compiled word
	link6:
		.byte 2
	length6:
		.byte 1
	name6:
		.byte 1
	flag6:
		.byte 1
	code6:
		.byte 10 ; 2 for each word -- start, line, find, exec, end
		; other than the first two of code6, this is technically into the parameter field
; I need one more word -- the endtoploop word
	link7:
		.byte 2
	length7:
		.byte 1
	name7:
		.byte 1
	flag7:
		.byte 1
	code7:
		.byte 2

; Different data array
.cseg
; Need to set registers to point at the correct values in data memory
; general purpose data memory starts at
.org 0x0000
	rjmp boot ; boot up
; other .orgs for interrupts
.org 0x0024 ; uart interrupt: interrupt vector 18
	rjmp input

boot:	
	; Start PSP at a different value other than the end of RAM
	ldi r16, low(PSPSTART)
	out SPL, r16
	ldi r16, high(PSPSTART)
	out SPH, r16

	; Initializing RSP
	ldi r16, low(RSPSTART)
	sts rsp, r16
	ldi r16, high(RSPSTART)
	sts rsp+1, r16

	; starting UART for key and emit
	rcall start_uart
	; enable global interrupts for keying in input
	sei

	; enable sleep  ->  idle mode
	ldi r16, (1<<SE)
	sts SMCR, r16

	; Initalize inpointer
	ldi r16, low(textBuffer)
	sts inpointer, r16
	ldi r16, high(textBuffer)
	sts inpointer+1, r16

	; Initialize parsepointer
	ldi r16, low(textBuffer)
	sts parsepointer, r16
	ldi r16, high(textBuffer)
	sts parsepointer+1, r16

	; Initialize latestlink
	ldi r16, low(length7)
	sts latestlink, r16
	ldi r16, high(length7)
	sts latestlink+1, r16


	; loading primitives
	; find
	ldi r16, 1      ; Load length
	sts length, r16
	ldi r16, 39     ; Load name -- 39 is '
	sts name, r16
	; link zeroed -- I will use this for error checking in the future, but for now
	; it is fine
	ldi r16, 0
	sts link, r16
	ldi r16, 0
	sts link+1, r16
	ldi r16, low(find) ; Load Code Field
	sts code, r16
	ldi r16, high(find)
	sts code+1, r16	

	; exec
	ldi r16, low(length) ; Load link -- pointing at previous word's length
	sts link2, r16
	ldi r16, high(length)
	sts link2+1, r16
	ldi r16, 4           ; Load length
	sts length2, r16
	ldi r16, 'e'         ; Load name
	sts name2, r16
	ldi r16, 'x'
	sts name2+1, r16
	ldi r16, 'e'
	sts name2+2, r16
	ldi r16, 'c'
	sts name2+3, r16
	ldi r16, low(exec)   ; Load code field
	sts code2, r16
	ldi r16, high(exec)
	sts code2+1, r16

	; emit
	ldi r16, low(length2) ; Load link -- pointing at previous word's length
	sts link3, r16
	ldi r16, high(length2)
	sts link3+1, r16
	ldi r16, 4            ; Loading length
	sts length3, r16
	ldi r16, 'e'          ; Loading name
	sts name3, r16
	ldi r16, 'm'
	sts name3+1, r16
	ldi r16, 'i'
	sts name3+2, r16
	ldi r16, 't'
	sts name3+3, r16
	ldi r16, low(emit)    ; Loading code field
	sts code3, r16
	ldi r16, high(emit)
	sts code3+1, r16

	; hello
	ldi r16, low(length3) ; Load link
	sts link4, r16
	ldi r16, high(length3)
	sts link4+1, r16
	ldi r16, 5            ; Loading length
	sts length4, r16
	ldi r16, 'h'          ; Loading Name
	sts name4, r16
	ldi r16, 'e'
	sts name4+1, r16
	ldi r16, 'l'
	sts name4+2, r16
	ldi r16, 'l'
	sts name4+3, r16
	ldi r16, 'o'
	sts name4+4, r16
	ldi r16, low(hello)   ; Loading code field
	sts code4, r16
	ldi r16, high(hello)
	sts code4+1, r16

	; line
	ldi r16, low(length4)
	sts link5, r16
	ldi r16, high(length4)
	sts link5+1, r16
	ldi r16, 4
	sts length5, r16
	ldi r16, 'l'
	sts name5, r16
	ldi r16, 'i'
	sts name5+1, r16
	ldi r16, 'n'
	sts name5+2, r16
	ldi r16, 'e'
	sts name5+3, r16
	ldi r16, low(line)
	sts code5, r16
	ldi r16, high(line)
	sts code5+1, r16

	; interpreter loop!
	ldi r16, low(length5)
	sts link6, r16
	ldi r16, high(length5)
	sts link6+1, r16
	ldi r16, 1
	sts length6, r16
	ldi r16, '$'
	sts name6, r16
	;longer code field
	ldi r16, low(entertoploop)
	sts code6, r16
	ldi r16, high(entertoploop)
	sts code6+1, r16
	ldi r16, low(code5) ; line's code field
	sts code6+2, r16
	ldi r16, high(code5)
	sts code6+3, r16
	ldi r16, low(code) ; find's code field
	sts code6+4, r16
	ldi r16, high(code)
	sts code6+5, r16
	ldi r16, low(code2) ; exec's code field
	sts code6+6, r16
	ldi r16, high(code2)
	sts code6+7, r16
	ldi r16, low(code7) ; end's code field
	sts code6+8, r16
	ldi r16, high(code7)
	sts code6+9, r16

	; end top loop
	ldi r16, low(length6)
	sts link7, r16
	ldi r16, high(length6)
	sts link7+1, r16
	ldi r16, 1
	sts length7, r16
	ldi r16, ')'
	sts name7, r16
	; back to short code fields
	ldi r16, low(endtoploop)
	sts code7, r16
	ldi r16, high(endtoploop)
	sts code7+1, r16
	
	; Since next isnt pointing at anything usable yet
;	rjmp loop ; Sleep until interrupt

	ldi r24, 'o'
	rcall putchar
	ldi r24, 'k'
	rcall putchar
	ldi r24, 10
	rcall putchar
;	rjmp loop
	; This needs some initial condition to start. Where is the code field?
	; Set code field as X, since that is where the program will actually start
	ldi r26, low(code6+2)
	ldi r27, high(code6+2)
	rjmp entertoploop

next:
	ld r26, Y+ ; Load (Y) to X: low then high
	ld r27, Y+

	; Incrementing IP is done with post-increment

	ld r30, X+ ; Load (X) to Z: low then high
	ld r31, X+
	ijmp       ; indirect jump to Z

line:
; goes to sleep until input is given -- checks to see if the input is /r, then resets
; inpointer and jumps to next
	sleep

	cpi r17, 13 ; r17 was set to the inputed letter in input
	breq resetline
	
	rjmp line
resetline:
	ldi r16, low(textBuffer)
	sts inpointer, r16
	ldi r16, high(textBuffer)
	sts inpointer+1, r16

	rjmp next

; This will be for the top level program. This will not push any IP onto the return stack
; since there is not program to return to
; this will be used for the interpreter program, since I am writing it in threaded code
entertoploop:
	; This needs to RSP, since 
	movw r28, r26 ; Y <- X to make next point at the threaded code of the top word
	sbiw r26, 2 ; needed to correct offset of X to point at code field again
	rpush r26, r27 ; push X, since X points at top word

	; Now the IP points at the threaded code of the word, and X is saved

	rjmp next

endtoploop:
;	X is the code field
;	X is overwritten in some programs (find)
;	X will be on the return stack

	rpop r26, r27 ; pops X from stack
	ld r30, X+ ; Load (X) into Z -- this will point at the first of the threaded code
	ld r31, X+

	; jumping to that first of the threaded code will start the word
	; since this ends the top definition, it will start the loop
	; after jumping to the start, the enter will push X onto the return stack,
	; will set IP as the threaded code, and will jump to next
	; this program will be called by next. Popping X will allow it to restart
	ijmp
	

enter:
	; I need to switch this to RSP instead of PSP
	rpush r28, r29 ; push Y
	; adiw r26, 2 ; X += 2 (This was done in next using post increment)
	movw r28, r26 ; Y <- X
	rjmp next

exit:
	; Switch to RSP instead of PSP
	rpop r28, r29 ; pop Y -- opposite order from how it was pushed
	rjmp next

; ---------------------
emit:
	pop r24 ; clear high
	pop r24 ; use low
	rcall putchar

	rjmp next

hello:
	ldi r24, 10
	rcall putchar
	ldi r24, 'H'
	rcall putchar
	ldi r24, 'e'
	rcall putchar
	ldi r24, 'l'
	rcall putchar
	ldi r24, 'l'
	rcall putchar
	ldi r24, 'o'
	rcall putchar
	ldi r24, 10
	rcall putchar

	rjmp next

exec:
; pops the top of the stack and uses it as the execution address
	pop r31 ; high, then
	pop r30 ; low

	ld r20, Z+
	ld r21, Z

	movw r30, r20

	ijmp ; indirect jump to Z -- now the execution token
	

find:
    ; Load current link into r18:r19
    lds     r18, latestlink
    lds     r19, latestlink+1

    ; Load buffer pointer into r20:r21
    lds     r20, inpointer
    lds     r21, inpointer+1

findloop:
    ; Put current link into Z pointer
    movw    r30, r18           ; Z = link

    ; Copy buffer pointer into X pointer
    movw    r26, r20           ; X = buffer

    ; -------------------------------------
    ; Load length byte of dictionary name
    ld      r16, Z+            ; r16 = length, Z points to first char of name

checkstring:
    ; If length is zero, we matched whole name
    tst     r16
    breq    matchfound

    ; Load one character from dictionary name
    ld      r22, Z+

    ; Load one character from buffer string
    ld      r23, X+

    ; Compare characters
    cp      r22, r23
    brne    nomatch            ; mismatch -> go to next link

    ; Decrement length and loop
    dec     r16
    rjmp    checkstring

; -------------------------------------
; SUCCESS: names matched
matchfound:
    ; At this point Z points to flags field
    ; (because we consumed length + name bytes)
	; RIGHT HERE I ALSO NEED TO INCREMENT INPOINTER TO THE NEXT TOKEN, SKIPPING THE SPACE
	; actually, find only deals with a single word at a time
	; maybe I can loop externally to call find on every next word
	; or maybe I can make line return the buffer unless it is cleared
	; ADD IN PARSEPOINTER FOR THIS
    adiw r30, 1
    push r30 ; low, then
    push r31 ; high
    rjmp next ; returning to the running program

; -------------------------------------
; FAIL: names did not match, follow link
nomatch:
    ; reload original link pointer
    ; link points at length in the word, so I need pre-decrement
    movw    r30, r18           ; Z = current node again
    ld      r19, -Z            ; load previous link high
    ld      r18, -Z            ; load previous link low
    rjmp    findloop           ; try again with next dictionary entry

loop:
	sleep
	rjmp loop

putchar:
wait_UDRE:
	lds r16, UCSR0A
	sbrs r16, UDRE0 ; skip the next instruction if the bit is set or not
	rjmp wait_UDRE

	sts UDR0, r24
	ret

; Since r17 is used here, I cannot use it elsewhere
input: ; Takes input over UART
	lds r17, UDR0
	push r30 ; low(Z)
	push r31 ; high(Z)

	;load pointer to buffer
	lds r30, inpointer     ; low(Z)
	lds r31, (inpointer+1) ; high(Z)

	; Write r17 to where we are pointing
	st Z, r17

	mov r24, r17
	rcall putchar

	;increment buffer
	adiw r30, 1
	sts inpointer, r30
	sts (inpointer+1), r31
	
	pop r31
	pop r30
	reti

; ---------------------

start_uart:
	; baud rate
	ldi r16, low(UBRR_VALUE)
	sts UBRR0L, r16
	ldi r16, high(UBRR_VALUE)
	sts UBRR0H, r16

	; enable: interrupt    receiver    transmitter
	ldi r16, (1<<RXCIE0) | (1<<RXEN0) | (1<<TXEN0)
	sts UCSR0B, r16

	; Set frame format -- 8 data bits, no parity, 1 stop bit
	ldi r16, (1<<UCSZ01) | (1<<UCSZ00)
	sts UCSR0C, r16
	ret
