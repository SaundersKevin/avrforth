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

.macro dpush; 0, 1
	push @0 ; push low
	push @1 ; push high
.endmacro

.macro dpop ; 0, 1
	pop @1 ; pop high
	pop @0 ; pop low
.endmacro

.dseg ; This can produce my starting words and code fields
.org SRAM_START
	state:
		.byte 2
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
	here:
		.byte 2
	rsp:
		.byte 2
; First Word -- find (')
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
	flag2:
		.byte 1
	code2:
		.byte 2
; Third Word -- emit
	link3:
		.byte 2
	length3:
		.byte 1
	name3:
		.byte 1
	flag3:
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
	flag4:
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
	flag5:
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
		.byte 36 ; 2 for each word -- line find state @ ifc exec end then here @ ! end
				 ; 26
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
; 8th -- dup
	link8:
		.byte 2
	length8:
		.byte 1
	name8:
		.byte 3
	flag8:
		.byte 1
	code8:
		.byte 2
; 9th -- add (+)
	link9:
		.byte 2
	length9:
		.byte 1
	name9:
		.byte 1
	flag9:
		.byte 1
	code9:
		.byte 2
; 10th -- one (1)
	link10:
		.byte 2
	length10:
		.byte 1
	name10:
		.byte 1
	flag10:
		.byte 1
	code10:
		.byte 2
; 11th -- state
	link11:
		.byte 2
	length11:
		.byte 1
	name11:
		.byte 5
	flag11:
		.byte 1
	code11:
		.byte 2
; 12th -- fetch (@)
	link12:
		.byte 2
	length12:
		.byte 1
	name12:
		.byte 1
	flag12:
		.byte 1
	code12:
		.byte 2
; 13th -- store (!)
	link13:
		.byte 2
	length13:
		.byte 1
	name13:
		.byte 1
	flag13:
		.byte 1
	code13:
		.byte 2
; 14th -- here
	link14:
		.byte 2
	length14:
		.byte 1
	name14:
		.byte 4
	flag14:
		.byte 1
	code14:
		.byte 2
; 15th -- if_compiled (ifc) | SHOULD NEVER BE CALLED IN INTERPRETER |
	link15:
		.byte 2
	length15:
		.byte 1
	name15:
		.byte 3
	flag15:
		.byte 1
	code15:
		.byte 2
; 16th -- subtract (-)
	link16:
		.byte 2
	length16:
		.byte 1
	name16:
		.byte 1
	flag16:
		.byte 1
	code16:
		.byte 2
; 17th -- latest
	link17:
		.byte 2
	length17:
		.byte 1
	name17:
		.byte 6
	flag17:
		.byte 1
	code17:
		.byte 2
; 18th -- or
	link18:
		.byte 2
	length18:
		.byte 1
	name18:
		.byte 2
	flag18:
		.byte 1
	code18:
		.byte 2
; 19th -- !c
	link19:
		.byte 2
	length19:
		.byte 1
	name19:
		.byte 2
	flag19:
		.byte 1
	code19:
		.byte 2
; 20th -- @c
	link20:
		.byte 2
	length20:
		.byte 1
	name20:
		.byte 2
	flag20:
		.byte 1
	code20:
		.byte 2
; 21st -- exit_compiled (exc)
	link21:
		.byte 2
	length20:
		.byte 1
	name21:
		.byte 3
	flag21:
		.byte 1
	code21:
		.byte 2
; 22nd -- enter (:)

	dataspace:
		.byte 1
; Different data array
.cseg
; Need to set registers to point at the correct values in data memory
; general purpose data memory starts at
.org 0x0000
	rjmp boot ; boot up
.org 0x0001
	rjmp boot
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

	; enable sleep  ->  idle mode
	ldi r16, (1<<SE), (0<<SM0), (0<<SM1), (0<<SM2)
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

	; textBuffer needs to start with <CR> for line
	ldi r16, 13
	sts textBuffer, r16

	; loading primitives
	; find
	ldi r16, 1      ; Load length
	sts length, r16
	ldi r16, 39     ; Load name -- 39 is '
	sts name, r16
	; link zeroed -- I will use this for error checking in the future
	ldi r16, 0
	sts link, r16
	ldi r16, 0
	sts link+1, r16
	ldi r20, 0
	sts flags, r20
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
	sts flag2, r20
	ldi r16, low(exec)   ; Load code field
	sts code2, r16
	ldi r16, high(exec)
	sts code2+1, r16

	; emit
	ldi r16, low(length2) ; Load link -- pointing at previous word's length
	sts link3, r16
	ldi r16, high(length2)
	sts link3+1, r16
	ldi r16, 1            ; Loading length
	sts length3, r16
	ldi r16, '.'          ; Loading name
	sts name3, r16
	sts flag3, r20
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
	sts name4+3, r16 ; r16 is still l
	ldi r16, 'o'
	sts name4+4, r16
	sts flag4, r20
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
	sts flag5, r20
	ldi r16, low(line)
	sts code5, r16
	ldi r16, high(line)
	sts code5+1, r16

	; interpreter loop!
	; line find dup 1 sub @c state @c or if exec endtoploop then here @ ! endtoploop
	; or I could do nand instead of and and swap the compiler and interpreter spots
	ldi r16, low(length5)
	sts link6, r16
	ldi r16, high(length5)
	sts link6+1, r16
	ldi r16, 1
	sts length6, r16
	ldi r16, '$'
	sts name6, r16
	sts flag6, r20
	;longer code field
	ldi r16, low(entertoploop)
	sts code6, r16
	ldi r16, high(entertoploop)
	sts code6+1, r16
	ldi r16, low(code5) ; line's code field
	sts code6+2, r16
	ldi r16, high(code5)
	sts code6+3, r16
	ldi r16, low(code)  ; find's code field
	sts code6+4, r16
	ldi r16, high(code)
	sts code6+5, r16
	; dup -- get the execution pointer again
	ldi r16, low(code8)
	sts code6+6, r16
	ldi r16, high(code8)
	sts code6+7, r16
	; 1 -- put one
	ldi r16, low(code10)
	sts code6+8, r16
	ldi r16, high(code10)
	sts code6+9, r16
	; sub -- move exec pointer to flags field
	ldi r16, low(code16)
	sts code6+10, r16
	ldi r16, high(code16)
	sts code6+11, r16
	; @c -- fetch flag
	ldi r16, low(code20)
	sts code6+12, r16
	ldi r16, high(code20)
	sts code6+13, r16
	ldi r16, low(code11)  ; state
	sts code6+14, r16
	ldi r16, high(code11)
	sts code6+15, r16
	ldi r16, low(code20)	; @c
	sts code6+16, r16
	ldi r16, high(code20)
	sts code6+17, r16
	; or
	ldi r16, low(code18)
	sts code6+18, r16
	ldi r16, high(code18)
	sts code6+19, r16
	ldi r16, low(code15)	; ifc
	sts code6+20, r16
	ldi r16, high(code15)
	sts code6+21, r16
; HERE IS THEN ADDRESS
	ldi r16, low(code6+28)
	sts code6+22, r16
	ldi r16, high(code6+28)
	sts code6+23, r16
	ldi r16, low(code2) ; exec's code field
	sts code6+24, r16
	ldi r16, high(code2)
	sts code6+25, r16
	ldi r16, low(code7) ; end's code field
	sts code6+26, r16
	ldi r16, high(code7)
	sts code6+27, r16
;	then
	ldi r16, low(code14) ; here
	sts code6+28, r16
	ldi r16, high(code14)
	sts code6+29, r16
	ldi r16, low(code12) ; @
	sts code6+30, r16
	ldi r16, high(code12)
	sts code6+31, r16
	ldi r16, low(code13) ; store
	sts code6+32, r16
	ldi r16, high(code13)
	sts code6+33, r16
	ldi r16, low(code7) ; end
	sts code6+34, r16
	ldi r16, high(code7)
	sts code6+35, r16

	; end top loop
	ldi r16, low(length6)
	sts link7, r16
	ldi r16, high(length6)
	sts link7+1, r16
	ldi r16, 1
	sts length7, r16
	ldi r16, ')'
	sts name7, r16
	sts flag7, r20
	; back to short code fields
	ldi r16, low(endtoploop)
	sts code7, r16
	ldi r16, high(endtoploop)
	sts code7+1, r16

	; dup
	ldi r16, low(length7)
	sts link8, r16
	ldi r16, high(length7)
	sts link8+1, r16
	ldi r16, 3
	sts length8, r16
	ldi r16, 'd'
	sts name8, r16
	ldi r16, 'u'
	sts name8+1, r16
	ldi r16, 'p'
	sts name8+2, r16
	sts flag8, r20
	ldi r16, low(dup)
	sts code8, r16
	ldi r16, high(dup)
	sts code8+1, r16

	; add
	ldi r16, low(length8)
	sts link9, r16
	ldi r16, high(length8)
	sts link9+1, r16
	ldi r16, 1
	sts length9, r16
	ldi r16, '+'
	sts name9, r16
	sts flag9, r20
	ldi r16, low(add_stack)
	sts code9, r16
	ldi r16, high(add_stack)
	sts code9+1, r16

	; one
	ldi r16, low(length9)
	sts link10, r16
	ldi r16, high(length9)
	sts link10+1, r16
	ldi r16, 1
	sts length10, r16
	ldi r16, '1'
	sts name10, r16
	sts flag10, r20
	ldi r16, low(one)
	sts code10, r16
	ldi r16, high(one)
	sts code10+1, r16

	; state
	ldi r16, low(length10)
	sts link11, r16
	ldi r16, high(length10)
	sts link11+1, r16
	ldi r16, 5
	sts length11, r16
	ldi r16, 's'
	sts name11, r16
	ldi r16, 't'
	sts name11+1, r16
	ldi r16, 'a'
	sts name11+2, r16
	ldi r16, 't'
	sts name11+3, r16
	ldi r16, 'e'
	sts name11+4, r16
	sts flag11, r20
	ldi r16, low(point_state)
	sts code11, r16
	ldi r16, high(point_state)
	sts code11+1, r16

	; fetch (@)
	ldi r16, low(length11)
	sts link12, r16
	ldi r16, high(length11)
	sts link12+1, r16
	ldi r16, 1
	sts length12, r16
	ldi r16, '@'
	sts name12, r16
	sts flag12, r20
	ldi r16, low(fetch)
	sts code12, r16
	ldi r16, high(fetch)
	sts code12+1, r16

	; store (!)
	ldi r16, low(length12)
	sts link13, r16
	ldi r16, high(length12)
	sts link13+1, r16
	ldi r16, 1
	sts length13, r16
	ldi r16, '!'
	sts name13, r16
	sts flag13, r20
	ldi r16, low(store)
	sts code13, r16
	ldi r16, high(store)
	sts code13+1, r16

	; here
	ldi r16, low(length13)
	sts link14, r16
	ldi r16, high(length13)
	sts link14+1, r16
	ldi r16, 4
	sts length14, r16
	ldi r16, 'h'
	sts name14, r16
	ldi r16, 'e'
	sts name14+1, r16
	ldi r16, 'r'
	sts name14+2, r16
	ldi r16, 'e'
	sts name14+3, r16
	sts flag14, r20
	ldi r16, low(point_here)
	sts code14, r16
	ldi r16, high(point_here)
	sts code14+1, r16

	; ifc
	ldi r16, low(length14)
	sts link15, r16
	ldi r16, high(length14)
	sts link15+1, r16
	ldi r16, 3
	sts length15, r16
	ldi r16, 'i'
	sts name15, r16
	ldi r16, 'f'
	sts name15+1, r16
	ldi r16, 'c'
	sts name15+2, r16
	sts flag15, r20
	ldi r16, low(if_program)
	sts code15, r16
	ldi r16, high(if_program)
	sts code15+1, r16

	; subtract
	ldi r16, low(length15)
	sts link16, r16
	ldi r16, high(length15)
	sts link16+1, r16
	ldi r16, 1
	sts length16, r16
	ldi r16, '-'
	sts name16, r16
	sts flag16, r20
	ldi r16, low(sub_stack)
	sts code16, r16
	ldi r16, high(sub_stack)
	sts code16+1, r16

	; latest
	ldi r16, low(length16)
	sts link17, r16
	ldi r16, high(length16)
	sts link17+1, r16
	ldi r16, 6
	sts length17, r16
	ldi r16, 'l'
	sts name17, r16
	ldi r16, 'a'
	sts name17+1, r16
	ldi r16, 't'
	sts name17+2, r16
	ldi r16, 'e'
	sts name17+3, r16
	ldi r16, 's'
	sts name17+4, r16
	ldi r16, 't'
	sts name17+5, r16
	sts flag17, r20
	ldi r16, low(point_latest)
	sts code17, r16
	ldi r16, high(point_latest)
	sts code17+1, r16

	; or
	ldi r16, low(length17)
	sts link18, r16
	ldi r16, high(length17)
	sts link18+1, r16
	ldi r16, 2
	sts length18, r16
	ldi r16, 'o'
	sts name18, r16
	ldi r16, 'r'
	sts name18+1, r16
	sts flag18, r20
	ldi r16, low(or_stack)
	sts code18, r16
	ldi r16, high(or_stack)
	sts code18+1, r16

	; !c
	ldi r16, low(length18)
	sts link19, r16
	ldi r26, high(length18)
	sts link19+1, r16
	ldi r16, 2
	sts length19, r16
	ldi r16, '!'
	sts name19, r16
	ldi r16, 'c'
	sts name19+1, r16
	sts flag19, r20
	ldi r16, low(storec)
	sts code19, r16
	ldi r16, high(storec)
	sts code19+1, r16

	; @c
	ldi r16, low(length19)
	sts link20, r16
	ldi r16, high(length19)
	sts link20+1, r16
	ldi r16, 2
	sts length20, r16
	ldi r16, '@'
	sts name20, r16
	ldi r16, 'c'
	sts name20+1, r16
	sts flag20, r20
	ldi r16, low(fetchc)
	sts code20, r16
	ldi r16, high(fetchc)
	sts code20+1, r16

	; VARIABLES -----

	; Initialize latestlink
	ldi r16, low(length20)
	sts latestlink, r16
	ldi r16, high(length20)
	sts latestlink+1, r16

	; here -- points at the start of open space
	ldi r16, low(dataspace)
	sts here, r16
	ldi r16, high(dataspace)
	sts here+1, r16

	ldi r16, 1
	sts state, r16
	sts state+1, r16

	; enable global interrupts for keying in input
	; This is done after all the loading up of things so nothings is
	; broken accidentally
;	ldi r24, 'I'
;	rcall putchar
	sei
	
	; Announcing that the device is ready for input
	ldi r24, 'o'
	rcall putchar
	ldi r24, 'k'
	rcall putchar
	ldi r24, 10
	rcall putchar
	ldi r24, 13
	rcall putchar

	; Set code field of interpreter as X, since that is where the program will actually start
	ldi r26, low(code6+2)
	ldi r27, high(code6+2)
	rjmp entertoploop

next:
;	ldi r24, 'N'
;	rcall putchar
	ld r26, Y+ ; Load (Y) to X: low then high
	ld r27, Y+

	; Incrementing IP is done with post-increment

	ld r30, X+ ; Load (X) to Z: low then high
	ld r31, X+
	ijmp       ; indirect jump to Z

line:
; goes to sleep until input is given -- checks to see if the input is /r, then resets
; inpointer and jumps to next
; idea for fixing interpreter
; I can have line return the buffer unless it is all the way consumed
; So, for example, I can check the most recent code looked at with parsepointer and 
; see if it is NULL. If it is not, I just return
; When actually taking input, I can set the next input at NULL when <CR> is found
;	check if parsepointer points to <CR>
;	ldi r24, 'L'
;	rcall putchar
	lds r26, parsepointer
	lds r27, parsepointer+1
	ld r16, X
	cpi r16, 13
	breq continueline ; If character is not equal to <CR>, return early
	cpi r16, 10
	breq continueline
	rjmp next

continueline:
;	ldi r24, 13
;	rcall putchar
;	ldi r24, 10
;	rcall putchar
;	reset inpointer
	ldi r16, low(textBuffer)
	sts inpointer, r16
	ldi r16, high(textBuffer)
	sts inpointer+1, r16
	
lineloop:
;	ldi r24, 'l'
;	rcall putchar
	sleep

	cpi r17, 13 ; r17 was set to the inputed letter in input
	breq resetline
	
	rjmp lineloop
	
resetline:
;	reset parsepointer before returning
	ldi r16, low(textBuffer-1)
	sts parsepointer, r16
	ldi r16, high(textBuffer-1)
	sts parsepointer+1, r16

	ldi r24, 10
	rcall putchar

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
	
enter_program:
	in psuedo code
	get here
	write latest link and increment here
	get here and dup
	increment top here
	write next token using top here while counting
	write count to bottom here
	here now points after name -- get here
	write zeros for flag
	increment here
	write enter pointer
	increment here
	change state

	compiler will now handle writing pointers, so rjmp next

enter:
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
	dpop r26,r27 ; move into X
	adiw r26, 48 ; display a single digit correctly
				 ; I need to make this display greater than 9, too
	mov r24, r26
	rcall putchar

	ldi r24, 10
	rcall putchar
	ldi r24, 13
	rcall putchar

	rjmp next

hello:
;	ldi r24, 10
;	rcall putchar
;	ldi r24, 13
;	rcall putchar
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
	ldi r24, 13
	rcall putchar

	rjmp next

point_state:
	ldi r16, low(state)
	ldi r18, high(state)
	dpush r16, r18 ; low, then high

	rjmp next

point_here:
	ldi r16, low(here)
	ldi r18, high(here)
;	Should I do it this way?
;	This would make execution a little faster
;	But I wouln't be able to change here this way
;	ldi r26, low(here)
;	ldi r27, high(here)
;	ld r16, X+
;	ld r18, X
	dpush r16, r18

	rjmp next

point_latest:
;	ldi r24, 'P'
;	rcall putchar
	ldi r16, low(latestlink)
	ldi r18, high(latestlink)

	dpush r16, r18

	rjmp next

fetch:
	; pops into X, then loads
	dpop r26, r27 ; low, then high

	; fetches cell
	ld r16, X+ ; numbers are stored low, then high
	ld r18, X

	; pushes the fetched info back onto stack
	dpush r16, r18

	rjmp next

store:
	dpop r26, r27 ; address in X

	dpop r30, r31 ; value in Z

	st X+, r30
	st X, r31

	rjmp next

fetchc:
	dpop r26, r27

	ld r16, X
	ldi r18, 0

	dpush r16, r18

	rjmp next

storec:
	dpop r26, r27

	dpop r30, r31

	st X, r30

	rjmp next

if_program:
	;pop the top of the stack
	;cannot be to X, since that is needed if branch
	dpop r30, r31
	
	;using or will prevent problems with only part being zeroed
	or r30, r31
	tst r30
	breq if_branch

	; skip the address for branching, then continue with next
	adiw r28, 2
	rjmp next

if_branch:
	;fetch next values from code field into Y (IP) -- this will skip execution to
	;where specified
	;X is already pointing at the correct spot
	ld r30, Y+
	ld r31, Y

	movw r28, r30

	; resume execution
	rjmp next

dup:
	dpop r26, r27
	dpush r26, r27
	dpush r26, r27

	rjmp next

add_stack:
	dpop r26, r27

	dpop r30, r31
	
	add r26, r30 ; I think this adds the full word
	adc r27, r31
	
	dpush r26, r27

	rjmp next

sub_stack:
	dpop r26, r27

	dpop r30, r31

	sub r30, r26
	sbc r31, r27

	dpush r30, r31

	rjmp next

one:
	ldi r16, 1
	ldi r18, 0
	dpush r16, r18 ; high is null

	rjmp next

or_stack:
	dpop r26, r27

	dpop r30, r31

	or r26, r30
	or r27, r31

	dpush r26, r27

	rjmp next

exec:
; pops the top of the stack and uses it as the execution address
	dpop r26, r27 ; data pop into X

	ld r20, X+
	ld r21, X+
; If I pop to X instead of Z then I could eliminate this command -- can I do that?
	movw r30, r20

	ijmp ; indirect jump to Z -- now the execution token
	

find:
;	ldi r24, 'F'
;	rcall putchar
    ; Load current link into r18:r19
    lds     r18, latestlink
    lds     r19, latestlink+1

    ; Load buffer pointer into r20:r21
    lds     r24, parsepointer
    lds     r25, parsepointer+1

	adiw r24, 1
	; I might move the pointers over to Z and X here so I can check and increment for spaces

findloop:
;	ldi r24, 'f'
;	rcall putchar
    ; Put current link into Z pointer
    movw    r30, r18           ; Z = link

    ; Copy buffer pointer into X pointer
    movw    r26, r24           ; X = buffer

    ; -------------------------------------
    ; Load length byte of dictionary name
    ld      r16, Z+            ; r16 = length, Z points to first char of name

checkstring:
    ; Load one character from buffer string
    ld      r23, X+

	; If length is zero, we matched whole name
    tst     r16
    breq    lengthend

    ; Load one character from dictionary name
    ld      r22, Z+

    ; Compare characters
    cp      r22, r23
    brne    nomatch            ; mismatch -> go to next link

    ; Decrement length and loop
    dec     r16
    rjmp    checkstring

; -------------------------------------
; SUCCESS: names matched
lengthend:
    ; At this point Z points to flags field
    ; (because we consumed length + name bytes)
	; I check to make sure the full token is consumed, then write out the new
	; parsepointer, push the exec token onto the stack and return
	cpi r23, 13
	breq matchfound
	cpi r23, ' '
	breq matchfound
	rjmp nomatch
matchfound:
	sbiw r26, 1
	sts parsepointer, r26 ; low
	sts parsepointer+1, r27 ; high
	
    adiw r30, 1
    dpush r30, r31 ; push to data stack
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
; I need to add in support for backspacing
input: ; Takes input over UART
	lds r17, UDR0
	push r30 ; low(Z)
	push r31 ; high(Z)

	;load pointer to buffer
	lds r30, inpointer     ; low(Z)
	lds r31, (inpointer+1) ; high(Z)

	cpi r17, 127 ; 8 is backspace character
	breq backspace
	cpi r17, 8
	breq backspace

	; Write r17 to where we are pointing
	st Z, r17

	; r24 is only used by putchar
	mov r24, r17
	rcall putchar

	;increment buffer
	adiw r30, 1
	sts inpointer, r30
	sts (inpointer+1), r31
	
	pop r31
	pop r30
	reti

backspace:
	; print backspace
	ldi r24, 8
	rcall putchar
	ldi r24, ' '
	rcall putchar
	ldi r24, 8
	rcall putchar
	; move buffer pointer back
	sbiw r30, 1
	sts inpointer, r30
	sts inpointer+1, r31

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
