@ ECE-372, Project 1, 07/07/2015
@ Casimir Sowinski
@ Talking heart rate monitor

.text
.global _start
.global INT_DIRECTOR

_start:
	@ Assign message length
	.EQU	LENGTH, 55
	.EQU	LENGTH_DEAD, 27
	.EQU	LENGTH_SONG, 100
	.EQU	OFFSET, 33
	.EQU	ASCII_NUM, 48		@ Value to add to number to get ASCII digits
	@----------------------------------------------------------------------------Init stack
	@ Set up SVC stack
	LDR		R13, =STACK1		@ Point to base of STACK1 for SVC mode
	ADD 	R13, R13, #0x1000	@ Point to top of STACK1
	@ Set up IRQ stack
	CPS 	#0x12 				@ Switch to IRQ mode
	LDR		R13, =STACK2 		@ Point to STACK2 for IRQ mode
	CPS 	#0x13 				@ Switch to SVC mode
	@-----------------------------------------------------------------------------Init INTC
	LDR		R1, =0x48200000		@ Base address for INTC
	MOV		R2, #0x02			@ Value to reset INTC
	STR		R2, [R1, #0x10]		@ Write to INTC config register
	@ Set up INTC for UART
	MOV 	R2, #0x4000			@ Value to unmask INTC_INT_46, UART5INT
	STR		R2, [R1, #0xA8] 	@ Write to INTC_MIR_CLEAR1 register
	@ Set up INTC for Timer2
	MOV		R2, #0x10			@ Value to unmask INTC_INT_68, Timer2 interrupt
	STR		R2, [R1, #0xC8]		@ Write to INTC_MIR_CLEAR2 register	
	@ Set up INTC for Button	
	MOV 	R2, #0x04			@ Value to unmask INTC_INT_98, GPIOINT1A
	STR 	R2, [R1, #0xE8]		@ Write to INTC_MIR_CLEAR3 register	
	@-----------------------------------------------------------------------------Init LEDs
	@ Set up LEDs - Init off
	LDR 	R0, =0x4804C000		@ Base address for GPIO1 registers	
	ADD 	R4, R0,#0x194		@ Address of GPIO1_SETDATAOUT register
	MOV 	R7, #0x01000000		@ Load value to turn on 1 LED
	STR 	R7, [R4]			@ Write to GPIO1_CLEARDATAOUT register
	@ Set up LEDs - Program GPIO1_21-24 as outputs
	ADD 	R1, R0,#0x0134 		@ Make GPIO1_OE register address
	LDR 	R6, [R1]			@ READ current GPIO1 Output Enable register
	LDR 	R7, =0xFE1FFFFF		@ Word to enable GPIO1_21-24 as outputs (0 enables)
	AND 	R6, R7, R6			@ Clear bits 21-24 (MODIFY)
	STR 	R6, [R1]			@ WRITE to GPIO1 Output Enable register
	@---------------------------------------------------------------------------Init Button
	@ Set up button, detect falling edge on GPIO1_31
	ADD 	R1, R0, #0x14C		@ Address for GPIO1_FALLINGDETECT register
	MOV 	R2, #0x80000000		@ Value for bit-31
	LDR 	R3, [R1] 			@ READ - contents of GPIO1_FALLINGDETECT register
	ORR		R3, R3, R2 			@ MOD - set bit-31
	STR 	R3, [R1]			@ WRITE - Store in address for bit-31
	@ Enable IRQs
	ADD 	R1, R0, #0x34		@ Address for GPIO1_IRQSTATUS_SET_0 register
	STR 	R2, [R1]			@ Enable GPIO_31 request on POINTERPEND1	
	@-----------------------------------------------------------------------------Init UART
	@ Turn on UART5 CLK
	LDR		R1, =0x44E00000		@ Base address for clock module peripheral register
	MOV		R3, #0x02			@ Value to turn on UART5 CLK
	STRB	R3, [R1, #0x38]		@ Write to CM_PER_UART5_CLKCTRL
	@ Change mode of pins - Map UART5 TxD, RxD, CTS, RTS to pins on P8
	MOV		R3, #0x04			@ Value for Mode-4
	LDR		R0, =0x44E108C0		@ Address for conf_lcd_data8
	STRH	R3, [R0]			@ Write value to conf_lcd_data8
	LDR		R0, =0x44E108C4		@ Address for conf_lcd_data9
	STRH	R3, [R0]			@ Write value to conf_lcd_data9
	MOV		R3, #0x06			@ Value for Mode-6 				
	LDR		R0, =0x44E108D8		@ Address for conf_lcd_data14
	STRH	R3, [R0]			@ Write value to conf_lcd_data14
	LDR		R0, =0x44E108DC		@ Address for conf_lcd_data15
	STRH	R3, [R0]			@ Write value to conf_lcd_data15
	@ Disable MDR register mode 
	LDR		R1, =0x481AA000		@ Base address for UART5
	MOV		R2, #0x07			@ Value to disable MDR1 register mode
	STR		R2, [R1, #0x20]		@ Write to MDR1 register
	@ Switch UART5 to Configuration mode A
	LDR		R1, =0x481AA000		@ Base address for UART5
	MOV		R2, #0x83			@ Value for no parity, one stop bit, and 8 data bits 
	STRB	R2, [R1, #0x0C]		@ Write to offset of LCR
	@ Set Baud rate  - 38.4 Kbps
	MOV		R2, #0x4E			@ Value for DLL, divisor low latch
	STRB	R2, [R1, #0x00]		@ Write to offset of DLL register
	MOV		R2, #0x00			@ Value for DHL, divisor high latch
	STRB	R2, [R1, #0x04]		@ Write to offset of DHL register
	@ Disable 
	MOV		R2, #0x80			@ Value to disable divisor latch enable
	STR		R2, [R1, #0x0C]		@ Write to LCR register - bit-7
	@ Set Baud rate - 16X mode
	MOV		R2, #0x00			@ Value for 16X divisor, resets whole register
	STRB	R2, [R1, #0x20]		@ Write to offset of MDR1 register
	@ Switch UART5 to Operational mode
	MOV		R2, #0x03			@ Value to return to Op mode, keep 8 data bits
	STRB	R2, [R1, #0x0C]		@ Write to offset of LCR
	@ Enable UART interrupts 
	LDR		R1, =0x481AA000		@ Base address for UART registers
	LDR     R2, [R1]			@ Load value
	BIC 	R2,	R2, #0x0A		@ Value to enable modem status reg. int. & THR
	STRB	R2, [R1, #0x04]		@ Write to offset of IER_UART	
	@ Disable FIFO 
	MOV		R2, #0x07			@ Value to disable DMA and FIFO 
	STR		R2, [R1, #0x08]		@ Write to offset of FCR register
	@ CARRIAGE RETURN
	MOV		R2, #0x0D			@   
	STRB	R2, [R1, #0x08]		@
	@----------------------------------------------------------------------------Init Timer
	@ Turn on Timer2 CLK
	MOV		R2, #0x02			@ Value to enable Timer2 CLK
	LDR		R1, =0x44E00080 	@ Address of CM_PER_TIMER2_CLKCTR
	STR		R2, [R1]			@ Turn on
	LDR		R1, =0x44E00508		@ Address of PRCMCLKSEL_TIMER2 register
	STR 	R2, [R1] 			@ Select 32.768 KHz CLK for Timer2
	@ Initialize Timer2 registers, with count, overflow INQ generation
	LDR		R1, =0x48040000		@ Base address for Timer2 registers
	MOV		R2, #0x01			@ Value to reset Timer2
	STR		R2, [R1, #0x10] 	@ Write to Timer2 CFG register
	MOV 	R2, #0x2 			@ Value to enable overflow interrupt
	STR 	R2, [R1, #0x2C]		@ Write to Timer2 IRQENABLE_SET
	@LDR 	R2, =0xFFFF8000		@ Count value for 1 second
	@LDR 	R2, =0xFFFD8000		@ Count value for 5 seconds
	@LDR 	R2, =0xFFFB0000		@ Count value for 10 seconds
	LDR 	R2, =0xFFF88000		@ Count value for 15 seconds
	STR		R2, [R1, #0x40]		@ Timer2 TLDR load register (Reload value)
	STR		R2, [R1, #0x3C]		@ Write to Timer2 TCRR count register	
	@ Start timer
	LDR		R1, =0x48040038		@ Address of Timer2 TCLR register
	MOV		R2, #0x03			@ Load value to auto reload timer and start
	STR		R2, [R1]			@ Write to TCLR register
	@---------------------------------------------------------------------------Enable IRQs
	@ Enable IRQ in CPSR
	MRS 	R3, CPSR			@ Copy CPSR to R3
	BIC 	R3, #0x80			@ Clear bit-7
	MSR 	CPSR_c, R3 			@ WB to CPSR
@---------------------------------------------------------------------------------Wait loop
LOOP:
	NOP
	B LOOP
@------------------------------------------------------------------------Interrupt director
INT_DIRECTOR: 
	STMFD 	SP!, {R0-R3, LR} 	@ Push registers on stack
CHECK_UART:
	@B 		SERVICE_TALKER
	@ Check for UART IRQ
	LDR		R0, =0x482000B8		@ Address of INTC_PENDING_IRQ1
	LDR		R1, [R0]			@ Read INTC_PENDING_IRQ1 register
 	TST		R1, #0x00004000		@ Test bit-14	
 	BEQ		CHECK_BUTTON		@ Interrupt not from UART, check button
	LDR		R0, =0x481AA000		@ Address of UART register
	LDR		R1, [R0, #0x08]		@ Offset of IIR_UART register
	TST		R1, #0x00			@ Test bit-0
	BEQ		SERVICE_TALKER		@ If from UART, service UART 
	BNE		RETURN_DIRECTOR		@ If not, clean up, return to wait LOOP		
CHECK_BUTTON:
	@ Check for Button IRQ
	LDR 	R0, =0x482000F8		@ Address of INTC_PENDING_IRQ3 register
	LDR 	R1, [R0] 			@ Read INTC_PENDING_IRQ3 register
	TST 	R1, #0x00000004 	@ Test bit-2	
	BEQ		CHECK_TIMER
	LDR 	R0, =0x4804C02C		@ Load GPIO1_IRQSTATUS_0 register address
	LDR 	R1, [R0] 			@ Read GPIO1_IRQSTATUS_0 register
	TST 	R1, #0x80000000 	@ Check if bit-14=1 (button pushed)
	@ BNE/BEQ
	BNE 	SERVICE_BUTTON		@ If bit-14=0 then go to BUTTON_SVC
	B	 	RETURN_DIRECTOR		@ If bit-14=0 then go back to wait loop
CHECK_TIMER:
	LDR		R1, =0x482000D8		@ Address of INTC_PENDING_IRQ2 register	
	LDR		R0, [R1]			@ Read value
	TST		R0, #0x10			@ Check if IRQ from Timer2
	BEQ		RETURN_DIRECTOR		@ No, return, Yes, check for overflow
	LDR		R1, =0x48040028		@ Address of Timer2 IRQSTATUS register
	LDR		R0, [R1]			@ Read value
	TST		R0, #0x2			@ Check bit 1
	BNE		SERVICE_TIMER		@ If overflow, then go to toggle LED	
	B 		RETURN_DIRECTOR		@ Redundant, but for adding more stuff later
RETURN_DIRECTOR: 
	LDR		R0, =0x48200048		@ Address of INTC_CONTROL register
	MOV		R1, #1				@ Value to clear bit-0
	STR		R1, [R0]			@ Write to INTC_CONTROL register
	LDMFD 	SP!, {R0-R3, LR} 	@ Pop stack
	SUBS 	PC, LR, #4 			@ Pass execution on to wait LOOP for now
@-------------------------------------------------------------------------------Handle UART
SERVICE_TALKER:
	STMFD	SP!, {R2-R5}		@ Save additional registers
	LDR		R0, =0x481AA000		@ Base address of UART registers
	LDR		R1, [R0, #0x18]		@ Offset for modem status register (MSR)
	TST		R1, #0x00000010		@ Test value for bit-4 - NCTS_STS
	BEQ 	NO_CTS				@ If no, then check THR
	LDR		R1, [R0, #0x14]		@ Offset for line status register (LSR)
	TST		R1, #0x20			@ Test value for bit-5 - TXFIFOE
	BEQ		RETURN_TALKER		@ If no, then exit and wait for THR:ready
	@BNE		SEND	
	BNE		DECIDE
DECIDE:	
	LDR		R9,	=FLAG_DEAD		@ Point to FLAG_DEAD
	LDR		R10, [R9]			@ Load value of DEAD_FLAG
	TST		R10, #1				@ See if DEAD_FLAG is set
	BEQ		SEND				@ Flag not set, go to normal SEND 
	BNE		SEND_DEAD			@ Flag set, go to SEND_DEAD	
NO_CTS:
	LDR		R1, [R0, #0x14]		@ Offset for line status register (LSR)
	TST		R1, #0x20			@ Test value for bit-5 - TXFIFOE
	BEQ		RETURN_TALKER		@ If no, then exit and wait for THR:ready	
	@ Else no CTS# but THR is asserted, disable interrupt on THR to prevent spinning while waiting for CTS#
	LDR		R4, =0x481AA004		@ Address of IER_UART register
	MOV		R5, #0x08			@ Value to mask THR
	STRB	R5, [R4]			@ Write to mask
	B		RETURN_TALKER		@ Exit to wait for CTS# interrupt	
	@ Unmask THR, send character, if ebd if nessage reset char count and disable UART interrupt
SEND:
	LDR		R4, =0x481AA004		@ Load ptr to IER register
	MOV		R5, #0x0A			@ Value to enable interrupt 
	STRB	R5, [R4]			@ Write to IER register
	LDR		R0, =CHAR_PTR		@ Pointer to pointer, stores current character to send
	LDR		R1, [R0]			@ Pointer to current character to send
	LDR		R2, =CHAR_COUNT		@ Pointer to character count, will count down
	LDR		R3, [R2]			@ Value of character count
	LDRB	R4, [R1], #1		@ Read character, increment pointer
	STR		R1, [R0]			@ Store incremented address back to CHAR_PTR location
	LDR		R5, =0x481AA000		@ Point at UART THR
	STRB	R4, [R5]			@ Write to THR
	SUBS	R3, R3, #1			@ Decrement character counter by 1
	STR		R3, [R2]			@ Store character value counter back to memory	
	BPL		RETURN_TALKER		@ Out of characters to send, return	
	@ Clean up
	LDR		R3, =MESSAGE		@ Reload pointer to MESSAGE 
	STR		R3, [R0]			@ Write to CHAR_PTR
	MOV		R3, #LENGTH			@ Load reset message length, LENGTH
	STR		R3, [R2]			@ Write back to CHAR_COUNT
	LDR		R0, =0x481AA004		@ Load address of IER
	LDRB	R1, [R0]			@ Load contents
	BIC		R1, R1, #0x0A		@ Clear bit-4 and bit-2
	STRB	R1, [R0]			@ Write back
	@ Clean up
	LDR		R3, =MESSAGE		@ Reload pointer to MESSAGE 
	STR		R3, [R0]			@ Write to CHAR_PTR
	MOV		R3, #LENGTH			@ Load reset message length, LENGTH
	STR		R3, [R2]			@ Write back to CHAR_COUNT
	LDR		R0, =0x481AA004		@ Load address of IER
	LDRB	R1, [R0]			@ Load contents
	BIC		R1, R1, #0x0A		@ Clear bit-4 and bit-2
	STRB	R1, [R0]			@ Write back
	B 		RETURN_TALKER		@ Exit
SEND_DEAD:
	LDR		R4, =0x481AA004		@ Load ptr to IER register
	MOV		R5, #0x0A			@ Value to enable interrupt 
	STRB	R5, [R4]			@ Write to IER register
	LDR		R0, =CHAR_PTR_DEAD	@ Pointer to pointer, stores current character to send
	@LDR		R0, =CHAR_PTR_SONG	@ Pointer to pointer, stores current character to send
	LDR		R1, [R0]			@ Pointer to current character to send
	LDR		R2, =CHAR_COUNT_DEAD@ Pointer to character count, will count down
	@LDR		R2, =CHAR_COUNT_SONG@ Pointer to character count, will count down
	LDR		R3, [R2]			@ Value of character count
	LDRB	R4, [R1], #1		@ Read character, increment pointer
	STR		R1, [R0]			@ Store incremented address back to CHAR_PTR location
	LDR		R5, =0x481AA000		@ Point at UART THR
	STRB	R4, [R5]			@ Write to THR
	SUBS	R3, R3, #1			@ Decrement character counter by 1
	STR		R3, [R2]			@ Store character value counter back to memory	
	BPL		RETURN_TALKER		@ Out of characters to send, return	
	@ Clean up
	LDR		R3, =MESSAGE_DEAD	@ Reload pointer to MESSAGE_DEAD 
	@LDR		R3, =MESSAGE_SONG	@ Reload pointer to MESSAGE 
	STR		R3, [R0]			@ Write to CHAR_PTR
	MOV		R3, #LENGTH_DEAD	@ Load reset message length, LENGTH_DEAD
	@MOV		R3, #LENGTH_SONG	@ Load reset message length, LENGTH_DEAD
	STR		R3, [R2]			@ Write back to CHAR_COUNT_DEAD
	LDR		R0, =0x481AA004		@ Load address of IER
	LDRB	R1, [R0]			@ Load contents
	BIC		R1, R1, #0x0A		@ Clear bit-4 and bit-2
	STRB	R1, [R0]			@ Write back
	B 		RETURN_TALKER		@ Exit
RETURN_TALKER:
	LDMFD	SP!, {R2-R5}		@ Pop TALKER registers
	LDMFD	SP!, {R0-R3, LR}	@ Pop registers
	SUBS	PC, LR, #4			@ Return to wait loop
@-----------------------------------------------------------------------------Handle button
SERVICE_BUTTON:
	LDR		R0, =0x4804C02C		@ Address of GPIO1_IRQSTATUS_0 register
	MOV 	R1, #0x80000000		@ Value to turn off GPIO1_14 Interrupt request (INTC too)
	STR 	R1, [R0]			@ Write to GPIO1_IRQSTATUS_0 register		
	@ Count button presses
	LDR		R5, =COUNT			@ Load address to COUNT
	LDR		R6, [R5]			@ Get value of COUNT
	ADD 	R6, R6, #1			@ Increment COUNT
	STR		R6, [R5]			@ Store value in COUNT
	@ Turn off NEWIRQA bit in INTC_CONTROL, so processor can respond to new IRQ
	LDR 	R0, =0x48200048		@ Address of INTC_CONTROL register
	MOV 	R1, #1				@ Value to clear bit 0
	STR 	R1, [R0]			@ Write to INTC_CONTROL register
	B		RETURN_BUTTON		@ Return to wait LOOP
RETURN_BUTTON:
	LDMFD 	SP!, {R0-R3, LR}	@ Restore registers
	SUBS	PC, LR, #4			@ Return from IRQ interrupt procedure
@------------------------------------------------------------------------------Handle Timer
SERVICE_TIMER:
	@ Turn off Timer2 interrupt request and enable INTC for next IRQ
	LDR		R1, =0x48040028		@ Address of Timer2 IRQSTATUS register
	MOV		R2, #0x02			@ Value to reset Timer2 overflow IRQ register
	STR		R2, [R1]			@ Write 
	@ Convert count to ASCII and store in string
	LDR		R11, =COUNT			@ Pointer to COUNT
	LDR		R12, [R11]			@ Read value of COUNT
	@----------------------------------------------------------------------------Conversion	
	@ Convert message from BIN to ASCII
	@ Get input from COUNT
	LDR		R0, =COUNT			@ Load pointer to COUNT 
	MOV		R2, #4				@ Multiply by 4 for heart rate
	LDR		R1, [R0]			@ Load value
	MUL		R1, R1, R2			@ Heart rate value
	MOV		R2, R1				@ Preserve original value
	MOV		R10, #0x00000F00	@ Mask for 1's place
	MOV		R11, #0x0000F000	@ Mask for 10's place
	MOV		R12, #0x000F0000	@ Mask for 100's place
	MOV		R7, #0x0500			@ CMP value for 1's place 
	MOV		R8, #0x5000			@ CMP value for 10's place
	MOV		R9, #0x50000		@ CMP value for 100's place
	MOV		R3, #7				@ Init loop counter for 8 shifts (1 at end)
	LDR		R5, =WORKSPACE		@ Pointer to WORKSPACE array
	MOV		R6, #1				@ DEBUG
	@ Convert from BIN to BCD
SHIFT:
	LSL		R2, R2, #1			@ Shift left
	STR		R2, [R5]			@ Store in memory
	@ 1's
	AND		R4, R2, R10			@ Mask 1's
	CMP		R4, R7				@ Compare 1's w/ 5
	ADDPL	R2, #0x0300			@ Add 3 to 1's nibble of R2 if 5 or greater
	STR		R2, [R5]			@ Store in memory
	@ 10's 
	AND		R4, R2, R11			@ Mask 10's
	CMP		R4, R8				@ Compare 10's w/ 5
	ADDPL	R2, #0x3000			@ Add 3 to 10's nibble of R2 if 5 or greater
	STR		R2, [R5]			@ Store in memory
	@ 100's 
	AND		R4, R2, R12			@ Mask 100's
	CMP		R4, R9				@ Compare 100's w/ 5
	ADDPL	R2, #0x30000		@ Add 3 to 100's nibble of R2 if 5 of greater
	STR		R2, [R5]			@ Store in memory
	@ Repeat 7 times
	ADD		R6, #1				@ DEBUG
	SUBS	R3, #1				@ Decrement loop counter
	BNE		SHIFT
	LSL		R2, R2, #1			@ Last shift
	STR		R2, [R5]			@ Store in memory
	@ Unpack BCD into 3 digits, place in R1, R2, and R3
	MOV		R12, R2				@ Move to new working register
	LSR		R12, R12, #8		@ Shift 1's digit to lowest byte 
	MOV		R10, #0x0000000F	@ Mask for first byte
	AND		R3, R12, R10		@ Mask register, store 1's digit in R3
	LSR		R12, R12, #4		@ Shift 10's digit to first byte
	AND		R2, R12, R10		@ Mask register, store 10's digit in R2
	LSR		R12, R12, #4		@ Shift 100's digit to first byte
	AND		R1, R12, R10		@ Mask register, store 100's digit in R1
	@ Store number in string in memory
	MOV		R8, #ASCII_NUM		@ Value to get ASCII digits
	LDR		R4, =MESSAGE		@ Pointer to MESSAGE
	ADD		R7, R4, #OFFSET		@ Pointer to number section of MESSAGE		
	ADD		R9, R8, R1			@ Simulate a 1 from conversion
	STRB	R9, [R7], #1		@ Store first ASCII number, inc ptr	
	ADD		R9, R8, R2			@ Simulate a 2 from conversion
	STRB	R9, [R7], #1		@ Store first ASCII number, inc ptr	
	ADD		R9, R8, R3			@ Simulate a 3 from conversion
	STRB	R9, [R7], #1		@ Store first ASCII number, inc ptr	
	@------------------------------------------------------------------------------Clean up	
	@ Set DEAD flag
	LDR		R11, =COUNT			@ Point to COUNT
	LDR		R10, [R11]			@ Read value of COUNT
	LDR		R9,	=FLAG_DEAD		@ Point to FLAG_DEAD
	MOV		R7, #0x00			@ Init flag value at 0
	MOV		R6, #0x00			@ Init to 0
	CMP		R10, #0x00			@ Test value
	MOVEQ	R7, #0x01			@ Change flag value if bpm=0
	ADD		R6, R6, R7			@ Add flag value
	STR		R6, [R9]			@ Store flag value
	@ Reset COUNT
	LDR		R11, =COUNT			@ Point to COUNT
	MOV		R12, #0x00			@ Value to store in COUNT
	STR		R12, [R11]			@ Write to COUNT
	@ Enable UART interrupts
	MOV		R3, #0xA			@ Value to enable MODEMSTSIT and THRIT
	LDR		R0, =0x481AA004	
	STRB	R3, [R0]	
	B 		RETURN_TIMER		@ Back to wait loop
RETURN_TIMER:
	LDMFD 	SP!, {R0-R3, LR}	@ Restore registers
	SUBS	PC, LR, #4			@ Return from IRQ interrupt procedure			
 @-------------------------------------------------------------------------------------Data
 @--------------------------------------------------------------------------Array variables
.data
.align 4	
FLAG_DEAD:						@ Tracks whether beats per minute is 0
	.byte	0x0
.align 4
COUNT:							@ Keeps track of button presses
	.byte 	0x0000		
.align 4
WORKSPACE:						@ For conversion memory monitoring
	.word 	0x0000
@-------------------------------------------------------------------------Main message data
.align 4
MESSAGE:						@ Main message
	.byte	0x0D				@ Start, set baud rate
	.byte 	0x01				@ Command
	.ascii	"10O"				@ Voice - 10 - Gretchen
	.byte	0x01				@ Command
	.ascii	"0S"				@ Speed - 0 - slow
	.byte	0x01				@ Command
	.ascii	"7V"				@ Volume - 7 - loudish
	.byte	0x01				@ Command
	.ascii	"7E"				@ Expression - 7 - expressive
	.ascii	"Your heart rate is 040 beats per minute!"
	@        12345678901234567890123456789012345678901234567890
	@        00000000010000000002000000000300000000040000000005
	.byte	0x0D				@ End, read
.align 4
CHAR_PTR:						@ Pointer to next char to send
	.word	MESSAGE
.align 4
CHAR_COUNT:						@ Number of chars, counts x-1 down to 0
	.word	55
@-------------------------------------------------------------------------Dead message data
.align 4
MESSAGE_DEAD:					@ Message if bpm = 0
	.byte	0x0D				@ Start, set baud rate
	.byte 	0x01				@ Command
	.ascii	"10O"				@ Voice - 10 - Gretchen
	.byte	0x01				@ Command
	.ascii	"0S"				@ Speed - 0 - slow
	.byte	0x01				@ Command
	.ascii	"7V"				@ Volume - 7 - loudish
	.byte	0x01				@ Command
	.ascii	"7E"				@ Expression - 7 - expressive
	.ascii 	"You are dead?"
	.byte	0x0D				@ End, read
.align 4
CHAR_PTR_DEAD:					@ Pointer to next char to send
	.word	MESSAGE_DEAD
.align 4
CHAR_COUNT_DEAD:				@ Number of chars, counts x-1 down to 0
	.word	27
@---------------------------------------------------------------------------------Song data
.align 4
MESSAGE_SONG:
	.byte	0x0D				@ Start, set baud rate
	.byte 	0x01				@ Command
	.ascii	"J"					@ Tone generator
	.word	0x00, 0x20, 0x56, 0x00	@ Init command
	.word	0x2E, 0x40, 0x2B, 0x20	@ C(mid), G (mid), C (+1 oct)
	.word	0x2E, 0x26, 0x00, 0x00	@ A(mid)
	.word	0x2E, 0x65, 0x00, 0x00	@ E(+1 oct)
	.word	0x2E, 0x40, 0x20, 0x00	@ C(mid), C(+1 oct)
	.word	0x00, 0x00, 0x01, 0x01	@ Play command
	.word	0x00, 0x00, 0x00, 0x00	@ Quit command
.align 4
CHAR_PTR_SONG:					@ Pointer to next char to send
	.word	MESSAGE_SONG
.align 4
CHAR_COUNT_SONG:				@ Number of chars, counts x-1 down to 0
	.word	100
@--------------------------------------------------------------------------------Stack data
.align 4
STACK1:							@ SVC mode stack 
	.rept 	1024
	.word 	0x0000
	.endr
.align 4
STACK2:							@ IRQ mode stack 
	.rept 	1024
	.word 	0x0000
	.endr
.END	
