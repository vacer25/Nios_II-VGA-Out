# -------------------- SCREEN DATA --------------------

# 320x240, 1024 bytes/row, 2 bytes per pixel: DE1-SoC, DE2, DE2-115
/*
.equ WIDTH, 320
.equ HEIGHT, 240
.equ BYTES_PER_ROW	, 1024
.equ LOG2_BYTES_PER_ROW, 10
.equ LOG2_BYTES_PER_PIXEL, 1
*/
# 160x120, 256 bytes/row, 1 byte per pixel: DE10-Lite
#.equ WIDTH, 160
#.equ HEIGHT, 120
#.equ LOG2_BYTES_PER_ROW, 8
#.equ LOG2_BYTES_PER_PIXEL, 0
# 128 bytes/row, 1 byte per pixel: DE0

.equ WIDTH, 80
.equ HEIGHT, 60
.equ BYTES_PER_ROW, 128
.equ LOG2_BYTES_PER_ROW, 7
.equ LOG2_BYTES_PER_PIXEL, 0

# -------------------- HARDWARE --------------------

.equ PIXBUF, 0x08000000					# Pixel buffer. Same on all boards.
.equ CHARBUF, 0x09000000				# Character buffer. Same on all boards.
.equ VGAPIX_CONTROL, 0x10003020
#.equ VGAPIX_CONTROL, 0xff203020

.equ TIMER_INTERVAL, 2500000
.equ TIMER_BASE_ADDR, 0x10002000
#.equ TIMER_BASE_ADDR, 0xff202000

.equ SWITCHES_BASE_ADDR, 0x10000040
.equ BUTTONS_BASE_ADDR, 0x10000050
#.equ SWITCHES_BASE_ADDR, 0xff200040
#.equ BUTTONS_BASE_ADDR, 0xff200050

# -------------------- DATA --------------------

.equ CHAR_WIDTH, 3
.equ CHAR_HEIGHT, 6

.equ P_WIDTH, 2
.equ P_HEIGHT, 10

.equ P1_X, 3
.equ P2_X, WIDTH-3-P_WIDTH

.equ SCORE_TO_WIN, 	9
.equ SCORE_1_X, WIDTH/2 - CHAR_WIDTH*3
.equ SCORE_2_X, WIDTH/2 + CHAR_WIDTH*2+1
.equ SCORE_Y,	4

.equ WIN_TEXT_X, WIDTH/2 - CHAR_WIDTH*4 - 1
.equ WIN_TEXT_Y, HEIGHT/2 - CHAR_HEIGHT + 1

.equ BG_COL, 	0x0
.equ P_COL, 	0xFFFF
.equ B_COL, 	0xF0
.equ MID_COL,	0xFFFF
.equ CHAR_COL, 	0xFFFF
.equ WIN_COL,   0xF0

.equ SPACE_CHAR,10
.equ P_CHAR,	11
.equ W_CHAR,	12
.equ I_CHAR,	13
.equ N_CHAR,	14
.equ S_CHAR,	15

# r18:	Ball X Pos
# r19:	Ball Y Pos
# r20:	Ball X Dir
# r21:	Ball Y Dir

# r22:	Paddle 1 (Left)  Y
# r23:	Paddle 2 (Right) Y

# -------------------- MACROS --------------------

.macro	H_LINE	x1, x2, y, col
	movi	r4, \x1
	movi	r5, \x2
    movi	r6, \y
    movui	r7, \col
    call 	DrawHLine
.endm

.macro	V_LINE	y1, y2, x, col
	movi	r4, \y1
	movi	r5, \y2
    movi	r6, \x
    movui	r7, \col
    call 	DrawVLine
.endm

.macro	DV_LINE	y1, y2, x, col
	movi	r4, \y1
	movi	r5, \y2
    movi	r6, \x
    movui	r7, \col
    call 	DrawDVLine
.endm

.macro	FILL_COLOR	col
	movui	r4, \col
    call 	FillColourFast
.endm

.macro	WRITE_CHAR	col, row, char
	movi	r4, \col
	movi	r5, \row
    movi	r6, \char
    call 	WriteChar
.endm

.macro	BIG_NUMBER	x, y, char, col
	movi	r4, \x
	movi	r5, \y
    mov 	r6, \char
    movui	r7, \col
    call 	DrawBigNumber
.endm

.macro	BIG_NUMBER_CONST	x, y, char, col
	movi	r4, \x
	movi	r5, \y
    movi 	r6, \char
    movui	r7, \col
    call 	DrawBigNumber
.endm

.macro	WRITE_PIXEL	x, y, col
	mov		r4, \x
	mov		r5, \y
    mov		r6, \col
    call 	WritePixel
.endm

.macro	PADDLE	x, y, col
	movi	r4, \x
    mov		r5, \y
    movui	r6, \col
    call 	DrawPaddle
.endm

# -------------------- INTERRUPTS --------------------
    .text
	.org	0x20
    
	# Exception handler - MUST BE AT ADDRESS 0x20
	subi	sp, sp, 8					# Make room on stack
	stw		ra, 0(sp)					# Save ra
	stw		r19, 4(sp)					# Save r19
    
	# Fetch contents of ctl4 to see if it is hardware interrupt.
	rdctl	et, ipending
    
	# If at least one bit is on, it means a hardware device is requesting attention
	beq 	et, r0, END_HANDLER			# No bits set - not hardware
    
	# Hardware interrupt code.  Have to set address back, so that when interrupt handler done, the instruction gets executed.
	subi	ea, ea, 4
    
	andi	r19, et, 1					# Check bit 0
	beq		r19, r0, END_HANDLER 		# Was not interrupt 0
	call	EXT_IRQ0

	# Exit point for the exception handler
END_HANDLER:
	ldw		r19, 4(sp)					# Restore saved register
	ldw		ra, 0(sp)					# Restore saved register
	addi	sp, sp, 8					# Adjust stack pointer
	eret

	.org	0x100
	.extern CONTINUE_FLAG
	# Device-specific code for each hardware interrupt source
EXT_IRQ0:
	subi 	sp, sp, 12
    stw 	ra,  0(sp)
    stw 	r4,  4(sp)
    stw 	r5,  8(sp)
    
    movia 	r5, TIMER_BASE_ADDR			# Interval timer base address
	sthio 	r0, 0(r5) 					# Clear the interrupt
    
    ldw		r4, CONTINUE_FLAG(r0)
    movi	r4, 1
    stw		r4, CONTINUE_FLAG(r0)
    
    #FILL_COLOR	0xF0
    
    ldw 	r5,  8(sp)  
    ldw 	r4,  4(sp)
    ldw 	ra,  0(sp)
    addi 	sp, sp, 12
	ret # Return from the interrupt-service routine

# -------------------- START --------------------

	.global _start
    
_start:
	movia 	sp, 0x800000				# Initial stack pointer
	
	# configure vga stuffs
	movia	r2, VGAPIX_CONTROL
	movia	r3, PIXBUF
	stwio	r3, 4(r2)
	stwio	r3, 0(r2)
	
	stwio	r3, 4(r2)
	stwio	r3, 0(r2)
    
    movi 	r16, WIDTH					# Width
    movi 	r17, HEIGHT-1				# Height
	
    movia	r9, SWITCHES_BASE_ADDR		# Switches I/O Address
    movia	r3, BUTTONS_BASE_ADDR		# Buttons I/O Address
    movi 	r15, 255					# Number of NOP delays
    
    movi 	r14, HEIGHT-P_HEIGHT		# Midline X coordinate
    movi	r22, HEIGHT/2-P_HEIGHT/2 	# Paddle 1 (Left)  in middle
    movi	r23, HEIGHT/2-P_HEIGHT/2 	# Paddle 2 (Right) in middle
    movi	r18, WIDTH/2				# Ball X Pos in middle
    movi	r19, HEIGHT/2				# Ball Y Pos in middle
    movi 	r20, 1						# Ball X dir
    movi 	r21, 1						# Ball Y dir
    
    # Setup interval timer
    movia 	r13, TIMER_BASE_ADDR		# Internal timer base address
    movia 	r12, TIMER_INTERVAL				# 1/(50 MHz) x (2500000) = 50 msec
    sthio 	r12, 8(r13) 				# Store the low halfword of counter start value
    srli 	r12, r12, 16
    sthio 	r12, 12(r13) 				# High halfword of counter start value
    
    # Start interval timer, enable its interrupts
    movi 	r15, 0b0111 				# START = 1, CONT = 1, ITO = 1
    sthio 	r15, 4(r13)
    
    # Enable Nios II processor interrupts
    movi 	r7, 0b1 					# Set interrupt mask bits for level 0 (interval timer)
    wrctl 	ienable, r7 					
    movi 	r7, 1
    wrctl 	status, r7 					# Turn on Nios II interrupt processing
    #movi 	r7, 0
    #wrctl 	ipending, r7 				# Clear pending interrupts
    
    # Set can continue running flag to 0
    ldw  	r4, CONTINUE_FLAG(r0)
    movi	r4, 0
    stw  	r4, CONTINUE_FLAG(r0)
    
    # Set scores to 0
    ldw  	r4, SCORE_P1(r0)
    ldw  	r5, SCORE_P2(r0)
    movi	r4, 0
    movi	r5, 0
    stw  	r4, SCORE_P1(r0)
    stw  	r5, SCORE_P2(r0)    
	
	call	ClearScreen
    
# -------------------- LOOP --------------------
    
Loop:
    FILL_COLOR	BG_COL
#asdfasdf:
#    ldw  	r4, CONTINUE_FLAG(r0)
#    beq		r4, r0, asdfasdf
#    movi	r4, 0
#    stw		r4, CONTINUE_FLAG(r0)
#    br		Loop
    
    /*
    FILL_COLOR	B_COL
    BIG_NUMBER_CONST	1, 1, 0, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*1, 1, 1, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*2, 1, 2, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*3, 1, 3, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*4, 1, 4, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*5, 1, 5, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*6, 1, 6, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*7, 1, 7, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*8, 1, 8, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*9, 1, 9, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*10, 1, SPACE_CHAR, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*11, 1, P_CHAR, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*12, 1, W_CHAR, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*13, 1, I_CHAR, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*14, 1, N_CHAR, CHAR_COL
    BIG_NUMBER_CONST	1+(CHAR_WIDTH+1)*15, 1, S_CHAR, CHAR_COL
    br		End
    */
    
    # -------------------- DRAWING --------------------
    
    PADDLE 	P1_X, r22, P_COL						# Left paddle
    PADDLE 	P2_X, r23, P_COL						# Right paddle
    
   	DV_LINE	0, HEIGHT, WIDTH/2, MID_COL				# Midline
    
    ldw		r11, SCORE_P1(r0)
    ldw		r12, SCORE_P2(r0)
    BIG_NUMBER SCORE_1_X, SCORE_Y, r11, CHAR_COL	# Left score
    BIG_NUMBER SCORE_2_X, SCORE_Y, r12, CHAR_COL	# Right score
    
    movui	r6, B_COL
    WRITE_PIXEL 	r18, r19, r6					# Ball
    
    	# -------------------- CHECK WIN --------------------
    
    ldw		r11, SCORE_P1(r0)
    ldw		r12, SCORE_P2(r0)
    
    movi	r10, SCORE_TO_WIN
    bge		r11, r10, P1_WINS
    bge		r12, r10, P2_WINS
    br 		Check_Input
    
P1_WINS:
    PADDLE 	P1_X, r22, WIN_COL						# Left paddle
    BIG_NUMBER SCORE_1_X, SCORE_Y, r11, WIN_COL		# Left score
    ldw		r2, WINNER(r0)							# Load winner from mem.
    movi	r2, 1									# Set winner to 1
    stw		r2, WINNER(r0)							# Store winner to mem.
    br		Print_Win_Text
P2_WINS:
    PADDLE 	P2_X, r23, WIN_COL						# Right paddle
    BIG_NUMBER SCORE_2_X, SCORE_Y, r12, WIN_COL		# Right score
    ldw		r2, WINNER(r0)							# Load winner from mem.
    movi	r2, 2									# Set winner to 2
    stw		r2, WINNER(r0)							# Store winner to mem.
    br		Print_Win_Text
    
	# -------------------- INPUTS --------------------
   
Check_Input:
    ldwio	r8, 0(r9)								# Read switches
    andi	r10, r8, 256							# Check switch 8 (Left paddle down)
    andi	r11, r8, 512							# Check switch 9 (Left paddle up)
    andi	r12, r8, 1								# Check switch 1 (Right paddle down)
    andi	r13, r8, 2								# Check switch 2 (Right paddle up)
    
	# -------------------- PADDLES --------------------
    
    beq		r10, r0, Skip_P1_Plus_Y
    beq		r22, r14, Skip_P1_Plus_Y
    addi	r22, r22, 1
Skip_P1_Plus_Y:
    beq		r11, r0, Skip_P1_Minus_Y
    beq		r22, r0, Skip_P1_Minus_Y
    subi	r22, r22, 1
Skip_P1_Minus_Y:
    beq		r12, r0, Skip_P2_Plus_Y
    beq		r23, r14, Skip_P2_Plus_Y
    addi	r23, r23, 1
Skip_P2_Plus_Y:
    beq		r13, r0, Skip_P2_Minus_Y
    beq		r23, r0, Skip_P2_Minus_Y
    subi	r23, r23, 1
Skip_P2_Minus_Y:

	# -------------------- BALL --------------------
    
    movi 	r10, P1_X+P_WIDTH						# Paddle 1 (Left)  X coordinate
    movi 	r11, P2_X-1								# Paddle 2 (Right) X coordinate

X_Calculations:
	beq		r18, r10, B_Check_P_1_Hit				# Ball X = Paddle 1 X
	beq		r18, r11, B_Check_P_2_Hit				# Ball X = Paddle 2 X
    beq		r18, r16, B_Hit_Right					# Ball X = Right wall
	beq		r18, r0,  B_Hit_Left					# Ball X = Left wall
    br		Update_B_X
B_Check_P_1_Hit:
	blt		r19, r22, Update_B_X					# If Ball Y higher than Paddle 1 Y, skip
    addi	r10, r22, P_HEIGHT
	bgt		r19, r10, Update_B_X					# If Ball Y lower than Paddle 1 Y, skip
	movi	r20, 1									# Set ball X direction to right
    br		Update_B_X
B_Check_P_2_Hit:
	blt		r19, r23, Update_B_X					# If Ball Y higher than Paddle 2 Y, skip
    addi	r11, r23, P_HEIGHT
	bgt		r19, r11, Update_B_X					# If Ball Y lower than Paddle 2 Y, skip
	movi	r20, -1									# Set ball X direction to left
    br    Update_B_X
    
B_Hit_Right:
	V_LINE	0, HEIGHT, WIDTH-1, B_COL
    movi	r18, WIDTH/2							# Ball X Pos in middle
    movi	r19, HEIGHT/2							# Ball Y Pos in middle
	movi	r20, -1									# Ball X direction set to left
    ldw		r11, SCORE_P1(r0)						# Load score 1 (left paddle)
    addi	r11, r11, 1								# Increment score 1 (left paddle)
    stw		r11, SCORE_P1(r0)						# Store score 1 (left paddle)
    br    Update_B_X
B_Hit_Left:
	V_LINE	0, HEIGHT, 0, B_COL
    movi	r18, WIDTH/2							# Ball X Pos in middle
    movi	r19, HEIGHT/2							# Ball Y Pos in middle
	movi	r20, 1									# # Ball X direction set to right
    ldw		r12, SCORE_P2(r0)						# Load score 2 (right paddle)
    addi	r12, r12, 1								# Increment score 21 (right paddle)
    stw		r12, SCORE_P2(r0)						# Store score 2 (right paddle)
    br    Update_B_X
    
Update_B_X:
    add		r18, r18, r20
    
Y_Calculations:
    beq		r19, r17, B_Set_Y_Dir_Minus
	beq		r19, r0, B_Set_Y_Dir_Plus
    br		Update_B_Y
B_Set_Y_Dir_Minus:
	movi	r21, -1
    br		Update_B_Y
B_Set_Y_Dir_Plus:
	movi	r21, 1
    
Update_B_Y:
    add		r19, r19, r21
    
	# -------------------- DELAY --------------------
    
Wait:
    ldw  	r4, CONTINUE_FLAG(r0)
    beq		r4, r0, Wait
    movi	r4, 0
    stw		r4, CONTINUE_FLAG(r0)
    br		Loop

	# -------------------- DISPLAY WIN TEXT --------------------

Print_Win_Text:
	ldw		r2, WINNER(r0)		# Load winner from mem.
    
    BIG_NUMBER_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*0, WIN_TEXT_Y, P_CHAR, WIN_COL
    BIG_NUMBER			WIN_TEXT_X+(CHAR_WIDTH+1)*1, WIN_TEXT_Y, r2, WIN_COL
    BIG_NUMBER_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*2, WIN_TEXT_Y, SPACE_CHAR, WIN_COL
    BIG_NUMBER_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*3, WIN_TEXT_Y, W_CHAR, WIN_COL
    BIG_NUMBER_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*4, WIN_TEXT_Y, I_CHAR, WIN_COL
    BIG_NUMBER_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*5, WIN_TEXT_Y, N_CHAR, WIN_COL
    BIG_NUMBER_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*6, WIN_TEXT_Y, S_CHAR, WIN_COL

Wait_For_Restart_Button:
    ldwio	r8, 0(r3)								# Read buttons
    andi	r10, r8, 1								# Check button 0
    bne		r10, r0, _start							# Branch to start if button is pressed
	br		Wait_For_Restart_Button					# Otherwise, keep waiting for a button press

End:
    br 		End  
    
    
	.data
	.global CONTINUE_FLAG
CONTINUE_FLAG:	.word 	0
SCORE_P1:		.word 	0
SCORE_P2:		.word 	0
WINNER:			.word	0

CHAR_MAP:		.word 0x3E223E00	# 0
				.word 0x243E2000	# 1
				.word 0x322A2400	# 2
				.word 0x222A3E00	# 3
				.word 0x0E083E00	# 4
				.word 0x2E2A3A00	# 5
				.word 0x3E2A3A00	# 6
				.word 0x023A0600	# 7
				.word 0x3E2A3E00	# 8
				.word 0x2E2A3E00	# 9
				.word 0x00000000	# [Space]
				.word 0x3E0A0E00	# P
				.word 0x3E103E00	# W
				.word 0x223E2200	# I
				.word 0x3C081E00	# N
				.word 0x2E2A3A00	# S

	.text
    
# -------------------- DRAWING --------------------

# r4: x
# r5: y
# r6: char
# r7: color value
DrawBigNumber:
	subi 	sp, sp, 44
    stw 	r4,  0(sp)
    stw 	r5,  4(sp)
    stw 	r14, 8(sp)
    stw 	r15, 12(sp)
    stw 	r16, 16(sp)
    stw 	r17, 20(sp)
    stw 	r18, 24(sp)
    stw 	r19, 28(sp)
    stw 	r20, 32(sp)
    stw 	r21, 36(sp)
    stw 	ra,  40(sp)
    
    mov 	r14, r4						# r14 <- Start X
    movi	r18, CHAR_HEIGHT
    movi	r16, CHAR_WIDTH-1
    
    movi	r21, BG_COL
    
    slli	r6, r6, 2					# Multiply number to draw in r6 by 4 to get proper offset
    
    movia	r19, 0x80000000				# r19 <- Bitmask (1 + 31 zeros)
    
    ldw		r20, CHAR_MAP(r6)			# Load current char map
    
    # Two loops to draw the big char
	LoopX:  
    	movi 	r17, 7					# Reset current Y to 7
    	ldw 	r15, 4(sp) 				# r15 <- Start Y
        addi	r15, r15, 8				# Add 8 to start Y to begin drawing from bottom
		LoopY:  
        	bge		r17, r18, Skip_Draw	# If current Y counter > Char weight, skip draw
			and		r5, r19, r20		# Check if current bit is set
            beq		r5, r0, Draw_BG_Col	# If current bit is 0, skip draw
            WRITE_PIXEL	r14, r15, r7	# Draw the pixel in the specified color
            br		Skip_Draw
        Draw_BG_Col:
        	WRITE_PIXEL	r14, r15, r21	# Draw the pixel in the background color
        Skip_Draw:
        	srli	r19, r19, 1			# Shift bitmask right
            subi 	r15, r15, 1			# Decrement Y pos
            subi 	r17, r17, 1			# Decrement Y counter
            bge 	r17, r0, LoopY		# Loop if Y counter >= 0
        addi 	r14, r14, 1				# Increment X pos
        subi 	r16, r16, 1				# Decrement X counter
        bge 	r16, r0, LoopX			# Loop if X counter >= 0
    
    ldw 	ra,  40(sp)
    ldw 	r21, 36(sp)
    ldw 	r20, 32(sp)
    ldw 	r19, 28(sp)
    ldw 	r18, 24(sp)
    ldw 	r17, 20(sp)
    ldw 	r16, 16(sp)
    ldw 	r15, 12(sp)
    ldw 	r14, 8(sp) 
    ldw 	r5,  4(sp)
    ldw 	r4,  0(sp)  
    addi 	sp, sp, 44
    ret
    
# r4: x
# r5: y
# r6: colour value
DrawPaddle:
	subi sp, sp, 40
    stw r4,  0(sp)
    stw r5,  4(sp)
    stw r6,  8(sp)
    stw r7,  12(sp)
    stw r16, 16(sp)
    stw r17, 20(sp)
    stw r18, 24(sp)
    stw r19, 28(sp)
    stw r20, 32(sp)
    stw ra,  36(sp)
    
	mov 	r16, r4
    mov 	r17, r4
    addi 	r17, r17, P_WIDTH
    mov 	r18, r5
    mov 	r19, r5
    addi 	r19, r19, P_HEIGHT
    mov 	r20, r6

    1:	mov		r4, r18
        mov		r5, r19
        mov		r6, r16
        mov		r7, r20
        call 	DrawVLine
        addi r16, r16, 1
        blt r16, r17, 1b
        
	ldw ra,  36(sp)
    ldw r20, 32(sp)
    ldw r19, 28(sp)
    ldw r18, 24(sp)
    ldw r17, 20(sp)
    ldw r16, 16(sp)
    ldw r7,  12(sp)  
    ldw r6,  8(sp)
    ldw r5,  4(sp)
    ldw r4,  0(sp)  
    addi sp, sp, 40
    ret

# r4: x1
# r5: x2
# r6: y
# r7: colour value
DrawHLine:
	subi sp, sp, 24
    stw r4,  0(sp)
    stw r5,  4(sp)
    stw r6,  8(sp)
    stw r16, 12(sp)
    stw r17, 16(sp)
    stw ra,  20(sp)
    
    mov  r17, r4		# x1
    mov  r16, r5		# x2
    mov  r5, r6			# y
    mov  r6, r7			# Colour
    
    1:	mov r4, r16
        call WritePixel		# Draw one pixel
        subi r16, r16, 1
        bge r16, r17, 1b
    
    ldw ra,  20(sp)
    ldw r17, 16(sp)  
    ldw r16, 12(sp)  
    ldw r6,  8(sp)
    ldw r5,  4(sp)
    ldw r4,  0(sp)   
    addi sp, sp, 24
    ret

# r4: y1
# r5: y2
# r6: x
# r7: colour value
DrawVLine:
	subi sp, sp, 24
    stw r4,  0(sp)
    stw r5,  4(sp)
    stw r6,  8(sp)
    stw r16, 12(sp)
    stw r17, 16(sp)
    stw ra,  20(sp)
    
    mov  r17, r4		# y1
    mov  r16, r5		# y2
    mov  r4, r6			# x
    mov  r6, r7			# Colour
    
    1:	mov r5, r16
        call WritePixel		# Draw one pixel
        subi r16, r16, 1
        bge r16, r17, 1b
    
    ldw ra,  20(sp)
    ldw r17, 16(sp)  
    ldw r16, 12(sp)
    ldw r6,  8(sp)
    ldw r5,  4(sp)
    ldw r4,  0(sp)   
    addi sp, sp, 24
    ret
    
# r4: y1
# r5: y2
# r6: x
# r7: colour value
DrawDVLine:
	subi sp, sp, 28
    stw r4,  0(sp)
    stw r5,  4(sp)
    stw r6,  8(sp)
    stw r16, 12(sp)
    stw r17, 16(sp)
    stw r18, 20(sp)
    stw ra,  24(sp)
    
    mov  r17, r4		# y1
    mov  r16, r5		# y2
    mov  r4, r6			# x
    mov  r6, r7			# Colour
    
    1:	mov 	r5, r16
    	andi	r18, r5, 1
        bne		r18, r0, Skip_Draw_Pixel
        call 	WritePixel		# Draw one pixel
Skip_Draw_Pixel:
        subi 	r16, r16, 1
        bge 	r16, r17, 1b
    
    ldw ra,  24(sp)
    ldw r18, 20(sp) 
    ldw r17, 16(sp)  
    ldw r16, 12(sp)
    ldw r6,  8(sp)
    ldw r5,  4(sp)
    ldw r4,  0(sp)   
    addi sp, sp, 28
    ret    

# r4: colour
FillColour:
	subi sp, sp, 28
    stw r4,  0(sp)
    stw r5,  4(sp)
    stw r6,  8(sp)
    stw r16, 12(sp)
    stw r17, 16(sp)
    stw r18, 20(sp)
    stw ra,  24(sp)
    
    mov r18, r4
    
    # Two loops to draw each pixel
    movi r16, WIDTH-1
    1:	movi r17, HEIGHT-1
        2:  mov r4, r16
            mov r5, r17
            mov r6, r18
            call WritePixel		# Draw one pixel
            subi r17, r17, 1
            bge r17, r0, 2b
        subi r16, r16, 1
        bge r16, r0, 1b
    
    ldw ra,  24(sp)
    ldw r18, 20(sp)
    ldw r17, 16(sp)
    ldw r16, 12(sp)  
    ldw r6,  8(sp)
    ldw r5,  4(sp)
    ldw r4,  0(sp)  
    addi sp, sp, 28
    ret


ClearScreen:
	subi sp, sp, 24
    stw r4,  0(sp)
    stw r5,  4(sp)
    stw r6,  8(sp)
    stw r16, 12(sp)
    stw r17, 16(sp)
    stw ra,  20(sp)
    
    mov r4, r0
    call FillColour
    
    # Two loops to draw each char
    movi r16, WIDTH-1
    1:	movi r17, HEIGHT-1
        2:  mov r4, r16
            mov r5, r17
            mov r6, r0
            call WriteChar		# Draw one char
            subi r17, r17, 1
            bge r17, r0, 2b
        subi r16, r16, 1
        bge r16, r0, 1b
    
    ldw ra,  20(sp)
    ldw r17, 16(sp)
    ldw r16, 12(sp)  
    ldw r6,  8(sp)
    ldw r5,  4(sp)
    ldw r4,  0(sp)  
    addi sp, sp, 24
    ret


# r4: col
# r5: row
# r6: character
WriteChar:
	subi sp, sp, 12
    stw r4, 0(sp)		# Save some registers
    stw r5, 4(sp)
    stw r6, 8(sp)
    
	slli r5, r5, 7
    add r5, r5, r4
    movia r4, CHARBUF
    add r5, r5, r4
    stbio r6, 0(r5)
    
    ldw r6, 8(sp)
    ldw r5, 4(sp)
    ldw r4, 0(sp)    
    addi sp, sp, 12
    ret


# r4: col (x)
# r5: row (y)
# r6: colour value
WritePixel:
	subi sp, sp, 20
    stw r2, 0(sp)		# Save some registers
    stw r3, 4(sp)
    stw r4, 8(sp)
    stw r5, 12(sp)
    stw r6, 16(sp)

	movi r2, LOG2_BYTES_PER_ROW		# log2(bytes per row)
    movi r3, LOG2_BYTES_PER_PIXEL	# log2(bytes per pixel)
    
    sll r5, r5, r2
    sll r4, r4, r3
    add r5, r5, r4
    movia r4, PIXBUF
    add r5, r5, r4
    
    bne r3, r0, 1f		# 8bpp or 16bpp?
  	stbio r6, 0(r5)		# Write 8-bit pixel
    
    ldw r6, 16(sp)
    ldw r5, 12(sp)
    ldw r4, 8(sp)
    ldw r3, 4(sp)
    ldw r2, 0(sp)  
    addi sp, sp, 20
    ret
    
1:	sthio r6, 0(r5)		# Write 16-bit pixel

	ldw r6, 16(sp)
    ldw r5, 12(sp)
    ldw r4, 8(sp)
    ldw r3, 4(sp)
    ldw r2, 0(sp)  
    addi sp, sp, 20
	ret

FillColourFast:
	subi sp, sp, 28
    stw r4,  0(sp)
    stw r5,  4(sp)
    stw r6,  8(sp)
    stw r16, 12(sp)
    stw r17, 16(sp)
    stw r18, 20(sp)
    #stw ra,  24(sp)
    
    slli r18, r4,16
	or r18,r18,r4
	
	movia r5, (BYTES_PER_ROW)*HEIGHT-4
	movia r6, PIXBUF
	add r4,r6,r5
	aaaa:
		stwio r18,0(r4)
		subi r4,r4,4
		nop
		nop
		nop
		nop
		nop
		nop
		bge r4,r6,aaaa

    #ldw ra,  24(sp)
    ldw r18, 20(sp)
    #ldw r17, 16(sp)
    #ldw r16, 12(sp)  
    ldw r6,  8(sp)
    ldw r5,  4(sp)
    ldw r4,  0(sp)  
    addi sp, sp, 28
    ret
	