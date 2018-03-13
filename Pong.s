# TARGET_SYSTEM = 0: DE0
# TARGET_SYSTEM = 1: DE1-SoC
# TARGET_SYSTEM = 2: DE2 / DE2-115
# TARGET_SYSTEM = 3: DE10-Lite

.equ TARGET_SYSTEM, 0					# Used to indicate which FPGA the code will be compiled for
.equ USE_DOUBLE_BUFFERED, 1
.equ USE_SNES_CONTROLLER, 0

# -------------------- SCREEN DATA --------------------

.if TARGET_SYSTEM == 0
    # 128 bytes/row, 1 byte per pixel: DE0
    .equ WIDTH, 80
    .equ HEIGHT, 60
    .equ LOG2_BYTES_PER_ROW, 7
    .equ LOG2_BYTES_PER_PIXEL, 0
.elseif TARGET_SYSTEM == 1 || TARGET_SYSTEM == 2
    # 320x240, 1024 bytes/row, 2 bytes per pixel: DE1-SoC, DE2, DE2-115
    .equ WIDTH, 320
    .equ HEIGHT, 240
    .equ LOG2_BYTES_PER_ROW, 10
    .equ LOG2_BYTES_PER_PIXEL, 1
.elseif TARGET_SYSTEM == 3
    # 160x120, 256 bytes/row, 1 byte per pixel: DE10-Lite
    .equ WIDTH, 160
    .equ HEIGHT, 120
    .equ LOG2_BYTES_PER_ROW, 8
    .equ LOG2_BYTES_PER_PIXEL, 0
.else
    .error "Error: Invalid target system"
.endif

.equ BYTES_PER_ROW, (1 << LOG2_BYTES_PER_ROW)
.equ BYTES_PER_PIXEL, (1 << LOG2_BYTES_PER_PIXEL)

.if TARGET_SYSTEM == 0 || TARGET_SYSTEM == 3
    # 8-bit colours: DE0, DE10-Lite
    .equ BG_COL, 	0x00
    .equ P_COL, 	0xFF
    .equ B_COL, 	0xF0
    .equ MID_COL,	0xFF
    .equ CHAR_COL, 	0xFF
    .equ WIN_COL,   0xF0
    .equ PAUSED_COL,0xF0
.elseif TARGET_SYSTEM == 1 || TARGET_SYSTEM == 2
    # 16-bit colours: DE1-SoC, DE2, DE2-115
    .equ BG_COL, 	0x0000
    .equ P_COL, 	0xFFFF
    .equ B_COL, 	0xFF00
    .equ MID_COL,	0xFFFF
    .equ CHAR_COL, 	0xFFFF
    .equ WIN_COL,   0xFF00
	.equ PAUSED_COL,0xFF00
.endif

.if USE_DOUBLE_BUFFERED == 0
.equ FRAMEBUFFER, prog_end					# Pixel buffer.     Same on all boards.
.else 
.equ FRAMEBUFFER_SIZE, HEIGHT * BYTES_PER_ROW
.equ FRAMEBUFFER_A, prog_end
.equ FRAMEBUFFER_B, FRAMEBUFFER_A + FRAMEBUFFER_SIZE
.endif

.equ CHARBUF, 0x09000000					# Character buffer. Same on all boards.
.equ CHARBUF_SIZE, 0x2000

# -------------------- HARDWARE --------------------

.if TARGET_SYSTEM == 0 || TARGET_SYSTEM == 2
    # DE0, DE2, DE2-115
    .equ VGAPIX_CONTROL, 0x10003020
    .equ TIMER_BASE_ADDR, 0x10002000
    .equ SWITCHES_BASE_ADDR, 0x10000040
    .equ BUTTONS_BASE_ADDR, 0x10000050
    .equ SNESCONTROLLER_BASE_ADDR, 0x10000070
.elseif TARGET_SYSTEM == 1 || TARGET_SYSTEM == 3
    # DE1-SoC, DE10-Lite
    .equ VGAPIX_CONTROL, 0xff203020
    .equ TIMER_BASE_ADDR, 0xff202000
    .equ SWITCHES_BASE_ADDR, 0xff200040
    .equ BUTTONS_BASE_ADDR, 0xff200050
    .equ SNESCONTROLLER_BASE_ADDR, 0xff200070
.endif

.equ TIMER_INTERVAL, 2500000

# -------------------- DATA --------------------

.equ CHAR_WIDTH, 3								# Character width
.equ CHAR_HEIGHT, 6								# Character height
.equ CHAR_BEGIN_Y, 1							# First non-empty row in the character
		
.equ P_WIDTH, 2									# paddle width
.equ P_HEIGHT, 10								# paddle height
		
.equ P1_X, 3									# Left  paddle X coordinate
.equ P2_X, WIDTH-3-P_WIDTH						# Right paddle X coordinate
		
.equ SCORE_TO_WIN, 	9							# Score reqired to win
.equ SCORE_1_X, WIDTH/2-CHAR_WIDTH*3			# Left  score X coordinate
.equ SCORE_2_X, WIDTH/2+CHAR_WIDTH*2+1			# Right score X coordinate
.equ SCORE_Y,	4								# Score number lables Y coordinate
		
.equ TEXT_Y, HEIGHT/2-CHAR_HEIGHT+1 			# All text lables Y coordinate
.equ WIN_TEXT_X, WIDTH/2-CHAR_WIDTH*4-1			# Win text lable X coordinate (7 characters long)
.equ PAUSED_TEXT_X, WIDTH/2-CHAR_WIDTH*4+1		# Paused text lable X coordinate
.equ PRESS_START_TEXT_X, WIDTH/2-CHAR_WIDTH*6-1	# Press start text lable X coordinate

# Character numbers in the character map

.equ SPACE_CHAR,10
.equ P_CHAR,	11
.equ W_CHAR,	12
.equ I_CHAR,	13
.equ N_CHAR,	14
.equ S_CHAR,	15
.equ A_CHAR,	16
.equ U_CHAR,	17
.equ E_CHAR,	18
.equ T_CHAR,	19
.equ R_CHAR,	20
.equ D_CHAR,	21

# Register usage:

# r18:	Ball X coordinate
# r19:	Ball Y coordinate
# r20:	Ball X direction
# r21:	Ball Y direction

# r22:	Left paddle  Y coordinate
# r23:	Right paddle Y coordinate

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

.macro	DRAW_CHAR	col, row, char
	movi	r4, \col
	movi	r5, \row
    movi	r6, \char
    call 	WriteChar
.endm

.macro	DRAW_BIG_CHAR	x, y, char, col
	movi	r4, \x
	movi	r5, \y
    mov 	r6, \char
    movui	r7, \col
    call 	DrawBigChar
.endm

.macro	DRAW_BIG_CHAR_CONST	x, y, char, col
	movi	r4, \x
	movi	r5, \y
    movi 	r6, \char
    movui	r7, \col
    call 	DrawBigChar
.endm

.macro	DRAW_PIXEL	x, y, col
	mov		r4, \x
	mov		r5, \y
    mov		r6, \col
    call 	WritePixel
.endm

.macro	DRAW_PIXEL_CONST_COL	x, y, col
	mov		r4, \x
	mov		r5, \y
    movui	r6, \col
    call 	WritePixel
.endm

.macro	DRAW_PADDLE	x, y, col
	movi	r4, \x
    mov		r5, \y
    movui	r6, \col
    call 	DrawPaddle
.endm

.macro  READ_SNES
    subi    sp, sp, 4 * 8
    stw     r8,   0(sp)
    stw     r9,   4(sp)
    stw     r10,  8(sp)
    stw     r11, 12(sp)
    stw     r12, 16(sp)
    stw     r13, 20(sp)
    stw     r14, 24(sp)
    stw     r15, 28(sp)

    call    SNESController_Read

    ldw     r8,   0(sp)
    ldw     r9,   4(sp)
    ldw     r10,  8(sp)
    ldw     r11, 12(sp)
    ldw     r12, 16(sp)
    ldw     r13, 20(sp)
    ldw     r14, 24(sp)
    ldw     r15, 28(sp)
    addi    sp, sp, 4 * 8
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
	call	TIMER_IRQ

	# Exit point for the exception handler
END_HANDLER:
	ldw		r19, 4(sp)					# Restore saved register
	ldw		ra, 0(sp)					# Restore saved register
	addi	sp, sp, 8					# Adjust stack pointer
	eret

	.org	0x100
	.extern CONTINUE_FLAG
    
	# Device-specific code for interval timer interrupt source
TIMER_IRQ:
	subi 	sp, sp, 12
    stw 	ra,  0(sp)
    stw 	r4,  4(sp)
    stw 	r5,  8(sp)
    
    movia 	r5, TIMER_BASE_ADDR			# Interval timer base address
	sthio 	r0, 0(r5) 					# Clear the interrupt
    
    # Set can continue running flag to 1
    ldw		r4, CONTINUE_FLAG(r0)
    movi	r4, 1
    stw		r4, CONTINUE_FLAG(r0)
        
    ldw 	r5,  8(sp)  
    ldw 	r4,  4(sp)
    ldw 	ra,  0(sp)
    addi 	sp, sp, 12
	ret 								# Return from the ISR
    
# -------------------- START --------------------

	.global _start
    
_start:
	movia 	sp, 0x800000				# Initial stack pointer

.if USE_DOUBLE_BUFFERED == 0
	# Tell VGA to use our framebuffer instead of the default
    # This is required for the non-double-buffered code to work on real hardware
	movia	r2, VGAPIX_CONTROL
	movia	r3, FRAMEBUFFER
	stwio	r3, 4(r2)
	stwio	r3, 0(r2)
.else 
	# Initialize framebuffers to empty
	movi	r4, 0x0000
	call	FillColourFast
	call	SwapBuffers
	movi	r4, 0x0000
	call	FillColourFast
	call	SwapBuffers
.endif

.if USE_SNES_CONTROLLER == 1
    call    SNESController_Initialize
.endif
	
    movi 	r16, WIDTH					# Width
    movi 	r17, HEIGHT-1				# Height
	
    movia	r9, SWITCHES_BASE_ADDR		# Switches I/O Address
    movia	r3, BUTTONS_BASE_ADDR		# Buttons I/O Address
    movi 	r15, 255					# Number of NOP delays
    
    movi 	r14, HEIGHT-P_HEIGHT		# Midline X coordinate in middle
	
	# Initialize variables
    movi	r18, WIDTH/2				# Ball X coordinate in middle
    movi	r19, HEIGHT/2				# Ball Y coordinate in middle
    movi 	r20, 1						# Ball X direction
    movi 	r21, 1						# Ball Y direction
    movi	r22, HEIGHT/2-P_HEIGHT/2 	# Left paddle  Y coordinate in middle
    movi	r23, HEIGHT/2-P_HEIGHT/2 	# Right paddle Y coordinate in middle
    
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
	
    # This is required in case the character buffer was in use before we started running
	call    ClearScreenFast
    
# -------------------- LOOP --------------------
    
Loop:
	
	# -------------------- TESTING CODE --------------------

/*
1:
    ldw  	r4, CONTINUE_FLAG(r0)
    beq		r4, r0, 1b
    movi	r4, 0
    stw		r4, CONTINUE_FLAG(r0)
    br		Loop
*/
    
/*
    FILL_COLOR	B_COL
    DRAW_BIG_CHAR_CONST	1, 1, 0, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*1, 1, 1, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*2, 1, 2, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*3, 1, 3, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*4, 1, 4, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*5, 1, 5, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*6, 1, 6, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*7, 1, 7, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*8, 1, 8, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*9, 1, 9, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*10, 1, SPACE_CHAR, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*11, 1, P_CHAR, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*12, 1, W_CHAR, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*13, 1, I_CHAR, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*14, 1, N_CHAR, CHAR_COL
    DRAW_BIG_CHAR_CONST	1+(CHAR_WIDTH+1)*15, 1, S_CHAR, CHAR_COL
    br		End
*/
    
    # -------------------- DRAWING --------------------
    
    FILL_COLOR	BG_COL								# Clear screen
    call 	DrawScreen
    
    # -------------------- CHECK WIN --------------------
    
    ldw		r11, SCORE_P1(r0)						# Load left scores
    ldw		r12, SCORE_P2(r0)						# Load right scores
    
    movi	r10, SCORE_TO_WIN						
    bge		r11, r10, P1_WINS						# If left score >= scrore to win, left player wins
    bge		r12, r10, P2_WINS						# If right score >= scrore to win, right player wins
    br 		Check_Input
    
P1_WINS:
    DRAW_PADDLE 	P1_X, r22, WIN_COL				# Draw left paddle
    DRAW_BIG_CHAR SCORE_1_X, SCORE_Y, r11, WIN_COL	# Draw left score
    ldw		r2, WINNER(r0)							
    movi	r2, 1									# Set winner to 1
    stw		r2, WINNER(r0)							
    br		Print_Win_Text
P2_WINS:
    DRAW_PADDLE 	P2_X, r23, WIN_COL				# Draw right paddle
    DRAW_BIG_CHAR SCORE_2_X, SCORE_Y, r12, WIN_COL	# Draw right score
    ldw		r2, WINNER(r0)							
    movi	r2, 2									# Set winner to 2
    stw		r2, WINNER(r0)							
    br		Print_Win_Text
    
	# -------------------- INPUTS --------------------
   
Check_Input:
.if USE_SNES_CONTROLLER == 1
    READ_SNES
    andi	r10, r2, 0x0400							# Check Down (Left paddle down)
    andi	r11, r2, 0x0800							# Check Up (Left paddle up)
    andi	r12, r2, 0x8000							# Check B (Right paddle down)
    andi	r13, r2, 0x0040							# Check X (Right paddle up)
.else
    ldwio	r8, 0(r9)								# Read switches
    andi	r10, r8, 256							# Check switch 8 (Left paddle down)
    andi	r11, r8, 512							# Check switch 9 (Left paddle up)
    andi	r12, r8, 1								# Check switch 1 (Right paddle down)
    andi	r13, r8, 2								# Check switch 2 (Right paddle up)
.endif
    
	# -------------------- PADDLE MOVEMENT --------------------
    
    beq		r10, r0, Skip_P1_Plus_Y					# If left paddle down switch is off, don't move it down
    beq		r22, r14, Skip_P1_Plus_Y				# If left paddle is at bottom, don't  move it down
    addi	r22, r22, 1								# Else, move it down
Skip_P1_Plus_Y:
    beq		r11, r0, Skip_P1_Minus_Y                # If left paddle up switch is off, don't  move it up
    beq		r22, r0, Skip_P1_Minus_Y                # If left paddle is at top, don't  move it up
    subi	r22, r22, 1                             # Else, move it up
Skip_P1_Minus_Y:
    beq		r12, r0, Skip_P2_Plus_Y                 # If right paddle down switch is off, don't move it down
    beq		r23, r14, Skip_P2_Plus_Y                # If right paddle is at bottom, don't  move it down
    addi	r23, r23, 1                             # Else, move it down
Skip_P2_Plus_Y:
    beq		r13, r0, Skip_P2_Minus_Y                # If right paddle up switch is off, don't  move it up
    beq		r23, r0, Skip_P2_Minus_Y                # If right paddle is at top, don't  move it up
    subi	r23, r23, 1                             # Else, move it up
Skip_P2_Minus_Y:

	# -------------------- BALL MOVEMENT --------------------
    
    movi 	r10, P1_X+P_WIDTH						# Left paddle  X coordinate
    movi 	r11, P2_X-1								# Right paddle X coordinate

X_Calculations:
	beq		r18, r10, B_Check_P_1_Hit				# If Ball X = Left paddle X, check if paddle was hit
	beq		r18, r11, B_Check_P_2_Hit				# If Ball X = Right paddle X, check if paddle was hit
    beq		r18, r16, B_Hit_Right					# If Ball X = Right wall, increase left score, reset ball
	beq		r18, r0,  B_Hit_Left					# If Ball X = Left wall, increase right score, reset ball
    br		Update_B_X
B_Check_P_1_Hit:
	blt		r19, r22, Update_B_X					# If Ball Y higher than left paddle Y, skip
    addi	r10, r22, P_HEIGHT
	bgt		r19, r10, Update_B_X					# If Ball Y lower than left paddle Y, skip
	movi	r20, 1									# Set ball X direction to right
    br		Update_B_X
B_Check_P_2_Hit:
	blt		r19, r23, Update_B_X					# If Ball Y higher than right paddle Y, skip
    addi	r11, r23, P_HEIGHT
	bgt		r19, r11, Update_B_X					# If Ball Y lower than right paddle Y, skip
	movi	r20, -1									# Set ball X direction to left
    br    Update_B_X
    
B_Hit_Right:
	V_LINE	0, HEIGHT, WIDTH-1, B_COL				# Draw vertical hit indicator line
    movi	r18, WIDTH/2							# Ball X Pos in middle
    movi	r19, HEIGHT/2							# Ball Y Pos in middle
	movi	r20, -1									# Ball X direction set to left
    ldw		r11, SCORE_P1(r0)						# Load score 1 (left paddle)
    addi	r11, r11, 1								# Increment score 1 (left paddle)
    stw		r11, SCORE_P1(r0)						# Store score 1 (left paddle)
    br    Update_B_X
B_Hit_Left:
	V_LINE	0, HEIGHT, 0, B_COL						# Draw vertical hit indicator line
    movi	r18, WIDTH/2							# Ball X Pos in middle
    movi	r19, HEIGHT/2							# Ball Y Pos in middle
	movi	r20, 1									# # Ball X direction set to right
    ldw		r12, SCORE_P2(r0)						# Load score 2 (right paddle)
    addi	r12, r12, 1								# Increment score 21 (right paddle)
    stw		r12, SCORE_P2(r0)						# Store score 2 (right paddle)
    br    Update_B_X
    
Update_B_X:
    add		r18, r18, r20							# Move the ball in the X direction
    
Y_Calculations:
    beq		r19, r17, B_Set_Y_Dir_Minus				# If Ball Y = Bottom wall, bounce
	beq		r19, r0, B_Set_Y_Dir_Plus               # If Ball Y = Top wall, bounce
    br		Update_B_Y
B_Set_Y_Dir_Minus:
	movi	r21, -1									# Ball Y direction set to up
    br		Update_B_Y
B_Set_Y_Dir_Plus:
	movi	r21, 1									# Ball Y direction set to down
    
Update_B_Y:
    add		r19, r19, r21							# Move the ball in the Y direction
    
    # -------------------- PAUSE --------------------
    
Pause:
.if USE_SNES_CONTROLLER == 1
	ldw		r8, CONTROLLER_A_FF(r0)
	andi	r8, r8, 0x3000							# Check if start or select is pressed
	beq		r0, r8, 1f
	
	# TODO: call fill color, call PrintPauseText, and call DrawScreen somewhere in here (probably on this line)
	
2:
	# We need to delay because it breaks if we don't
	# This will delay for 2 ms
    movui	r8, 50000
3:	subi	r8, r8, 1
	bgtu	r8, r0, 3b
	
	READ_SNES
	andi	r3, r3, 0x3000							# Loop while start or select hasn't been pressed again
	beq		r0, r3, 2b
1:
.else
    ldwio	r8, 0(r9)								# Read switches
    andi	r10, r8, 8								# Get switches 3 status
	beq		r10, r0, 1f								# Skip drawing pause text if button not pressed
    FILL_COLOR	BG_COL								# Clear screen
	call 	DrawScreen
	call	PrintPauseText
.if USE_DOUBLE_BUFFERED == 1
	call	SwapBuffers
.endif
1:	
    bne		r10, r0, Pause							# Branch to pause if switch 3 is on (stop looping)
.endif
    
    # -------------------- SWAP BUFFERS --------------------
.if USE_DOUBLE_BUFFERED == 1
	
    # Instead of waiting using the timer interrupt, we're going to swap the buffers
	# Part of swapping the buffers requires us to wait until the buffer has actually been swapped
	# This synchronizes us so the code runs once per frame
	call	SwapBuffers   
    # If this point is reached, pause is not activated, so loop to draw the next frame
	br Loop
    
.else
    
Wait:
    ldw  	r4, CONTINUE_FLAG(r0)					# Load the continue flag from mem.
    beq		r4, r0, Wait							# If flag is not set, loop back and keep waiting
    movi	r4, 0									# Otherwise, clear the flag
    stw		r4, CONTINUE_FLAG(r0)					# Store the continue flag from mem.
    br		Loop
	
.endif	

	# -------------------- DISPLAY WIN TEXT --------------------

Print_Win_Text:
	ldw		r2, WINNER(r0)							# Load winner from mem.
    
	# Draw the "PX Wins" where X is the winner number (1 or 2)
    DRAW_BIG_CHAR_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*0, TEXT_Y, P_CHAR, WIN_COL
    DRAW_BIG_CHAR			WIN_TEXT_X+(CHAR_WIDTH+1)*1, TEXT_Y, r2, WIN_COL
    DRAW_BIG_CHAR_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*2, TEXT_Y, SPACE_CHAR, WIN_COL
    DRAW_BIG_CHAR_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*3, TEXT_Y, W_CHAR, WIN_COL
    DRAW_BIG_CHAR_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*4, TEXT_Y, I_CHAR, WIN_COL
    DRAW_BIG_CHAR_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*5, TEXT_Y, N_CHAR, WIN_COL
    DRAW_BIG_CHAR_CONST	WIN_TEXT_X+(CHAR_WIDTH+1)*6, TEXT_Y, S_CHAR, WIN_COL
    
.if USE_DOUBLE_BUFFERED == 1
	call	SwapBuffers
.endif
    

Wait_For_Restart_Button:
    ldwio	r8, 0(r3)								# Read buttons
    andi	r10, r8, 1								# Get button 0 status
    bne		r10, r0, _start							# Branch to start if button is pressed
	br		Wait_For_Restart_Button					# Otherwise, keep waiting for a button press

End:
    br 		End


# -------------------- DRAWING --------------------

DrawScreen:
	subi 	sp, sp, 4
    stw 	ra,  0(sp)
	
	DRAW_PADDLE 	P1_X, r22, P_COL				# Draw left paddle
    DRAW_PADDLE 	P2_X, r23, P_COL				# Draw right paddle
    
   	DV_LINE	0, HEIGHT, WIDTH/2, MID_COL				# Draw midline
    
    ldw		r11, SCORE_P1(r0)						# Load left score
    ldw		r12, SCORE_P2(r0)						# Load right score
    DRAW_BIG_CHAR SCORE_1_X, SCORE_Y, r11, CHAR_COL	# Draw left score
    DRAW_BIG_CHAR SCORE_2_X, SCORE_Y, r12, CHAR_COL	# Draw right score
    
    DRAW_PIXEL_CONST_COL	r18, r19, B_COL			# Ball

	ldw 	ra,  0(sp)
    addi 	sp, sp, 4
	ret

PrintPauseText:
	subi 	sp, sp, 4
    stw 	ra,  0(sp)
	
	DRAW_BIG_CHAR_CONST	PAUSED_TEXT_X+(CHAR_WIDTH+1)*0, TEXT_Y, P_CHAR, PAUSED_COL
    DRAW_BIG_CHAR_CONST	PAUSED_TEXT_X+(CHAR_WIDTH+1)*1, TEXT_Y, A_CHAR, PAUSED_COL
    DRAW_BIG_CHAR_CONST	PAUSED_TEXT_X+(CHAR_WIDTH+1)*2, TEXT_Y, U_CHAR, PAUSED_COL
    DRAW_BIG_CHAR_CONST	PAUSED_TEXT_X+(CHAR_WIDTH+1)*3, TEXT_Y, S_CHAR, PAUSED_COL
    DRAW_BIG_CHAR_CONST	PAUSED_TEXT_X+(CHAR_WIDTH+1)*4, TEXT_Y, E_CHAR, PAUSED_COL
	DRAW_BIG_CHAR_CONST	PAUSED_TEXT_X+(CHAR_WIDTH+1)*5, TEXT_Y, D_CHAR, PAUSED_COL
	
    ldw 	ra,  0(sp)  
    addi 	sp, sp, 4
	ret

# r4: X
# r5: Y
# r6: Character number in character map
# r7: Colour value
DrawBigChar:
	subi 	sp, sp, 4*12
    stw 	r4,  0(sp)
    stw 	r5,  4(sp)
    stw 	r13, 8(sp)
    stw 	r14, 12(sp)
    stw 	r15, 16(sp)
    stw 	r16, 20(sp)
    stw 	r17, 24(sp)
    stw 	r18, 28(sp)
    stw 	r19, 32(sp)
    stw 	r20, 36(sp)
    stw 	r21, 40(sp)
    stw 	ra,  44(sp)
    
    mov 	r14, r4									# r14 <- Start X
    movi	r18, CHAR_HEIGHT
    movi	r13, CHAR_BEGIN_Y
    movi	r16, CHAR_WIDTH
    
    movi	r21, BG_COL
    
    slli	r6, r6, 2								# Multiply number to draw in r6 by 4 to get proper offset
    
    movia	r19, 0x80000000							# r19 <- Bitmask (1 + 31 zeros)
    
    ldw		r20, BIG_CHAR_MAP(r6)					# Load current char map
    
    # Two loops to draw the big char
	LoopX:  
    	movi 	r17, 7								# Reset current Y counter to 7
    	ldw 	r15, 4(sp) 							# r15 <- Start Y
        addi	r15, r15, 8							# Add 8 to start Y to begin drawing from bottom
		LoopY:  			
        	bge		r17, r18, Skip_Draw				# If current Y counter >= char height, skip draw
        	blt		r17, r13, Skip_Draw				# If current Y counter < char begin y, skip draw
			and		r5, r19, r20					# Check if current bit is set
            beq		r5, r0, Draw_BG_Col				# If current bit is 0, draw the background colour
            DRAW_PIXEL	r14, r15, r7				# Draw the pixel in the specified colour
            br		Skip_Draw			
        Draw_BG_Col:			
        	DRAW_PIXEL	r14, r15, r21				# Draw the pixel in the background colour
        Skip_Draw:			
        	srli	r19, r19, 1						# Shift bitmask right
            subi 	r15, r15, 1						# Decrement Y coordinate
            subi 	r17, r17, 1						# Decrement Y counter
            bge 	r17, r0, LoopY					# Loop if Y counter >= 0
        addi 	r14, r14, 1							# Increment X coordinate
        subi 	r16, r16, 1							# Decrement X counter
        bge 	r16, r0, LoopX						# Loop if X counter >= 0
    
    ldw 	ra,  44(sp)
    ldw 	r21, 40(sp)
    ldw 	r20, 36(sp)
    ldw 	r19, 32(sp)
    ldw 	r18, 28(sp)
    ldw 	r17, 24(sp)
    ldw 	r16, 20(sp)
    ldw 	r15, 16(sp)
    ldw 	r14, 12(sp) 
    stw 	r13, 8(sp)
    ldw 	r5,  4(sp)
    ldw 	r4,  0(sp)  
    addi 	sp, sp, 4*12
    ret
  
  
# r4: X
# r5: Y
# r6: Colour value
DrawPaddle:
	subi 	sp, sp, 40
    stw 	r4,  0(sp)
    stw 	r5,  4(sp)
    stw 	r6,  8(sp)
    stw 	r7,  12(sp)
    stw 	r16, 16(sp)
    stw 	r17, 20(sp)
    stw 	r18, 24(sp)
    stw 	r19, 28(sp)
    stw 	r20, 32(sp)
    stw 	ra,  36(sp)
    
	mov 	r16, r4									# r16 <- Start X
    mov 	r17, r4									
    addi 	r17, r17, P_WIDTH						# r17 <- End X
	
    mov 	r18, r5                                 # r18 <- Start Y
    mov 	r19, r5                                 
    addi 	r19, r19, P_HEIGHT                      # r19 <- End Y
	
    mov 	r20, r6									# r20 <- Colour

    1:  mov		r4, r18								# Line start Y <- start X
        mov		r5, r19								# Line end Y   <- end Y
        mov		r6, r16								# Line X	   <- current X
        mov		r7, r20								# Line colour  <- specified colour
        call 	DrawVLine							# Draw a single vertical line
        addi 	r16, r16, 1							# Increment current X
        blt 	r16, r17, 1b						# Loop if current X < end X
        
	ldw 	ra,  36(sp)
    ldw 	r20, 32(sp)
    ldw 	r19, 28(sp)
    ldw 	r18, 24(sp)
    ldw 	r17, 20(sp)
    ldw 	r16, 16(sp)
    ldw 	r7,  12(sp)  
    ldw 	r6,  8(sp)
    ldw 	r5,  4(sp)
    ldw 	r4,  0(sp)  
    addi	sp, sp, 40
    ret

	
# r4: Start X
# r5: End X
# r6: Y
# r7: Colour value
DrawHLine:
	subi 	sp, sp, 24
    stw 	r4,  0(sp)
    stw 	r5,  4(sp)
    stw 	r6,  8(sp)
    stw 	r16, 12(sp)
    stw 	r17, 16(sp)
    stw 	ra,  20(sp)
    
	mov  	r17, r4									# r17 <- Start X
    mov  	r16, r5									# r16 <- End X
    mov  	r5, r6									# r5  <- Y
    mov  	r6, r7									# r6  <- Colour
    
    1:	mov 	r4, r16								# Pixel X <- current X
        call 	WritePixel							# Draw one pixel
        subi	r16, r16, 1							# Decrement current X
        bge 	r16, r17, 1b						# Loop if current X >= start X
    
    ldw 	ra,  20(sp)
    ldw 	r17, 16(sp)  
    ldw 	r16, 12(sp)  
    ldw 	r6,  8(sp)
    ldw 	r5,  4(sp)
    ldw 	r4,  0(sp)   
    addi	sp, sp, 24
    ret

	
# r4: Start Y
# r5: End Y
# r6: X
# r7: Colour value
DrawVLine:
	subi	sp, sp, 24
    stw 	r4,  0(sp)
    stw 	r5,  4(sp)
    stw 	r6,  8(sp)
    stw 	r16, 12(sp)
    stw 	r17, 16(sp)
    stw 	ra,  20(sp)
    
    mov  	r17, r4									# r17 <- Start Y
    mov  	r16, r5									# r16 <- End Y
    mov  	r4, r6									# r4  <- X
    mov  	r6, r7									# r6  <- Colour
    
    1:	mov 	r5, r16                             # Pixel Y <- current Y
        call 	WritePixel							# Draw one pixel
        subi	r16, r16, 1                         # Decrement current Y
        bge 	r16, r17, 1b                        # Loop if current Y >= start Y
    
    ldw 	ra,  20(sp)
    ldw 	r17, 16(sp)  
    ldw 	r16, 12(sp)
    ldw 	r6,  8(sp)
    ldw 	r5,  4(sp)
    ldw 	r4,  0(sp)   
    addi	sp, sp, 24
    ret
    
	
# r4: Start Y
# r5: End Y
# r6: X
# r7: Colour value
DrawDVLine:
	subi 	sp, sp, 28
    stw 	r4,  0(sp)
    stw 	r5,  4(sp)
    stw 	r6,  8(sp)
    stw 	r16, 12(sp)
    stw 	r17, 16(sp)
    stw 	r18, 20(sp)
    stw 	ra,  24(sp)
    
    mov  	r17, r4									# r17 <- Start Y
    mov  	r16, r5									# r16 <- End Y
    mov  	r4, r6									# r4  <- X
    mov  	r6, r7									# r6  <- Colour
    
    1:	andi	r18, r16, 1							# r18 <- LSB of current Y
        bne		r18, r0, Skip_Draw_Pixel			# If current Y is odd, don't draw the current pixel
		mov 	r5, r16                             # Pixel Y <- current Y		
        call 	WritePixel							# Draw one pixel
Skip_Draw_Pixel:                                    
        subi 	r16, r16, 1                         # Decrement current Y
        bge 	r16, r17, 1b                        # Loop if current Y >= start Y
    
    ldw 	ra,  24(sp)
    ldw 	r18, 20(sp) 
    ldw 	r17, 16(sp)  
    ldw 	r16, 12(sp)
    ldw 	r6,  8(sp)
    ldw 	r5,  4(sp)
    ldw 	r4,  0(sp)   
    addi 	sp, sp, 28
    ret    

	
# r4: Colour value
FillColourFast:
	subi 	sp, sp, 20
    stw 	r8,   0(sp)
    stw 	r9,   4(sp)
    stw 	r10,  8(sp)
    stw 	r2,  12(sp)
    stw 	ra,  16(sp)

.if LOG2_BYTES_PER_PIXEL == 0
	# One pixel takes up 1 byte
	# Copy the colour stored in r4 into r8 4 times
	# This way r8 contains 4 colour values to write at once into the vga pixel buffer
    andi 	r8, r4, 0xff							# r8 <- LSB of r4 (1 colour value)
    slli 	r9, r8, 8								# r9 <- LSB of r4 shifted 1 byte left
    or   	r8, r8, r9								# r8 <- LSB of r4 in 1st byte + LSB of r4 in 0th byte
    slli 	r9, r8, 16                              # r9 <- r8 shifted 2 bytes left
    or   	r8, r9, r8                              # r8 <- LSB of r4 in all 4 bytes (4 colour values)
.elseif LOG2_BYTES_PER_PIXEL == 1
	# One pixel takes up 2 bytes
	# Copy the colour stored in r4 into r8 2 times
	# This way r8 contains 2 colour values to write at once into the vga pixel buffer
    andi 	r8, r4, 0xffff                          # r8 <- Last 2 bytes of r4 (1 colour value)
    slli 	r9, r8, 16                              # r9 <- r8 shifted 2 bytes left
	or   	r8, r9, r8                              # r8 <- Last 2 bytes of r4 both high and low order (2 colour values)
.elseif LOG2_BYTES_PER_PIXEL == 2
    # One pixel takes up 4 bytes
	# Don't need to do anything
.else
    .error "Error: Unknown pixel size"
.endif
	
	movia 	r9, (BYTES_PER_ROW)*HEIGHT-4			# r9 <- Offset of last pixel in vga pixel buffer

.if USE_DOUBLE_BUFFERED == 0
	movia 	r10, FRAMEBUFFER								# r10 <- Base address of vga pixel buffer
.else 
	call	GetBufferPointer
	mov		r10, r2									# r10 <- Base address of vga pixel buffer
.endif
	
	add   	r9, r9, r10								# r9 <- Address of last pixel in vga pixel buffer

# This is a temporary label!! You can do this!! :)
1:
    stwio 	r8, 0(r9)								# Write 1, 2, or 4 pixels at once to current pixel in vga pixel buffer
    subi  	r9, r9, 4								# Move to previous pixel
    nop												# Wait for the vga pixel buffer hardware
    nop												# Keep waiting ...
    nop												# Keep waiting ...
    nop												# Keep waiting ...
    nop												# Keep waiting ...
    nop												# Keep waiting ...
    bge   	r9, r10, 1b								# Loop if current pixel address >= first pixel address

    ldw 	r8,   0(sp)
    ldw 	r9,   4(sp)
    ldw 	r10,  8(sp)
    ldw 	r2,  12(sp)
    ldw 	ra,  16(sp)
    addi 	sp, sp, 20
    ret

	
ClearScreen:
	subi 	sp, sp, 24
    stw 	r4,  0(sp)
    stw 	r5,  4(sp)
    stw 	r6,  8(sp)
    stw 	r16, 12(sp)
    stw 	r17, 16(sp)
    stw 	ra,  20(sp)
    
    FILL_COLOR	BG_COL								# Fill the screen with the background colour
    
    # Clear the pixel buffer as well
    movi 	r16, WIDTH-1							# r16 <- End X
    1:	movi 	r17, HEIGHT-1						# r17 <- End Y
        2:  mov 	r4, r16							# Char X <- current X
            mov 	r5, r17							# Char Y <- current Y
            mov 	r6, r0                          # Char value <- 0 (blank)
            call 	WriteChar						# Draw one char
            subi 	r17, r17, 1                     # Decrement current Y
            bge 	r17, r0, 2b                     # Loop if current Y >= 0
        subi 	r16, r16, 1                         # Decrement current X
        bge 	r16, r0, 1b                         # Loop if current X >= 0
    
    ldw 	ra,  20(sp)
    ldw 	r17, 16(sp)
    ldw 	r16, 12(sp)  
    ldw 	r6,  8(sp)
    ldw 	r5,  4(sp)
    ldw 	r4,  0(sp)  
    addi 	sp, sp, 24
    ret

ClearScreenFast:
	subi 	sp, sp, 16
    stw 	r8,   0(sp)
    stw 	r9,   4(sp)
    stw 	r10,  8(sp)
    stw 	ra,  12(sp)
    
    movia   r8, CHARBUF
    movia   r9, CHARBUF + CHARBUF_SIZE - 4
    movia   r10, 0x20202020
1:
    stw     r10, 0(r9)
    # maybe we have to nop here?
    subi    r9, r9, 4
    bge     r9, r8, 1b
    
    ldw 	r8,   0(sp)
    ldw 	r9,   4(sp)
    ldw 	r10,  8(sp)
    ldw 	ra,  12(sp)
    addi 	sp, sp, 16
    ret

# r4: Column (x)
# r5: Row    (y)
# r6: Character value
WriteChar:
	subi 	sp, sp, 12								
    stw 	r4, 0(sp)								
    stw 	r5, 4(sp)
    stw 	r6, 8(sp)
    
	slli 	r5, r5, 7                               # r5 <- Calculated memory offset due to Y
    add 	r5, r5, r4                              # r5 <- Calculated memory offset for specified X,Y
    movia 	r4, CHARBUF                             # r4 <- Address of character buffer
    add 	r5, r5, r4                              # r5 <- Calculated memory address for specified X,Y
    stbio 	r6, 0(r5)                               # Write character to character buffer
    
    ldw 	r6, 8(sp)
    ldw 	r5, 4(sp)
    ldw 	r4, 0(sp)    
    addi	sp, sp, 12
    ret


# r4: X
# r5: Y
# r6: Colour value
WritePixel:
	subi 	sp, sp, 20
    stw 	r2, 0(sp)
    stw 	r4, 4(sp)
    stw 	r5, 8(sp)
    stw 	r6, 12(sp)
    stw 	ra, 16(sp)
    
.if USE_DOUBLE_BUFFERED == 0
    slli 	r5, r5, LOG2_BYTES_PER_ROW              # r5 <- Calculated memory offset due to Y
    slli 	r4, r4, LOG2_BYTES_PER_PIXEL            # r5 <- Calculated memory offset due to X
    add 	r5, r5, r4                              # r5 <- Calculated memory offset for specified X,Y
    movia 	r4, FRAMEBUFFER                         # r4 <- Address of character buffer
    add 	r5, r5, r4                              # r5 <- Calculated memory address for specified X,Y
.else 
    slli 	r5, r5, LOG2_BYTES_PER_ROW              # r5 <- Calculated memory offset due to Y
    slli 	r4, r4, LOG2_BYTES_PER_PIXEL            # r4 <- Calculated memory offset due to X
    add 	r5, r5, r4                              # r5 <- Calculated memory offset for specified X,Y
    call	GetBufferPointer			            # r2 <- Current buffer address
    add 	r5, r5, r2                              # r5 <- Calculated memory address for specified X,Y
.endif
 
.if LOG2_BYTES_PER_PIXEL == 0
  	stbio 	r6, 0(r5)								# Write 8-bit pixel to vga pixel buffer
.elseif LOG2_BYTES_PER_PIXEL == 1
    sthio 	r6, 0(r5)								# Write 16-bit pixel to vga pixel buffer
.elseif LOG2_BYTES_PER_PIXEL == 2
    stwio 	r6, 0(r5)								# Write 32-bit pixel to vga pixel buffer
.else
    .error "Error: Unknown pixel size"
.endif
    
    ldw 	ra, 16(sp)
    ldw 	r6, 12(sp)
    ldw 	r5, 8(sp)
    ldw 	r4, 4(sp)
    ldw 	r2, 0(sp)  
    addi 	sp, sp, 20
    ret

.if USE_DOUBLE_BUFFERED == 1

# u16 * GetBufferPointer(void)
# Returns a pointer to the current framebuffer
GetBufferPointer:
	ldb		r2, CUR_BUFFER(r0)
	bne		r0, r2, 1f
	movia	r2, FRAMEBUFFER_B
	ret
1:
	movia	r2, FRAMEBUFFER_A
	ret

# void SwapBuffers(void)
# Flips the current drawing buffer to screen
SwapBuffers:
	subi	sp, sp, 16
	stw		ra,  0(sp)
	stw		r2,  4(sp)
	stw		r8,  8(sp)
	stw		r9, 12(sp)
	
	call	GetBufferPointer
	movia	r8, VGAPIX_CONTROL
	stwio	r2, 4(r8)
	stwio	r2, 0(r8)
	
	# Busy-loop until buffer is done being displayed
1:
	ldwio	r9, 12(r8)
	andi	r9, r9, 1
	bne	r0, r9, 1b
	
	# Swap which buffer is drawn
	# CUR_BUFFER = (CUR_BUFFER == 0) ? 1 : 0;
	ldb		r8, CUR_BUFFER(r0)
	cmpeq	r8, r0, r8
	stb		r8, CUR_BUFFER(r0)
	
	ldw		ra,  0(sp)
	ldw		r2,  4(sp)
	ldw		r8,  8(sp)
	ldw		r9, 12(sp)
	addi	sp, sp, 16
	ret

.endif

.if USE_SNES_CONTROLLER == 1
# void SNESController_Initialize(void)
# Initializes registers relevant to the SNES controller reading
SNESController_Initialize:
    movia   r8, SNESCONTROLLER_BASE_ADDR
    # set the clock and data pins to outputs, everything else to inputs
    # (for now, (clock, latch, data) are (0, 1, 2))
    movia   r9, 0x00000003
    stwio   r9, 4(r8) # direction register
   
    # set clock high, latch low
    movia   r9, 0x00000001
    stwio   r9, 0(r8) # i/o reg
   
    # disable interrupts on parallel port
    stwio   r0, 8(r8) # intmask reg
    stwio   r0, 12(r8) # intmask reg
   
    ret

# R2: 16 bits of controller data
# R3: 16 bits of controller data (first frame)
# The values of CONTROLLER_A and CONTROLLER_A_FF are updated as well
SNESController_Read:
    movia   r8, SNESCONTROLLER_BASE_ADDR

    # prepare bit counter
    movi    r10, 16

    # prepare output register
    mov     r2, r0

    # set latch high
    movia   r9, 0x00000003
    stwio   r9, 0(r8)

    # set latch low
    andi    r9, r9, 1
1:
    # delay
    movui   r11, 10
2:  subi    r11, r11, 1
    bge     r11, r0, 2b

    # set clock low
    andi    r9, r9, 0xfffe
    stwio   r9, 0(r8)

    # read data bit
    ldwio   r12, 0(r8)
    slli    r2, r2, 1
    andi    r12, r12, 4
    cmpeq   r12, r12, r0
    or      r2, r2, r12

    # delay
    movui   r11, 10
2:  subi    r11, r11, 1
    bge     r11, r0, 2b

    # set clock high
    ori     r9, r9, 1
    stwio   r9, 0(r8)

    # if we need to read more bits, loop
    subi    r10, r10, 1
    bgt     r10, r0, 1b
	
	# calculate first-frame-ness
	# a button is first frame if it was not pressed last frame and is pressed this frame
	ldw		r3, CONTROLLER_A(r0)
	nor		r3, r3, r0
	and		r3, r3, r2
	stw		r3, CONTROLLER_A_FF(r0)
	stw		r2, CONTROLLER_A(r0)
	
	ret
   
.endif

.data
.global CONTINUE_FLAG
CONTINUE_FLAG:		.word 	0				# Gets set by the interval timer indicating that the loop can continue
SCORE_P1:			.word 	0				# Left player score
SCORE_P2:			.word 	0				# Right player score
WINNER:				.word	0				# Temporary variable to record the winning player's number (1 or 2)
CUR_BUFFER:			.word	0				# 1 if we're currently writing to framebuffer A, 0 for buffer B

.if USE_SNES_CONTROLLER == 1
CONTROLLER_A:		.word	0				# Somewhere to store the controller values (we can't just read the controller again)
CONTROLLER_A_FF:	.word	0				# Keeps track of when a new button is pressed
.endif

# Character map holds the data for the big score characters
# The MSB       is column 0 (left)  of the char, drawn from bottom to top
# The next byte is column 1         of the char, drawn from bottom to top
# The next byte is column 2         of the char, drawn from bottom to top
# The LSB       is column 3 (right) of the char, drawn from bottom to top

BIG_CHAR_MAP:	.word 0x3E223E00	# 0
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
				.word 0x3E0A3E00	# A
				.word 0x3E203E00	# U
				.word 0x3E2A2200	# E
				.word 0x023E0200	# T
				.word 0x3E0A3400	# R
				.word 0x3e221c00	# D

.align 4
prog_end:
# Data will be placed here at runtime; namely, the framebuffers
