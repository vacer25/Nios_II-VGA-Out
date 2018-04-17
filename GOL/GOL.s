# TARGET_SYSTEM = 0: DE0
# TARGET_SYSTEM = 1: DE1-SoC
# TARGET_SYSTEM = 2: DE2 / DE2-115
# TARGET_SYSTEM = 3: DE10-Lite

.equ TARGET_SYSTEM, 0					# Used to indicate which FPGA the code will be compiled for
.equ USE_WRAPAROUND, 0					# Used to make the simulation wrap around the edges

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
    .equ D_COL, 	0x00
    .equ L_COL, 	0xFF
.elseif TARGET_SYSTEM == 1 || TARGET_SYSTEM == 2
    # 16-bit colours: DE1-SoC, DE2, DE2-115
    .equ D_COL, 	0x0000
    .equ L_COL, 	0xFFFF
.endif

.equ FRAMEBUFFER_SIZE, HEIGHT * BYTES_PER_ROW
.equ FRAMEBUFFER_A, prog_end
.equ FRAMEBUFFER_B, FRAMEBUFFER_A + FRAMEBUFFER_SIZE

.equ CHARBUF, 0x09000000					# Character buffer. Same on all boards.
.equ CHARBUF_SIZE, 0x2000

# -------------------- HARDWARE --------------------

.if TARGET_SYSTEM == 0 || TARGET_SYSTEM == 2
    # DE0, DE2, DE2-115
    .equ VGAPIX_CONTROL, 0x10003020
    .equ TIMER_BASE_ADDR, 0x10002000
    .equ SWITCHES_BASE_ADDR, 0x10000040
    .equ BUTTONS_BASE_ADDR, 0x10000050
.elseif TARGET_SYSTEM == 1 || TARGET_SYSTEM == 3
    # DE1-SoC, DE10-Lite
    .equ VGAPIX_CONTROL, 0xff203020
    .equ TIMER_BASE_ADDR, 0xff202000
    .equ SWITCHES_BASE_ADDR, 0xff200040
    .equ BUTTONS_BASE_ADDR, 0xff200050
.endif

.equ TIMER_INTERVAL, 2500000

# -------------------- REGISTER USAGE --------------------

# r10: Current frame buffer address (set in GetBufferPointer called by SwapBuffers)
# r11: Non-current frame buffer address (set in GetOppositeBufferPointer called by SwapBuffers)

# -------------------- MACROS --------------------

.macro	FILL_COLOR	col
	movui	r4, \col
    call 	FillColourFast
.endm

.macro	DRAW_PIXEL	x, y, col
	mov		r4, \x
	mov		r5, \y
    mov		r6, \col
    call 	WritePixel
.endm

.macro	DRAW_PIXEL_CONST	x, y, col
	movui		r4, \x
	movui		r5, \y
    movui		r6, \col
    call 	WritePixel
.endm

.macro	DRAW_PIXEL_CONST_COL	x, y, col
	mov		r4, \x
	mov		r5, \y
    movui	r6, \col
    call 	WritePixel
.endm

.macro	IS_LIVE_CELL	x, y
	subi 	sp, sp, 4
    stw 	r4, 0(sp)
	
	mov		r4, \x
	mov		r5, \y
    call 	ReadPixel
    movia	r4, L_COL
	xor		r2, r4, r2
	cmpeq	r2, r0, r2
	
    ldw 	r4, 0(sp)
    addi 	sp, sp, 4
.endm

.macro	IS_LIVE_CELL_CONST	x, y
	subi 	sp, sp, 4
    stw 	r4, 0(sp)
	
	movui	r4, \x
	movui	r5, \y
    call 	ReadPixel
    movia	r4, L_COL
	xor		r2, r4, r2
	cmpeq	r2, r0, r2
	
    ldw 	r4, 0(sp)
    addi 	sp, sp, 4
.endm

.macro  READ_BUTTONS
    call    Buttons_Read
.endm

.macro  GET_RANDOM_NUM
    call    GenerateRandomNumber
.endm

.macro  MIN, dest, src1, src2
    blt		\src1, \src2, 1f
	mov		\dest, \src2
	br		2f
1:
	mov		\dest, \src1
2:
.endm

.macro  MAX, dest, src1, src2
    bgt		\src1, \src2, 1f
	mov		\dest, \src2
	br		2f
1:
	mov		\dest, \src1
2:
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

	# Initialize framebuffers to empty
	call 	GetBufferPointer
	movi	r4, 0x0000
	call	FillColourFast
	call	SwapBuffers
	movi	r4, 0x0000
	call	FillColourFast
	call	SwapBuffers
	
    /*
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
	*/
	
    # This is required in case the character buffer might of been in use before we started running
	call    ClearScreenFast
	
	# Make two gliders on both buffers
	call 	InitBoard
	call	SwapBuffers   
	call 	InitBoard
	call	SwapBuffers   
	
	# -------------------- TESTING --------------------
	
	/*
	DRAW_PIXEL_CONST	0, 3, 0xF0
    
    IS_LIVE_CELL_CONST	0, 3
	beq		r2, r0, 1f
	DRAW_PIXEL_CONST	0, 5, 0xAA
1:
	IS_LIVE_CELL_CONST	2, 3
	beq		r2, r0, 2f
	DRAW_PIXEL_CONST	2, 5, 0xAA
2:
	*/
	
	# -------------------- WAIT FOR START --------------------

WaitForStart:
    # Advance the RNG
    # This way, we will get different random values depending on when they start the game
    GET_RANDOM_NUM

    READ_BUTTONS									# Read buttons
    andi	r3, r3, 1								# Get button 0 status (first frame)
    bne		r3, r0, Loop							# Branch to Loop if button is pressed
    br		WaitForStart							# Keep waiting otherwise
    
# -------------------- LOOP --------------------
    
Loop:
    
    # -------------------- DRAWING --------------------
    
    call 	UpdateGOL
    
    # -------------------- PAUSE --------------------
    
#Pause:
#    movia	r9, SWITCHES_BASE_ADDR					# Switches I/O Address
#    ldwio	r8, 0(r9)								# Read switches
#    andi	r12, r8, 1								# Get switches 1 status
#	 beq		r12, r0, 1f							# Skip drawing pause text if button not pressed
#    bne		r12, r0, Pause						# Branch to pause if switch 3 is on (stop looping)

    # -------------------- SWAP BUFFERS --------------------
	
    # Instead of waiting using the timer interrupt, we're going to swap the buffers
	# Part of swapping the buffers requires us to wait until the buffer has actually been swapped
	# This synchronizes us so the code runs once per frame
	call	SwapBuffers   
    # If this point is reached, pause is not activated, so loop to draw the next frame
	br Loop

End:
    br 		End

# -------------------- LOGIC --------------------

InitBoard:
	subi 	sp, sp, 4
    stw 	ra,  0(sp)

	#GLIDER 1
	#      0
	#  x   1
	#   x  2
	# xxx  3
	#0123  
	
	#GLIDER 2
	#      0
	#  x   1
	# x    2
	# xxx  3
	#0123  

    DRAW_PIXEL_CONST	1+1, 1+0, L_COL
    DRAW_PIXEL_CONST	1+2, 1+1, L_COL
    DRAW_PIXEL_CONST	1+0, 1+2, L_COL
    DRAW_PIXEL_CONST	1+1, 1+2, L_COL
    DRAW_PIXEL_CONST	1+2, 1+2, L_COL
	
    DRAW_PIXEL_CONST	(HEIGHT-4)+1, 2+0, L_COL
    DRAW_PIXEL_CONST	(HEIGHT-4)+0, 2+1, L_COL
    DRAW_PIXEL_CONST	(HEIGHT-4)+0, 2+2, L_COL
    DRAW_PIXEL_CONST	(HEIGHT-4)+1, 2+2, L_COL
    DRAW_PIXEL_CONST	(HEIGHT-4)+2, 2+2, L_COL
	
	# Init. max & min values to worst-case senario
	mov		r12, r0                                         # Current min X = 0
	movi	r13, WIDTH										# Current max X = WIDTH
	mov		r14, r0                                         # Current min Y = 0
	movi	r15, HEIGHT                                     # Current max Y = HEIGHT

    ldw 	ra,  0(sp)  
    addi 	sp, sp, 4
    ret

UpdateGOL:
	subi 	sp, sp, 4*14
    stw 	r2,  0(sp)										# Cell is alive flag
    stw 	r3,  4(sp)										# Neighbor counter
    # stw 	r4,  8(sp)										# Reserved for pixel write
    # stw 	r5,  12(sp)										# Reserved for pixel write
    # stw 	r6,  16(sp)										# Unused
    stw 	r16, 20(sp)										# Current X
	stw 	r17, 24(sp)										# Current Y
    stw 	r18, 28(sp)                                     # Current offset X
    stw 	r19, 32(sp)										# Current offset Y
    stw 	r20, 36(sp)										# Current X + 1
    stw 	r21, 40(sp)										# Current Y + 1
    stw 	r22, 44(sp)										# Constant 2
    stw 	r23, 48(sp)										# Constant 3
    stw 	ra,  52(sp)

	movi	r22, 2
	movi	r23, 3
	
	mov  	r17, r15										# r17 <- Max Y of curent GOL pattern
    1:	mov  	r16, r13									# r16 <- Max X of curent GOL pattern
		2:	mov		r3, r0									# Reset neighbor counter		
			
			subi	r19, r17, 1								# r19 <- Current Y - 1
			addi	r21, r17, 1								# r21 <- Current Y - 1
			3:	subi	r18, r16, 1							# r18 <- Current X - 1
				addi	r20, r16, 1							# r20 <- Current X + 1
				4:		
						
					IS_LIVE_CELL	r18, r19				# r2 <- Is current neighbor alive (in prev. itteration)
					add		r3, r3, r2						# r3 <- Add up neighbor count around current cell
						
				addi 	r18, r18, 1                 		# Increment current offset X
				ble 	r18, r20, 4b                		# Loop if current offset X <= Current X + 1
			addi 	r19, r19, 1             				# Increment current offset Y
			ble 	r19, r21, 3b            				# Loop if current offset Y <= Current X + 1
			
				IS_LIVE_CELL	r16, r17					# r2 <- Is current cell alive (in prev. itteration)
				sub		r3, r3, r2							# Subtract current cell's state
						
				bne		r2, r0, Current_Cell_Alive			# Go to Current_Cell_Alive is current cell is not 0 (alive)
		
Current_Cell_Not_Alive:			
				bne		r3, r23, Set_Current_Cell_Not_Alive	# If current cell is not alive, only make it alive if neighbor count is exactly 3
				DRAW_PIXEL_CONST_COL	r16, r17, L_COL		# Otherwise, this cell is now alive
				br		Continue_GOL_Loop	
				
Current_Cell_Alive:
				bge		r3, r22, _2_Or_More_Neighbors:		# Go to _2_Or_More_Neighbors is neighbor count >= 2
				DRAW_PIXEL_CONST_COL	r16, r17, D_COL		# Otherwise, this cell is now not alive
				br		Continue_GOL_Loop	
_2_Or_More_Neighbors:
				ble		r3, r23, Set_Current_Cell_Alive		# Keep alive if neighbor count <= 3
				DRAW_PIXEL_CONST_COL	r16, r17, D_COL		# Otherwise, this cell is now not alive
				br		Continue_GOL_Loop	

Set_Current_Cell_Not_Alive:
				DRAW_PIXEL_CONST_COL	r16, r17, D_COL		
				br		Continue_GOL_Loop	
Set_Current_Cell_Alive:
				DRAW_PIXEL_CONST_COL	r16, r17, L_COL		
				
Continue_GOL_Loop:
				
			#IS_LIVE_CELL	r16, r17
			#bne		r2, r0, Skip_Fill
			#DRAW_PIXEL_CONST_COL	r16, r17, 0xF0
    
#Skip_Fill:
				
		subi 	r16, r16, 1                         		# Decrement current X
		bge 	r16, r12, 2b                         		# Loop if current X >= Min X of curent GOL pattern
	subi 	r17, r17, 1                     				# Decrement current Y
	bge 	r17, r14, 1b                     				# Loop if current Y >= Min Y of curent GOL pattern
	
	call 	GetGOLBounds									# Get the min/max X&Y coordinates of curent GOL pattern
	
    ldw 	ra,  52(sp)
    ldw 	r23, 48(sp)
    ldw 	r22, 44(sp)
    ldw 	r21, 40(sp)
    ldw 	r20, 36(sp)
    ldw 	r19, 32(sp)
    ldw 	r18, 28(sp)
    ldw 	r17, 24(sp)
    ldw 	r16, 20(sp)  
    # ldw 	r6,  16(sp)
    # ldw 	r5,  12(sp)
    # ldw 	r4,  8(sp)  
    ldw 	r3,  4(sp)
    ldw 	r2,  0(sp)  
    addi 	sp, sp, 4*14
    ret
	
# r12: Returned min X coordinate
# r13: Returned max X coordinate
# r14: Returned min Y coordinate
# r15: Returned max Y coordinate
GetGOLBounds:
	subi 	sp, sp, 4*11
    stw 	r2,  0(sp)										# Cell is alive flag
    stw 	r3,  4(sp)										# Did check current row flag
    # stw 	r4,  8(sp)										# Reserved for pixel write
    # stw 	r5,  12(sp)										# Reserved for pixel write
    stw 	r6,  16(sp)										# Current min X
	stw 	r7,  20(sp)										# Current max X
	stw 	r8,  24(sp)										# Current min Y
	stw 	r9,  28(sp)										# Current max Y
    stw 	r16, 32(sp)										# Current X
	stw 	r17, 36(sp)										# Current Y
	stw 	ra,  40(sp)
    
	# Init. max & min values to worst-case senario
	movi	r6, WIDTH										# Current min X = WIDTH
	mov		r7, r0                                          # Current max X = 0
	movi	r8, HEIGHT                                      # Current min Y = HEIGHT
	mov		r9, r0                                          # Current max Y = 0
	
	# Reset min/max Y & Y
	mov		r12, r6
	mov		r13, r7
	mov		r14, r8
	mov		r15, r9
	
	movi 	r17, HEIGHT-2									# r17 <- End Y
    LoopY:	
	
		mov		r3, r0										# Reset did check current row flag
	
		movi 	r16, WIDTH-2								# r16 <- End X
		LoopX:	
	
		IS_LIVE_CELL	r16, r17							# r2 <- Is current call alive (in prev. itteration)
		
		beq		r2,  r0, Cell_Not_Alive_Col_Check			# If this cell is not alive, skip it
		MIN		r12, r6, r16								# Min X = MIN(Min X, Current X)
		MAX		r13, r7, r16								# Max X = MAX(Max X, Current X)
		mov		r6,  r12									# Current min X = Min X
		mov		r7,  r13									# Current max X = Max X
		
		bne		r3,  r0, Cell_Not_Alive_Col_Check			# If this row has already been used to set min/max Y, skip it
		MIN		r14, r8, r17								# Min Y = MIN(Min Y, Current Y)
		MAX		r15, r9, r17								# Max Y = MAX(Max Y, Current Y)
		mov		r8,  r14									# Current min Y = Min Y
		mov		r9,  r15									# Current max Y = Max Y
		movi	r3, 1										# Did check current row = true
		
Cell_Not_Alive_Col_Check:		
	
		subi 	r16, r16, 1                         		# Decrement current X
		bgt 	r16, r0, LoopX                        		# Loop if current X > 0
	subi 	r17, r17, 1                     				# Decrement current Y
	bgt 	r17, r0, LoopY                     				# Loop if current Y > 0
	
	subi	r12, r12, 2										# Decrease min X by 2 (for padding)
	addi	r13, r13, 2                                     # Increase max X by 2 (for padding)
	subi	r14, r14, 2										# Decrease min Y by 2 (for padding)
	addi	r15, r15, 2                                     # Increase max Y by 2 (for padding)
	
	ldw 	ra,  40(sp)
	ldw 	r17, 36(sp)
	ldw 	r16, 32(sp)
    ldw 	r9,  28(sp)
	ldw 	r8,  24(sp)
	ldw 	r7,  20(sp)
	ldw 	r6,  16(sp)
    # ldw 	r5,  12(sp)
    # ldw 	r4,  8(sp)  
    ldw 	r3,  4(sp)
    ldw 	r2,  0(sp)  
    addi 	sp, sp, 4*11
    ret

	
# -------------------- DRAWING --------------------
	
# r4: Colour value
FillColourFast:
	subi 	sp, sp, 20
    stw 	r8,   0(sp)
    stw 	r9,   4(sp)
    stw 	r12,  8(sp)
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

	# call	GetBufferPointer
	mov		r12, r10								# r12 <- Base address of vga pixel buffer (r10 contains current framebuffer address)
	
	add   	r9, r9, r12 							# r9 <- Address of last pixel in vga pixel buffer

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
    bge   	r9, r12, 1b								# Loop if current pixel address >= first pixel address

    ldw 	r8,   0(sp)
    ldw 	r9,   4(sp)
    ldw 	r12,  8(sp)
    ldw 	r2,  12(sp)
    ldw 	ra,  16(sp)
    addi 	sp, sp, 20
    ret


ClearScreenFast:
	subi 	sp, sp, 16
    stw 	r8,   0(sp)
    stw 	r9,   4(sp)
    stw 	r12,  8(sp)
    stw 	ra,  12(sp)
    
    movia   r8, CHARBUF
    movia   r9, CHARBUF + CHARBUF_SIZE - 4
    movia   r12, 0x20202020
1:
    stw     r12, 0(r9)
    # maybe we have to nop here?
    subi    r9, r9, 4
    bge     r9, r8, 1b
    
    ldw 	r8,   0(sp)
    ldw 	r9,   4(sp)
    ldw 	r12,  8(sp)
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
    #stw 	r6, 12(sp)
    stw 	ra, 16(sp)
    
Check_To_Limit_X_Write:
	cmpgei	r2, r4, WIDTH							# r2 <- X >= WIDTH
	bne		r2, r0, Limit_X_To_Max_Write			# Limit X if X >= WIDTH
	cmplt	r2, r4, r0								# r2 <- X < 0
	bne		r2, r0, Limit_X_To_Min_Write			# Limit X if X < WIDTH
	br 		Check_To_Limit_Y_Write					# Otherwise, check to limit Y
Limit_X_To_Max_Write:
.if USE_WRAPAROUND == 1
	subi	r4, r4, WIDTH							# r4 <- X - WIDTH
	br		Check_To_Limit_Y_Write
.else
	#mov		r6, r0									# Without wraparound, if X is out of bounds, this cell is not alive
	br		End_WritePixel
.endif
Limit_X_To_Min_Write:
.if USE_WRAPAROUND == 1
	movui	r2, WIDTH								# r2 <- WIDTH
	add		r4, r2, r4								# r4 <- WIDTH + (-X)
	br		Check_To_Limit_Y_Write
.else
	#mov		r6, r0									# Without wraparound, if Y is out of bounds, this cell is not alive
	br		End_WritePixel
.endif
	
Check_To_Limit_Y_Write:
	cmpgei	r2, r5, HEIGHT							# r2 <- Y >= HEIGHT
	bne		r2, r0, Limit_Y_To_Max_Write			# Limit Y if Y >= HEIGHT
	cmplt	r2, r5, r0								# r2 <- Y < 0
	bne		r2, r0, Limit_Y_To_Min_Write			# Limit Y if Y < HEIGHT
	br 		Write_Pixel_Value						# Otherwise, just write pixel value
Limit_Y_To_Max_Write:
.if USE_WRAPAROUND == 1
	subi	r5, r5, HEIGHT							# r5 <- Y - HEIGHT
	br		Write_Pixel_Value
.else
	#mov		r6, r0									# Without wraparound, if Y is out of bounds, this cell is not alive
	br		End_WritePixel
.endif
Limit_Y_To_Min_Write:
.if USE_WRAPAROUND == 1
	movui	r2, HEIGHT								# r2 <- HEIGHT
	add		r5, r2, r5								# r5 <- HEIGHT + (-Y)
	br		Write_Pixel_Value
.else
	#mov		r6, r0									# Without wraparound, if Y is out of bounds, this cell is not alive
	br		End_WritePixel
.endif
	
Write_Pixel_Value:
    slli 	r5, r5, LOG2_BYTES_PER_ROW              # r5 <- Calculated memory offset due to Y
    slli 	r4, r4, LOG2_BYTES_PER_PIXEL            # r4 <- Calculated memory offset due to X
    add 	r5, r5, r4                              # r5 <- Calculated memory offset for specified X,Y
    # call	GetBufferPointer			            # r2 <- Current buffer address
    add 	r5, r5, r10                             # r5 <- Calculated memory address for specified X,Y (r10 contains current framebuffer address)
 
.if LOG2_BYTES_PER_PIXEL == 0
  	stbio 	r6, 0(r5)								# Write 8-bit pixel to vga pixel buffer
.elseif LOG2_BYTES_PER_PIXEL == 1
    sthio 	r6, 0(r5)								# Write 16-bit pixel to vga pixel buffer
.elseif LOG2_BYTES_PER_PIXEL == 2
    stwio 	r6, 0(r5)								# Write 32-bit pixel to vga pixel buffer
.else
    .error "Error: Unknown pixel size"
.endif
    
End_WritePixel:
    ldw 	ra, 16(sp)
    #ldw 	r6, 12(sp)
    ldw 	r5, 8(sp)
    ldw 	r4, 4(sp)
    ldw 	r2, 0(sp)  
    addi 	sp, sp, 20
    ret

	
# r4: X
# r5: Y
# r2: Returned colour value (output)
ReadPixel:
	subi 	sp, sp, 12
    stw 	r4, 0(sp)
    stw 	r5, 4(sp)
    stw 	ra, 8(sp)
	
Check_To_Limit_X_Read:
	cmpgei	r2, r4, WIDTH							# r2 <- X >= WIDTH
	bne		r2, r0, Limit_X_To_Max_Read				# Limit X if X >= WIDTH
	cmplt	r2, r4, r0								# r2 <- X < 0
	bne		r2, r0, Limit_X_To_Min_Read				# Limit X if X < WIDTH
	br 		Check_To_Limit_Y_Read					# Otherwise, check to limit Y
Limit_X_To_Max_Read:
.if USE_WRAPAROUND == 1
	subi	r4, r4, WIDTH							# r4 <- X - WIDTH
	br		Check_To_Limit_Y_Read
.else
	mov		r2, r0									# Without wraparound, if X is out of bounds, this cell is not alive
	br		End_ReadPixel
.endif
Limit_X_To_Min_Read:
.if USE_WRAPAROUND == 1
	movui	r2, WIDTH								# r2 <- WIDTH
	add		r4, r2, r4								# r4 <- WIDTH + (-X)
	br		Check_To_Limit_Y_Read
.else
	mov		r2, r0									# Without wraparound, if Y is out of bounds, this cell is not alive
	br		End_ReadPixel
.endif
	
Check_To_Limit_Y_Read:
	cmpgei	r2, r5, HEIGHT							# r2 <- Y >= HEIGHT
	bne		r2, r0, Limit_Y_To_Max_Read				# Limit Y if Y >= HEIGHT
	cmplt	r2, r5, r0								# r2 <- Y < 0
	bne		r2, r0, Limit_Y_To_Min_Read				# Limit Y if Y < HEIGHT
	br 		Read_Pixel_Value						# Otherwise, just read pixel value
Limit_Y_To_Max_Read:
.if USE_WRAPAROUND == 1
	subi	r5, r5, HEIGHT							# r5 <- Y - HEIGHT
	br		Read_Pixel_Value
.else
	mov		r2, r0									# Without wraparound, if Y is out of bounds, this cell is not alive
	br		End_ReadPixel
.endif
Limit_Y_To_Min_Read:
.if USE_WRAPAROUND == 1
	movui	r2, HEIGHT								# r2 <- HEIGHT
	add		r5, r2, r5								# r5 <- HEIGHT + (-Y)
	br		Read_Pixel_Value
.else
	mov		r2, r0									# Without wraparound, if Y is out of bounds, this cell is not alive
	br		End_ReadPixel
.endif
	
Read_Pixel_Value:
    slli 	r5, r5, LOG2_BYTES_PER_ROW              # r5 <- Calculated memory offset due to Y
    slli 	r4, r4, LOG2_BYTES_PER_PIXEL            # r4 <- Calculated memory offset due to X
    add 	r5, r5, r4                              # r5 <- Calculated memory offset for specified X,Y
    # call	GetOppositeBufferPointer			    # r2 <- Non-current buffer address
    add 	r5, r5, r11                             # r5 <- Calculated memory address for specified X,Y (r11 contains non-current framebuffer address)
 
.if LOG2_BYTES_PER_PIXEL == 0
  	ldbuio 	r2, 0(r5)								# Read 8-bit pixel from vga pixel buffer
.elseif LOG2_BYTES_PER_PIXEL == 1
    ldhuio 	r2, 0(r5)								# Read 16-bit pixel from vga pixel buffer
.elseif LOG2_BYTES_PER_PIXEL == 2
    ldwuio 	r2, 0(r5)								# Read 32-bit pixel from vga pixel buffer
.else
    .error "Error: Unknown pixel size"
.endif
   
End_ReadPixel:
    ldw 	ra, 8(sp)
    ldw 	r5, 4(sp)
    ldw 	r4, 0(sp)
    addi 	sp, sp, 12
    ret


# r10: Returns a pointer to the current framebuffer
GetBufferPointer:
	ldb		r10, CUR_BUFFER(r0)
	bne		r0, r10, 1f
	movia	r10, FRAMEBUFFER_B
	ret
1:
	movia	r10, FRAMEBUFFER_A
	ret

# r11: Returns a pointer to the non-current framebuffer
GetOppositeBufferPointer:
	ldb		r11, CUR_BUFFER(r0)
	beq		r0, r11, 1f
	movia	r11, FRAMEBUFFER_B
	ret
1:
	movia	r11, FRAMEBUFFER_A
	ret

# void SwapBuffers(void)
# Flips the current drawing buffer to screen
SwapBuffers:
	subi	sp, sp, 16
	stw		ra,  0(sp)
	stw		r2,  4(sp)
	stw		r8,  8(sp)
	stw		r9, 12(sp)
	
	# call	GetBufferPointer
	movia	r8, VGAPIX_CONTROL
	stwio	r10, 4(r8)						# (r10 contains current framebuffer address)
	stwio	r10, 0(r8)						# (r10 contains current framebuffer address)
	
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
	
	call	GetBufferPointer				# r10 <- Current buffer pointer
	call	GetOppositeBufferPointer		# r11 <- Non-current buffer address
	
	ldw		ra,  0(sp)
	ldw		r2,  4(sp)
	ldw		r8,  8(sp)
	ldw		r9, 12(sp)
	addi	sp, sp, 16
	ret

	
# r2: 32 bits of button data
# r3: 32 bits of button data (first frame)
# The values of BUTTON_STATUS and BUTTON_STATUS_FF are updated as well
Buttons_Read:
    subi    sp, sp, 4
    stw     r8,   0(sp)
	
    movia   r8, BUTTONS_BASE_ADDR
	
	# Read buttons
	ldwio	r2, 0(r8)								
	
	# calculate first-frame-ness
	# a button is first frame if it was not pressed last frame and is pressed this frame
	ldw		r3, BUTTON_STATUS(r0)
	nor		r3, r3, r0
	and		r3, r3, r2
	stw		r3, BUTTON_STATUS_FF(r0)
	stw		r2, BUTTON_STATUS(r0)
	
    ldw     r8,   0(sp)
    addi    sp, sp, 4
	ret


# Taken from https://en.wikipedia.org/wiki/Xorshift
GenerateRandomNumber:
    subi    sp, sp, 4*3
    stw     r21, 4*0(sp)
    stw     r22, 4*1(sp)
    stw     r23, 4*2(sp)

    # r2: output/temp, r21: state ptr, r22: s, r23: t
    movia   r21, RNGState

#     uint32_t s, t = state[3];
    ldw     r23, 4*3(r21)

#     t ^= t >> 2;
    srli    r2, r23, 2
    xor     r23, r2, r23

#     t ^= t << 1;
    slli    r2, r23, 1
    xor     r23, r2, r23

#     state[3] = state[2]; state[2] = state[1]; state[1] = s = state[0];
    ldw     r2, 4*2(r21)
    stw     r2, 4*3(r21)
    ldw     r2, 4*1(r21)
    stw     r2, 4*2(r21)
    ldw     r22, 4*0(r21)
    stw     r22, 4*1(r21)

#     t ^= s;
    xor     r23, r22, r23

#     t ^= s << 4;
    slli    r2, r22, 4
    xor     r23, r2, r23

#     state[0] = t;
    stw     r23, 4*0(r21)

#     return t + (state[4] += 362437);
    # we don't care about s anymore, so we can use r22 as a temp. reg
    ldw     r2, 4*4(r21)
    movia   r22, 362437
    add     r2, r2, r22
    stw     r2, 4*4(r21)
    add     r2, r2, r23
    
    ldw     r21, 4*0(sp)
    ldw     r22, 4*1(sp)
    ldw     r23, 4*2(sp)
    addi    sp, sp, 4*3
    ret

.data
.global CONTINUE_FLAG
CONTINUE_FLAG:		.word 	0				# Gets set by the interval timer indicating that the loop can continue
CUR_BUFFER:			.word	0				# 1 if we're currently writing to framebuffer A, 0 for buffer B

BUTTON_STATUS:		.word 	0				# Somewhere to store the buttons values
BUTTON_STATUS_FF:	.word	0				# Keeps track of when a new button is pressed

RNGState:
    # default RNG seed
    .word 0xdeadbeef, 0xcafef00d, 0x31415927, 0xfedcba98, 0x01234567

.align 4
prog_end:
# Data will be placed here at runtime; namely, the framebuffers
