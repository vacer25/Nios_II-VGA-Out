# Nios II VGA Out
Various VGA video output projects on the NIOS II processor, written in assembly.

The programs are designed to be compatible with Altera DE0, DE1-SoC, DE2 / DE2-115, and DE10-Lite FPGA boards.  
So far, the code has only been tested on the DE1-SoC board and is confirmed to be working.

## Projects so far:                                         
1. **Pong**
	- Basic two player implementation
	- Switches 9/8 used to control left paddle, 1/0 for right paddle
	- Button 0 is used to restart the game
	- First player to reach a score of 9 wins
	
## Todo:
1. ~~Figure out double buffering~~
	- Done
1. Implement some form of randomness (starting ball directions, etc...)
1. Make the score numbers the same size on different display sizes
1. Make the ball speed constant on all display sizes
	- In progress
1. Interface with the SNES controller
	- In progress