# Nios II VGA Out
Various VGA video output projects on the NIOS II processor, written in assembly.

The programs are designed to be compatible with Altera DE0, DE1-SoC, DE2 / DE2-115, and DE10-Lite FPGA boards.  
So far, the code has only been tested on the DE1-SoC board and is confirmed to be working.

## [Simulator website](https://cpulator.01xz.net/?sys=nios-de0)

## Projects so far:                                         
* **Pong**  
	- Basic two player implementation
	- Switches 9/8 control left paddle and 1/0 control right paddle
	- With SNES controller support, Up/Down control left paddle and X/B control right paddle
	- Switch 3 is used to pause the game
	- Button 0 is used to restart the game
	- First player to reach a score of 9 wins
	
	<br>
	<img src="Pong/Screenshots/Start.png" alt="Start" style="width: 500px;"/>
	<img src="Pong/Screenshots/Playing.png" alt="Start" style="width: 500px;"/>
	<img src="Pong/Screenshots/Paused.png" alt="Start" style="width: 500px;"/>
	<img src="Pong/Screenshots/Win.png" alt="Start" style="width: 500px;"/>
	
## Todo:
1. ~~Figure out double buffering~~
	- Done
1. Implement some form of randomness (starting ball directions, etc...)
1. Make the score numbers the same size on different display sizes
1. Make the ball speed constant on all display sizes
	- In progress
1. Interface with the SNES controller
	- Working with one controller as of 2018-03-11
	- Two controller support in progress
1. Add Pong AI
