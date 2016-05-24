# ConnectFour
A scheme program that will allow you to play Connect Four vs an AI using commands on the console.

Steps to get the game up and running:
  1. RZZStartGame() - will set the board up and initialize all the board elements to zero.
  2. RZZMakeMove( # ) - where number has to be  equal to or in between 1-8.
  3. RZZMarkMove(RZZMakeMove())- this will allow the AI to make a move and update the board state.
  4. RZZShowGame() will show the results of the board.
  5. Player 1 is represented as a number 1, and Player 2 is represented as a number 2.

In essence the game commands will look like this:
note: "#" has to be between 1-8 because the columns you cant place a move outside of the game board.
    >RZZStartGame()
    >RZZMarkMove( # )
    >RZZShowGame()
    >RZZMarkMove(RZZMakeMove())
    >RZZShowGame()
    >RZZMarkMove( # )
    
    
    
    ; Have fun.
