; ConnectFour.rkt
; Created By Rafael Zamora.
; On April 29, 2015.

; This program is a connect four game.
; Capable of playing semi-autonomously.
; Picking moves at random when there are no visible
; winning moves.

(define RZZGAME '())

(define (RZZStartGame)
  (begin
    (set! RZZGAME
          '(  ( 0 0 0 0 0 0 )
              ( 0 0 0 0 0 0 )
              ( 0 0 0 0 0 0 )
              ( 0 0 0 0 0 0 )
              ( 0 0 0 0 0 0 )
              ( 0 0 0 0 0 0 )
              ( 0 0 0 0 0 0 )
               0 
              )                          
          )
    (display "Anonymous. \n")
    )
  )

; (RZZStartGame) (RZZShowGame)
(define (RZZShowGame)
  (begin
    (display (RZZMakeRow 1 6 RZZGAME))
    (display "\n")
    (display (RZZMakeRow 1 5 RZZGAME))
    (display "\n")
    (display (RZZMakeRow 1 4 RZZGAME))
    (display "\n")
    (display (RZZMakeRow 1 3 RZZGAME))
    (display "\n")
    (display (RZZMakeRow 1 2 RZZGAME))
    (display "\n")
    (display (RZZMakeRow 1 1 RZZGAME))
    )
  )
  
; Parameters:                |  Output:
;    Column : Starts at 1.   |     A new List representing a Row on the Gameboard.
;    Row : Integer 1-6.      |
;    RZZGAME : Gameboard.    |
(define (RZZMakeRow Column Row RZZGAME)
  (if (= Column 8)
      ()
      (cons
       (RZZGetCell Column Row RZZGAME)
       (RZZMakeRow (+ Column 1) Row RZZGAME)
       )
       )
  )

; Parameters:                | Output:
;     Column : Integer 1-7   |    #t : Column resulted in a win.
;                            |    #f : Column didn't result in a win.
(define (RZZWinP Column)
  (cond
    [(RZZWinVertical Column (RZZFindTopOfColumn Column 6 RZZGAME) 0) #t]
    [(RZZWinHorizontal Column (RZZFindTopOfColumn Column 6 RZZGAME) ) #t]
    [(RZZWinDiagonal Column (RZZFindTopOfColumn Column 6 RZZGAME)) #t]
    [()#f]
    )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    #t : Found 4 common connections.
;    Row : Integer 1-6.     |    #f : Didn't find 4 common connections.
(define (RZZWinDiagonal Column Row)
  (cond
    [(>= (+ (RZZAddIncRight Column Row 0) (RZZAddDecLeft Column Row 0)) 3) #t]
    [(>= (+ (RZZAddDecRight Column Row 0) (RZZAddIncLeft Column Row 0)) 3) #t]
    [()#f]
    )

  )
  
  
; Parameters:               |  Output:
;    Column : Integer 1-7.  |    Connects : Number of Connections found heading 
;    Row : Integer 1-6.     |               left on an increasing slope.
;    Connects : Starts at 0.|
(define (RZZAddIncLeft Column Row Connects)
  (if (equal? (RZZGetCell Column Row RZZGAME) (RZZPlayerMarker?))
      (RZZAddIncLeft (- Column 1) (+ Row 1) (+ Connects 1))
      (- Connects 1)
      )
  )
; Parameters:               |  Output:
;    Column : Integer 1-7.  |    Connects : Number of Connections found heading 
;    Row : Integer 1-6.     |               right on an decreasing slope.
;    Connects : Starts at 0.|
(define (RZZAddDecRight Column Row Connects)
  (if (equal? (RZZGetCell Column Row RZZGAME) (RZZPlayerMarker?))
      (RZZAddDecRight (+ Column 1) (- Row 1) (+ Connects 1))
      (- Connects 1)
      )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    Connects : Number of Connections found heading 
;    Row : Integer 1-6.     |               left on an decreasing slope.
;    Connects : Starts at 0.|
(define (RZZAddDecLeft Column Row Connects)
  (if (equal? (RZZGetCell Column Row RZZGAME) (RZZPlayerMarker?))
      (RZZAddDecLeft (- Column 1) (- Row 1) (+ Connects 1))
      (- Connects 1)
      )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    Connects : Number of Connections found heading 
;    Row : Integer 1-6.     |               right on an increasing slope.
;    Connects : Starts at 0.|
(define (RZZAddIncRight Column Row Connects)
  (if (equal? (RZZGetCell Column Row RZZGAME) (RZZPlayerMarker?))
      (RZZAddIncRight (+ Column 1) (+ Row 1) (+ Connects 1))
      (- Connects 1)
      ) 
  )

; Parameters:                 | Output:
;     Column : Integer 1-7.   |    Row : The most recently filled row in the given column.
;     Row : Starts at 6.      |    
(define (RZZFindTopOfColumn Column Row RZZGAME)
  (if (equal? (RZZGetCell Column Row RZZGAME) 0)
      (RZZFindTopOfColumn Column (- Row 1) RZZGAME)
      Row
      )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    #t : Found 4 common connections.
;    Row : Integer 1-6.     |    #f : Didn't find 4 common connections.
;    Connects : Starts at 0.|
(define (RZZWinVertical Column Row Connects)
  (cond
    [ (= Connects 3) #t ] ; Connect Four!
      
    ; Is Marker = Marker? If so, Connect + 1
    [(equal? (RZZGetCell Column Row RZZGAME) (RZZPlayerMarker?))
     (RZZWinVertical Column (- Row 1) (+ Connects 1))
     ]
    
    ; Default : No win, return false.
    [()#f]
    )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    #t : Found 4 common connections.
;    Row : Integer 1-6.     |    #f : Didn't find 4 common connections.
;    Connects : Starts at 0.|
(define (RZZWinHorizontal Column Row )
  (if (>= (+ (RZZConnectLeft Column Row 0)
             (RZZConnectRight Column Row 0)) 3)
      #t
      #f  
      )
  )
; Parameters:               |  Output:
;    Column : Integer 1-7.  |    Connects : Number of connections found right of item.
;    Row : Integer 1-6.     |    
;    Connects : Starts at 0.|
(define (RZZConnectRight Column Row Connects)
  (if (equal? (RZZGetCell Column Row RZZGAME) (RZZPlayerMarker?))
     (RZZConnectRight (+ Column 1) Row (+ Connects 1))
     (- Connects 1) ; Return Number of Connections
     )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    Connects : Number of connections found left of item.
;    Row : Integer 1-6.     |    
;    Connects : Starts at 0.|
(define (RZZConnectLeft Column Row Connects)
  (if(equal? (RZZGetCell Column Row RZZGAME) (RZZPlayerMarker?))
     (RZZConnectLeft (- Column 1) Row (+ Connects 1))
     (- Connects 1) ; Return Number of Connections
     )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    (C,R) : Item located at the given Column and Row.
;    Row : Integer 1-6.     |    
;    RZZGAME : Gameboard.   |
(define (RZZGetCell Column Row RZZGAME)
  (if (null? RZZGAME)
      ()
      (if (= Column 1)
          (RZZGetRow Row (car RZZGAME))
          (RZZGetCell (- Column 1) Row (cdr RZZGAME))
          )
      )
  )

; Parameters:                | Output:
;     Row : Integer 1-6.     |    (C,R) : Item located at the given Column and Row.
;     RZZGAME : Gameboard representing one column.    
(define (RZZGetRow Row RZZGAME)
  (if (null? RZZGAME)
      ()
      (if (= Row 1)
          (car RZZGAME)
          (RZZGetRow (- Row 1) (cdr RZZGAME))
          )
      )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    (list) : Returns a new gameboard with updated move.
(define (RZZDropMove Column)
  (if (not(equal? (car Column) 0))
      (cons
       (car Column)
       (RZZDropMove (cdr Column)
       ))
      (cons (RZZPlayerMarker?) (cdr Column))
       ))

; Parameters:                 |  Output:
; Column : Integer 1-7.       |    Returns all the columns up to the new column.
; CurrentColumn : Integer 1-7.|    
; RZZGAME : Gameboard.        |
(define (RZZCopyFrontOfList Column CurrentColumn RZZGAME)
  (cons
      (car RZZGAME)
      (RZZUpdateGame Column (+ CurrentColumn 1) (cdr RZZGAME))
      
      ))

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    RZZGAME : New gameboard state with updated move.
(define (RZZMarkMove Column)
  (begin
    (set! RZZGAME
        (RZZUpdateGame Column 1 RZZGAME)
          )
    )
  )

; Output:
;    Returns the total number of moves in the game.
(define (RZZGetMoveCount)
   (car(cdr(cdr(cdr(cdr(cdr(cdr(cdr  RZZGAME))))))))
  )

(define (RZZUpdateGame Column CurrentColumn RZZGAME)
  (if (not(= CurrentColumn Column))
     (RZZCopyFrontOfList Column CurrentColumn RZZGAME)
     (CopyRestOfList RZZGAME) ; giving the (cdr GAME)
    )
  )





(define (RZZPlayerMarker?)
  (if (even? (RZZGetMoveCount))
      "X" ; X - Player 1
      "O" ; O - Player 2
      )
  )
;will return the previous player marker.
(define (RZZREV)
  (if (even? (RZZGetMoveCount))
      "O"
      "X"
      )
  )
  

; CopyRestOfList will create a new column and overide the current
; GameState column. Then it will copy the rest of the list.

; Parameters:                 |  Output:
; RZZGAME : Gameboard.        |    Returns a new column with the current move.    
(define (CopyRestOfList RZZGAME)
  (cons
   (RZZDropMove (car RZZGAME))
   (RZZUpdateMoveCount (cdr RZZGAME))
  ))

; Parameters:                 |  Output:
; RZZGAME : Gameboard.        |    Returns moves + 1.  
(define (RZZUpdateMoveCount RZZGame)
  (if (not(null? (cdr RZZGame)))
      (cons
       (car RZZGame)
       (RZZUpdateMoveCount (cdr RZZGame))
       )
      (cons
       (+ (RZZGetMoveCount) 1)
       ()
       )
  )
  ) 
; Parameters:                 |  Output:
; Column : Integer 1-7.       |    Column : Returns a random valid move 1-7.
(define (RZZRandom Column)
  (if (equal? (RZZLegalMoveP Column) #t)
      Column
      (RZZRandom (+ (random 7) 1))
      )
  )

; Output:
;   Returns a move to the gameboard.
(define (RZZMakeMove)
  ; if checkforwinningcolumn returns a column choose the column
  ; else choose a random
  (if (equal? (RZZCheckForWinningColumn 1) #t)
      (RZZMarkMove (RZZCheckForWinningColumn 1))
      (RZZMarkMove (RZZRandom (+ (random 7) 1)))
      )
  )

; Parameters:                 |  Output:
; Column : Integer 1-7.       |    #t : If top of column is not full.
;                             |    #f : If top of column is full.
(define (RZZLegalMoveP Column)
  (if (equal? (RZZTopOfColumn (RZZGetColumn Column RZZGAME) ) 0 )
      #t
      #f
      )
  )

; Parameters:                 |  Output:
; list : A Column.            |    Returns the top of a column.  
(define (RZZTopOfColumn list)
  (car(cdr(cdr(cdr(cdr(cdr list))))))
  )

; Parameters:                 |  Output:
; Column : Integer 1-7.       |    Returns an entire column.
; RZZGAME : Gameboard.        |
(define (RZZGetColumn Column RZZGAME)
  (if (not(= Column 1))
      (RZZGetColumn (- Column 1) (cdr RZZGAME))
      (car RZZGAME)
      )
  )

; Parameters:               |  Output:
;    Column : Integer 1-7.  |    #t : Returns true if the column given will generate a win.
;                           |    #f : Returns false if the column given will not generate a win.
(define (RZZWillWinP Column)
  (if (equal? (RZZWinP Column) #t)
      #t
      #f
      )
  )

; Parameters:               |  Output:
;    Column : Starts at 1.  |   Column : The column that will generate a win next move.
;                           |   
(define (RZZCheckForWinningColumn Column)
  ; go through all the columns checking RZZWillWinP
  (if (>= Column 7)
      #f
      (if (equal? (RZZWillWinP Column) #t)
          Column
          (RZZCheckForWinningColumn (+ Column 1))
          )
      )
  )




