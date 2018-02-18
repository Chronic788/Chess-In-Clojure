
(require '[clojure.string :as str])


;;Define the starting position
(def FEN "RNBQKBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbqkbnr w KQkq - 0 1")

(def FEN-indeces {:board 0
                  :player 1
                  :castling 2
                  :enpassant-target 3
                  :half-move-clock 4
                  :full-move-clock 5})

;;Helper Functions
;;------------------------------------------------------------------------------------------

(defn isDigit
  ;;True if the given character is a positive digit 1 - 9
  [c]
  (let [x (Character/digit c 10)]
    (not= x -1)))

(defn toInt
  ;;Casts a String representation of a integer to an int :: "1" -> 1
  [i]
  (read-string i))

(defn toLowerCase
  ;;Converts a given Character literal to lower case
  [c]
  (str/lower-case (str c)))

(defn lower-case?
  [c]
  (let [ascii (int c)]
    (if (or (< ascii 97) (> ascii 122))
      false
      true)))

(defn upper-case?
  [c]
  (not (lower-case? c)))

(defn characters-equal?
  ;;Determines if the two characters passed in are the same
  [x y]
  (= (str x) (str y)))

(defn strings-equal?
  ;;Determines if the two Strings passed in are the same
  [x y]
  (= x y))

;;Game Helper Functions
;;------------------------------------------------------------------------------------------

(defn rank-file-to-alpha
  [rank file]
  (def alphas '(A B C D E F G H))
  (if (and (> file 0) (<= file 8))
    (str (nth alphas (dec file)) rank)))

(defn dot
  [c]
  (characters-equal? c \.))

(defn pieceColor
  ;;Returns the color of a piece, either w or b
  [piece]
  (if-not (dot piece)
    (if (Character/isUpperCase piece)
    \w
    \b)))

(defn piece-is-white
  ;;Determines if the piece is white
  [piece]
  (characters-equal? \w (get piece :color)))

(defn piece-is-black
  ;;Determines if the pieces is black
  [piece]
  (not (piece-is-white piece)))

;;-------------------------------------------------------------------------------------------
;;Define functions for working with the board in coordinate form
;;
;; Board positions will look like this:
;;
;;    {:piece piece-letter
;;     :coordinates {:x x :y y}
;;     :algebraic square :: eg. a1, b3, etc..
;;     :color piece-color
;;    }
;;
;; Coordinates will start at the bottom left of the board at (1,1) and increment to 8 on both axes
;;
;; Ultimately, the board is represented as a list of coordinate-piece tuples
;; ({tuple 1}, {tuple 2}, {tuple 3})
;;------------------------------------------------------------------------------------------


(defn print-piece
  ;;Pretty prints the contents of a piece
  [piece]
  (let [coordinates (get piece :coordinates)
        p (get piece :piece)
        alpha (get piece :algebraic)
        color (get piece :color)]
    (println (str
              "\n    Piece: " p 
              ", Coordinates: {" (get coordinates :x) "," (get coordinates :y) 
              "}, Algebraic: " alpha 
              ", Color: " color))))

(defn print-pieces
  ;;Pretty prints a collection of pieces
  [pieces]
  (if (not-empty pieces)
    (do 
      (print-piece (first pieces))
      (recur (rest pieces)))))

(defn print-board-coordinate
  ;;Prints the board in its coordinate form
  [board]
  (if (not-empty board)
    (let [piece-entry (first board)]
       (let [coordinates (get piece-entry :coordinates)
             piece (get piece-entry :piece)
             alpha (get piece-entry :algebraic)
             color (get piece-entry :color)]
         (println (str ":" piece " - (" (get coordinates :x) "," (get coordinates :y) ") - :" alpha " :" color))
         (recur (rest board))))
    (println)))

(defn process-rank
  [rank rankNumber file board]
  (let [piece (first rank)
        entry (assoc {} 
                :piece piece
                :coordinates (assoc {} :x rankNumber :y file)    
                :algebraic (rank-file-to-alpha rankNumber file)
                :color (pieceColor piece))]
    (if (not-empty (rest rank))
      (recur (rest rank) rankNumber (inc file) (conj board entry))
      (conj board entry))))

(defn compose-board
  [ranks rankNumber board]
  (let [rank (first ranks) file 1]
    (if (not-empty (rest ranks))
      (recur (rest ranks) (inc rankNumber) (process-rank rank rankNumber file board))
      (process-rank rank rankNumber file board))))

(defn get-FEN-ranks
  ;;Get the ranks from the FEN String
  [FEN]
  (str/split (first (str/split FEN #" "))  #"/"))

(defn dotify
  ;;In the given String, replaces digits with the same number of '.' characters for use with
  ;;traditional board formats
  [FEN]
  (apply str
         (mapcat (fn [c]
                   (if (Character/isDigit c)
                     (repeat (Integer/parseInt (str c)) \.)
                     [c]))
                 FEN)))

(defn board-from-FEN
  ;;Builds a coordinate board from a FEN string
  [FEN]
  (let [ranks (get-FEN-ranks (dotify FEN))]
    (compose-board ranks 1 '())))

;;-------------------------------------------------------------------------------------------
;;Define functions to work with the board in FEN String form
;;-------------------------------------------------------------------------------------------

(defn print-rank
  ;;Prints the given rank's pieces
  [pieces]
  (let [piece (first pieces)]
    (if (isDigit piece)
      (dotimes [n (Character/digit piece 10)] (print " ."))
      (print (str " " piece))))
  (if (not-empty (rest pieces))
    (recur (rest pieces))))

(defn print-ranks
  ;;Prints the ranks of the board
  [ranks]
  (if (not-empty ranks)
    (do
      (println)
      (print-rank (seq (first ranks)))
      (recur (rest ranks)))))

(defn print-board-FEN
  ;;Prints the board using a FEN String
  [FEN]
  (print-ranks (get-FEN-ranks FEN))
  (println)
  (println))

(defn ranks-from-board
  ;;Splits the 64 spaces into ranks of 8
  [board ranks]
  (if (not-empty board)    
    (recur (drop 8 board) (cons (take 8 board) ranks))
    ranks))

(defn count-dot-pieces
  ;;Counts consequtive blank spaces in a rank
  [pieces conseq]
  (let [piece (first pieces)]
    (let [type (get piece :piece)]
      (if (dot type)
        (recur (rest pieces) (inc conseq))
        conseq))))

(defn get-blank-pieces-from-rank
  ;;Returns the given blank pieces from the given rank
  [rank]
  (filter (fn [piece] (characters-equal? \. (get piece :piece))) rank))

(defn FENify-rank
  ;;Takes a list of pieces in a rank and returns a String representation of it in FEN form
  ;;Notably, if replaces consequtive occurences of blank pieces with a numeric digit of the
  ;;same quanity of blank pieces in a row in that particular instance
  [rank FEN-rank discarding-dots]
  (if (not-empty rank)
    (let [piece (get (first rank) :piece)]
      (if (not (dot piece))
        (recur (rest rank) (str FEN-rank piece) false)
        (if (and (dot piece) (not discarding-dots))
          (recur (rest rank) (str FEN-rank (count-dot-pieces rank 0)) true)
          (if (dot piece)
            (recur (rest rank) FEN-rank true)))))
    FEN-rank))

(defn rankify
  ;;Composes a list of String representations of the ranks of the board
  [board-ranks string-ranks]
  (if (not-empty board-ranks)
    (let [rank (first board-ranks)]
      (recur (rest board-ranks) (cons (FENify-rank rank "" false) string-ranks)))
    string-ranks))

(defn rankify-board
  ;;Resolves to a list of String representations of the ranks in the piece list that is the board 
  [board]
  (rankify (ranks-from-board board '()) '()))

(defn FENify-ranks
  ;;Consolidates a board into a single representative FEN string
  [board]
  (str/join "/" (rankify-board board)))

;;------------------------------------------------------------------------------------------
;;Board Access Helper Functions
;;------------------------------------------------------------------------------------------

(defn vector-FEN
  ;;Translates the FEN string into a vector of elements
  [FEN]
  (into [] (str/split FEN #" ")))

(defn replace-in-FEN
  ;;Inserts the given FEN component into the FEN String
  [FEN component value]
  (str/join " " (assoc (vector-FEN FEN) (get FEN-indeces component) value)))

(defn get-from-FEN
  ;;Retrieves the give component from the FEN String
  [FEN component]
  (nth (str/split FEN #" ") (get FEN-indeces component)))

;;------------------------------------------------------------------------------------------
;;Board Access Functions
;;------------------------------------------------------------------------------------------

(defn get-board-FEN
  ;;Retrieves the board component from the FEN string
  [FEN]
  (get-from-FEN FEN :board))

(defn get-player
  ;;Retrieves the character in String form from the FEN string that represents whose turn it is: w or b
  [FEN]  (get-from-FEN FEN :player))

(defn get-castling-availability
  ;;Gets the castling availability component fromt the FEN String
  [FEN]
  (get-from-FEN FEN :castling))

(defn enpassant-target
  ;;Retrieves the currect enpassant square from the FEN string
  [FEN]
  (get-from-FEN FEN :enpassant-target))

(defn half-move-clock
  ;;Retrieves the half move clock from the FEN string
  [FEN]
  (get-from-FEN FEN :half-move-clock))

(defn full-move-clock
  ;;Retrieves the full move clock from the FEN string
  [FEN]
  (get-from-FEN FEN :full-move-clock))

(defn white-pieces
  ;;Retrieves all of the white pieces on the board
  [board]
  (filter (fn [piece] (characters-equal? \w (get piece :color))) board))

(defn black-pieces
  ;;Retrieves all of the black pieces on the board
  [board]
  (filter (fn [piece] (characters-equal? \b (get piece :color))) board))

(defn pieces-of-color
  ;;Retrieves the pieces on the board of a given color
  [board color]
  (if (characters-equal? \w color)
    (white-pieces board)
    (black-pieces board)))

;;------------------------------------------------------------------------------------------
;;Piece Countine Functions
;;------------------------------------------------------------------------------------------

;;Note that the following 'piece counting' functions should have been already filtered by color, so
;;they will only be counting pieces of that color.

(defn count-kings
  ;;Counts the number of kings on the board
  [board]
  (count (filter (fn [piece] (characters-equal? \k (toLowerCase (get piece :piece)))) board)))

(defn count-queens
  ;;Counts the number of queens on the board
  [board]
  (count (filter (fn [piece] (characters-equal? \q (toLowerCase (get piece :piece)))) board)))

(defn count-castles
  ;;Counts the number of castles on the board
  [board]
  (count (filter (fn [piece] (characters-equal? \r (toLowerCase (get piece :piece)))) board)))

(defn count-bishops
  ;;Counts the number of bishops on the board
  [board]
  (count (filter (fn [piece] (characters-equal? \b (toLowerCase (get piece :piece)))) board)))

(defn count-knights
  [board]
  ;;Counts the number of knights on the board
  (count (filter (fn [piece] (characters-equal? \n (toLowerCase (get piece :piece)))) board)))

(defn count-pawns
  [board]
  ;;Counts the number of pawns on the board
  (count (filter (fn [piece] (characters-equal? \p (toLowerCase (get piece :piece)))) board)))


;;------------------------------------------------------------------------------------------
;;Board Alteration Functions
;;------------------------------------------------------------------------------------------

(defn change-player
  ;;Change's whose turn it is in the FEN string
  [FEN]
  (if (characters-equal? \w (first (get-player FEN)))
    (replace-in-FEN FEN :player "b")
    (replace-in-FEN FEN :player "w")))

(defn increment-clock
  ;;Increments the half clock indicator on the FEN string, the 4th Entry
  [FEN which-clock]
  (let [clock (get-from-FEN FEN which-clock)]
    (replace-in-FEN FEN which-clock (inc (toInt clock)))))

(defn reset-halfmove-clock
  ;;Resets the halfmove clock to 0
  [FEN]
  (replace-in-FEN FEN :half-move-clock 0))

;;Now we have functions to work with both FEN and traditional algebraic board formats
;;
;;The game itself will be played in this way:
;; 1. The FEN will be read to determine whose turn it is
;; 2. For that color, we can then determine all possible moves
;; 3. We can then rate each of the possible moves using Alpha Beta Minimax
;; 4. We can then select the maximally valued move from the list of rated moves
;; 5. We can then increment the half move clock if a pawn has not been captured
;; 6. We can then switch whose turn it is
;; 7. We can the increment the full move number
;; 8. And repeat.

;;Chess stopping conditions are:
;; 1. Checkmate
;; 2. Draw by:
;;      - Stalemate: When the player to move is not in check but has no legal moves
;;      - Insufficient Material: When there are endgames that look like:
;;                               1. King against King
;;                               2. King against King and Bishop
;;                               3. King against King and Knight
;;                               4. King and Bishop against King and Bishop where Bishops
;;                                  are on the same colored squares
;;                                  (I will not likely implement this)
;;      - The fifty move rule: There has been no capture or pawn move in the last fifty moves
;;                             by both players
;;      - Threefold repetition: The same board position has occurred three times

;;------------------------------------------------------------------------------------------
;;Moving Functions
;;------------------------------------------------------------------------------------------

(def test-piece
  (first (board-from-FEN FEN)))

(def test-board
  (board-from-FEN FEN))

(defn out-of-bounds
  ;;Determines if the given piece is out of bounds.
  ;;Failing criteria are: x < 1 || x > 8 || y < 1 || y > 8
  [piece]
  (let [coordinates (get piece :coordinates) x (get coordinates :x) y (get coordinates :y)]
    (or (< x 1) (> x 8) (< y 1) (> y 8))))

(defn in-bounds
  ;;Determines if the given piece is in bounds.
  [piece]
  (not (out-of-bounds piece)))

(defn get-space-on-board
  ;;Returns the space on the board that the potiential move is targeting.
  ;;Due to the way movements are structured, the potential move may possibly be off of
  ;;the board so nil must be accounted for in calling functions!
  [potential-piece board]
  (if (not-empty board)
      (let [test-piece (first board)

            test-coordinates (get test-piece :coordinates) 
            test-x (get test-coordinates :x) 
            test-y (get test-coordinates :y)

            this-coordinates (get potential-piece :coordinates)
            this-x (get this-coordinates :x)
            this-y (get this-coordinates :y)]

        (if (and (= test-x this-x) (= test-y this-y))
          test-piece
          (recur potential-piece (rest board))))))

(defn space-occupied
  ;;Determines if the space already contains a space: it is empty
  [potential-piece board]
  (not (characters-equal? \. (get (get-space-on-board potential-piece board) :piece))))

(defn landing-on-space
  ;;Determines if a potential piece's move will land on a space
  [potential-piece board]
  (let [space-on-board (get-space-on-board potential-piece board)]
    (characters-equal? \. (get space-on-board :piece))))

(defn landing-on-same-color
  ;;Determines if a potential piece's move will land it on the same color piece
  [potential-piece board]
  (let [space-on-board (get-space-on-board potential-piece board)
        board-color (get space-on-board :color)
        moving-piece-color (get potential-piece :color)]
    (characters-equal? moving-piece-color board-color)))

(defn landing-on-opposite-color
  ;;Determines if a potential piece's move will land it on a piece of the opposite color
  [potential-piece board]
  (let [space-on-board (get-space-on-board potential-piece board)
        board-color (get space-on-board :color)
        moving-piece-color (get potential-piece :color)]
    (and (not (characters-equal? \. (get space-on-board :piece))) (not (characters-equal? moving-piece-color board-color)))))


(defn landing-on-space
  ;;Determines if a potential piece's move will land on a space
  [potential-piece board]
  (let [space-on-board (get-space-on-board potential-piece board)]
    (characters-equal? \. (get space-on-board :piece))))

(defn define-move
  ;;Defines the abstract action that produces a move.
  ;;For a given piece, we translate it in x,y terms by applying the additive terms
  ;;which can be positively, negatively, or zero valued.
  [[piece x-additive y-additive]]
  (let [current-coordinates (get piece :coordinates)
        x (+ (get current-coordinates :x) x-additive)
        y (+ (get current-coordinates :y) y-additive)]
    (assoc piece :coordinates (assoc {} :x x :y y)
                 :algebraic (rank-file-to-alpha x y))))

;;--------------------
;;Pawns

(defn attempt-to-promote-pawn
  ;;Detects if either a white or a black pawn reaches the relevant edge of the board
  ;;and if so, converts the pawn to a queen
  [pawn]
  (let [coordinates (get pawn :coordinates)
        x (get coordinates :x)]
    (if (and (piece-is-white pawn) (= x 8))
      (assoc pawn :piece \Q)
      (if (and (piece-is-black pawn) (= x 1))
        (assoc pawn :piece \q)
        pawn))))

(defn pawn-move
  ;;The generic action of moving a pawn
  [pawn board x y]
  (attempt-to-promote-pawn (define-move [pawn x y])))

(defn white-pawn-moves
  ;;Defines the moving behavior of white pawns
  ([pawn board]
   (filter some? (white-pawn-moves pawn board '())))
  ([pawn board accm]
   (let [coordinates (get pawn :coordinates)
         x (get coordinates :x)
         one-space (pawn-move pawn board 1 0)
         two-spaces (if (and (= 2 x) (not (space-occupied one-space board))) (pawn-move pawn board 2 0))
         potential-capture-left (pawn-move pawn board 1 -1)
         potential-capture-right (pawn-move pawn board 1 1)
         capture-left (if (landing-on-opposite-color potential-capture-left board) potential-capture-left)
         capture-right (if (landing-on-opposite-color potential-capture-right board) potential-capture-right)]
     (conj accm one-space two-spaces capture-left capture-right))))


(defn black-pawn-moves
  ;;Defines the moving behavior of black pawns
  ([pawn board]
   (filter some? (black-pawn-moves pawn board '())))
  ([pawn board accm]
   (let [coordinates (get pawn :coordinates)
         x (get coordinates :x)
         one-space (pawn-move pawn board -1 0)
         two-spaces (if (and (= 7 x) (not (landing-on-same-color one-space board))) (pawn-move pawn board -2 0))
         potential-capture-left (pawn-move pawn board -1 -1)
         potential-capture-right (pawn-move pawn board -1 1)
         capture-left (if (landing-on-opposite-color potential-capture-left board) potential-capture-left)
         capture-right (if (landing-on-opposite-color potential-capture-right board) potential-capture-right)]
     (conj accm one-space two-spaces capture-left capture-right))))

;;DO ENPASSANT AND PAWN CAPTURING MOVES

;;--------------------
;;King

(defn king-moves
  [king]
  (map define-move [[king 1 1] 
                    [king 1 0]
                    [king 0 1]
                    [king 0 -1]
                    [king -1 0]
                    [king -1 -1]
                    [king 1 -1]
                    [king -1 1]]))

;;-------------------
;;Knights

(defn knight-moves
  [knight]
  (map define-move [[knight -1 2]
                    [knight -2 1]
                    [knight -2 -1]
                    [knight -1 -2]
                    [knight 1 2]
                    [knight 2 1]
                    [knight 2 -1]
                    [knight 1 -2]]))

;;----------------------------------------------------------
;;Tiled pieces:
;;
;;    Tiled Piece: A tiled piece is a piece that can move in some direction(s) for
;;                 for as far as the player would like to move it. I define that
;;                 behavior as the repeated action of moving that piece one space
;;                 in that direction for a maximum of eight spaces in light of the fact
;;                 that a chess board is 8x8 spaces.
;;
;;    Thus, the tiled movement of these peices have special stopping conditions:
;;        - The tiling of a piece will stop if:
;;          1. The piece runs off of the board
;;          2. The piece lands on a piece of its own color
;;          3. The piece lands on a piece of the opposite color
;;
;;        - To address these behaviors, the following rules are implemented:
;;          1. I let the piece run off of the board. I filter out these out-of-bounds moves
;;             at a higher level to save code.
;;          2. If the piece lands on a piece of its own color, recursion will stop WITHOUT adding
;;             that piece to the move accumulator.
;;          3. If the piece lands on a piece of the opposite color, recursion will stop WITH adding
;;             that piece to the move accumulator.
;;
;;         Pieces that tile are the Queen, the castles, and the bishops
;;
;;----------------------------------------------------------


;;------------------
;;Castles


(defn castle-moves
  ;;Generates the potential moves a given castle can make on the board.
  ([castle board]
   (concat (castle-moves castle board \N 8 '())
           (castle-moves castle board \S 8 '())
           (castle-moves castle board \E 8 '())
           (castle-moves castle board \W 8 '())))
  ([castle board direction times moves]
   (if (< 0 times)
     (cond 
      (characters-equal? \N direction) (let [move (define-move [castle 1 0])]
                                         (tile move board direction times moves))
      (characters-equal? \S direction) (let [move (define-move [castle -1 0])]
                                         (tile move board direction times moves))
      (characters-equal? \E direction) (let [move (define-move [castle 0 1])]
                                         (tile move board direction times moves))
      (characters-equal? \W direction) (let [move (define-move [castle 0 -1])]
                                         (tile move board direction times moves))
      ) moves)))

;;------------------
;;Bishops

(defn bishop-moves
  ;;Generates the potential moves a given bishop can make on the board.
  ([bishop board]
   (concat (bishop-moves bishop board "NW" 8 '())
           (bishop-moves bishop board "NE" 8 '())
           (bishop-moves bishop board "SE" 8 '())
           (bishop-moves bishop board "SW" 8 '())))
  ([bishop board direction times moves]
   (if (< 0 times)
     (cond
      (strings-equal? "NW" direction) (let [move (define-move [bishop -1 1])]
                                        (tile move board direction times moves))
      (strings-equal? "NE" direction) (let [move (define-move [bishop 1 1])]
                                        (tile move board direction times moves))
      (strings-equal? "SE" direction) (let [move (define-move [bishop 1 -1])]
                                        (tile move board direction times moves))
      (strings-equal? "SW" direction) (let [move (define-move [bishop -1 -1])]
                                        (tile move board direction times moves))
      ) moves)))

;;------------------
;;Queens

(defn queens-moves
  ;;Generates the potential moves a given queen can make on the board. Defined in
  ;;terms of a bishop and a castle, we have all the moves! WOOO Functional Programming!
  [queen board]
  (concat (castle-moves (assoc queen :piece \r) board) 
          (bishop-moves (assoc queen :piece \b) board)))

;;-----------------
;;Abstract tiling behavior

(defn tile
  ;;Conditionally tiles a piece along the given direction
  [moved-piece board direction times moves]
  (let [piece-type (toLowerCase (get moved-piece :piece))]
    (cond
     (characters-equal? \r piece-type) (if (landing-on-same-color moved-piece board)
                                         (castle-moves moved-piece board direction 0 moves)
                                         (if (landing-on-opposite-color moved-piece board)
                                           (castle-moves moved-piece board direction 0 (cons moved-piece moves))
                                           (castle-moves moved-piece board direction (dec times) (cons moved-piece moves))))

     (characters-equal? \b piece-type) (if (landing-on-same-color moved-piece board)
                                         (bishop-moves moved-piece board direction 0 moves)
                                         (if (landing-on-opposite-color moved-piece board)
                                           (bishop-moves moved-piece board direction 0 (cons moved-piece moves))
                                           (bishop-moves moved-piece board direction (dec times) (cons moved-piece moves)))))))

;;------------------
;;All Moves

(defn valid-moves
  ;;Filters out invalid moves:
  ;; 1. Landing out of bounds
  ;; 2. Landing on a piece of the same color
  ([moves board]
   (valid-moves moves board '()))
  ([moves board valids]
   (if (not-empty moves)
     (let [move (first moves)]
           (if (and (some? move) (in-bounds move) (not (landing-on-same-color move board)))
             (valid-moves (rest moves) board (cons move valids))
             (valid-moves (rest moves) board valids)))
     valids)))

;;--------------------------------------------------------------------------------
;;A move is a construct of the potential of a piece and will be defined as a map:
;;
;;  {:from piece :to moves}
;;
;;  where piece is the piece on the current board and moves is the set of potential moves for that piece

(defn all-available-moves
  ;;Compiles a list of all moves available to the given player on the given board
  ([board color]
   (all-available-moves board board color '()))
  ([dummy-board board color moves]
   (if (not-empty dummy-board)
     (let [piece (first dummy-board) piece-color (get piece :color)]
     (if (characters-equal? color piece-color)
       (let [piece-type (toLowerCase (get piece :piece))]
         (cond
          (characters-equal? \p piece-type) 
              (if (characters-equal? \w piece-color)
                (recur (rest dummy-board) board color (conj moves (assoc {} :piece piece :moves (valid-moves (white-pawn-moves piece board) board))))
                (recur (rest dummy-board) board color (conj moves (assoc {} :piece piece :moves (valid-moves (black-pawn-moves piece board) board)))))
          (characters-equal? \r piece-type) 
              (recur (rest dummy-board) board color (conj moves (assoc {} :piece piece :moves (valid-moves (castle-moves piece board) board))))
          (characters-equal? \b piece-type) 
              (recur (rest dummy-board) board color (conj moves (assoc {} :piece piece :moves (valid-moves (bishop-moves piece board) board))))
          (characters-equal? \q piece-type)
              (recur (rest dummy-board) board color (conj moves (assoc {} :piece piece :moves (valid-moves (queens-moves piece board) board))))
          (characters-equal? \k piece-type) 
              (recur (rest dummy-board) board color (conj moves (assoc {} :piece piece :moves (valid-moves (king-moves piece) board))))
          (characters-equal? \n piece-type)
              (recur (rest dummy-board) board color (conj moves (assoc {} :piece piece :moves (valid-moves (knight-moves piece) board))))))
       (recur (rest dummy-board) board color moves)))
     moves)))


(defn count-available-moves
  ([moves]
   (count-available-moves moves 0))
  ([moves number]
   (if (not-empty moves)
     (let [move (first moves)]
       (recur (rest moves) (+ number (count (get move :moves)))))
     number)
   )
)

(defn list-available-moves-for-piece
  [piece-moves]
  (if (not-empty piece-moves)
    (do (print-piece (first piece-moves))
    (recur (rest piece-moves)))))

(defn list-pieces-and-moves
  [moves]
  (if (not-empty moves)
    (let [move (first moves)]
      (do (println "----------------------")
          (print-piece (get move :piece))
          (println "----------------------")
          (list-available-moves-for-piece (get move :moves)))
       (recur (rest moves)))))


;;------------------------------------------------------------------------------------------
;; Game Play
;;------------------------------------------------------------------------------------------


;;Left:
;;
;;    -En Passant
;;    -Castling Moves - Add to king
;;    -Scoring
;;    -Alpha Beta Minimax
;;    -Random Moving
;;    -API with Chess.js
