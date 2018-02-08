
(require '[clojure.string :as str])


;;Define the starting position
(def FEN "RNBQKBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbqkbnr w KQkq = 0 1")

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

;;Game Helper Functions
;;------------------------------------------------------------------------------------------

(defn file-to-alpha
  [file]
  (def alphas '(a b c d e f g h))
  (nth alphas (dec file)))

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
                :algebraic (str (file-to-alpha file) rankNumber)
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

(FENify-ranks (board-from-FEN FEN))

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

;;A move is a construct of the potential of a piece and will be defined as a map:
;;
;;  {:piece p :from {:x x :y y} :to {:x x :y y}}

(defn out-of-bounds
  ;;Determines if the given piece is out of bounds.
  ;;Failing criteria are: x < 0 || x > 8 || y < 0 || y > 8
  [piece]
  (let [coordinates (get piece :coordinates) x (get coordinates :x) y (get coordinates :y)]
    (or (< x 0) (> x 8) (< y 0) (> y 8))))

(defn in-bounds
  ;;Determines if the given piece is in bounds.
  [coordinates]
  (not (out-of-bounds coordinates)))

(defn landing-on-same-color
  ;;Search through the board for a piece that occupies the potential move's landing spot
  ;;If we find one, then test to see if it is the same color, returning the result.
  ;;If we process the entire board nil is returned which is falsey and thus correct for
  ;;the intent of this function.
  ;;Note that the piece passed in is a 'potential' piece
  [potential-piece board]
  (if (not-empty board)
      (let [test-piece (first board)
            test-coordinates (get test-piece :coordinates) 
            test-x (get test-coordinates :x) 
            test-y (get test-coordinates :y)
            test-color (get test-piece :color)

            this-coordinates (get potential-piece :coordinates)
            this-x (get this-coordinates :x)
            this-y (get this-coordinates :y)
            this-color (get potential-piece :color)]

        (if (and (= test-x this-x) (= test-y this-y))
          (let [same-color (characters-equal? this-color test-color)]
            (if (not same-color)
              (recur potential-piece (rest board))
              same-color))))))

(defn define-move
  ;;Defines the abstract action that produces a move.
  ;;For a given piece, we translate it in x,y terms by applying the additive terms
  ;;which can be positively, negatively, or zero valued.
  [[piece x-additive y-additive]]
  (let [current-coordinates (get piece :coordinates)]
    (assoc piece :coordinates (assoc {} :x (+ (get current-coordinates :x) x-additive) 
                                        :y (+ (get current-coordinates :y) y-additive)))))

(defn valid-moves
  ;;Determines if the given moves are valid.
  ;;Valid moves are:
  ;;    1. In bounds
  ;;    2. On a blank space 
  ;;    3. On another player's piece
  [moves]
  (filter in-bounds moves)
  
  ;;Implement filters on #2 and #3
  )

;;--------------------
;;Pawns

(defn white-pawn-moves
  ;;Defines the moving behavior of white pawns
  [pawn]
  (let [coordinates (get pawn :coordinates) 
        y (get coordinates :y)]
    (if (= 2 y)
      (conj '() 
            (define-move [pawn 1 0]) 
            (define-move [pawn 2 0])) 
      (define-move [pawn 1 0]))))

(defn black-pawn-moves
  ;;Defines the moving behavior of white pawns
  [pawn]
  (let [coordinates (get pawn :coordinates) 
        y (get coordinates :y)]
    (if (= 7 y)
      (conj '() 
            (define-move [pawn -1 0]) 
            (define-move [pawn -2 0])) 
      (define-move [pawn -1 0]))))


;;DO ENPASSANT MOVES

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

;;------------------
;;Castles


(defn castle-moves
  ([castle]
   (concat (castle-moves castle \N 8 '())
           (castle-moves castle \S 8 '())
           (castle-moves castle \E 8 '())
           (castle-moves castle \W 8 '())))
  ([castle direction times moves]
   (if (< 0 times)
     (cond 
      (characters-equal? \N direction) (let [move (define-move [castle 0 1])]
                                         (castle-moves move direction (dec times) (cons move moves)))
      (characters-equal? \S direction) (let [move (define-move [castle 0 -1])]
                                         (castle-moves move direction (dec times) (cons move moves)))
      (characters-equal? \E direction) (let [move (define-move [castle 1 0])]
                                         (castle-moves move direction (dec times) (cons move moves)))
      (characters-equal? \W direction) (let [move (define-move [castle -1 0])]
                                         (castle-moves move direction (dec times) (cons move moves)))
      ) moves)))

;;------------------
;;Bishops


;;------------------
;;Queens

;;(reduce cons '() [(+ 1 1)])


;;Low hanging fruit:
;;Landing on piece of same color.
;;I need a piece potential, and the board. Look up the space on the board and check color.
