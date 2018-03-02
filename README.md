# Chess, written in Clojure

In an effort to dive into the exciting world of functional programming, I chose to write an Artificially Intelligent chess playing program in Clojure. The primary objective was to learn the language, so I left out some of the fancier features such as a fancy graphical representation of the board, keeping track of the score, showing which pieces were taken, and other such items which you might see in a polished product. You can follow along with the game as the board is printed in text to watch gameplay. 

##Notes on Method and Experience

I also chose to focus on syntax and functionality in my learning so I neglected to break out code into separate files and organize it in any such manner. In short, the source is a single file which is sectioned off through comments to chunk the many functions in coherent groups. Flailing about to honor engineering principals such as logic segregation, project organization, and namespace considerations seemed excessive for my first run at a substantive program in a new language. It is my view that we programmers are so conditioned to avert the horror of frighteningly disorganized projects that we over tax ourselves when we attempt to learn new things. The understanding of the Right and Proper method of doing things should follow out of the methodically applied layers of learning that starts from the bottom up.

With that thought in mind, an interesting thing to note about my experience with writing a program like this in Clojure was that it was a natural to simply write one long file. As I went along building functions that operated upon the functions that I had just written, these logical chunks of functionality just seemed to form on their own simply by virtue of the method of repeated function composition. By writing 10 functions that operated together to accomplished a task, I had suddenly composed a task in itself. Perhaps this may be a functional and prudent heuristic that I might follow to organize Functional Programming based projects in future.

Also to look out for is how much my Clojure **improved** over the course of writing the file. First starting out I used a few rudimentary forms but struggled to avoid writing three functions to accomplish something. Further on I discovered function overloading, anonymous functions, and the power of the let form. Of course, there is much to be improved upon but overall it is interesting to watch the coherence increase over the course of the file.

### Representation

Any sufficiently complex problem must be approached through coherent representations of data such that it can be easily solved by effective and concise logic. This posed a problem in Chess especially because not only are some of the representations compound but so is some of the behavior. There are relationships that are immediately evident in chess such as the relationship between the board and the pieces, and the pieces and their positions, and even between the pieces and their set of next potential positions. The eager Object Oriented programmer may immediately jump into a complex modeling task whereby the pieces might be members of a board object which may contain information about their individual position. However, in Functional Programming, things aren't so simple because although we may model data structures, we may not operate on that model to alter state. In Functional Programming, all operations on data produce new **versions** of themselves. It was obvious that I would have to set down an initial game state and compose functions to operate on that original state over time to accomplish the task of playing chess. This is where the FEN String comes in.

#### FEN Notation

FEN notation was a natural choice because it captures all of the information that is going on in a chess game at any given time. In it we can capture the board state, whose turn it is, how many moves have been player, etc.

The FEN starting position:

**rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1**

I've included a document describing the facets of FEN notation in the repository so take a gander at that to learn a little more about the facets of FEN notation, algebraic notation, scoring, and pseudocode for Alpha Beta MiniMax search.

The problem with FEN notation is that much of its information is implicit. We can read the FEN string and know everything about the game, including where the pieces lie but that requires **functionality**. Thus I spent a lot of time working on functions to be able to extract information from the FEN string such that I can work with it. Primarily, what FEN lacks is numerical representation of the coordinates of the pieces on the board. This must be interpreted from the board component of the String. In truth, nearly half of the code I wrote was just to translate the FEN String back and forth into what I call the Board representation.

#### The Board Representation

The FEN had to be kept to keep information about the state of the game such as whose turn it was, but it was necessary to also have another representation to work with the pieces on the board. What I came up with was a list of maps where the entries in the list were pieces represented by maps of attributes:

  {:piece piece-letter
  :coordinates {:x x :y y}
  :algebraic square :: eg. a1, b3, etc..
  :color piece-color}

In order to totally represent the board I had to include and treat blank spaces as pieces in themselves. They are marked as '.' pieces. By doing this I could represent the complete board as a list! This smoothed out the entirety of the strategy to create functions around movement, capturing, bounds checking, and move filtering.

### Abstraction

I really relished when I discovered that I could define very abstract functions that could concretize transformations. This came to be very useful in the recursive movement behaviors of the bishops, castles, and the queens. In fact, I was able to abstract away both the movement and the definition of the movement. See the tile function and the define-move function for examples.

Another note on abstraction is noticing the power of Clojure's ability to define functions that operate the same irregardless of the values within the collections passed to them. Check out this function to randomly shuffle a set of moves:

```clojure
(defn shuffle-moves
  ;;Randomly shuffles the given list of moves
  ([moves]
   (shuffle-moves moves '()))
  ([moves shuffled]
   (if (not-empty moves)
     (let [random-index (rand-int (count moves))
           precedent (take random-index moves)
           move (nth moves random-index)
           antecedent (nthrest moves (+ 1 random-index))]
       (recur (concat precedent antecedent) (conj shuffled move)))
     shuffled)))
```

And now consider this function:

```clojure
(shuffle-moves (range 10))
(2 6 3 1 9 0 8 7 4 5)
```

It didn't matter that my intended purpose was to shuffle a set of moves. As long as it was a collection, the entries would be randomly shuffled.

### The Most Useful Discovery

By far the most useful pattern that I employed was the recursive accumulator. This was a useful way to operate on a sequence and return a new version of the sequence upon finishing. Now, I am quite certain that there are much more kosher ways of accomplishing the same thing in Clojure, but I found this way to be very useful at my skill level.

The idea is to overload a function and pass in an empty list as an accumulator to collect results in as the recursive logic is performed:

```clojure
(defn generate-board-states
  ;;Generates a list of board states that are the results of making the moves passed to it
  ([moves board]
   (generate-board-states moves board '()))
  ([moves board states] 
   (if (not-empty moves)
     (let [move (first moves)]
       (recur (rest moves) board (conj states (get (make-move move board) :new-board))))
     states)))
```

 ## What I Struggled With

**Primitives**

Check this function out as the best answer I found on how to compare primitive characters:

```clojure
(defn characters-equal?
  ;;Determines if the two characters passed in are the same
  [x y]
  (= (str x) (str y)))
```

This is misleading in quite a few ways:

1. What happens if I pass in two maps? It would be true.

2. What happens if I pass in two Strings. It would be true.

3.  This function that I also wrote is identical in function but would not accomplish my purpose:

   ```clojure
   (defn strings-equal?
     ;;Determines if the two Strings passed in are the same
     [x y]
     (= x y))
   ```

**String manipulation**

Using Java Interop to work on Strings was easy but at one point I had to conditionally work with strings as I read its characters. Read this and tell me your head doesn't hurt:

```clojure
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
```

# Conclusion

I love Clojure. **SO MUCH**. In truth, what I really grew to love is the way in which I got to think about modeling and operating on a problem using Functional Programming. Things felt fluid with Collection abstractions, function composability, and anonymous functions. I didn't have to worry about off by one indeces, nulls, or other such pernicious evils. I just got to think logically, and the rest flowed outwards. I found it to be a much more linear development process. In past projects using OO, it just seemed to me that I was lending all of my efforts towards constructing highly complex models of problem and then writing icky functions to operate on them that just weren't flexible. Of course, debating OO vs. FP would be imprudent given the fact that I would likely write several hundred pages on the topic. But I really did enjoy learning FP! Metaphorically, writing chess in Java (I have done this) was like prefabricating a wall horizontally on the ground by nailing together subsections containing doors, windows, and pretty side panels and finally hoisting the wall to the vertical. Writing chess in Clojure felt like methodically laying bricks in whatever order I pleased in the vertical until I had a sturdy wall which I could remove and add sections of bricks as I pleased.

To which I have to say:

**LET THE PEOPLE LAY BRICKS!!**

## Caveats and Notes

I will reiterate that I wrote this for the purposes of learning Clojure. That being said, there are things that were just left out because I want to move on to other things:

1. I didn't implement every movement in the game. I left out en passant and castling.
2. The program is terribly inefficient. It repeatedly calls very expensive functions.
3. There seem to be a few bugs. Every once in a while a king may choose to remain in check...which is bad.
4. The scoring and heuristics are basic at best. This is no advanced chess engine but the player with the AI (white) beats the player that moves randomly (black) on a consistent basis. Of course, I didn't have seven million years to watch it play but it seemed to do okay. I'll leave the advanced game theory for another time.