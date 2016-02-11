---
layout: post
title: Haskell Dice of Doom - Part 3
summary: We continue our port of the Land of Lisp game "Dice of Doom". In this part we concentrate on the game's performance.
---

We continue our port of the Land of Lisp game "Dice of Doom" from 
[Part 1](http://derekmcloughlin.github.io/2014/09/13/Haskell-Dice-Of-Doom-Part-1/) 
and [Part 2](http://derekmcloughlin.github.io/2014/10/04/Haskell-Dice-Of-Doom-Part-2/).

Source code can be found on [Github](https://github.com/derekmcloughlin/DiceOfDoom).

We can now play 2x2 games with no problem. However, trying to play a 3x3 
game poses a significant challenge for us - it can be very slow.  In this 
part, we'll take a look at improving the performance of the game for 
boards bigger than 2x2.

## Performance Characteristics

Before doing anything we should look at the performance characteristics of the 
game, in particular the size of the problem. For an N x N board with 2 players 
and a max of 3 dice per cell, the number of board configurations 
is (2*3)^(N*N). A 2x2 game has 1,296 possible 
boards, while a 3x3 one has 10,077,696. A 4x4 game has 2,821,109,907,456 
possible configurations - that's over two trillion!

For the 2x2 boards, we can generate game trees for all scenarios and look
at some stats, depending on whether A starts or B starts:

* The number of nodes in the tree.
* The eventual winners in each tree.

{% highlight haskell %}
treeSize :: Tree a -> Int
treeSize = length . flatten

leaves :: Tree a -> [a]
leaves tree = getLeaves tree []
    where
        getLeaves :: Tree a -> [a] -> [a]
        getLeaves (Node root children) total = 
            total ++ (if null children 
                        then [root]
                        else concat [getLeaves c total | c <- children])

allWinners :: Tree GameState -> [([Player], Int)]
allWinners tree = map (\xs@(x:_) -> (x, length xs)) 
                    . group . sort $ map (winners . currentBoard) $ leaves tree 
{% endhighlight %}

Getting the tree size by flattening the entire tree into a list and then getting
the length of the list probably isn't the most efficient way of doing it, but
we'll get to that in a while.

We can generate all possible 2x2 boards:

{% highlight haskell %}
all2x2Boards :: [Board]
all2x2Boards = [
    Board {
        size = 2,
        maxDice = 3,
        numPlayers = 2,
        cells = [
              Cell {player = cell0Player, dice = cell0Dice}
            , Cell {player = cell1Player, dice = cell1Dice}
            , Cell {player = cell2Player, dice = cell2Dice}
            , Cell {player = cell3Player, dice = cell3Dice}
        ],
        conqueredDice = 0
    } |   cell0Player <- allPlayers
        , cell1Player <- allPlayers 
        , cell2Player <- allPlayers
        , cell3Player <- allPlayers
        , cell0Dice   <- allDice
        , cell1Dice   <- allDice
        , cell2Dice   <- allDice
        , cell3Dice   <- allDice ]
    where
        allPlayers = [Player 0, Player 1]
        allDice = [1, 2, 3]
{% endhighlight %}

The function to print out the board statistics for these boards is:

{% highlight haskell %}
boardStats :: Board -> IO ()
boardStats board = do
    drawBoard board
    putStrLn $ printf "A: %d\tB: %d\tA Winners: %s\tB Winners: %s" 
        numPlayerANodes numPlayerBNodes (show winnersA) (show winnersB)
    putStrLn ""
    where
        treeA = gameTree board (Player 0) Pass True
        treeB = gameTree board (Player 1) Pass True
        numPlayerANodes = treeSize treeA
        numPlayerBNodes = treeSize treeB
        winnersA = allWinners treeA
        winnersB = allWinners treeB

playAll2x2Boards :: IO ()
playAll2x2Boards = mapM_ boardStats all2x2Boards
{% endhighlight %}

The results are given [here]({{ site.url}}/images/2x2-results-sorted.txt)

Most of these games are short or very one-sided. Interesting games
are balanced ones, with a large number of nodes usually leading
to a more even possibility of A winning, B winning or a tie.

This doesn't look too bad. How about a 3x3 game? We won't generate all
possible boards, but take a few random ones and play them.

**Note**: GHC vs GHCi

Whenever you're doing performance work I recommend you compile your code
with optimisations switched on, rather than running it through GHCi. To do this
we'll need to add a `main` function and change the module name to "Main".

{% highlight haskell %}
import Data.Time (getCurrentTime)

playRandom3x3Board :: IO ()
playRandom3x3Board = genBoard 3 2 3 >>= boardStats

main :: IO ()
main = do
    t1 <- getCurrentTime
    print t1
    playRandom3x3Board 
    t2 <- getCurrentTime
    print t2
{% endhighlight %}

Then compile with optimisations using the `-O2` flag:

{% highlight text %}
ghc -O2 DiceOfDoom-h.hs
{% endhighlight %}

Playing this a few times:

{% highlight text%}
2014-10-05 08:55:58.526566 UTC
      A-1 B-2 A-1
    B-1 B-2 A-1
  A-1 A-1 A-3
A: 75	B: 1307	A Winners: [([A],31)]	B Winners: [([A],388)]
2014-10-05 08:55:58.539907 UTC

2014-10-05 08:57:12.153751 UTC
      A-3 A-1 B-2
    B-2 A-3 A-2
  B-1 B-2 B-1
A: 845109	B: 137339	A Winners: [([A],340484),([B],10202)]	B Winners: [([A],62628),([B],260)]
2014-10-05 08:57:19.853758 UTC

2014-10-05 08:58:18.31154 UTC
      A-1 A-3 A-1
    B-2 B-2 B-3
  A-2 B-1 B-2
A: 3890171	B: 5065637	A Winners: [([A],186513),([B],1065740)]	B Winners: [([A],440445),([B],1322381)]
2014-10-05 08:59:26.736873 UTC
{% endhighlight %}

Wow! There's a huge variation here. The last game tree, when play is started by
B, has over 5 million nodes.

That's nothing: the following game tree has 1.4 BILLION nodes when play
is started by A:

{% highlight text %}
2014-10-05 11:40:35.780121 UTC
      A-2 A-2 B-2
    A-3 B-1 A-3
  A-3 A-3 B-3
A: 1468919491
B: 1
2014-10-05 13:24:24.230547 UTC
{% endhighlight %}

You might ask yourself how this was calculated, given that my laptop
only has 8GB of RAM. Let's say that each node is 100 bytes. This is assuming 
1 Int == 4 bytes and summing up the Ints used in players, cells etc. The actual
value is much larger, but let's be conservative. We can fit 10 boards
in 1KB, 10,000 boards in 1MB, 10 million boards in 1GB, which means
that the max tree size I should be able to accomodate in memory is one
with 80 million nodes. So how could I calculate the size of the above game
tree?

The answer is laziness and garbage collection. I initially only wanted to know 
the tree sizes, so my main function was like this:

{% highlight haskell %}
main :: IO ()
main = do
    t1 <- getCurrentTime
    print t1
    putStrLn $ printf "A: %d" (treeSize $ gameTree test3x3BoardE (Player 0) Pass True)
    putStrLn $ printf "B: %d" (treeSize $ gameTree test3x3BoardE (Player 1) Pass True)
    t2 <- getCurrentTime
    print t2
{% endhighlight %}

It took a couple of hours, but gave the following results:

{% highlight text %}
2014-10-05 11:40:35.780121 UTC
      A-2 A-2 B-2
    A-3 B-1 A-3
  A-3 A-3 B-3
A: 1468919491
B: 1
2014-10-05 13:24:24.230547 UTC
{% endhighlight %}

The impressive thing was that it ran in constant memory. Looking at the
process stats in Mac OS X's Activity Monitor, the private memory usage
never got above 2.5 MB.

Buoyed on by this, I added more stats, including the winners. Re-running, 
it didn't take long before I consumed all available memory.

The reason for this is that if I just get the tree size, unused nodes are
garbage collected. I don't need to generate the whole tree up-front to start
calculating the tree size - laziness helps here. However, if I need the tree for 
separate calls to get the size and the winners, the nodes can't be garbage 
collected before the
second call, and the whole tree resides in memory.

### Aside: Calculating the Size of the Tree

The initial version of `treeSize` flattens the entire tree and then gets its
length. I thought this would be horribly ineffieicent, but that it would
do me for a while. After getting an initial set of stats, I resolved to 'fix'
the issue with `treeSize`:

{% highlight haskell %}
import qualified Data.Foldable as F

treeSize :: Tree a -> Int
treeSize = F.foldl (\x _ -> x + 1) 0 
{% endhighlight %}

Re-running this on a large tree (several million nodes) resulted in all
memory being consumed.

The problem here is with the `foldl` function. It's much better to use
`foldl'`. 

{% highlight haskell %}
import qualified Data.Foldable as F

treeSize :: Tree a -> Int
treeSize = F.foldl' (\x _ -> x + 1) 0 
{% endhighlight %}

See the [WikiBooks](http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl') 
entry for more information on this.

Even with this, the function wasn't as fast as the flattening one. I finally 
settled on the following simple function, which turns out to be faster still:

{% highlight haskell %}
treeSize :: Tree a -> Integer
treeSize (Node _ children) = 1 + sum (map treeSize children)
{% endhighlight %}

## Shared Structure

There's an interesting thing about the huge game tree above: if there are 
1.4 billion nodes in a tree, but only 10 million possible board configurations,
then there's proabably a lot of repetition going on - the same branches may 
occur multiple times within the tree. 

Let's see an example of this. The game tree used in Part 2 is a good example:

<object type="image/svg+xml" data="{{ site.url }}/images/test2x2BoardC-shared.svg" style="width: 900px;">Your browser does not support SVG</object>


The shared nodes are coloured in various shades of yellow.

The Lisp implementation memoises the calls to `get-tree`
by using a hash-based lookup table. By doing this it only calculates a
sub-tree once. Let's see what effect this memoisation has on our test trees.

By modifying the memoised function, we can log the 'cache-hits' vs the 
'cache-misses':

{% highlight lisp %}
; memoization
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (progn 
            (fresh-line)
            (princ "HIT")
            (gethash rest previous))
        (progn
            (fresh-line)
            (princ "MISS")
            (setf (gethash rest previous)
                  (apply old-game-tree rest))))))
{% endhighlight %}

Run the game-tree function on `*test-board-3x3-e*`:

{% highlight sh %}
sh$ clisp -q <<
(load "dice_of_doom_v1.lisp")
(tree-size (game-tree *test-board-3x3-e* 0 0 t))
(quit)
-EOF 
| sort | uniq -c
{% endhighlight %}

As with the Haskell version, this runs in fairly-constant space, but still
takes a couple of hours.

For the board with 1.4 billion nodes, the results are interesting

{% highlight text %}
472109 HIT
201861 MISS
{% endhighlight %}

In other words, in the tree of 1.4 billion nodes, with 472,109 sub-trees,
only 201,861 of these sub-trees had to be computed. Even if you play this 
game in the CLisp REPL, it only takes around 30 seconds for it 
to generate the game tree.

The Lisp memoization solves our two problems above:

* The time taken to generate the game tree is reduced. Also, by memoising
the other calls, the time taken to calculate moves is also severly reduced.
* The space taken up by the tree is also a lot less.

This second point is interesting, and not immediately obvious on a casual 
glance at the Lisp code. Once a sub-tree has been stored in the hash table,
all subsequent 'cache hits' returned by the `game-tree` function
are referenced, not copied, in the parent. While logically the structure 
is a tree, several sub-trees share the same structure. In effect, 
this turns the tree into a **Directed Acyclic Graph**. The 'magic' 
behind this is the Lisp Cons Cell. If you want to find more information on 
this, Peter Siebel's book 
[Practical Common Lisp](http://www.gigamonkeys.com/book/), in particular 
Chapter 12, 
[They Called It Lisp For A Reason](http://www.gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html)
is a good read.

Haskell also has cons cells where we can have several items in a list referring
to a single shared data structure. Using this, we can memoise the game tree 
creation in a similar way.

### Aside: Profiling an Application

Before we second-guess the performance issues in the game, it's good to get
a real idea of where the bottlenecks lie by profiling it.

GHC allows us to turn on profiling:

{% highlight text %}
ghc -O2 -prof -auto-all DiceOfDoom-h.hs
{% endhighlight %}

To run, we need to tell the GHC runtime that we want do do the profiling (-p)
and that we want garbage collection stats as well (-s):
(This test was done using test3x3BoardB)

{% highlight text %}
./DiceOfDoom-h +RTS -p -s
{% endhighlight %}

The profiling output is stored in DiceOfDoom-h.prof.

[test3x3BoardB]({{ site.url }}/images/test3x3BoardB.txt)

[test3x3BoardC]({{ site.url }}/images/test3x3BoardC.txt)

The GC stats are very telling. For `test3x3BoardC` we have:

{% highlight text %}
 112,814,519,176 bytes allocated in the heap
  17,082,086,768 bytes copied during GC
   2,771,996,128 bytes maximum residency (17 sample(s))
      50,573,008 bytes maximum slop
            7803 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     218297 colls,     0 par   14.70s   20.17s     0.0001s    0.1349s
  Gen  1        17 colls,     0 par   14.46s   44.15s     2.5968s    21.5761s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time  116.08s  (135.91s elapsed)
  GC      time   29.16s  ( 64.31s elapsed)
  RP      time    0.00s  (  0.00s elapsed)
  PROF    time    0.00s  (  0.00s elapsed)
  EXIT    time    0.10s  (  1.50s elapsed)
  Total   time  145.35s  (201.72s elapsed)

  %GC     time      20.1%  (31.9% elapsed)

  Alloc rate    971,852,043 bytes per MUT second

  Productivity  79.9% of total user, 57.6% of total elapsed
{% endhighlight %}

There's a lot of GC going on - which shouldn't be a surprise as the game
trees have millions of nodes.

For info on the interpretation of GC stats, see 
[Running a compiled program](http://www.haskell.org/ghc/docs/7.8.3/html/users_guide/runtime-control.html)
in the GHC docs.

## Memoising Tree Creation

In languages that allow mutation, memoising is a fairly straightforward
process: take a function you want memoised, wrap it in another function that
only calls it if it hasn't already been called with those arguments, and that
stores the result for subsequent calls.

Haskell doesn't allow mutation directly, so we have to use some sort of
state mechanism. The approach we'll use is described in
[http://www.maztravel.com/haskell/memofib.html](http://www.maztravel.com/haskell/memofib.html) 
and in a thread on the
[comp.lang.haskell]( https://groups.google.com/forum/#!topic/comp.lang.haskell/iXA6Wq1SPcU)
group.

In some ways the approach is similar to the Lisp one above: we
have a function that stores calculated values in a map. However, we can't just
wrap the old function in another one - we have to modify the old function
to enable us to memoise it.

**NOTE** It's important to note that the type of memoisation presented here
is slightly different to what you'd expect if you were memoising functions
in Python or Ruby. In the Haskell memoisation code above, the memoisation
only works within recursive calls to the same function. It doesn't work
across non-recursive calls. So if you're in GHCI and you do the following:

{% highlight text %}
ghci> treeSize $ gameTree test3x3BoardB (Player 0) True
26359321
ghci> treeSize $ gameTree test3x3BoardB (Player 0) True
26359321
{% endhighlight %}

Then the second call to `gameTree` isn't "cached" in any way - it goes through
exactly the same process of trying to memoise recursive calls to itself
starting off with an empty map. We'll see an example of memoising that can do
this when we memoise the calls to get the ratings.

### Fixing a Design Flaw

Before we apply memomisation to the game tree creation, we need to fix
a flaw in the way the tree was designed. Recall that the game state is defined by:

{% highlight haskell %}
data GameState = GameState {
                    currentPlayer :: Player,
                    moveMade :: Move,
                    currentBoard :: Board
                 }
{% endhighlight %}

We stored the `moveMade` in the game state to tell us that a particular
move was made to get to this state. It made it easy to enumerate the 
child nodes and see what move would result in that board.

However, several different moves from different boards can result in the 
same end state. If we want to memoise the game tree from a possible board, 
we need to remove the `moveMade` reference and put this into the parent. The 
game state should only have the current player, the current board, and 
the list of possible moves *from* that board.

To do this we're going to put the moves made from a parent node to a child
node into the parent itself, as an array of moves.

{% highlight haskell %}
data GameState = GameState {
                    currentPlayer :: Player,
                    currentMoves :: [Move],
                    currentBoard :: Board
                 }
{% endhighlight %}

This changes the `gameTree` function slightly, in particular we don't
need the `fromMove` parameter any more. Two other functions that change are
`allowedMoves` that maps the possible moves to numbers, and `showGameGraphTree`
used to generate the Graphviz output:

{% highlight haskell %}
gameTree :: Board -> Player -> Bool -> Tree GameState
gameTree board p isFirstMove 
    -- No further moves possible. Switch players and add the reinforcements
    | null possibleMoves && not isFirstMove = 
        Node GameState {
            currentPlayer = p,
            currentMoves = [Pass],
            currentBoard = board
        } passingGameTree
    --  No moves possible - END OF GAME
    | null possibleMoves && isFirstMove = 
        Node GameState {
            currentPlayer = p,
            currentMoves = [Pass],
            currentBoard = board
        } []
    -- Keeping with the same player, recurse through all moves                                              
    | otherwise = 
        Node GameState {
            currentPlayer = p,
            currentMoves = passingMove ++ possibleMoves,
            currentBoard = board
        } (passingGameTree
          ++ 
          [gameTree (makeAMove board p m) p False | m <- possibleMoves] 
          )
     where
        possibleMoves = attackMoves board p
        passingMove = if isFirstMove
                        then []
                        else [Pass]
        passingGameTree = if isFirstMove
                          then []
                          else [gameTree
                                 (reinforce board p)     -- Add reinforcements
                                 (nextPlayer board p)    -- Switch player
                                 True                    -- First move for new player
                             ]

allowedMoves :: Tree GameState -> [(Int, Move)]
allowedMoves (Node root _) = zip [1..] $ currentMoves root 

showGameGraphTree :: Tree (GameState, Int) -> String
showGameGraphTree (Node (root, number) children) = 
    concat $ [printf "\"%d\" -> \"%d\" [label=\"%s\";];\n" 
                        number child_number (show moveMade) :: String
                | (moveMade, (Node (child, child_number) _)) 
                    <- zip (currentMoves root) children]
             ++ [showGameGraphTree c | c <- children] 
{% endhighlight %}

The code is in DiceOfDoom-i.hs.

### Setting up the Memoising Map

In order to memoize the calls to `gameTree` using the method above, we'll 
need to be able to map a single something to a game tree. The `gametree` 
function takes three parameters: a board, a player and a boolean indicating
if it's the first move or not. We'll wrap these three items in a single tuple.
The structure of the code remains the same as the non-memoizing version:

{% highlight haskell %}
type GameTree = Tree GameState

gameTree :: Board -> Player -> Bool -> GameTree
gameTree board p isFirstMove = memoizeM gameTreeM (board, p, isFirstMove)

gameTreeM :: Monad m => 
    ((Board, Player, Bool) -> m GameTree) -> (Board, Player, Bool) -> m GameTree
gameTreeM f' (board, p, isFirstMove)
    -- No further moves possible. Switch players and add the reinforcements
    | null possibleMoves && not isFirstMove = do
            passTree <- f' (reinforce board p,    -- Add reinforcements
                            nextPlayer board p,   -- Switch player
                            True)                 -- First move for new player
            return $ Node GameState {
                    currentPlayer = p,
                    currentMoves = [Pass],
                    currentBoard = board
                 } [passTree]
    --  No moves possible - END OF GAME
    | null possibleMoves && isFirstMove =
            return $ Node GameState {
                    currentPlayer = p,
                    currentMoves = [Pass],
                    currentBoard = board
                 } []
    -- Keeping with the same player, recurse through all moves                                              
    | otherwise = do
        childTrees <- mapM (\b -> f' (b, p, False)) 
                           [makeAMove board p m | m <- possibleMoves] 
        if isFirstMove
            then
                return $ Node GameState {
                            currentPlayer = p,
                            currentMoves = possibleMoves,
                            currentBoard = board
                         } childTrees
            else do
                passTree <- f' (reinforce board p,  -- Add reinforcements
                                nextPlayer board p, -- Switch player
                                True)               -- First move for new player
                return $ Node GameState {
                            currentPlayer = p,
                            currentMoves = Pass : possibleMoves,
                            currentBoard = board
                         } (passTree : childTrees)
     where
        possibleMoves = attackMoves board p


type StateMap a b = State (Map.Map a b) b
 
memoizeM :: (Show a, Show b, Ord a) => 
            ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = evalState (f x) Map.empty 
    where
        -- Cache miss
        g z = do
            y <- t f z  
            m <- get
            put $ Map.insert z y m
            return y
        -- Cache hit
        f z = get >>= \m -> maybe (g z) return (Map.lookup z m)
{% endhighlight %}

Code in DiceOfDoom-j.hs

Let's test this out on the 1.4 billion-node game tree:

{% highlight haskell %}
main :: IO ()
main = do
    t1 <- getCurrentTime
    print t1
    putStrLn $ printf "Size: %d" (treeSize tree) 
    putStrLn $ printf "Depth: %d" (treeDepth tree) 
    t2 <- getCurrentTime
    print t2
    where
        tree = gameTree test3x3BoardE (Player 0) True
{% endhighlight %}

Running this:

{% highlight text %}
ghc -O2 DiceOfDoom-j.hs
./DiceOfDoom-j
2014-11-01 09:21:49.2206886 UTC
Size: 1468919491
Depth: 40
2014-11-01 09:23:28.837608 UTC
{% endhighlight %}

One minute and 40 seconds - that's not bad. Memory usage peaks at 145MB.

If we profile this, and look at the GC stats for `test3x3BoardC`

{% highlight text %}
ghc -O2 -prof -auto-all DiceOfDoom-j.hs
./DiceOfDoom-j +RTS -p -s
  34,575,453,936 bytes allocated in the heap
   4,376,663,592 bytes copied during GC
     146,595,568 bytes maximum residency (35 sample(s))
       3,292,400 bytes maximum slop
             425 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     66920 colls,     0 par    2.86s    3.07s     0.0000s    0.0376s
  Gen  1        35 colls,     0 par    3.01s    3.75s     0.1073s    0.2740s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   21.64s  ( 22.14s elapsed)
  GC      time    5.88s  (  6.82s elapsed)
  RP      time    0.00s  (  0.00s elapsed)
  PROF    time    0.00s  (  0.00s elapsed)
  EXIT    time    0.00s  (  0.07s elapsed)
  Total   time   27.52s  ( 29.04s elapsed)

  %GC     time      21.4%  (23.5% elapsed)

  Alloc rate    1,597,733,984 bytes per MUT second

  Productivity  78.6% of total user, 74.5% of total elapsed
{% endhighlight %}

There's a lot less GC going on, and the total memory in use has gone
down from 7.8 GB to 425 MB.

### Memoising the ratings functions

We can now play the large game above against another human:

{% highlight haskell %}
main :: IO ()
main = do
    playVsHuman tree 
    where
        tree = gameTree test3x3BoardE (Player 0) True
{% endhighlight %}

{% highlight text %}
$ ghc -O2 DiceOfDoom-j.hs
Current player: A
      A-2 A-2 B-2
    A-3 B-1 A-3
  A-3 A-3 B-3
choose your move:
1: Attack 0 4
2: Attack 1 4
3: Attack 3 4
4: Attack 5 2
5: Attack 5 4
6: Attack 7 4
...
{% endhighlight %}

However, if we try to play against the computer, we're still in trouble:

{% highlight haskell %}
main :: IO ()
main = do
    playVsComputer (Player 0) tree 
    where
        tree = gameTree test3x3BoardE (Player 0) True
{% endhighlight %}

{% highlight text %}
$ ghc -O2 DiceOfDoom-k.hs
Current player: A
      A-2 A-2 B-2
    A-3 B-1 A-3
  A-3 A-3 B-3
...
{% endhighlight %}

It hangs for a long time. If we take a look at the profiling info, we can
see that the various ratings functions are taking up a lot of time. We
need to memoise these as well.

We can't use the same memoising technique that we used to generate the 
tree. The `childRatings` function does make recursive calls to itself
indirectly via `ratePosition`, but it is called several times independently
by `handleComputer`. We'd really like to memoise separate calls. 

We'll take the approach used in 
[Ugly Memoization](http://augustss.blogspot.ie/2011/04/ugly-memoization-heres-problem-that-i.html)
by Lennart Augustsson. It uses unsafe IO and MVars. 

{% highlight haskell %}
childRatingsM :: Tree GameState -> GameState -> Player -> [Double]
childRatingsM (Node _ children) _ nodePlayer = 
    [ratePosition c nodePlayer | c <- children]

childRatings :: Tree GameState -> Player -> [Double]
childRatings tree@(Node root _) p = memo childRatingsM tree root p

memo :: (Show b, Ord b, Ord c) => (a -> b -> c -> d) -> (a -> b -> c -> d)
memo f = let f' = unsafePerformIO (memoIO f) in \ x y z -> unsafePerformIO (f' x y z)

memoIO :: (Ord b, Ord c) => (a -> b -> c -> d) -> IO (a -> b -> c -> IO d)
memoIO f = do
    v <- newMVar Map.empty
    let f' x y z = do
        m <- readMVar v
        case Map.lookup (y, z) m of
            Nothing -> do 
                let r = f x y z
                n <- takeMVar v
                putMVar v (Map.insert (y, z) r n)
                return r
            Just r -> 
                return r
    return f'
{% endhighlight %}

The `childRatings` function takes a game tree and a player. However, we only
need to memoise the root of the tree and the player, so the `childRatingsM` 
function takes the root as a parameter but does nothing with it.

Code in DiceOfDoom-k.hs.

With this in place, we can now play against the computer:

{% highlight text %}
ghc -O2 DiceOfDoom-k.hs
./DiceOfDoom-k
Current player: A
      A-2 A-2 B-2
    A-3 B-1 A-3
  A-3 A-3 B-3
choose your move:
1: Attack 0 4
2: Attack 1 4
3: Attack 3 4
4: Attack 5 2
5: Attack 5 4
6: Attack 7 4
1
...
{% endhighlight %}

That's it for part 3. The game is at the same stage as the end of chapter 
15 in Land of Lisp. In the next part we'll get the game to handle even 
larger boards.

