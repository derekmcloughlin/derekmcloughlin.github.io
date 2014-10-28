---
layout: post
title: Haskell Dice of Doom - Part 3
summary: We continue our port of the Land of Lisp game "Dice of Doom". In this part we concentrate on the game's performance.
---

We continue our port of the Land of Lisp game "Dice of Doom" from 
[Part 1](http://derekmcloughlin.github.io/2014/09/13/Haskell-Dice-Of-Doom-Part-1/) 
and [Part 2](http://derekmcloughlin.github.io/2014/10/04/Haskell-Dice-Of-Doom-Part-2/).

We can now play 2x2 games with no problem. However, trying to play a 3x3 
game poses a significant challenge for us - it can be very slow.  In this 
part, we'll take a look at improving the performance of the game for 
boards bigger than 2x2.

## Performance Characteristics

Before doing anything we should look at the performance characteristics of the 
game, in particular the size of the problem. For an N x N board with 2 players 
and a max of 3 dice per cell, the number of board configurations 
is (2*3)^(N\*N). A 2x2 game has 1,296 possible 
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
    boardStats test3x3BoardB 
    t2 <- getCurrentTime
    print t2
{% endhighlight %}

Then compile with optimisations using the `-O2` flag:

{% highlight haskell %}
ghc -O2 DiceOfDoom-f.hs
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

The answer is that, entirely by accident, I used properties
of Haskell's laziness and the garbage collector.

### Haskell Laziness and Garbage Collection

When I wrote the initial code for `treeSize` I did the simplest thing 
possible:

{% highlight haskell %}
treeSize :: Tree a -> Int
treeSize = length . flatten
{% endhighlight %}

I reckoned that this would be horribly inefficient, but that it would do me
for a while. I initially only wanted to know the tree sizes, so my main 
function was like this:

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

Buoyed on by this, I went back to "fix" the problem with `treeSize`:

{% highlight haskell %}
import qualified Data.Foldable as F

treeSize :: Tree a -> Int
treeSize = F.foldl (\x _ -> x + 1) 0 
{% endhighlight %}

Re-running, it didn't take long before I consumed all available memory.

The problem here is with the `foldl` function. It's much better to use
`foldl'`. 

{% highlight haskell %}
import qualified Data.Foldable as F

treeSize :: Tree a -> Int
treeSize = F.foldl' (\x _ -> x + 1) 0 
{% endhighlight %}

See the [WikiBooks](http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl') 
entry for more information on this.

### Aside: Profiling an Application

Before we second-guess the performance issues in the game, it's good to get
a real idea of where the bottlenecks lie by profiling it.

GHC allows us to turn on profiling:

{% highlight haskell %}
ghc -O2 -prof -auto-all DiceOfDoom-h.hs
{% endhighlight %}

To run, we need to tell the GHC runtime that we want do do the profiling:
(This test was done using test3x3BoardB)

{% highlight haskell %}
./DiceOfDoom-h +RTS -p
{% endhighlight %}

The profiling output is stored in DiceOfDoom-h.prof.

[test3x3BoardB]({{ site.url }}/images/test3x3BoardB.txt)

[test3x3BoardC](test3x3BoardC.txt)

## Shared Structure

There's an interesting thing about the huge game tree above: if there are 
1.4 billion nodes in a tree, but only 10 million possible board configurations,
then there's proabably a lot of repetition going on - the same branches may 
occur multiple times within the tree. 

Let's see an example of this. The game tree used in Part 2 is a good example:

<img src="{{ site.url }}/images/test2x2BoardC-shared.svg" alt="Drawing" style="width: 900px;"/>

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

In other words, of the 1.4 billion nodes, with 472,109 sub-trees,
only 201,861 had to be computed, which is around one hundreth of a percent 
of the original value. Even if you play this game in the CLisp REPL, it 
only takes around 30 seconds for it to generate the game tree.

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

## Memoising Tree Creation

To see this in practice, let's play around with trees in a very contrived 
scenario.

Suppose we have the following tree:

<img src="{{ site.url }}/images/contrived.svg" alt="Drawing" style="width: 500px;"/>

There are 19 unique nodes in the DAG, but 311 nodes in the tree.

Let's say that this tree has a width of 5 (max number of nodes across)
and a depth of 7. 

Here's some Haskell code to generate such a tree, as well as some utility
functions to find the tree size and a count of the value of each node.

{% highlight haskell %}
module Main
where

import Data.Tree

treeWidth :: Integer
treeWidth = 5

treeDepth :: Integer
treeDepth = 7   -- Must be odd.

treeSize :: Tree a -> Integer
treeSize (Node _ children) = 1 + sum (map treeSize children)

treeCount :: Tree Integer -> Integer
treeCount (Node root children) = root + sum (map treeCount children)

mkTree :: Integer -> Tree Integer
mkTree n 
    | n >= (treeDepth `div` 2) * (treeWidth + 1) 
        = Node n []
    | n `mod` (treeWidth + 1) == 0 
        = Node n [mkTree (n + c) | c <- [1 .. treeWidth] ] 
    | otherwise 
        = Node n [mkTree (((n `div` (treeWidth + 1)) + 1) * (treeWidth + 1))] 

main :: IO ()
main = do
    putStrLn ("Tree size = " ++ show (treeSize tree))
    putStrLn ("Tree count = " ++ show (treeCount tree))
    where 
        tree = mkTree 0
{% endhighlight %}

Let's make the tree size much bigger - a width of 60 and a height of 9. Compiling
this with optimisations on:

TODO: rename file.

{% highlight text %}
ghc -O2 treetest6.hs
time ./treetest6
Tree size = 26359321
Tree count = 6002442090
./treetest6  17.23s user 7.21s system 91% cpu 26.666 total
{% endhighlight %}

It's more interesting to compile with profiling switched on:

{% highlight text %}
ghc -O2 -prof -auto-all treetest6.hs
{% endhighlight %}

We'll run with the profiling output plus the garbage collector stats.

{% highlight text %}
./treetest6 +RTS -s -p
Tree size = 26359321
Tree count = 6002442090
  16,576,601,784 bytes allocated in the heap
   8,685,498,016 bytes copied during GC
   2,836,646,040 bytes maximum residency (14 sample(s))
      21,969,480 bytes maximum slop
            5535 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     32012 colls,     0 par    2.63s    2.80s     0.0001s    0.0478s
  Gen  1        14 colls,     0 par    5.80s   11.80s     0.8429s    5.8293s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    8.76s  (  9.51s elapsed)
  GC      time    8.43s  ( 14.60s elapsed)
  RP      time    0.00s  (  0.00s elapsed)
  PROF    time    0.00s  (  0.00s elapsed)
  EXIT    time    0.06s  (  0.94s elapsed)
  Total   time   17.25s  ( 25.06s elapsed)

  %GC     time      48.9%  (58.3% elapsed)

  Alloc rate    1,892,048,682 bytes per MUT second

  Productivity  51.1% of total user, 35.2% of total elapsed
{% endhighlight %}

The profiling info is:

{% highlight text %}
    Mon Oct 27 18:39 2014 Time and Allocation Profiling Report  (Final)

       treetest6 +RTS -s -p -RTS

    total time  =       10.21 secs   (10209 ticks @ 1000 us, 1 processor)
    total alloc = 9,818,026,808 bytes  (excludes profiling overheads)

COST CENTRE MODULE  %time %alloc

treeCount   Main     47.9   32.2
mkTree      Main     30.4   35.6
treeSize    Main     21.7   32.2

COST CENTRE  MODULE                  no.     entries  %time %alloc   %time %alloc

MAIN         MAIN                     46           0    0.0    0.0   100.0  100.0
 main        Main                     93           0    0.0    0.0     0.0    0.0
 CAF         Main                     91           0    0.0    0.0   100.0  100.0
  treeWidth  Main                     98           1    0.0    0.0     0.0    0.0
  treeDepth  Main                     97           1    0.0    0.0     0.0    0.0
  mkTree     Main                     96           0    0.0    0.0     0.0    0.0
  main       Main                     92           1    0.0    0.0   100.0  100.0
   treeCount Main                    100    26359321   47.9   32.2    47.9   32.2
   treeSize  Main                     99    26359321   21.7   32.2    21.7   32.2
   main.tree Main                     94           1    0.0    0.0    30.4   35.6
    mkTree   Main                     95    26359321   30.4   35.6    30.4   35.6
 CAF         GHC.Conc.Signal          86           0    0.0    0.0     0.0    0.0
 CAF         GHC.IO.Encoding          79           0    0.0    0.0     0.0    0.0
 CAF         GHC.IO.Encoding.Iconv    77           0    0.0    0.0     0.0    0.0
 CAF         GHC.IO.Handle.FD         70           0    0.0    0.0     0.0    0.0
{% endhighlight %}

For info on the interpretation of GC stats, see 
[Running a compiled program](http://www.haskell.org/ghc/docs/7.8.3/html/users_guide/runtime-control.html)
in the GHC docs.

The program takes up a lot of memory (several GB), and there's a lot 
of garbage collection going on.

Our goal is to:

* Reduce the amount of memory used by the tree.
* Reduce the time taken to construct it.

### A General-Purpose Memoising Function

In languages that allow mutation, memoising is a fairly straightforward
process: take a function you want memoised, wrap it in another function that
only calls it if it hasn't already been called with those arguments, and that
stores the result for subsequent calls.

Haskell doesn't allow mutation directly, so we have to use some sort of
state mechanism. The approach we'll use is described in
[http://www.maztravel.com/haskell/memofib.html](http://www.maztravel.com/haskell/memofib.html) 
by Henry Laxen, who in turn got it from the 
[Haskell Cafe]( https://groups.google.com/forum/#!topic/comp.lang.haskell/iXA6Wq1SPcU).

In some ways the approach is similar to the Lisp one above: we
have a function that stores calculated values in a map. However, we can't just
wrap the old function in another one - we have to modify the old function
to enable us to memoise it.

Here's the memoised version of `mkTree`:

{% highlight haskell %}
import Data.Map as Map
import Control.Monad.State.Lazy as State

type IntTree = Tree Integer

mkTree :: Integer -> IntTree 
mkTree = memoizeM mkTreeM

mkTreeM :: Monad m => (Integer -> m IntTree) -> Integer -> m IntTree
mkTreeM f' n 
    | n >= (treeDepth `div` 2) * (treeWidth + 1)
        = return $ Node n []
    | n `mod` (treeWidth + 1) == 0
        = do
            cs <- mapM f' [n + c | c <- [1 .. treeWidth] ]
            return $ Node n cs
    | otherwise 
        = do
            cs <- mapM f' [((n `div` (treeWidth + 1)) + 1) * (treeWidth + 1)]
            return $ Node n cs

type StateMap a b = State (M.Map a b) b
 
memoizeM :: (Show a, Show b, Ord a) => 
            ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = evalState (f x) M.empty 
    where
        -- Cache miss
        g z = do
            y <- t f z  
            m <- get
            put $ M.insert z y m
            return y
        -- Cache hit
        f z = get >>= \m -> maybe (g z) return (M.lookup z m)
{% endhighlight %}

The caclulated results are stored in a `StateMap`, which is a mapping in our
case of an integer to a tree of integers, which is then wrapped up in the
`State` monad. When memomising a function, we evaluate the state of the 
function `f` initialised with an empty map. `f` then looks up the value in
the map. If it's found, that value is returned. Otherwise, we call function
`g`, which performs the actual calulation and stores the result in the map
and then returns the value.

When running this for the first time, it's useful to keep the `trace`
calls in the code as 
[Henry has done](http://www.maztravel.com/haskell/memofib.html#memoize).
That way you can see the state of the map on each call.

One major difference between this code and the Lisp version is that the
Haskell memoisation only lasts for the duration of the call, whereas 
the Lisp version keeps it's state across invocations.

Let's run this to see what effect it has on space and time:

{% highlight text %}
ghc -O2 treetest7.hs
time ./treetest7
Tree size = 26359321
Tree count = 6002442090
./treetest7  4.68s user 0.07s system 99% cpu 4.768 total
{% endhighlight %}

That's much quicker than the non-memoised version.

What about the profiling stats?

{% highlight text %}
ghc -O2 -prof -auto-all treetest6.hs
{% endhighlight %}

We'll run with the profiling output plus the garbage collector stats.

{% highlight text %}
./treetest7 +RTS -s -p
Tree size = 26359321
Tree count = 6002442090
  10,544,394,352 bytes allocated in the heap
      10,321,960 bytes copied during GC
         116,104 bytes maximum residency (4 sample(s))
          27,456 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     20586 colls,     0 par    0.09s    0.12s     0.0000s    0.0001s
  Gen  1         4 colls,     0 par    0.00s    0.00s     0.0002s    0.0003s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    4.39s  (  4.46s elapsed)
  GC      time    0.09s  (  0.12s elapsed)
  RP      time    0.00s  (  0.00s elapsed)
  PROF    time    0.00s  (  0.00s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    4.48s  (  4.58s elapsed)

  %GC     time       2.0%  (2.6% elapsed)

  Alloc rate    2,400,859,384 bytes per MUT second

  Productivity  98.0% of total user, 96.0% of total elapsed
{% endhighlight %}

Just look at the difference in garbage collection - only 10MB copied here
vs. 8GB in the first version! Also note that only a tiny fraction of time
is spent in GC - 2% vs. 48%.

The profiling info is:

{% highlight text %}
	Mon Oct 27 19:11 2014 Time and Allocation Profiling Report  (Final)

	   treetest7 +RTS -s -p -RTS

	total time  =        4.41 secs   (4412 ticks @ 1000 us, 1 processor)
	total alloc = 6,326,664,144 bytes  (excludes profiling overheads)

COST CENTRE MODULE  %time %alloc

treeSize    Main     51.0   50.0
treeCount   Main     49.0   50.0

COST CENTRE         MODULE                  no.     entries  %time %alloc   %time %alloc

MAIN                MAIN                     47           0    0.0    0.0   100.0  100.0
 main               Main                     95           0    0.0    0.0     0.0    0.0
 CAF                Main                     93           0    0.0    0.0   100.0  100.0
  treeWidth         Main                    106           1    0.0    0.0     0.0    0.0
  treeDepth         Main                    105           1    0.0    0.0     0.0    0.0
  mkTreeM           Main                    104           0    0.0    0.0     0.0    0.0
  mkTree            Main                     97           1    0.0    0.0     0.0    0.0
  main              Main                     94           1    0.0    0.0   100.0  100.0
   treeCount        Main                    108    26359321   49.0   50.0    49.0   50.0
   treeSize         Main                    107    26359321   51.0   50.0    51.0   50.0
   main.tree        Main                     96           1    0.0    0.0     0.0    0.0
    mkTree          Main                     98           0    0.0    0.0     0.0    0.0
     memoizeM       Main                     99           1    0.0    0.0     0.0    0.0
      memoizeM.f    Main                    100         481    0.0    0.0     0.0    0.0
       memoizeM.f.\ Main                    101         481    0.0    0.0     0.0    0.0
        memoizeM.g  Main                    102         245    0.0    0.0     0.0    0.0
         mkTreeM    Main                    103         245    0.0    0.0     0.0    0.0
 CAF                GHC.Conc.Signal          87           0    0.0    0.0     0.0    0.0
 CAF                GHC.IO.Encoding          80           0    0.0    0.0     0.0    0.0
 CAF                GHC.IO.Encoding.Iconv    78           0    0.0    0.0     0.0    0.0
 CAF                GHC.IO.Handle.FD         71           0    0.0    0.0     0.0    0.0
{% endhighlight %}

We can see that the number of calls to the memomised function `mkTreeM` is
245, which is the number of unique nodes in the DAG. This compares to 
26359321, the size of the tree, for the non-memoised function.

### Applying Memoisation to the Game Tree

We can use this same memoisation technique for constructing the game tree.
However, we have a bit of a flaw in the way we designed the tree, and this
will get in the way of memoisation. Recall that the game state is defined by:

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
the list of possible moves from that board.

To do this we're going to put the moves made from a parent node to a child
node into the parent itself, as an array of moves.

{% highlight haskell %}
data GameState = GameState {
                    currentPlayer :: Player,
                    possibleMoves :: [Move],
                    currentBoard :: Board
                 }
{% endhighlight %}


