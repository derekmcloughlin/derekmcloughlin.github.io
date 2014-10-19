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

It's important to note that the game can never reach a stalemate e.g.
where player A passes, then player B passes, then player A passes again. This
is because the player must make a move at the start of their turn. If they 
can't then the game ends.  We also can't get stalemate situations where a 
succession of moves by A and B result in the same board that A started 
with. This is because the number of dice decreases as time goes on - not all 
the conquered dice are returned to the board.

For the 2x2 boards, we can generate game trees for all scenarios and look
at some stats, depending on whether A starts or B starts:

* The number of nodes in the tree.
* The eventual winners in each tree.

To help with this it's useful to check whether the Lisp code gives the same
results. Here are some helper functions that we can run in CLisp:

{% highlight lisp %}
(defun tree-size (tree)
  (let ((moves (caddr tree)))
    (+ 1 (if moves
           (apply #'+ (get-tree-size tree))
           0))))

(defun get-tree-size (tree)
  (mapcar (lambda (move)
	    (tree-size (cadr move)))
	  (caddr tree)))

(defun all-winners (tree)
TODO
{% endhighlight %}

The equivalents in Haskell:

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
with optimisations switched on, rather than run it through GHCi. To do this
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
  
    A-2 A-2 B-2
  A-3 B-1 A-3
A-3 A-3 B-3

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

{% highlight haskell %}
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

The difference between the two implementations is that the one using the fold
runs the folding function over the entire tree, keeping the whole thing in 
memory. The implementation that flattens the tree doesn't need to keep the
whole tree, nor the whole flattened tree, in memory. As nodes are unused, the
garbage collector takes care of them.

TODO: better explanation 

This isn't much use to us for the game in general, as we really do need to keep
the whole tree in memory to play the game. Such large trees pose two separate
problems for us:

* It takes a long time to go through the whole tree.
* It takes up a lot of memory.

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
behind this is the Lisp Cons Cell.

### Lisp Cons Cells

Lists in Lisp are linked lists. Each node in the list can point or refer to 
another list. A list can have multiple references to the same list: here's
a simple example:

{% highlight lisp %}
(defparameter *zero*    '("zero"))
(defparameter *one*     '("one"))
(defparameter *two*     '("two"))

(defparameter *game-tree*
    (list (get-node 1)
          (get-node 2)
          (get-node 0)
          (get-node 1)
          (get-node 2)))

(defun get-node (node_number)
    (cond ((eq 0 node_number)
                *zero*)
          ((eq 1 node_number)
                *one*)
          ((eq 2 node_number)
                *two*)))
{% endhighlight %}

In CLisp:

{% highlight lisp %}
[1] *game-tree*
(("one") ("two") ("zero") ("one") ("two"))
{% endhighlight %}

Lisp does allow mutation, so we can do the following:

{% highlight lisp %}
[2] (setf (car *one*) "une")
"une"
[3] *game-tree*
(("une") ("two") ("zero") ("une") ("two"))
{% endhighlight %}

There's only one instance each of `*zero*`, `*one*` and `*two*` in the list.

If you want to find more information on this, Peter Siebel's book 
[Practical Common Lisp](http://www.gigamonkeys.com/book/), in particular Chapter 12, 
[They Called It Lisp For A Reason](http://www.gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html)
is a good read.

Haskell has a similar concept to Lisp Cons Cells, but with a significant
difference: unlke Lisp, Haskell's type system doesn't allow us to define 
arbitrarily deeply nested lists. Haskell cons cells only work with 
single-dimension lists.

TODO: is this true??





