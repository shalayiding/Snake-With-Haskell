# Snake With Haskell

## Team members:

- Darian Hodzic
- Aierken Shalayiding

## Demo
![](docs/snake.gif)

## How to Play the Game
1. Clone the project via `git clone` in a local folder.
2. Open your terminal and change your directory via `cd` so that you're in the `snake` folder.
3. Run the command `stack runhaskell src/Lib.hs` and the game will begin.
4. Follow the onscreen directions and play the game!

## Summary Description

Our project goal was to create a terminal-based snake game which allows for
controlling movement and interacting with the game environment through multiple game states such as start state, running state, and end state. In this game, interaction with the environment includes collision with the borders of the board as well as eating food and increasing the length of the snake.

## Project Execution Summary
The order of our approach from the start of this project was to start with rendering the game board, then working on snake movement, food generation, and then snake interaction with the food last.

The first task we attempted was setting up the border of our game board. We accomplished this by utilizing `[[Int]]` as our data structure. We recognized that this was not the best design decision, so as we had progressed further into the project, we attempted to use an `Array` data type as a safer design choice which didn't work as intended. Finally, we utilized `Elt` as our data type defined as `data Elt = Elt Int Char (Int, Int)` which we were able to integrate with existing functions written alongside a few more additions. In regards to setting up the border of our game board, we were able to use this `Elt` data type so that `setBorder` is returning an `[[Elt]]` instead of an `[[Int]]` as it was in our previous version. Utilizing `Elt` proved safer as it fixed an issue we encountered in an earlier version utilizing `[[Int]]` where doing `setBorder n n` would work, but doing`setBorder n m` would not work since the snake would hit an invisible border in the middle of our game board. Essentially, having a rectangular border in the earlier version would break the game, but now this is not the case. 

Next, we needed to render not only the border, but the necessary game elements such as the snake and food. Thus, we defined codes for each element of the game where `1 = '#', 2 = '@', 3 = 'o', 4 = '0'` such that `#` represents the border, `@` represents the head of the snake, `o` represents the body of the snake, and `0` represents food. There were functions written that would insert the corresponding code values into the board which were `foodCode`, `headCode`, and `insertHeadPos`. To draw the board with the snake, border, and food, we wrote a function called `convertString` that would convert our `[[Elt]]` board that has all the corresponding element codes within it into a `String`. Finally, by utilizing `putStrLn` in a `draw` function, we rendered the respective `String` onto the terminal.

The second task we attempted was working on snake status and snake movement. The first thing we thought about was how we would know whether the snake was alive or dead. To determine this, we set up a data type called `SnakeStatus` which holds the values of `Alive` or `Dead` and could be helpful in determining not only the status of the snake, but also the status of the game itself.  In regards snake movement, we set up a data type called `SnakeDirection` that would be utilized in determining the snake's current direction. This data type was used when we wrote a function `sdir` to associate WASD keys with the corresponding snake direction. Combining, our implementation of snake movement and our `SnakeStatus` data type, we wrote a `gameStatus` function that would recognize whether the snake had turned in upon itself or whether the snake had collided with the border. Either one of those cases resulted in a status of `Dead` and eventually within our game loop, a game over.

The third task we attempted was figuring out how food generation was going to work. We first wrote a `foodCandidates` function which would pass back a list of possible food spawn locations on the board such that none of those locations were the border or the snake itself. Then, we decided on importing `System.Random` and utilizing a `getStdRandom` to generate a random integer within some bound. This integer value was used as an indexing value within our `generateFood` function which took in a list of locations (the `foodCandidates` list of available locations) and would be indexed by our random integer from utilization of `getStdRandom`.

The last major task accomplished was making sure the snake interacted with the food properly. Within our `gmLoop` function where the core logic of the game is taking place, we have a condition that checks if the location of the head of the snake is equivalent to the location of the current food on the board. If so, we make sure to update the snake's body with another `o` (happening in our `eat` function) and then recurse on `gmLoop` with this update.

Finally, we decided to add a bit more flair to our project that wasn't in our original project goal. We made sure to add a basic UI which just has the title of the game, and directions on how to play. In addition, we have a score tracker that increases for each food item consumed by the snake as well as a speed tracker that will increase with each drastic change in speed (for every two food items consumed).

## Additional Details
- Our project utilizes a few dependencies added in the `dependencies` section of the `package.yaml` file. Here are the ones we used and reasons why for each:
  1. `random` for generating random values when determining possible food spawn locations on the game board.
  2. `ansi-terminal` for hiding the cursor on the terminal (we utilized `hideCursor` within `main`).
  3. `process` for clearing the screen via `system "clear"`
  4. `QuickCheck` for writing test case properties that can be run to test the correctness of our project.

## Code Structure 
The code is structured in a manner that follows our order of approach mentioned in the execution summary.

Our datatypes, game status, border/board, rendering, and code functions for corresponding elements of the game are all organized towards the top. What follows is our code for random generation of food items and then our core game loop and main functions. The main component of our code is the function `gmLoop` which is where the majority of the core logic is taking place as we recurse on it.

### Code Examples
An example code excerpt within our project that is a good example of Haskell features is the following:

```
-- Gives back a list of possible food candidate locations on the board.
foodCandidates :: [[Elt]] -> [(Int, Int)]
foodCandidates [] = []
foodCandidates (b : bs) = locals ++ foodCandidates bs
  where
    emptyspace = filter (\(Elt code _ _) -> code == 0) b
    locals = map (\(Elt _ _ xy) -> xy) emptyspace
```

The utilization of higher order functions such as `filter` and `map` alongside an anonymous function defined as `(\(Elt _ _ xy) -> xy)` is inherently a Haskell feature. The way `foodCandidates` is written follows typical Haskell style where we simply filter all empty spaces in one line and map the correspond coordinate of each empty space in one line as well. The result of this map is then appended to a recursive call of `foodCandidates` which results in a function that accomplishes the necessary task in a very concise fashion.


Another example code excerpt within our project that we found interesting enough to mention is the following:

```
forkIO $ ((>>) .) . (>>) 
  <$> putMVar future_input 
  <*> tryPutMVar latest_input 
  <*> (putStrLn . ("pressed key: " ++) . return 
  =<< getChar
```

This snippet was sourced from the following url: https://stackoverflow.com/questions/63874453/keep-reading-user-keyboard-input-if-available-while-echoing-back-the-latest-avaso 

We utilized `Control.Concurrent` in our project so that we could work with concurrent threads. In the case of this code excerpt, we were able to break this down into pieces and rewrite it into a format that is more readable to better our understanding of it as well as getting our game functioning properly. We rewrote it as the following:

```
_ <-
     forkIO
        ( do
            c <- getChar
            putMVar future_input_real c
            _ <- tryPutMVar latest_input_real c -- tryPutMVar is a non-blocking version of putMVar
            return ()
        )
```

We were able to see how the use of the composition operator `.` and the sequence operation `<<` would translate easily over to a `do` block in regards the sequence of `putMVar` and `tryPutMVar`. Also, we recognized that the reversal of the monadic bind operation defined as `=<<` would mean that the function `getChar` was binded to the result of all the stuff before it. This directly translates over into our `do` block where we just utilized `getChar` in a more familiar fashion as `c <- getChar`. The overall point being made here is that again, whether this code excerpt was written our way or the original way (in a more concise fashion), there are features of Haskell (monadic bind, higher order functions) that exist to make it easier to accomplish our goal in a concise manner.
      
## Difficult Code Expression with Haskell
The nature of Haskell being a language built on immutable data resulted in some difficult code expression with regards to making a game. One problem we ran into was when we were trying to have our snake eat randomly generated food. The problem was that after the first food item was consumed, the second one was unable to be consumed. This problem was something we took some time to figure out, but because of the functional nature of Haskell, we determined that passing in a default food location and then recursing on an updated food location would be a sufficient solution. Another difficulty with Haskell was the strict type definitions on functions. Utilizing functions with returns like `IO ()` and `IO String` made it difficult for us to utilize both IO and pure functions in our `gmLoop`.

## Other Approaches We Took
In the early stages of the project we used `[[Int]]` as our main data type. It made the board easy to create at first, but searching the elements from this type of structure resulted in some major problems. With our usage of `[[Int]]`, searching for a non-existing index of the matrix was an unsafe operation in which there was no way to detect whether the given `(x,y)` was out of the bounds of the matrix. Based off of our checkpoint feedback, we made the change towards `[[Elt]]` as our datatype. `Elt` is defined in a way that the position of the current `Elt` is inside of itself. This made searching much safer since we just had to loop all of the elements within the matrix and detect whether or not the searching target location exists inside of the matrix.

With quickCheck, we wrote a few different properties to test our function correctness. For example, the following code checks our function `randomGenerator` which should give us a random value between a lower bound `a` and an upper bound `b`.

```
prop_randomGenerator :: Int -> Int -> Bool
prop_randomGenerator a b = 
(a <= 0 || b <= 0 || a > b) 
|| (a <= result && result <= b)
  where
    result = getRandomValue a b
```

Issues we ran into with quickcheck involved problems with importing established data types such as `SnakeDirection` defined in our `Lib.hs` file.

Another issue we ran into was regarding terminal refreshing. We tried two different methods to refresh the terminal, the first one being `clearScreen` from `System.Console.ANSI`. Even though we acknowledge that utilizing `clearScreen` is much more efficient and cheaper computationally than our current method of clearing the screen, it gave us an issue where it kept offsetting the position of the board, thus making the game unplayable. For our current method of clearing the screen, we utilized `system "clear"` from `System.Process`. In comparison with `clearScreen` this may not be the cheapest option for refreshing the screen of our game, but it has been working very well for attaining the goals of our project. Overall, we have tested our snake game to make sure it doesn't crash due to the computationally expensive nature of the `system "clear"` command.

## What We Learned
This has been a challenging project for the following reasons : 
- In this project we used a self-made data type `[[Elt]]` for searching elements using the given index from the board.
  Our usage of `[[Int]]` resulted in a plethora of issues that were resolved once we had learned how to utilize `Elt` properly.
  We also learned the effect of using `[[Elt]]` in terms of better performance due to the issues we faced otherwise.

- forkIO is used to get input from another thread and return it to the main thread.

- Finding all the possible empty locations inside of the board and returning the valid locations.

- We learned how to grab our UI contents and split them accordingly so that the `String` would output properly onto the terminal.

- Adding a new piece of the snake to the snake body after consuming a food item while not causing other bugs to arise.

- Writing the code in the more efficent way to help the game run smoothly without glitches (issues with clearing the terminal).

- The biggest challenge we ran into with Haskell was multithreading for our game input. The problem we ran into was the game not wanting to end even after the `gameStatus` changed accordingly, so the process would keep running in the background. We realized afterwards that this was an issue with ghci itself where the ghci thread would not end after the game should end, resulting in the process still running. This issue was fixed by running the game with `stack runhaskell` rather than `stack ghci` which resulted in all of the necessary threads being killed.
