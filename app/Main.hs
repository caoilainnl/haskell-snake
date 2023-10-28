{-# LANGUAGE Arrows #-}

import Control.Monad
import Data.List
import System.Random
import Reactive.Banana
import Reactive.Banana.Frameworks

-- Define custom types for direction
data Direction = Up | Down | LeftDir | RightDir
  deriving (Eq, Show)

-- Custom definitions for directions to avoid ambiguity with Prelude
myLeft  = LeftDir
myRight = RightDir

-- Game state
data GameState = GameState { snake :: [(Int, Int)], direction :: Direction, food :: (Int, Int) }

-- Movement function to update the snake's position
move :: Direction -> (Int, Int) -> (Int, Int)
move Up (x, y) = (x, y - 1)
move Down (x, y) = (x, y + 1)
move LeftDir (x, y) = (x - 1, y)
move RightDir (x, y) = (x + 1, y)

-- Update the game state based on direction
updateState :: GameState -> GameState
updateState (GameState snake dir food) =
    let newHead = move dir (head snake)
        newSnake = newHead : init snake
    in GameState newSnake dir food

-- Parse direction input from the user
parseDirection :: Char -> Direction
parseDirection 'w' = Up
parseDirection 's' = Down
parseDirection 'a' = myLeft
parseDirection 'd' = myRight
parseDirection _ = Up -- Default to 'up' if invalid input

-- Randomly generate a food location
generateFood :: IO (Int, Int)
generateFood = do
    x <- randomRIO (0, 19)
    y <- randomRIO (0, 19)
    return (x, y)

-- The FRP (Functional Reactive Programming) network
networkDescription :: Event (Maybe Char) -> Moment (Event GameState)
networkDescription inputEvent = do
    -- Initialize game state
    let initialState = GameState [(10, 10)] Up (0, 0)

    -- Define the event to update the game state
    let updatedState = fmap (const initialState) inputEvent

    -- Return the event for further handling
    return updatedState

-- The main function
main :: IO ()
main = do
    -- Get the initial food location
    foodPos <- generateFood

    -- Create an event network
    (addHandler, fireEvent) <- newAddHandler
    let inputEvent = fromAddHandler addHandler

    -- Create the network and compile it
    network <- compile $ networkDescription inputEvent

    -- Activate the network
    actuate network

    -- Game loop: Get user input and trigger events
    forever $ do
        -- Get user input
        putStrLn "Enter direction (w/a/s/d):"
        input <- getLine
        -- Fire the event with the user's input (as `Maybe Char`)
        fireEvent (Just (head input))  -- Assuming input is not empty

        -- Print the direction for feedback
        putStrLn $ "Direction: " ++ show (parseDirection (head input))
