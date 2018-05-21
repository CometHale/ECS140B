-- Capture The Flag

{-
Revised Algorithm

- Given a list of states, a current player and a max number of lookaheads:

- Check to make sure the first state in the list contains only '-wWbB', else error
- Check to make sure the current player is one of 'wb', else error
- Check to make sure the number of lookaheads is greater than 0, else error (or return the initial state??)
- [ MIGHT NOT NEED TO DO THIS:

    Check if next move will be the very first move in the game, i.e. : 
    If first state is similar to the form:

        - w  W  w -
        - w  w  w -
        - -  -  - -
        - b  b  b -
        - b  B  b -
    
     -- and the current player is black, then generate a set of possible moves for white first (as white always makes the starting move), 
    then run the entire alg for each of the subtrees. 

     -- if the current player is white or the board is not at a beginning of the game state, then just run the alg normally
  ]
- 

- Generate all child adversarial trees of the first state in the list
    - Generate one child adversarial tree
        - given a start state, determine all the moves the 
        current player can make based on that start state
            - generate a legal move
                - for each of the current player's pawns, determine what legal moves that can make
                - repeat for each pawn until a previously seen board is seen again
            - [generated_move] to list of lists
            - repeat until a previously seen board state is reached
            - return list of lists, where each inner list contains only one string
        - Repeat for each subtree (using the root of the subtree as the initial move)
        until max lookahead is reached or until a previously seen state is reached. Swap the current
        player as needed
    - append generated tree to list of possible trees
- apply static eval func to list of possible trees
- determine which tree has the max value and return it (or it's root)
-}

{- 

Minimax algorithm

-- 1. Generate the game tree to as many levels (plies) that time and space 
    constraints allow.  The top level is called MAX (as in it’s now MAX’s turn to move),
    the next level is called MIN, the next level is MAX, and so on.  

-- 2. Apply the evaluation function to all the terminal (leaf) states/boards to get 
    “goodness” values  

-- 3. Use those terminal board values to determine the values to be assigned to the immediate parents: 
        a) if the parent is at a MIN level, then the value is the minimum of the values of its children 
        b) if the parent is at a MAX level, then the value is the maximum of the values of its children  
-- 4. Keep propagating values upward as in step 3 
-- 5. When the values reach the top of the game tree, MAX chooses the move indicated by the highest value  

-}

-- Description: Given a board position, determines the best next move for a
-- given player. 
-- Expects: 
    -- List of Lists containing strings representing the history of the board 
    -- (with the current board position as the first entry)
    -- a Char representing whose turn it currently is
    -- an Int representing the maximum number of moves to look ahead to
-- Returns:
    -- a String representing the next best move for the initial player 
    -- (i.e. the person whose turn it was when the function was originally called)

import Data.Fixed
data Space = Space {xpos::Float, ypos::Float, occupant::Char} deriving (Eq, Show, Read)
data Board =  Board {spaces :: [Space], width :: Float} deriving (Eq, Show, Read)

capture :: [String] -> Char -> Float -> String
capture history fealty lookahead_count
  | validate_input history fealty lookahead_count && is_beginning_board (head history) && elem fealty "b" = 
    convert_output (
        capture_helper
            (convert_input 
                (head history)
                (Board {spaces=[], width=(
                    sqrt (
                        fromIntegral (length (head history))
                        )
                )})
                0
            )
            fealty
            lookahead_count
            'w'
            0
        )
  -- capture_helper history fealty lookahead_count 'w' 0 -- run alg on white first, then run alg normally
  | validate_input history fealty lookahead_count && is_beginning_board (head history) /= True || is_beginning_board (head history) && elem fealty "w" =
    convert_output (
        capture_helper (
            convert_input
                (head history)
                (Board {spaces=[], width=(
                    sqrt (
                        fromIntegral (length (head history))
                    )
            )})
                0
        ) 
        fealty
        lookahead_count
        fealty
        0
    )
    -- convert_input history fealty lookahead_count Board {spaces=[], width=(sqrt (length (head history)))}
  -- capture_helper history fealty lookahead_count fealty 0 -- run alg normally
  | otherwise  = "False" -- Change this to something that makes more sense

-- Description: Given a list of board states, a player and a max lookahead count, validates the input
-- Expects: 
    -- List of Lists containing strings representing the history of the board 
    -- (with the current board position as the first entry)
    -- a Char representing whose turn it currently is
    -- an Int representing the maximum number of moves to look ahead to
-- Returns:
    -- a Bool representing whether the input was valid, or an error if it wasn't valid

validate_input :: [String] -> Char -> Float -> Bool
validate_input history fealty lookahead_count = validate_input_state (head history) && validate_input_player fealty && validate_input_look_ahead lookahead_count

validate_input_state :: String -> Bool
validate_input_state state
  | all valid_state_char state = True
  | null state = error "Input current board state cannot be empty."
  | otherwise = error "Input current board state contains characters other than 'w', 'W', 'B', 'b' or '-'."

validate_input_player :: Char -> Bool
validate_input_player player 
  | valid_state_char player = True
  | otherwise = error "Input current player is neither 'w' nor 'b'."

validate_input_look_ahead :: Float -> Bool
validate_input_look_ahead lookahead_count
  | lookahead_count > 0 = True
  | otherwise = error "Max lookahead count must be greater than 0."


-- Description: Given a character, makes sure it is 'w', 'W', 'B', 'b' or '-'
-- Expects: 
    -- A character
-- Returns:
    -- A boolean
valid_state_char :: Char -> Bool
valid_state_char character
    | elem character "wWbB-" = True
    | otherwise  = False


-- Description: Given a state, determines whether or not is is a 'starting board', i.e. if it is the board before any moves have been made
-- Expects: 
    -- A String
-- Returns:
    -- A boolean
is_beginning_board :: String -> Bool
is_beginning_board state = False -- needs to be changed; function stub


-- Description: Given state, and a (preferably empty) Board, creates a Board based on the state
-- Expects:
    -- A String representing a state
    -- A Board representing the game board
-- Returns:
    -- A Board representing the game board at the given state
convert_input :: String -> Board -> Float -> Board
convert_input [] board (_) = board
convert_input (x:xs) (Board {spaces=spcs, width=w}) ypos
    |(mod' (fromIntegral (length spcs)) w) < (w - 1) && ypos <= (w - 1)= convert_input xs (
        Board {spaces=spcs ++ [
            (create_space
                x 
                (mod' (fromIntegral (length spcs)) w)
                ypos
                
            )
            ],
            width=w
        }
    )
    ypos
    |(mod' (fromIntegral (length spcs)) w) == (w - 1) && ypos <= (w - 1) = convert_input xs (
        Board {spaces=spcs ++ [
            (create_space
                x 
                (mod' (fromIntegral (length spcs)) w)
                ypos
                
            )
            ],
            width=w
        }
    )
    (ypos + 1)
    | otherwise = convert_input xs (
        Board {spaces=spcs ++ [
            (create_space
                x 
                (mod' (fromIntegral (length spcs)) w)
                0
                
            )
            ],
            width=w
        }
    )
    0


  -- |(mod' (fromIntegral (length spcs)) w) < w && xpos < w  = convert_input xs (
  --       Board {spaces=spcs ++ [
  --           (create_space
  --               x 
  --               xpos
  --               (mod' (fromIntegral (length spcs)) w)
  --           )
  --           ],
  --           width=w
  --       }
  --   )
  --   xpos
  -- |(mod' (fromIntegral (length spcs)) w) == (w - 1) = convert_input xs (
  --       Board {spaces=spcs ++ [
  --           (create_space
  --               x 
  --               0
  --               (mod' (fromIntegral (length spcs)) w)
  --           )
  --           ],
  --           width=w
  --       }
  --   )
  --   0
  -- |(mod' (fromIntegral (length spcs)) w) < (w - 1) = 
  -- |otherwise = convert_input xs (
  --       Board {spaces=spcs ++ [
  --           (create_space
  --               x 
  --               xpos
  --               (mod' (fromIntegral (length spcs)) w)
  --           )
  --           ],
  --           width=w
  --       }
  --   )
  --   (xpos + 1)

-- Description: Given a piece, an x coordinate and a y coordinate, creates a Space 
-- Expects:
    -- A Char representing an empty space or a piece
    -- An Int representing an x coordinate
    -- An Int representing a y coordinate
-- Returns:
    -- A Space with the given x,y coordinates and the given occupant
create_space :: Char -> Float -> Float -> Space
create_space occ x y = (Space {xpos=x, ypos=y, occupant=occ})

convert_output :: Board -> String
convert_output (Board {spaces = spcs, width=w}) = "" ++ show spcs

-- Description: Given a list of states, a player, a max lookahead count, the current player and the current lookahead count, 
--  does the work of gathering the list of move subtrees
-- Expects: 
    -- A list of strings, where each string represents a state
    -- A character representing the initial player
    -- An Int representing the original lookahead count
    -- A character representing the player about to take a move
    -- An Int representing the number of lookaheads remaining
-- Returns:
    -- A String representing the next best move for the initial player

capture_helper :: Board -> Char -> Float -> Char -> Float -> Board
capture_helper (Board {spaces=spcs, width=w}) player max_lookahead current_player current_lookahead = Board {spaces=spcs, width=w}



{-
capture_helper :: [String] -> Char -> Int -> Char -> Int -> String -- needs to be changed; function stub
capture_helper history initial_fealty look_ahead_count current_fealty current_look_ahead_count = static_board_eval (generate_moves (head history) [])

-}

-- | current_look_ahead_count == look_ahead_count  = ""


{-
    Convert input board string into a list of rows

    to generate a list of moves, try moving each piece in it's legal ways, one piece at a time
-}
-- Description: Generates a list of lists of moves that represent adversarial trees
-- Expects: 
        -- A String representing the current board state
        -- A list of lists of strings representing the adversarial trees possible based on the current board state
-- Returns:
    -- A list of lists of Strings representing the various child adversarial trees of the initial board state
    -- (i.e. the first one in the list is the leftmost child tree of the initial state, the next one is the next leftmost, etc.)
{-

generate_moves :: String -> [[String]] -> [[String]] -- needs to call generate_move_option until a move is repeated
generate_moves state [] = generate_move_option state []
generate_moves state moves = null moves \= True && generate_move_option state moves

-- a step in the tree is all of the possible moves a player could make based on the other player's previous moves
-- Description: Generates a move based on the current board state and the current player
-- Expects: 
        -- A String representing the current board state
        -- A Char representing the player whose turn it is
        -- A String representing the eventual output
-- Returns:
    -- A String representing a possible next move for the player

generate_move_option :: String -> Char -> String
generate_move_option [x:xs] player result
  | elem x "-" = generate_move_option xs player result
  | elem x "wW" && elem player "w" && (head xs)


-}


{-

  -- | null result && null xs \= True = generate_move_options xs (""++(generate_move x)):[]):result -- generate a move for the piece represented by x
  -- | null result && null xs = error "generate_move_options error -- state and result were empty"
  -- | null result \= True && null xs = result
  -- | otherwise = generate_move_options xs (++(generate_move x)):[]):result
-}



-- a step in the tree is all of the possible moves a player could make based on the other player's previous moves
-- Description: Generates a list of lists of moves that represent an adversarial tree
-- Expects: 
        -- A list of lists of Strings representing the initial board state and a history
        -- A Char representing whose turn it currently is
        -- An Int representing the number of moves to look ahead to
-- Returns:
    -- A list of lists of Strings representing the various child adversarial trees of the initial board state
    -- (i.e. the first one in the list is the leftmost child tree of the initial state, the next one is the next leftmost, etc.)

-- generate_adversarial_trees :: String -> [[String]] -> [[String]]

{-

-- generate_adversarial_trees start_state [] = (generate_tree start_state):[]
-- generate_adversarial_trees start_state result
--     | elem (generate_tree start_state) result /= True = result
--     | otherwise = result
-}


-- move_generator => [[String]] => Char => Int => [[String]]
-- move_generator history player look_ahead_count
  -- | look_ahead_count == 0  = ""
  -- | otherwise 


-- Description:
-- Expects:
-- Returns:
-- static_board_eval





