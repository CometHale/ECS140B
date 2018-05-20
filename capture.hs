-- Capture The Flag


{- 
Original Algorithm

-- select best move by using MiniMax search
-- static board eval function 
-- necessary move generation capability

- Generate all child adversarial trees
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

capture :: [String] -> Char -> Int -> String
capture history fealty lookahead_count
  | validate_input history fealty lookahead_count && is_beginning_board (head history) && elem fealty "b" = capture_helper history fealty lookahead_count 'w' 0 -- run alg on white first, then run alg normally
  | validate_input history fealty lookahead_count && is_beginning_board (head history) /= True || is_beginning_board (head history) && elem fealty "w" = capture_helper history fealty lookahead_count fealty 0 -- run alg normally
  | otherwise  = "False" -- Change this to something that makes more sense

-- Description: Given a list of board states, a player and a max lookahead count, validates the input
-- Expects: 
    -- List of Lists containing strings representing the history of the board 
    -- (with the current board position as the first entry)
    -- a Char representing whose turn it currently is
    -- an Int representing the maximum number of moves to look ahead to
-- Returns:
    -- a Bool representing whether the input was valid, or an error if it wasn't valid

validate_input :: [String] -> Char -> Int -> Bool
validate_input history fealty lookahead_count = validate_input_state (head history) && validate_input_player fealty && validate_input_look_ahead lookahead_count

validate_input_state :: String -> Bool
validate_input_state state
  | all valid_state_char state = True
  | otherwise = error "Input current board state contains characters other than 'w', 'W', 'B', 'b' or '-'."

validate_input_player :: Char -> Bool
validate_input_player player 
  | valid_state_char player = True
  | otherwise = error "Input current player is neither 'w' nor 'b'."

validate_input_look_ahead :: Int -> Bool
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


capture_helper :: [String] -> Char -> Int -> Char -> Int -> String
capture_helper history fealty look_ahead_count initial_fealty current_look_ahead_count = "Hello" -- needs to be changed; function stub

-- | current_look_ahead_count == look_ahead_count  = ""


-- a step in the tree is all of the possible moves a player could make based on the other player's previous moves
-- Description: Generates a list of lists of moves that represent an adversarial tree
-- Expects: 
        -- A list of lists of Strings representing the initial board state and a history
        -- A Char representing whose turn it currently is
        -- An Int representing the number of moves to look ahead to
-- Returns:
    -- A list of lists of Strings representing the various child adversarial trees of the initial board state
    -- (i.e. the first one in the list is the leftmost child tree of the initial state, the next one is the next leftmost, etc.)
-- move_generator => [[String]] => Char => Int => [[String]]
-- move_generator history player look_ahead_count
  -- | look_ahead_count == 0  = ""
  -- | otherwise 


-- Description:
-- Expects:
-- Returns:
-- static_board_eval





