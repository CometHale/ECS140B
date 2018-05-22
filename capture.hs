-- Capture The Flag

{-
STATIC EVALUATION STRATEGY

1) Player has winning board (no Opponent flag) : +50
2) Player has losing board (no Player flag): -50
3) Otherwise, count the number of pawns surrounding player's flag * 5

Explanation: We have the standard lose and win situation where the player having a winning board will have the highest value and a losing board will have the lowest value. 
We chose to use a heuristic based on how many pawns are surrounding the player's flag. Since to win the game, the opponent needs to either capture all the pawns or to capture
the flag, counting the number of pawns surrounding a flag seemed the best fit to calculate how well the player is doing. For example, the more pawns that are closer to the 
flag means that the pawn isn't as vulnerable and easily taken by an opponent's pawn. Furthermore, this allows a rough estimate of how many pawns the player currently has as 
pawns that aren't close to the flag may be as vulnerable and taken the next turn. 

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

import Data.Fixed
data Space = Space {xpos::Float, ypos::Float, occupant::Char} deriving (Eq, Show, Read) -- One 'square' on a game board
data Board =  Board {spaces :: [Space], width :: Float} deriving (Eq, Show, Read) -- One state of the game board
-- [Board] -- A list of game states (as Boards), with the first being the current state

capture :: [String] -> Char -> Float -> [[String]] -- !!!!!!!!!!!!!!!!!!!!!!!! OUTPUT NEEDS TO BE A STRING
capture history fealty lookahead_count
  | validate_input history fealty lookahead_count && is_beginning_board (head history) && elem fealty "b" = 
    -- convert_output (
        __to_eval__build_adversarial_tree
            (capture_helper
                (((convert_input 
                    (head history)
                    (Board {spaces=[], width=( sqrt (fromIntegral (length (head history))))})
                    0
                ):[]):[])
                fealty
                lookahead_count
                'w'
                []
            )
            []
        -- )
  -- capture_helper history fealty lookahead_count 'w' 0 -- run alg on white first, then run alg normally
  | validate_input history fealty lookahead_count && is_beginning_board (head history) /= True || is_beginning_board (head history) && elem fealty "w" =
    -- convert_output (
        __to_eval__build_adversarial_tree (
            capture_helper (((convert_input (head history) (Board {spaces=[], width=(sqrt (fromIntegral (length (head history))))}) 0):[]):[]) fealty lookahead_count fealty []
            )
            []
    -- )
    -- convert_input history fealty lookahead_count Board {spaces=[], width=(sqrt (length (head history)))}
  -- capture_helper history fealty lookahead_count fealty 0 -- run alg normally
  | otherwise  = error "Something happened in capture" -- Change this to something that makes more sense !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
is_beginning_board state = True -- needs to be changed; function  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


-- Description: Given state, and a (preferably empty) Board, creates a Board based on the state
-- Expects:
    -- A String representing a state
    -- A Board representing the game board
-- Returns:
    -- A Board representing the game board at the given state
convert_input :: String -> Board -> Float -> Board
convert_input [] board (_) = board
convert_input (x:xs) (Board {spaces=spcs, width=w}) ypos
    |(mod' (fromIntegral (length spcs)) w) < (w - 1) && ypos <= (w - 1) = convert_input xs (
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
    |otherwise = error "Something happened in convert_input"


-- Description: Given a piece, an x coordinate and a y coordinate, creates a Space 
-- Expects:
    -- A Char representing an empty space or a piece
    -- An Int representing an x coordinate
    -- An Int representing a y coordinate
-- Returns:
    -- A Space with the given x,y coordinates and the given occupant
create_space :: Char -> Float -> Float -> Space
create_space occ x y = (Space {xpos=x, ypos=y, occupant=occ})

-- convert_output :: Board -> String -- stub !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- convert_output (Board {spaces = spcs, width=w}) = "" ++ show spcs

__to_eval__build_adversarial_tree :: [[Board]] -> [[String]] -> [[String]] 
__to_eval__build_adversarial_tree boards adversarial_tree
  |null boards = adversarial_tree
  |null boards /= True = __to_eval__build_adversarial_tree (tail boards) ((__to_eval__build_adversarial_path (head boards) [] []):adversarial_tree)
  |otherwise = error "Something happened in __to_eval__build_adversarial_tree"

__to_eval__build_adversarial_path :: [Board] -> String -> [String] -> [String]
__to_eval__build_adversarial_path ((Board {spaces = spcs, width=w}):xs) building_state path
    |null spcs = building_state:path
    |null spcs /= True = __to_eval__build_adversarial_path ((Board {spaces = spcs, width=w}):xs) (__build_state_string (Board {spaces = spcs, width=w}) building_state) path
    |otherwise = error "Something happened in __to_eval__build_adversarial_path"

__build_state_string :: Board -> String -> String
__build_state_string (Board {spaces=spcs, width=w}) str
  |null spcs = str
  |null spcs /= True = __build_state_string (Board {spaces=(tail spcs), width=w}) (let (ys,zs) =  splitAt (fromIntegral (round (((ypos (head spcs)) *w) + (xpos (head spcs))))) str in ys ++ [(occupant (head spcs))] ++ zs)
  |otherwise = error "Something happened at __build_state_string"
-- __build_state_string (Board {spaces=spcs, width=w}) str

-- __build_state_string (Board {spaces = [], width=w}) str = str -- all spaces accounted for
-- __build_state_string (Board {spaces = ((Space {xpos=x, ypos=y, occupant=occ}):xs), width=w}) str = let (ys,zs) =  splitAt (fromIntegral (round ((y*w) + x))) str in ys ++ [occ] ++ zs


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

-- decrease the lookahead count here
capture_helper :: [[Board]] -> Char -> Float -> Char -> [[Board]] -> [[Board]]
capture_helper boards player max_lookahead current_player result
  | max_lookahead == 0 && null boards = result
  | null boards && max_lookahead > 0 && null result /= True = capture_helper
                    result
                    player
                    (max_lookahead - 1)
                    (switch_player player)
                    (generate_trees (head (head result)) player current_player []) ++ result
  | null boards /= True && max_lookahead > 0 = capture_helper
                    (tail boards)
                    player
                    max_lookahead
                    (switch_player player)
                    (generate_trees (head (head boards)) player current_player []) ++ result
  |otherwise = error "Somethign happened in capture_helper"


switch_player :: Char -> Char
switch_player player
  |elem player "w" = 'b'
  |otherwise = 'w'

-- Description:
-- Expects:
-- Returns:
generate_trees :: Board -> Char -> Char -> [[Board]] -> [[Board]]
generate_trees (Board {spaces = spcs, width=w}) player current_player trees
  | null spcs = trees
  | null spcs /= True = (generate_a_tree (Board {spaces = (tail spcs), width=w}) (head spcs) player []) ++ trees
  | otherwise = error "Something happend in generate trees"
-- Description:
-- Expects:
-- Returns:
-- needs to generate a tree based on each possible move a pawn of the current player can make
-- for a given pawn or flag, generate all the moves it can make 

generate_a_tree :: Board -> Space -> Char -> [[Board]] -> [[Board]]
generate_a_tree (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant=occ}) player result
  |null spcs /= True && elem player "w" = (move_pawn_forward (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='w'})):
        (move_pawn_right (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='w'})) :
        ((move_pawn_left (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='w'})) : result)
  |null spcs /= True && elem player "b" =  (move_pawn_forward (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='b'})):
        (move_pawn_right (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='b'})) :
        ((move_pawn_left (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='b'})) : result)
  |null spcs /= True && elem player "W" =         (move_flag_forward (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='W'})):
        (move_flag_right (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='W'})) :
        ((move_flag_left (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='W'})) : result)
  |null spcs /= True && elem player "B" =     (move_flag_forward (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='B'})):
    (move_flag_right (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='B'})):
    ((move_flag_left (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant='B'})) : result)
  |null spcs /= True && elem player "-" = generate_a_tree (Board {spaces = (tail spcs), width=w}) (head spcs) player result
  |null spcs = result
  |otherwise = error "Something happened in generate_a_tree"

-- use filter to find the appropriate spaces in the board
-- is there a space that has xxx position values?
move_flag_left :: Board -> Space -> [Board]
move_flag_left (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant=occ})
  |elem occ "W" = (legal_flag_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x+1) (y)) spcs) (Space {xpos=x, ypos=y, occupant=occ})) -- for white, moving left actually means moving right, need to check if legal move
  |elem occ "B" = (legal_flag_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x-1) (y)) spcs) (Space {xpos=x, ypos=y, occupant=occ})) -- for black, moving left just means moving left need to check if legal move
  |otherwise = error "Something happened in move flag "
move_flag_right :: Board -> Space -> [Board]
move_flag_right (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant=occ})
  |elem occ "W" = (legal_flag_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x-1) (y)) spcs) (Space {xpos=x, ypos=y, occupant=occ})) -- for white, moving right actually means moving left, need to check if legal move
  |elem occ "B" = (legal_flag_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x+1) (y)) spcs) (Space {xpos=x, ypos=y, occupant=occ})) -- for black, moving right just means moving right, need to check if legal move
  |otherwise = error "Something happened in move flag "
move_flag_forward :: Board -> Space -> [Board]
move_flag_forward (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant=occ})
  |elem occ "W" = (legal_flag_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x) (y+1)) spcs) (Space {xpos=x, ypos=y, occupant=occ})) -- for white, moving forward actually means moving down, need to check if legal move
  |elem occ "B" = (legal_flag_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x) (y-1)) spcs) (Space {xpos=x, ypos=y, occupant=occ})) -- for black, moving forward just means moving forward need to check if legal move
  |otherwise = error "Something happened in move flag "
-- replace the Space in Board.spaces that has the same position as the given Space, with the given Space
replace_space :: Board -> Space -> Board
replace_space (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant=occ}) = (Board {
        spaces=((filter (space_not_with_position (x) (y)) spcs) ++ [(Space {xpos=x, ypos=y, occupant=occ})]),
         width=w
        })

space_not_with_position :: Float -> Float -> Space -> Bool
space_not_with_position suggested_x suggested_y (Space {xpos=x, ypos=y, occupant=occ})
  |suggested_x /= x && suggested_y /= y = True
  |otherwise = False

space_with_position :: Float -> Float -> Space -> Bool
space_with_position suggested_x suggested_y (Space {xpos=x, ypos=y, occupant=occ})
  |suggested_x == x && suggested_y == y = True
  |otherwise = False

-- attempt to move Space1's occupant to Space2
legal_flag_move :: Board -> [Space] -> Space -> [Board]
legal_flag_move board spaces (Space {xpos=x2, ypos=y2, occupant=occ2})
  |null spaces = []
    -- Space2 is an empty space
  |null spaces /= True && elem occ2 "-" =   (replace_space (replace_space board (head spaces)) (Space {xpos=x2, ypos=y2, occupant=(occupant (head spaces))})):[]
  -- move isn't legal
  |otherwise = error "Something happened in legal flag "


move_pawn_left :: Board -> Space -> [Board]
move_pawn_left (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant=occ})
  |elem occ "w" = (legal_pawn_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x+1) (y)) spcs) (Space {xpos=x, ypos=y, occupant=occ}) "left") -- for white, moving left actually means moving right, need to check if legal move
  |elem occ "b" = (legal_pawn_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x-1) (y)) spcs) (Space {xpos=x, ypos=y, occupant=occ}) "left") -- for black, moving left just means moving left need to check if legal move
  |otherwise = error "Something happened in move pawn "

move_pawn_right :: Board -> Space -> [Board]
move_pawn_right (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant=occ})
  |elem occ "W" = (legal_pawn_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x-1) (y)) spcs) (Space {xpos=x, ypos=y, occupant=occ}) "right") -- for white, moving right actually means moving left, need to check if legal move
  |elem occ "B" = (legal_pawn_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x+1) (y)) spcs) (Space {xpos=x, ypos=y, occupant=occ}) "right") -- for black, moving right just means moving right, need to check if legal move
  |otherwise = error "Something happened in move pawn "

move_pawn_forward :: Board -> Space -> [Board]
move_pawn_forward (Board {spaces=spcs, width=w}) (Space {xpos=x, ypos=y, occupant=occ})
  |elem occ "W" = (legal_pawn_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x) (y+1)) spcs) (Space {xpos=x, ypos=y, occupant=occ}) "forward") -- for white, moving forward actually means moving down, need to check if legal move
  |elem occ "B" = (legal_pawn_move (Board {spaces=spcs, width=w}) (filter (space_with_position (x) (y+1)) spcs) (Space {xpos=x, ypos=y, occupant=occ}) "forward") -- for black, moving forward just means moving forward need to check if legal move
  |otherwise = error "Something happened in move pawn "

legal_pawn_move :: Board -> [Space] -> Space -> String -> [Board]
legal_pawn_move board spaces (Space {xpos=x2, ypos=y2, occupant=occ2}) direction
    -- Space2 is an empty space
  |null spaces = []
  |elem occ2 "-" =   (replace_space (replace_space board (Space {xpos=(xpos (head spaces)), ypos=(ypos (head spaces)), occupant='-'})) (Space {xpos=x2, ypos=y2, occupant=(occupant (head spaces))})):[]
  |elem occ2 "wW" && elem (occupant (head spaces)) "b" && "left" == direction = legal_pawn_move board ((Space {xpos=(xpos (head spaces)), ypos=(ypos (head spaces)), occupant=(occupant (head spaces))}):[]) (Space {xpos=(x2+1), ypos=y2, occupant=occ2}) "none"
  |elem occ2 "bB" && elem (occupant (head spaces)) "w" && "left" == direction = legal_pawn_move board ((Space {xpos=(xpos (head spaces)), ypos=(ypos (head spaces)), occupant=(occupant (head spaces))}):[])(Space {xpos=(x2-1), ypos=y2, occupant=occ2}) "none"
  |elem occ2 "wW" && elem (occupant (head spaces)) "b" && "right" == direction = legal_pawn_move board ((Space {xpos=(xpos (head spaces)), ypos=(ypos (head spaces)), occupant=(occupant (head spaces))}):[]) (Space {xpos=(x2-1), ypos=y2, occupant=occ2}) "none"
  |elem occ2 "bB" && elem (occupant (head spaces)) "w" && "right" == direction = legal_pawn_move board ((Space {xpos=(xpos (head spaces)), ypos=(ypos (head spaces)), occupant=(occupant (head spaces))}):[])(Space {xpos=(x2+1), ypos=y2, occupant=occ2}) "none"
  |elem occ2 "wW" && elem (occupant (head spaces)) "b" && "forward" == direction = legal_pawn_move board ((Space {xpos=(xpos (head spaces)), ypos=(ypos (head spaces)), occupant=(occupant (head spaces))}):[]) (Space {xpos=x2, ypos=(y2 + 1), occupant=occ2}) "none"
  |elem occ2 "bB" && elem (occupant (head spaces)) "w" && "forward" == direction = legal_pawn_move board ((Space {xpos=(xpos (head spaces)), ypos=(ypos (head spaces)), occupant=(occupant (head spaces))}):[]) (Space {xpos=x2, ypos=(y2 - 1), occupant=occ2}) "none"
  |direction == "none" = []
  |elem occ2 "wW" && elem (occupant (head spaces)) "w" = []
  |elem occ2 "bB" && elem (occupant (head spaces)) "b" = []
  -- move isn't legal
  |otherwise = error "Something happened in legal pawn"


-- Description: Given a list of AdversarialTrees, determines what Board represents the best move for the player
-- Expects:
    -- A list of AdversarialTrees representing the different possible results of the next few moves
-- Returns:
    -- A Board

{- For each space, try moving it's occupent in valid ways -}
-- generate_move :: Board -> Char -> Float -> Board

{-
    Description: Evaluates leaf state then propagates values up and picks best move
    Expects: 
        -- Tree of Moves (List of Lists of each path in tree)
        -- Board and History
        -- List Parents
        -- Player Character 'w' or 'b'
        -- Current Player (determines if row is min or max)
    Returns:
        -- next move
-}

-- choose_move :: [[String]] -> [String] -> [String] -> Char -> Char -> String
-- choose_move tree board_history parents_list player curr_player 
--   | -- go through each list and evalusate and propagate up
--   | 
--   |
{-
    Description: Evaluates board with static evaluation
        Strategy:
            1) Number of pawns surrounding player's flag (how vulnerable is flag + counts how many pawns you have) +numPawns * 5
            2) Player wins game +50
            3) Player loses game -50
    Expects: 
        -- Board to be evaluated
        -- Player Character 'w' or 'b'
    Returns:
        -- Evaluation Number

-}


static_eval :: [Char] -> Char -> Int
static_eval board player 
    | player == 'w' && length (filter (\c -> c == 'B') board) == 0  = 50
    | player == 'w' && length (filter (\c -> c == 'W') board) == 0  = -50
    | player == 'b' && length (filter (\c -> c == 'W') board) == 0  = 50
    | player == 'b' && length (filter (\c -> c == 'B') board) == 0  = -50
    | otherwise                                               = 5 * pawns_near_flag board player (floor (sqrt (fromIntegral (length board)))) (find_flag_index board player) 0


{-
    Description: Counts nearby pawns near flag 
    Expects: 
        -- Board to be evaluated
        -- Player 'w' or 'b'
        -- width of board
        -- Index Where Flag is 
        -- current index
    Returns:
        -- number of pawns
-}

pawns_near_flag :: [Char] -> Char -> Int -> Int -> Int -> Int
pawns_near_flag board player width flag_index curr_index
    | board == []                                                                                                                = 0
    | curr_index == flag_index - 1 && abs (mod curr_index width - mod flag_index width) <= 1 && (head board) == player           = 1 + pawns_near_flag (tail board) player width flag_index (curr_index + 1) 
    | curr_index == flag_index + 1 && abs (mod curr_index width - mod flag_index width) <= 1 && (head board) == player           = 1 + pawns_near_flag (tail board) player width flag_index (curr_index + 1) 
    | curr_index == flag_index + width - 1 && abs (mod curr_index width - mod flag_index width) <= 1 && (head board) == player   = 1 + pawns_near_flag (tail board) player width flag_index (curr_index + 1) 
    | curr_index == flag_index + width && abs (mod curr_index width - mod flag_index width) <= 1 && (head board) == player       = 1 + pawns_near_flag (tail board) player width flag_index (curr_index + 1) 
    | curr_index == flag_index + width + 1 && abs (mod curr_index width - mod flag_index width) <= 1 && (head board) == player   = 1 + pawns_near_flag (tail board) player width flag_index (curr_index + 1) 
    | otherwise                                                                                                                  = 0 + pawns_near_flag (tail board) player width flag_index (curr_index + 1)

{-
    Description: Returns index of where player flag is 
    Expects: 
        -- Board to be evaluated
        -- Player 'w' or 'b'
    Returns:
        -- Index
-}
find_flag_index :: [Char] -> Char -> Int
find_flag_index board player
    | player == 'w' && (head board) == 'W'    = 0
    | player == 'b' && (head board) == 'B'    = 0
    | otherwise               = 1 + find_flag_index (tail board) player
