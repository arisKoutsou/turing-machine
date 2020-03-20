import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

-- The state space of the Turing Machine.
data State = State Int | Accept | Reject | Output deriving (Show, Eq)

-- The tape alphabet of the Turing Machine.
data Symbol = Symbol Char | Start | Null deriving (Eq)

instance Show Symbol where
    show (Symbol x) = show x
    show Null  = "_"
    show Start = "->"

data Tape = Tape { left :: [Symbol]
                 , current :: Symbol
                 , right :: [Symbol]
                 }

instance Show Tape where
    show tape = tape_pp tape

data Move = S | L | R deriving (Show, Eq)

-- The core data type representing a Turing Machine (current state, tape and transition function)

data Machine = 
    Machine { 
        state :: State,
        tape :: Tape, 
        delta :: State->Symbol->(State, Symbol, Move)} 
    |
    MemMachine {
        state :: State, 
        tape :: Tape,
        mem_tape :: Tape,
        mem_delta :: State->Symbol->Symbol
            ->(State, Symbol, Move, Symbol, Move)}

instance Show Machine where
    show (Machine s t d) = 
        "Machine {"                    ++ 
        "\n    State: " ++   (show s)  ++ 
        "\n    Tape: "  ++   (show t)  ++ 
        "\n}"
    show (MemMachine s t m_t d) = 
        "Machine {"                    ++ 
        "\n    State: " ++   (show s)  ++ 
        "\n    Main Tape: "  ++   (show t)  ++ 
        "\n    Memory Tape: "  ++   (show m_t)  ++ 
        "\n}"


-- Utils
isNotNull Null = False 
isNotNull _    = True 

-- Pretty print function for Tape
-- Note: The 'right' list of the tape is infinite, so take care when handling it.
tape_pp (Tape left current right) =          (show $ reverse left)  
                                    ++ " >> "++ (show current) ++ " << " ++ 
                                     (show $ (takeWhile isNotNull right) ++ [Null])


data CompGraph = CompGraph { vertices :: Map Int Vertex
                           , edges :: Map Int [(Symbol->Bool, Int)]
                           }

data Vertex = CompVertex CompGraph | LM | RM | PushM | PopM | WriteM | WriteCM Symbol




-- Create a tape from a string. The string goes 
-- at the start of the tape, the rest of the tape
-- is an infinite list of nulls.
create_tape::[Char]->Tape
create_tape [] = Tape {
    left = [Start],
    current = Null,
    right = (repeat Null) 
}
create_tape (c:cs) = Tape { 
    left = [Start], 
    current = if c == '_' then Null else Symbol c, 
    right = [if x == Symbol '_' then Null 
        else x | x <- (map Symbol cs)++(repeat Null)]
} 

-- Given a tape a symbol and a move, update the tape
-- so that the symbol is written in the current position
-- and the r/w head is moved based on the move. 
-- Returns the updated tape.
update_tape::Tape->Symbol->Move->Tape
update_tape tape symbol S = Tape { 
    left = left tape, 
    current = symbol, 
    right = right tape 
}
update_tape tape symbol R = Tape {
    left = symbol:(left tape), 
    current = head (right tape), 
    right = tail (right tape)
}
update_tape tape symbol L = Tape { 
    left = tail (left tape), 
    current = head (left tape), 
    right = symbol:(right tape)
}

-- Given an initial state, a string and a transition function(delta),
-- returns a turing machine.
init_machine::State->[Char]->(State->Symbol->(State, Symbol, Move))
    ->Machine
init_machine initial_state cs delta = Machine {
    state = initial_state,
    tape = create_tape cs,
    delta = delta
}

-- Given a turing machine, a step of the machine is performed
-- and the machine is returned in it's new state.
step::Machine->Machine
step (Machine old_state old_tape old_delta) = Machine {
    state = new_state,
    tape = new_tape,
    delta = old_delta
}
    where
    current_symbol = current old_tape
    (new_state, symbol, move) = old_delta old_state current_symbol
    new_tape = update_tape old_tape symbol move
-- Given a double tape turing machine, a step of the machine is 
-- performed and the machine is returned in it's new state.
-- This is an extension of the step function in the simple TM.
step (MemMachine old_state old_main_tape old_mem_tape old_delta)
    = MemMachine {
        state = new_state,
        tape = new_main_tape,
        mem_tape = new_mem_tape,
        mem_delta = old_delta
    }
    where
    main_current_symbol = current old_main_tape
    mem_current_symbol = current old_mem_tape
    (new_state, main_symbol, main_move, mem_symbol, mem_move) 
        = old_delta old_state main_current_symbol mem_current_symbol
    new_main_tape = update_tape old_main_tape main_symbol main_move
    new_mem_tape = update_tape old_mem_tape mem_symbol mem_move

-- Given a turing machine, performs steps until it reaches a halting
-- state. If the turing machine doesn't halt this function will not return.
-- On halt returns the updated turing machine.
run::Machine->Machine
run machine = 
    if s == Accept || s == Reject 
    then machine else run (step machine)
    where s = state machine


-- Some transition functions for the turing machine, that define
-- simple languages. If the turing machine ends up in the 'Accept'
-- state, then the given string is IN the language. 

-- The empty language. The turing machine should 'Reject' every string.
empty::State->Symbol->(State, Symbol, Move)
empty state symbol = (Reject, Null, S)

-- The language that contains only the empty string(e or "").
only_e::State->Symbol->(State, Symbol, Move)
only_e state symbol
    | state == (State 0) && symbol == Null = (Accept, symbol, S)
    | otherwise = (Reject, symbol, S)

-- The language described by the regular expression 'aaa'.
a3::State->Symbol->(State, Symbol, Move)
a3 state symbol 
    | state == (State 0) && symbol == Symbol 'a' = (State 1, symbol, R)
    | state == (State 1) && symbol == Symbol 'a' = (State 2, symbol, R)
    | state == (State 2) && symbol == Symbol 'a' = (State 3, symbol, R)
    | state == (State 3) && symbol == Null = (Accept, symbol, S)
    | otherwise = (Reject, symbol, S)

-- The language described by the regular expression 'ab*c'.
absc::State->Symbol->(State, Symbol, Move)
absc state symbol
    | state == (State 0) && symbol == Symbol 'a' = (State 1, symbol, R)
    | state == (State 1) && symbol == Symbol 'b' = (State 1, symbol, R)
    | state == (State 1) && symbol == Symbol 'c' = (State 2, symbol, R)
    | state == (State 2) && symbol == Null = (Accept, symbol, S)
    | otherwise = (Reject, symbol, S)

-- The language a^(2^n) where n >= 0. i.e. {'a', 'aa', 'aaaa'} subset of L
-- We replace every half of the 'a' characters of the string with 'x' in
-- each iteration (more specifically we replace every other 'a' we encounter).
-- We iterate logn times(where n is the length of the string).
-- If at the last iteration the string is of the form 'x*a' or 'x'
-- Then the initial string has 2^n a's.
po2::State->Symbol->(State, Symbol, Move)
po2 state symbol
    | state == (State 0) && symbol == Symbol 'x' = (State 0, symbol, R)
    | state == (State 0) && symbol == Symbol 'a' = (State 1, Symbol 'x', R)
    | state == (State 0) && symbol == Null = (State 3, symbol, L)
    | state == (State 1) && symbol == Symbol 'x' = (State 1, symbol, R)
    | state == (State 1) && symbol == Symbol 'a' = (State 0, Symbol 'a', R)
    | state == (State 1) && symbol == Null = (State 3, symbol, L)

    | state == (State 3) && symbol == Start = (State 4, symbol, R)
    | state == (State 3) = (State 3, symbol, L)

    | state == (State 4) && symbol == Symbol 'x' = (State 2, symbol, R)
    | state == (State 2) && symbol == Null = (Accept, symbol, S)
    | state == (State 2) && symbol == Symbol 'x' = (State 7, symbol, R)
    | state == (State 7) && symbol == Symbol 'x' = (State 7, symbol, R)
    | state == (State 7) && symbol == Symbol 'a' = (State 5, symbol, R)
    | state == (State 2) && symbol == Symbol 'a' = (State 5, symbol, R)
    | state == (State 5) && symbol == Null = (Accept, symbol, S)
    | state == (State 5) = (State 6, symbol, S)

    | state == (State 6) && symbol == Start = (State 0, symbol, R)
    | state == (State 6) = (State 6, symbol, L)

    | otherwise = (Reject, symbol, S)

-- Let's define the machine that accepts the concatenation of 2 languages.
-- For example if L1 = 'a' and L2 = 'ab*c' then the string 'aac' exists in
-- the concatenation of the two languages.
conc::[Char]
    ->(State->Symbol->(State, Symbol, Move))
    ->(State->Symbol->(State, Symbol, Move))
    ->Maybe ([Char], [Char])
conc cs delta1 delta2 = 
    _conc [] cs delta1 delta2
    where
    _conc prefix postfix delta1 delta2 =
        if m1_result == Accept && m2_result == Accept
        then Just (prefix, postfix) 
        else if postfix == [] then Nothing 
            else _conc (prefix ++ [head postfix]) (tail postfix) delta1 delta2
        where
            m1 = init_machine (State 0) prefix delta1
            m2 = init_machine (State 0) postfix delta2
            m1_result = state (run m1)
            m2_result = state (run m2)


-- Enumerator Turing Machine

-- This is similar to 'run'. In this case though, when
-- the contents of the tape exist in the language defined
-- by machine.delta, we append them in the result list.
enum::Machine->[[Symbol]]
enum machine
    | m_state == Output = tape_string:(enum (step machine))
    | m_state == Accept || m_state == Reject = []
    | otherwise = enum (step machine)
    where
        m_state = state machine
        m_tape = tape machine
        tape_string = filter (/=Null) (
            (tail (reverse (left m_tape))) ++ 
            [current m_tape] ++ 
            (takeWhile (/=Null) (right m_tape)))

-- An enumarator turing machine that enumerates the strings of
-- the language L = '1*'. The turing machine must be initialized
-- with state Output and the empty string.
unary::State->Symbol->(State, Symbol, Move)
unary state symbol
    | state == Output && symbol == Null = (Output, Symbol '1', R)
    | otherwise = (Reject, symbol, S)

-- Enumerator for the binary numbers. Each binary number is
-- generated in reverse for ease. For example 2 = 10(binary)
-- will be printed 01. In order to implement this enumerator
-- we iterate the string from left to right, if we encounter 
-- a 0 we change it to 1, otherwise we shift right. When the 
-- overwrite of the 0 found is done we overwrite all preceeding 
-- 1's with 0's.
binary::State->Symbol->(State, Symbol, Move)
binary state symbol
    | state == State 0 && symbol == Null = (Output, Symbol '0', S)
    | state == Output = (State 1, symbol, S)
    | state == State 1 && symbol == Symbol '0' || symbol == Null 
        = (State 2, Symbol '1', L)
    | state == State 1 && symbol == Symbol '1' = (State 1, symbol, R)
    | state == State 2 && symbol == Start = (Output, symbol, R)
    | state == State 2 = (State 2, Symbol '0', L)
    | otherwise = (Reject, symbol, S)


-- Turing machine with 2 tapes and 2 heads.

init_mem_machine::State
    ->[Char]
    ->(State->Symbol->Symbol
        ->(State, Symbol, Move, Symbol, Move))
    ->Machine
init_mem_machine initial_state cs delta = MemMachine {
    state = initial_state,
    tape = create_tape cs,
    mem_tape = create_tape "",
    mem_delta = delta
}

-- A test delta to check if the double tape machine works.
m_as (State 0) (Symbol 'a') m = (State 0, Symbol 'a', R, m, S)
m_as (State 0) Null m = (Accept, Null, R, m, S)
m_as (State 0) x m = (Reject, x, S, m, S)


-- Representing complex Turing Machines with a graph.

-- Transition function for the turing machine that just
-- moves the head of the main tape left.
left_::State->Symbol->Symbol
    ->(State, Symbol, Move, Symbol, Move)
left_ _ c m = (Accept, c, L, m, S)

-- Transition function for the turing machine that just
-- moves the head of the main tape right.
right_::State->Symbol->Symbol
    ->(State, Symbol, Move, Symbol, Move)
right_ _ c m = (Accept, c, R, m, S)

-- Advances the head of the memory tape and writes the 
-- current symbol of the main tape at that position.
push_::State->Symbol->Symbol
    ->(State, Symbol, Move, Symbol, Move)
push_ (State 0) c m = (State 1, c, S, m, R)
push_ (State 1) c m = (Accept, c, S, c, S)

-- Writes the current symbol(top of the stack) of the memory
-- tape to the current position of the main tape.
write_::State->Symbol->Symbol
    ->(State, Symbol, Move, Symbol, Move)
write_ _ c m = (Accept, m, S, c, S)

-- Writes null at the current symbol of the memory tape(stack top)
-- and moves the head of that tape to the left.
pop_::State->Symbol->Symbol
    ->(State, Symbol, Move, Symbol, Move)
pop_ (State 0) c _ = (State 1, c, S, Null, L)
pop_ (State 1) c m = (Accept, c, S, m, S)

-- Returns a delta function that writes a symbol at the current
-- position of the main tape. For example 'writec_ Symbol 'a''
-- returns a delta that writes 'a' at the current position.
writec_::Symbol->State->Symbol->Symbol
    ->(State, Symbol, Move, Symbol, Move)
writec_ w _ _ m = (Accept, w, S, m, S)

-- Maps machines to their delta functions we defined above.
delta_from_vertex v = case v of
    LM -> left_
    RM -> right_
    PushM -> push_
    WriteM -> write_
    PopM -> pop_
    WriteCM s -> writec_ s

-- Performs a step of the graph. The vertice may be complex.
-- So we traverse the node graph recursivelly.
graph_step::Vertex->Tape->Tape->(Tape, Tape)
graph_step (CompVertex graph) main_tape memory_tape 
    = traverse_graph graph main_tape memory_tape 0
graph_step vertex main_tape memory_tape = (tape m, mem_tape m)
    where
        m = run (MemMachine (State 0) main_tape memory_tape 
            (delta_from_vertex vertex))

-- Utils...
headMaybe::[t]->Maybe t
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

-- Traverses the graph given a starting vertex index.
traverse_graph::CompGraph->Tape->Tape->Int->(Tape, Tape)
traverse_graph graph t1 t2 v_index
    | neighbor_maybe == Nothing = (t1', t2')
    | otherwise = traverse_graph graph t1' t2' neighbor
    where 
        v = vertices graph Map.! v_index
        (t1', t2') = graph_step v t1 t2
        current_symbol = current t1'
        neighbor_maybe = headMaybe (map snd (
            filter (\x -> fst x current_symbol) 
                (if Map.member v_index (edges graph) then
                     edges graph Map.! v_index else [])))
        (Just neighbor) = neighbor_maybe
        

-- Wrapper for traverse_graph.
interpret::[(Int, Vertex)]->[(Int, Symbol->Bool, Int)]->[Char]
    ->(Tape, Tape)
interpret vs es cs 
    = traverse_graph graph main_tape memory_tape 0
    where
        graph_edges = Map.fromListWith (++) 
            [(a, [(b,c)]) | (a, b, c) <- es]
        graph_vertices = Map.fromList vs
        graph = CompGraph graph_vertices graph_edges
        main_tape = create_tape cs
        memory_tape = create_tape ""

-- Wrapper for traverse_graph.
interpret_graph::CompGraph->[Char]->(Tape, Tape)
interpret_graph graph cs =
    traverse_graph graph main_tape memory_tape 0
    where
        main_tape = create_tape cs
        memory_tape = create_tape ""

-- Creates a graph with only one vertex.
create_singleton_graph::Vertex->CompGraph
create_singleton_graph vertex = 
    CompGraph (Map.fromList [(0, vertex)]) (Map.fromList [])

-- Creates a chain graph, essentially a list.
create_chain_graph::[Vertex]->CompGraph
create_chain_graph vertices =
    CompGraph 
    (Map.fromList [(i, vertices !! i) | 
        i<-[0..((length vertices) - 1)]])
    (Map.fromList [(i, [((\x -> True), i+1)]) | 
        i <- [0..((length vertices) - 2)]])

-- Implements the turing machine that goes left until a 
-- specific symbol is encountered.
l_until::Symbol->Vertex
l_until symbol = CompVertex (CompGraph
    (Map.fromList [(0, LM)])
    (Map.fromList [(0, [(\x -> x /= symbol, 0)])]))

-- Implements the turing machine that goes right until a 
-- specific symbol is encountered.
r_until::Symbol->Vertex
r_until symbol = CompVertex (CompGraph
    (Map.fromList [(0, RM)])
    (Map.fromList [(0, [(\x -> x /= symbol, 0)])]))

-- Graph for the machine that shifts the input right.
-- Here the memory tape is used, in order to remeber the
-- the last character.
shift::Vertex
shift = CompVertex (CompGraph
    (Map.fromList [(0, v0), (1, v1), (2, v2), (3, v3)])
    (Map.fromList graph_edges))
    where
        v0 = r_until Null
        v1 = LM
        v2 = CompVertex (create_chain_graph [
            PushM,
            WriteCM Null,
            r_until Null,
            WriteM,
            PopM,
            l_until Null])
        v3 = RM
        graph_edges = [
            (0, [(\x -> True, 1)]),
            (1, [(\x -> x /= Null, 2), (\x -> x == Null, 3)]),
            (2, [(\x -> True, 1)])]

-- Graph for the turing machine that copies the input
-- and pastes it at the end of the tape.
-- Here the memory tape is used similarly to the shift machine.
copy::Vertex
copy = CompVertex (CompGraph
    (Map.fromList [(0, v0), (1, v1), (2, v2)])
    (Map.fromList graph_edges))
    where
        v0 = RM
        v1 = CompVertex (create_chain_graph [
            PushM,
            WriteCM Null,
            r_until Null, r_until Null,
            WriteM,
            PopM,
            PushM,
            l_until Null, l_until Null,
            WriteM,
            PopM])
        v2 = r_until Null
        graph_edges = [
            (0, [(\x -> x /= Null, 1), (\x -> x == Null, 2)]),
            (1, [(\x -> True, 0)])]

-- Graph for the machine that writes 2 Nulls on the tape
-- followed by 2 copies of the input string.
ww::Vertex
ww = CompVertex (create_chain_graph 
    [copy, l_until Null, l_until Null, shift])
    

