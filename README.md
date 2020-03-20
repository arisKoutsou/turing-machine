# turing-machine
Functional Implementation of Turing Machines using the Haskell programming language.


## Turing machine
A Turing machine is a mathematical model of computation.
Any program can be expressed by a turing machine.

A Turing machine is defined by 3 things:
* The state of the machine (can be expressed as an integer)
* A tape containing symbols
* A transition function 


## State
The states of the machine are crutial to understanding what the machine does.
For example a state can denote that the machince is at it's entry point,
or that it's skiping all blank symbols, or that it has read an even number of symbols etc.

## Tape
The tape is a list of symbols starting with a special start symbol let's say '>'.
This list may extend to the right infinatelly like so:

```
['>', 'a', 'b', 'c', '_', '_', '_', '_', ... ]
            ^
        current position
```

The trailing empty symbols are considered as 'nulls'.
This concept of an infinite list can be expressed beatifully in haskell due to lazy evaluation.
We must note that the tape always has a 'current' position, like a head reading a disk.


## Transition function
The idea of the transition function is that we need a way to 'jump' between states in order to
perform our computation. The mathematical model that achieves this is the transition function.
For example let's say that we are in state 0 and want to go in state 1 when we encounter the symbol 'a'.
Also before doing that transition we want to write the symbol 'b' at the current position of the tape and shift the current position right by one.
Then we write:

```
delta(State 0, Symbol 'a') = (State 1, Symbol 'b', R) // R here means Right.
```
We can see here that any transition function 'delta' takes 2 arguments(State and Symbol) and outputs 3 things(NextState, SymbolToWrite and Move).
In Haskell notation any transition function has the following type:

```
delta :: State->Symbol->(State, Symbol, Move)
```

## Running the machine
In order to do actual computation with this model, we need to define two special states called 'Accept' and 'Reject'. These are the final states of a series of 'jumps' between states and signal the termination of the computation.
A simple machince that reads the string "ab" and terminates is the following:
```
      a         b
-> [S0] ---> [S1] ---> [Accept]
     |         |   
     -----------------> [Reject]
        else
```

To run this example with the existing Haskell code one must define the transition function
and then evaluate this expression:
```
-? run (init_machine (State 0) "ab" delta_ab)
```

## Making things more interesting
Although defining delta's for a program is fun in the begining, when programs get slightly complicated like copying a string on the tape or shifting it by one symbol, writing the transition function may be quite hard. 
To address this complication we define a new Turing machine that has another tape (a memory tape). We also define simple turing machines that do simple jobs like shifting, writing, reading from/to the memory tape and we link them together by traversing a graph(we feed the result of one machine as the input for another).
Finally we are able to do complicated stuff like the following, without writing complicated delta functions:
```
-? interpret_graph (create_singleton_graph copy) "_bb"
```
This will copy the string "_bb" and the output will be "_bb_bb_".

