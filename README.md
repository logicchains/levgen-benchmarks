For best performance: 
The Rust benchmark is compiled with --opt-level=3, 
DMD with -release -O -noboundscheck, 
GHC with -O3 -funbox-strict-fields
and the rest with -O3. 

D and Go take the random seed as a command line paramater in the form "-v=int", the rest take it just as plain "int". Be warned, some will segfault without it.
