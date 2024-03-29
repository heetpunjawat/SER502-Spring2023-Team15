/*
Author: Shivanjay Wagh
Purpose: Main file for the program
*/

:- use_module(tokenReader).
:- use_module(parser).
:- use_module(evaluator).

main(Filename) :- nl,
    ansi_format([bold,fg(yellow)], 'Starting Parser', []), nl,
    read_file(Filename, FileData),
    program(ParseTree, FileData, []),
    write("Generating Parse Tree: "), successful_flag, nl,
    write(ParseTree), nl,
    ansi_format([bold,fg(yellow)], 'Starting Evaluation', []), nl,
    eval_program(ParseTree, NewEnv), nl,
    ansi_format([bold,fg(yellow)], 'Environment after evaluation', []), nl,
    write(NewEnv), nl,
    halt.

successful_flag :- ansi_format([bold,fg(green)], 'SUCCESSFUL', []).