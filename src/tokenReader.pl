/* Define the name of the module and the predicate it exports */
:- module(read_file, [read_file/2]).

/* Read a file and convert its contents to a list of atoms or numbers */
read_file(FileName, ConvertedData) :-
    /* Open the file for reading and create a Stream object */
    open(FileName, read, Stream), 
    /* Read the entire stream into a list of character codes */    
    read_stream(Stream, FileData),
    /* Convert the list of character codes to a list of atoms or numbers */
    convert(FileData, ConvertedData), !,
    /* Close the file stream */
    close(Stream).

/* Read a stream of characters and convert each line to a list of atoms */
read_stream(Stream, [CurrentLineCharacters | List]) :-
    /* Check if the end of the stream has been reached */
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_codes(CurrentLineCharacters, Codes),
    /* Recursively read the rest of the lines in the stream */
    read_stream(Stream, List), !.

/* Base case: end of the stream has been reached */
read_stream(Stream, []) :- at_end_of_stream(Stream).

/* Convert a list of atoms to a list of numbers if possible */
/* Check if the current element can be converted to a number */ 
convert([H|T], [N|R]) :- atom_number(H, N), convert(T, R).
/* Recursively convert the rest of the list */
convert([H|T], [H|R]) :- atom(H), convert(T, R).
convert([], []).
