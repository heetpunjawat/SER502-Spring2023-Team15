% Define the name of the module and the predicate it exports

:- module(file_reader, [file_read/2]).

% Read a file and convert its contents to a list of atoms or numbers
file_read(File, Data) :-
    open(File, read, Stream),               % Open the file for reading and create a Stream object
    read_file_lines(Stream, Lines),         % Read the entire stream into a list of character codes
    convert_lines(Lines, Data), !,          % Convert the list of character codes to a list of atoms or numbers
    close(Stream).                          % Close the file stream

% Read a stream of characters and convert each line to a list of atoms
read_file_lines(Stream, [Chars | List]) :-
    \+ end_of_file(Stream),                                     % Check if the end of the stream has been reached
    read_line_to_characters(Stream, CurrentLineCodes),          
    atom(Chars, CurrentLineCodes),
    read_file_lines(Stream, List), !.                           % Recursively read the rest of the lines in the stream

% Base case: end of the stream has been reached
read_file_lines(Stream, []) :- end_of_file(Stream).

% Convert a list of atoms to a list of numbers if possible
convert_lines([Atom|RestAtoms], [Number|RestNumbers]) :-
    atom_number(Atom, Number),                                      % Check if the current element can be converted to a number
    convert_lines(RestAtoms, RestNumbers).                          % Recursively convert the rest of the list
convert_lines([Atom|RestAtoms], [Atom|RestAtomsConverted]) :-
    atom(Atom),
    convert_lines(RestAtoms, RestAtomsConverted).
convert_lines([], []). 
