:- module(file_reader, [file_read/2]).

file_read(File, Data) :-
    open(File, read, Stream),
    read_file_lines(Stream, Lines),
    convert_lines(Lines, Data), !,
    close(Stream).

read_file_lines(Stream, [Chars | List]) :-
    \+ end_of_file(Stream),
    read_line_to_characters(Stream, CurrentLineCodes),
    atom(Chars, CurrentLineCodes),
    read_file_lines(Stream, List), !.

read_file_lines(Stream, []) :- end_of_file(Stream).

convert_lines([Atom|RestAtoms], [Number|RestNumbers]) :-
    atom_number(Atom, Number),
    convert_lines(RestAtoms, RestNumbers).
convert_lines([Atom|RestAtoms], [Atom|RestAtomsConverted]) :-
    atom(Atom),
    convert_lines(RestAtoms, RestAtomsConverted).
convert_lines([], []). 
