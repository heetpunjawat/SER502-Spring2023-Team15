main(InputFileName) :- nl,
    format([bold,fg(yellow)], 'Initiating the program', []), nl,
    file_reader(InputFileName, File_Data),
    program(ParsedTree, File_Data, []),
