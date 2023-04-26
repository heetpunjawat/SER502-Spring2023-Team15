/* Start Symbol */
program(p_program(P)) --> statement_list(P).
program --> statement_list.

/* Blocks */
block(t_block(StatementList)) --> ['{'], statement_list(StatementList), ['}'].

/* Statements List  */
statement_list(t_statement_list(Statement, StatementList)) -->
    statement(Statement),
    statement_list(StatementList).

statement_list(t_statement(Statement)) -->
    statement(Statement).

/* Statements */
statement(S) -->
    print_statement(S) |
    assign_statement(S) |
    var_decl_statement(S) |
    if_statement(S) |
    for_loop_statement(S) | 
    for_enhanced_statement(S) |
    while_loop_statement(S).
