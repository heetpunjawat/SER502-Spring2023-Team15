/* Start Symbol */
program(t_program(P)) --> statement_list(P).


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


/* If, Elif, Else Statements */
if_statement(t_if_statement(IfLoop, ElifLoop, ElseLoop)) -->
    if_clause(IfLoop),
    elif_clause(ElifLoop),
    else_clause(ElseLoop).

if_statement(t_if_statement(IfLoop, ElseLoop)) -->
    if_clause(IfLoop),
    else_clause(ElseLoop).

if_statement(t_if_statement(IfLoop)) -->
    if_clause(IfLoop).

/* If, Elif, Else Clauses */
if_clause(t_if(Condition, Block)) -->
    [if],
    ['('],
    condition(Condition),
    [')'],
    block(Block).

elif_clause(t_elif(Condition, Block)) -->
    [elif],
    ['('],
    condition(Condition),
    [')'],
    block(Block).

elif_clause(t_elif(Condition, Block, Elifclause)) -->
    [elif],
    ['('],
    condition(Condition),
    [')'],
    block(Block),
    elif_clause(Elifclause).

else_clause(t_else(Block)) -->
    [else],
    block(Block).


/* Print statements */
print_statement(t_print_str(Value)) --> [print_str], ['('], string_val(t_string(Value)), [')'], end_of_statement(_).

print_statement(t_print_str(Value)) --> [print_str], ['('], var_name(t_var_name(Value)), [')'], end_of_statement(_).

print_statement(t_print_expr(Expression)) --> [print_expr], ['('], expression(Expression), [')'], end_of_statement(_).
