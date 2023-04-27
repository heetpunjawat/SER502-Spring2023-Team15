:- module(program, [program/3]).
:- table expr_level_1/3, expr_level_2/3, expr_level_3/3.

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

/* Variable declaration statements */
var_decl_statement(t_var_decl_statement(Type, Name)) -->
    var_type(Type),
    var_name(Name),
    end_of_statement(_).
var_decl_statement(t_var_decl_statement(Type, Name, Expression)) -->
    var_type(Type),
    var_name(Name),
    assignment_operator(_),
    expression(Expression),
    end_of_statement(_).

/* Assignment statement */
assign_statement(Expression) --> assignment_expression(Expression), end_of_statement(_).

/* For loop statement */
for_loop_statement(t_for_loop_statement(Assignment, Condition, VarChangePart, Block)) -->
    [for],
    ['('],
    assignment_expression(Assignment), [;],
    condition(Condition), [;],
    var_change_part(VarChangePart),
    [')'],
    block(Block).

/* Variable change statememnts */
var_change_part(Expression) --> inc_expression(Expression) | dec_expression(Expression).
var_change_part(Expression) --> assignment_expression(Expression).

/* While loop statement */
while_loop_statement(t_while_statement(Condition, Block)) -->
    [while],
    ['('],
    condition(Condition),
    [')'],
    block(Block).

/* For enhanced statement */
for_enhanced_statement(t_for_enhanced_statement(Variable, Expression1, Expression2, Block)) -->
    [for],
    var_name(Variable),
    [in],
    [range],
    ['('],
    expression(Expression1),
    [; ],
    expression(Expression2),
    [')'],
    block(Block).

/* Comaparing two expressions with operators */
condition(t_condition(Expression1, Comp_Operator, Expression2)) -->
    expression(Expression1),
    comp_operator(Comp_Operator),
    expression(Expression2).

/* EXPRESSIONS (HIGHER THE LEVEL OF EXPRESSION, HIGHER THE PRECEDENCE OF OPERATOR) */
expression(t_expression(Expression)) --> expr_level_1(Expression).

expr_level_1(t_add(X, Y)) --> expr_level_1(X), [+], expr_level_2(Y).
expr_level_1(t_sub(X, Y)) --> expr_level_1(X), [-], expr_level_2(Y).
expr_level_1(X) --> expr_level_2(X).

expr_level_2(t_multiply(X, Y)) --> expr_level_2(X), [*], expr_level_3(Y).
expr_level_2(t_divide(X, Y)) --> expr_level_2(X), [/], expr_level_3(Y).
expr_level_2(t_bool_expression(X, Operator, Y)) --> expression(X), boolean_operator(Operator), expression(Y).
expr_level_2(X) --> expr_level_3(X).

expr_level_3(X) --> ['('], expression(X), [')'].
expr_level_3(X) -->
    ternary_expression(X) |
    var_name(X) |
    value(X).

ternary_expression(t_ternary_expression(Condition, TrueExpression, FalseExpression)) -->
    ['('],
    condition(Condition),
    ['?'],
    expression(TrueExpression),
    [':'],
    expression(FalseExpression),
    [')'].

assignment_expression(t_assignment_expression(Name, Expression)) -->
    var_name(Name),
    assignment_operator(_),
    expression(Expression).

value(Variable) -->
    integer_val(Variable) |
    float_val(Variable) |
    string_val(Variable) |
    boolean_val(Variable).

dec_expression(t_after_decrement(Variable)) --> var_name(Variable), [--].
dec_expression(t_before_decrement(Variable)) --> [--], var_name(Variable).
inc_expression(t_after_increment(Variable)) --> var_name(Variable), [++].
inc_expression(t_before_increment(Variable)) --> [++], var_name(Variable).    