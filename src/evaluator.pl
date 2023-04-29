/* Evaluation*/

:- module(eval_program, [eval_program/2]).

eval_program(t_program(P), NewEnv) :- evaluator_comm_list(P, [], NewEnv).

evaluator_comm_list(t_statement_list(Statement, StatementList), Env, NewEnv) :-
    evaluator_comm(Statement, Env, E1),
    evaluator_comm_list(StatementList, E1, NewEnv).
evaluator_comm_list(t_statement(Statement), Env, NewEnv) :-
    evaluator_comm(Statement, Env, NewEnv).

eval_block(t_block(StatementList), Env, NewEnv) :- evaluator_comm_list(StatementList, Env, NewEnv).

evaluator_comm(t_assignment_expression(t_var_name(Name), Expression), Env, NewEnv) :-
    evaluator_expr(Expression, Env, R1),
    change_comm(Name, R1, Env, NewEnv).
evaluator_comm(t_var_decl_statement(Type, t_var_name(Name), Expression), Env, NewEnv) :-
    evaluator_var_type(Type, Env, R1),
    evaluator_expr(Expression, Env, R2),
    change_comm(R1, Name, R2, Env, NewEnv).

evaluator_comm(t_var_decl_statement(Type, t_var_name(Name)), Env, NewEnv) :- evaluator_var_type(Type, Env, R1), R1 = int,  change_comm(R1, Name, 0, Env, NewEnv).
evaluator_comm(t_var_decl_statement(Type, t_var_name(Name)), Env, NewEnv) :- evaluator_var_type(Type, Env, R1), R1 = float,  change_comm(R1, Name, 0.0, Env, NewEnv).
evaluator_comm(t_var_decl_statement(Type, t_var_name(Name)), Env, NewEnv) :- evaluator_var_type(Type, Env, R1), R1 = string,  change_comm(R1, Name, "", Env, NewEnv).
evaluator_comm(t_var_decl_statement(Type, t_var_name(Name)), Env, NewEnv) :- evaluator_var_type(Type, Env, R1), R1 = bool,  change_comm(R1, Name, false, Env, NewEnv).

evaluator_comm(t_print_expr(Expression), Env, Env) :- evaluator_expr(Expression, Env, Result), write(Result), nl.
evaluator_comm(t_print_str(String), Env, Env) :- write(String), nl.

evaluator_comm(t_for_loop_statement(Assignment, Condition, Variable_Change, Block), Env, NewEnv) :-
    evaluator_comm(Assignment, Env, E1),
    evaluator_for(Condition, Variable_Change, Block, E1, NewEnv).

evaluator_comm(t_while_statement(C, B), Env, NewEnv) :-
    evaluator_condition(C, Env, true),
    eval_block(B, Env, E1),
    evaluator_comm(t_while_statement(C, B), E1, NewEnv).
evaluator_comm(t_while_statement(C, _), Env, _) :-
    evaluator_condition(C, Env, false).

evaluator_comm(t_for_enhanced_statement(Variable, Expression1, Expression2, Block), Env, NewEnv) :-
    evaluator_comm(t_assignment_expression(Variable, Expression1), Env, E1),
    evaluator_condition(t_condition(Expression1, t_comp_operator(>), Expression2), E1, false),
    evaluator_for(t_condition(Variable, t_comp_operator(=<), Expression2), t_before_increment(Variable), Block, E1, NewEnv).

evaluator_comm(t_for_enhanced_statement(Variable, Expression1, Expression2, Block), Env, NewEnv) :-
    evaluator_comm(t_assignment_expression(Variable, Expression1), Env, E1),
    evaluator_condition(t_condition(Expression1, t_comp_operator(<), Expression2), E1, false),
    evaluator_for(t_condition(Variable, t_comp_operator(>=), Expression2), t_before_decrement(Variable), Block, E1, NewEnv).

evaluator_comm(t_if_statement(IfLoop), Env, NewEnv) :- evaluator_if(IfLoop, Env, NewEnv, _).
evaluator_comm(t_if_statement(IfLoop, _, _), Env, NewEnv) :-
    evaluator_if(IfLoop, Env, NewEnv, true).
evaluator_comm(t_if_statement(IfLoop, ElifLoop, _), Env, NewEnv) :-
    evaluator_if(IfLoop, Env, _, false),
    evaluator_elif(ElifLoop, Env, NewEnv, true).
evaluator_comm(t_if_statement(IfLoop, ElifLoop, ElseLoop), Env, NewEnv) :-
    evaluator_if(IfLoop, Env, _, false),
    evaluator_elif(ElifLoop, Env, _, false),
    evaluator_else(ElseLoop, Env, NewEnv, true).
evaluator_comm(t_if_statement(IfLoop, _), Env, NewEnv) :-
    evaluator_if(IfLoop, Env, NewEnv, true).
evaluator_comm(t_if_statement(IfLoop, ElseLoop), Env, NewEnv) :-
    evaluator_if(IfLoop, Env, _, false),
    evaluator_else(ElseLoop, Env, NewEnv, true).

evaluator_for(Condition, _, _, Env, Env) :-
    evaluator_condition(Condition, Env, false).

evaluator_for(Condition, t_before_increment(Variable), Block, Env, NewEnv) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    evaluator_expr(t_increment(Variable), E1, E2),
    evaluator_for(Condition, t_before_increment(Variable), Block, E2, NewEnv).

evaluator_for(Condition, t_before_decrement(Variable), Block, Env, NewEnv) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    evaluator_expr(t_decrement(Variable), E1, E2),
    evaluator_for(Condition, t_before_decrement(Variable), Block, E2, NewEnv).

evaluator_for(Condition, t_after_decrement(Variable), Block, Env, NewEnv) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    evaluator_expr(t_decrement(Variable), E1, E2),
    evaluator_for(Condition, t_after_decrement(Variable), Block, E2, NewEnv).

evaluator_for(Condition, t_after_increment(Variable), Block, Env, NewEnv) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    evaluator_expr(t_increment(Variable), E1, E2),
    evaluator_for(Condition, t_after_increment(Variable), Block, E2, NewEnv).

evaluator_if(if_m(Condition, Block), Env, NewEnv, true) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, NewEnv).
evaluator_if(if_m(Condition, _), Env, Env, false) :-
    evaluator_condition(Condition, Env, false).

evaluator_elif(elif_m(Condition, Block), Env, NewEnv, true) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, NewEnv).
evaluator_elif(elif_m(Condition, Block, _), Env, NewEnv, true) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, NewEnv).
evaluator_elif(elif_m(Condition, _, ElifPart), Env, NewEnv, R) :-
    evaluator_condition(Condition, Env, false),
    evaluator_elif(ElifPart, Env, NewEnv, R).
evaluator_elif(elif_m(Condition, _, _), Env, Env, false) :-
    evaluator_condition(Condition, Env, false).

evaluator_else(t_else(Block), Env, NewEnv, true) :-
    eval_block(Block, Env, NewEnv).

evaluator_condition(t_condition(Expression1, Operator, Expression2), Env, Result) :-
    evaluator_expr(Expression1, Env, R1),
    evaluator_expr(Expression2, Env, R2),
    evaluator_comparators(R1, Operator, R2, Result).

evaluator_comparators(V1, t_comp_operator(>), V2, true)  :- V1 > V2.
evaluator_comparators(V1, t_comp_operator(>), V2, false)  :- V1 =< V2.
evaluator_comparators(V1, t_comp_operator(<), V2, true)  :- V1 < V2.
evaluator_comparators(V1, t_comp_operator(<), V2, false)  :- V1 >= V2.
evaluator_comparators(V1, t_comp_operator(>=), V2, true)  :- V1 >= V2.
evaluator_comparators(V1, t_comp_operator(>=), V2, false) :- V1 < V2.
evaluator_comparators(V1, t_comp_operator(=<), V2, true)  :- V1 =< V2.
evaluator_comparators(V1, t_comp_operator(=<), V2, false) :- V1 > V2.
evaluator_comparators(V1, t_comp_operator(==), V2, true)  :- V1 =:= V2.
evaluator_comparators(V1, t_comp_operator(==), V2, false) :- V1 =\= V2.
evaluator_comparators(V1, t_comp_operator('!='), V2, true)  :- V1 =\= V2.
evaluator_comparators(V1, t_comp_operator('!='), V2, false) :- V1 =:= V2.

evaluator_expr(t_expression(X), Env, Result) :- evaluator_expr(X, Env, Result).
evaluator_expr(t_add(X, Y), Env, Result) :- evaluator_expr(X, Env, R1), evaluator_expr(Y, Env, R2), Result is R1+R2.
evaluator_expr(t_sub(X, Y), Env, Result) :- evaluator_expr(X, Env, R1), evaluator_expr(Y, Env, R2), Result is R1-R2.
evaluator_expr(t_multiply(X, Y), Env, Result) :- evaluator_expr(X, Env, R1), evaluator_expr(Y, Env, R2), Result is R1*R2.
evaluator_expr(t_divide(X, Y), Env, Result) :- evaluator_expr(X, Env, R1), evaluator_expr(Y, Env, R2), Result is R1/R2.
evaluator_expr(t_boolean(Variable), _, Variable).
evaluator_expr(t_integer(Variable), _, Variable).
evaluator_expr(t_float(Variable) , _, Variable).
evaluator_expr(t_string(Variable) , _, Variable).
evaluator_expr(t_var_name(Name), Env, Value) :- seek(Name, Value, Env).
evaluator_expr(t_var_name(Name), Env, Name) :- not(seek(Name, _, Env)), string(Name).
evaluator_expr(t_increment(t_var_name(VariableName)), Env, NewEnv) :- seek(VariableName, Result, Env), NewValue is Result+1, change_comm(VariableName, NewValue, Env, NewEnv).
evaluator_expr(t_decrement(t_var_name(VariableName)), Env, NewEnv) :- seek(VariableName, Result, Env), NewValue is Result-1, change_comm(VariableName, NewValue, Env, NewEnv).
evaluator_expr(t_increment(Variable), Env, NewEnv) :- seek(Variable, Result, Env), NewValue is Result+1, change_comm(Variable, NewValue, Env, NewEnv).
evaluator_expr(t_decrement(Variable), Env, NewEnv) :- seek(Variable, Result, Env), NewValue is Result-1, change_comm(Variable, NewValue, Env, NewEnv).

evaluator_expr(t_ternary_expression(Condition, TrueExpression, _), Env, Result) :-
    evaluator_condition(Condition, Env, true),
    evaluator_expr(TrueExpression, Env, Result).

evaluator_expr(t_ternary_expression(Condition, _, FalseExpression), Env, Result) :-
    evaluator_condition(Condition, Env, false),
    evaluator_expr(FalseExpression, Env, Result).

evaluator_expr(t_boolean_expression(X, t_boolean_operator(Operator), Y), Env, Result) :-
    evaluator_expr(X, Env, R1),
    evaluator_expr(Y, Env, R2),
    evaluator_bool(R1,  Operator, R2, Result).

evaluator_bool(true , and, true  , true).
evaluator_bool(true , and, false , false).
evaluator_bool(false , and, true  , false).
evaluator_bool(false , and, false , false).
evaluator_bool(true , or , true  , true).
evaluator_bool(true , or , false , true).
evaluator_bool(false , or , true  , true).
evaluator_bool(false , or , false , false).

evaluator_var_type(t_var_type(Type), _, Type).


/* Environment */

% seek(Name, Value, Env)
seek(Name, Value, [(_, Name, Value) | _]).
seek(Name, Value, [_Head | Tail]) :- seek(Name, Value, Tail).

% change_comm(Name, Value, Env, NewEnv)
change_comm(Name, _, [], []) :- error_undeclared(Name).
change_comm(Name, Value, [Head | Tail], [Head | NewEnv]) :- Head \= (_, Name, _), change_comm(Name, Value, Tail, NewEnv).
change_comm(Name, Value, [(int , Name, _) | Env], [ (int , Name, Value) | Env]) :- integer(Value).
change_comm(Name, Value, [(float , Name, _) | Env], [ (float , Name, Value) | Env]) :- float(Value).
change_comm(Name, Value, [(bool , Name, _) | Env], [ (bool , Name, Value) | Env]) :- member(Value, [true, false]).
change_comm(Name, Value, [(string, Name, _) | Env], [ (string, Name, Value) | Env]) :- string(Value).

% change_comm - errors for type mismatch
change_comm(Name, Value, [(int , Name, _) | _], _)  :- not(integer(Value)),     error_type_conversion(Name, int).
change_comm(Name, Value, [(float, Name, _) | _], _)  :- not(float(Value)),     error_type_conversion(Name, float).
change_comm(Name, Value, [(bool , Name, _) | _], _)  :- not(member(Value, [true, false])), error_type_conversion(Name, bool).
change_comm(Name, Value, [(string, Name, _) | _], _) :- not(string(Value)) ,     error_type_conversion(Name, string).

% change_comm(Type, Name, Value, Env, NewEnv)
change_comm(Type, Name, Value, [], [(Type, Name, Value)]).
change_comm(Type, Name, Value, [Head | Tail], [Head | NewEnv]) :- Head \= (_, Name, _), change_comm(Type, Name, Value, Tail, NewEnv).
change_comm(_, Name, _, [(_, Name, _) | _], _NewEnv) :- error_redefinition(Name).


/* Errors */

error(String, List) :-
    ansi_format([bold, fg(red)], String, List), halt.
error_redefinition(Name) :-
    error('Error: Redefinition of ~w', [Name]).
error_type_conversion(Name, Type) :-
    error('Error: PHOENIX doesn\'t support type conversion. (Variable \'~w\' is not of type \'~w\')', [Name, Type]).
error_undeclared(Name) :- error('Error: ~w Undeclared', [Name]).
