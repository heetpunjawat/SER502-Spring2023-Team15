:- module(eval_program, [eval_program/2]).
%%%%%%%%%%%%%%
% Evaluation %
%%%%%%%%%%%%%%

eval_program(t_program(P), NewEnv) :- evaluator_comm_list(P, [], NewEnv).

evaluator_comm_list(comm_list_m(Command, CommandList), Env, NewEnv) :-
    evaluator_comm(Command, Env, E1),
    evaluator_comm_list(CommandList, E1, NewEnv).
evaluator_comm_list(comm_m(Command), Env, NewEnv) :-
    evaluator_comm(Command, Env, NewEnv).

eval_block(block_m(CommandList), Env, NewEnv) :- evaluator_comm_list(CommandList, Env, NewEnv).

evaluator_comm(expr_assign_m(t_variable_name(Name), Expression), Env, NewEnv) :-
    evaluator_expr(Expression, Env, R1),
    change_comm(Name, R1, Env, NewEnv).
evaluator_comm(declare_comm_var_m(Type, t_variable_name(Name), Expression), Env, NewEnv) :-
    evaluator_var_type(Type, Env, R1),
    evaluator_expr(Expression, Env, R2),
    change_comm(R1, Name, R2, Env, NewEnv).

evaluator_comm(declare_comm_var_m(Type, t_variable_name(Name)), Env, NewEnv) :- evaluator_var_type(Type, Env, R1), R1 = int,  change_comm(R1, Name, 0, Env, NewEnv).
evaluator_comm(declare_comm_var_m(Type, t_variable_name(Name)), Env, NewEnv) :- evaluator_var_type(Type, Env, R1), R1 = float,  change_comm(R1, Name, 0.0, Env, NewEnv).
evaluator_comm(declare_comm_var_m(Type, t_variable_name(Name)), Env, NewEnv) :- evaluator_var_type(Type, Env, R1), R1 = string,  change_comm(R1, Name, "", Env, NewEnv).
evaluator_comm(declare_comm_var_m(Type, t_variable_name(Name)), Env, NewEnv) :- evaluator_var_type(Type, Env, R1), R1 = bool,  change_comm(R1, Name, false, Env, NewEnv).

evaluator_comm(expr_print_m(Expression), Env, Env) :- evaluator_expr(Expression, Env, Result), write(Result), nl.
evaluator_comm(str_print_m(String), Env, Env) :- write(String), nl.

evaluator_comm(comm_for_loop_m(Assignment, Condition, Variable_Change, Block), Env, NewEnv) :-
    evaluator_comm(Assignment, Env, E1),
    evaluator_for(Condition, Variable_Change, Block, E1, NewEnv).

evaluator_comm(comm_while_m(C, B), Env, NewEnv) :-
    evaluator_condition(C, Env, true),
    eval_block(B, Env, E1),
    evaluator_comm(comm_while_m(C, B), E1, NewEnv).
evaluator_comm(comm_while_m(C, _), Env, _) :-
    evaluator_condition(C, Env, false).

evaluator_comm(comm_enhanced_for_m(Variable, Expression1, Expression2, Block), Env, NewEnv) :-
    evaluator_comm(expr_assign_m(Variable, Expression1), Env, E1),
    evaluator_condition(t_condition(Expression1, compare_op_m(>), Expression2), E1, false),
    evaluator_for(t_condition(Variable, compare_op_m(=<), Expression2), increm_pre_m(Variable), Block, E1, NewEnv).

evaluator_comm(comm_enhanced_for_m(Variable, Expression1, Expression2, Block), Env, NewEnv) :-
    evaluator_comm(expr_assign_m(Variable, Expression1), Env, E1),
    evaluator_condition(t_condition(Expression1, compare_op_m(<), Expression2), E1, false),
    evaluator_for(t_condition(Variable, compare_op_m(>=), Expression2), decrem_pre_m(Variable), Block, E1, NewEnv).

evaluator_comm(comm_if_m(IfTree), Env, NewEnv) :- evaluator_if(IfTree, Env, NewEnv, _).
evaluator_comm(comm_if_m(IfTree, _, _), Env, NewEnv) :-
    evaluator_if(IfTree, Env, NewEnv, true).
evaluator_comm(comm_if_m(IfTree, ElifTree, _), Env, NewEnv) :-
    evaluator_if(IfTree, Env, _, false),
    evaluator_elif(ElifTree, Env, NewEnv, true).
evaluator_comm(comm_if_m(IfTree, ElifTree, ElseTree), Env, NewEnv) :-
    evaluator_if(IfTree, Env, _, false),
    evaluator_elif(ElifTree, Env, _, false),
    evaluator_else(ElseTree, Env, NewEnv, true).
evaluator_comm(comm_if_m(IfTree, _), Env, NewEnv) :-
    evaluator_if(IfTree, Env, NewEnv, true).
evaluator_comm(comm_if_m(IfTree, ElseTree), Env, NewEnv) :-
    evaluator_if(IfTree, Env, _, false),
    evaluator_else(ElseTree, Env, NewEnv, true).

evaluator_for(Condition, _, _, Env, Env) :-
    evaluator_condition(Condition, Env, false).

evaluator_for(Condition, increm_pre_m(Variable), Block, Env, NewEnv) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    evaluator_expr(increm_m(Variable), E1, E2),
    evaluator_for(Condition, increm_pre_m(Variable), Block, E2, NewEnv).

evaluator_for(Condition, decrem_pre_m(Variable), Block, Env, NewEnv) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    evaluator_expr(decrem_m(Variable), E1, E2),
    evaluator_for(Condition, decrem_pre_m(Variable), Block, E2, NewEnv).

evaluator_for(Condition, decrem_post(Variable), Block, Env, NewEnv) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    evaluator_expr(decrem_m(Variable), E1, E2),
    evaluator_for(Condition, decrem_post(Variable), Block, E2, NewEnv).

evaluator_for(Condition, increm_post_m(Variable), Block, Env, NewEnv) :-
    evaluator_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    evaluator_expr(increm_m(Variable), E1, E2),
    evaluator_for(Condition, increm_post_m(Variable), Block, E2, NewEnv).

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

evaluator_comparators(S1, compare_op_m(>), S2, true)  :- S1 > S2.
evaluator_comparators(S1, compare_op_m(>), S2, false)  :- S1 =< S2.
evaluator_comparators(S1, compare_op_m(<), S2, true)  :- S1 < S2.
evaluator_comparators(S1, compare_op_m(<), S2, false)  :- S1 >= S2.
evaluator_comparators(S1, compare_op_m(>=), S2, true)  :- S1 >= S2.
evaluator_comparators(S1, compare_op_m(>=), S2, false) :- S1 < S2.
evaluator_comparators(S1, compare_op_m(=<), S2, true)  :- S1 =< S2.
evaluator_comparators(S1, compare_op_m(=<), S2, false) :- S1 > S2.
evaluator_comparators(S1, compare_op_m(==), S2, true)  :- S1 =:= S2.
evaluator_comparators(S1, compare_op_m(==), S2, false) :- S1 =\= S2.
evaluator_comparators(S1, compare_op_m('!='), S2, true)  :- S1 =\= S2.
evaluator_comparators(S1, compare_op_m('!='), S2, false) :- S1 =:= S2.

evaluator_expr(t_expression(X), Env, Result) :- evaluator_expr(X, Env, Result).
evaluator_expr(t_add(X, Y), Env, Result) :- evaluator_expr(X, Env, R1), evaluator_expr(Y, Env, R2), Result is R1+R2.
evaluator_expr(t_sub(X, Y), Env, Result) :- evaluator_expr(X, Env, R1), evaluator_expr(Y, Env, R2), Result is R1-R2.
evaluator_expr(t_multiply(X, Y), Env, Result) :- evaluator_expr(X, Env, R1), evaluator_expr(Y, Env, R2), Result is R1*R2.
evaluator_expr(t_divide(X, Y), Env, Result) :- evaluator_expr(X, Env, R1), evaluator_expr(Y, Env, R2), Result is R1/R2.
evaluator_expr(t_boolean(Variable), _, Variable).
evaluator_expr(t_integer(Variable), _, Variable).
evaluator_expr(t_float(Variable) , _, Variable).
evaluator_expr(t_string(Variable) , _, Variable).
evaluator_expr(t_variable_name(Name), Env, Value) :- seek(Name, Value, Env).
evaluator_expr(t_variable_name(Name), Env, Name) :- not(seek(Name, _, Env)), string(Name).
evaluator_expr(increm_m(t_variable_name(VariableName)), Env, NewEnv) :- seek(VariableName, Result, Env), NewValue is Result+1, change_comm(VariableName, NewValue, Env, NewEnv).
evaluator_expr(decrem_m(t_variable_name(VariableName)), Env, NewEnv) :- seek(VariableName, Result, Env), NewValue is Result-1, change_comm(VariableName, NewValue, Env, NewEnv).
evaluator_expr(increm_m(Variable), Env, NewEnv) :- seek(Variable, Result, Env), NewValue is Result+1, change_comm(Variable, NewValue, Env, NewEnv).
evaluator_expr(decrem_m(Variable), Env, NewEnv) :- seek(Variable, Result, Env), NewValue is Result-1, change_comm(Variable, NewValue, Env, NewEnv).

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

evaluator_var_type(t_variable_type(Type), _, Type).

%%%%%%%%%%%%%%%
% Environment %
%%%%%%%%%%%%%%%

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

%%%%%%%%%
% ERROR %
%%%%%%%%%
error(String, List) :-
    ansi_format([bold, fg(red)], String, List), halt.
error_redefinition(Name) :-
    error('Error: Redefinition of ~w', [Name]).
error_type_conversion(Name, Type) :-
    error('Error: IMPRO doesn\'t support type conversion. (Variable \'~w\' is not of type \'~w\')', [Name, Type]).
error_undeclared(Name) :- error('Error: ~w Undeclared', [Name]).

%%%%%%%%%%%
% TESTING %
%%%%%%%%%%%

?- change_comm(x, 5, [(int, x, 6)], [(int, x, 5)]).
?- change_comm(x, 5, [(int, x, 2), (float, y, 3.4)], [(int, x, 5), (float, y, 3.4)]).

?- change_comm(int, x, 5, [], [(int, x, 5)]).
?- change_comm(int, x, 5, [(int, y, 6)], [(int, y, 6), (int, x, 5)]).

?- evaluator_expr(t_add(t_integer(3), t_integer(5)), [], 8).
?- evaluator_expr(t_sub(t_integer(3), t_integer(5)), [], -2).
?- evaluator_expr(t_multiply(t_integer(3), t_integer(5)), [], 15).
?- evaluator_expr(t_divide(t_integer(3), t_integer(6)), [], 0.5).

?- not(evaluator_expr(t_variable_name(x), [], _)).
?- evaluator_expr(t_variable_name("String"), [], "String").

?- evaluator_condition(t_condition(t_variable_name(x), compare_op_m(>),  t_integer(4)), [(int, x, 6)], true).
?- evaluator_condition(t_condition(t_variable_name(x), compare_op_m(<),  t_integer(4)), [(int, x, 2)], true).
?- evaluator_condition(t_condition(t_variable_name(x), compare_op_m(>=), t_integer(4)), [(int, x, 6)], true).
?- evaluator_condition(t_condition(t_variable_name(x), compare_op_m(>=), t_integer(4)), [(int, x, 4)], true).
?- evaluator_condition(t_condition(t_variable_name(x), compare_op_m(=<), t_integer(4)), [(int, x, 2)], true).
?- evaluator_condition(t_condition(t_variable_name(x), compare_op_m(=<), t_integer(4)), [(int, x, 4)], true).
?- evaluator_condition(t_condition(t_variable_name(x), compare_op_m(==), t_integer(4)), [(int, x, 4)], true).
?- evaluator_condition(t_condition(t_variable_name(x), compare_op_m('!='), t_integer(4)), [(int, x, 2)], true).
