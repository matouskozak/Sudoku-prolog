r():- reconsult("sudoku.pl").

/*
	S1|S4|S7
	S2|S5|S8
	S3|S6|S9
*/
squares([
          [X11, X12, X13, X14, X15, X16, X17, X18, X19],
          [X21, X22, X23, X24, X25, X26, X27, X28, X29],
          [X31, X32, X33, X34, X35, X36, X37, X38, X39],
          [X41, X42, X43, X44, X45, X46, X47, X48, X49],
          [X51, X52, X53, X54, X55, X56, X57, X58, X59],
          [X61, X62, X63, X64, X65, X66, X67, X68, X69],
          [X71, X72, X73, X74, X75, X76, X77, X78, X79],
          [X81, X82, X83, X84, X85, X86, X87, X88, X89],
          [X91, X92, X93, X94, X95, X96, X97, X98, X99]
        ],
        [
          [X11, X12, X13, X21, X22, X23, X31, X32, X33],
          [X41, X42, X43, X51, X52, X53, X61, X62, X63],
          [X71, X72, X73, X81, X82, X83, X91, X92, X93],
          [X14, X15, X16, X24, X25, X26, X34, X35, X36],
          [X44, X45, X46, X54, X55, X56, X64, X65, X66],
          [X74, X75, X76, X84, X85, X86, X94, X95, X96],
          [X17, X18, X19, X27, X28, X29, X37, X38, X39],
          [X47, X48, X49, X57, X58, X59, X67, X68, X69],
          [X77, X78, X79, X87, X88, X89, X97, X98, X99]
        ]).
        
squares_to_rows([
          [X11, X12, X13, X21, X22, X23, X31, X32, X33],
          [X41, X42, X43, X51, X52, X53, X61, X62, X63],
          [X71, X72, X73, X81, X82, X83, X91, X92, X93],
          [X14, X15, X16, X24, X25, X26, X34, X35, X36],
          [X44, X45, X46, X54, X55, X56, X64, X65, X66],
          [X74, X75, X76, X84, X85, X86, X94, X95, X96],
          [X17, X18, X19, X27, X28, X29, X37, X38, X39],
          [X47, X48, X49, X57, X58, X59, X67, X68, X69],
          [X77, X78, X79, X87, X88, X89, X97, X98, X99]
        ], 
        [
          [X11, X12, X13, X14, X15, X16, X17, X18, X19],
          [X21, X22, X23, X24, X25, X26, X27, X28, X29],
          [X31, X32, X33, X34, X35, X36, X37, X38, X39],
          [X41, X42, X43, X44, X45, X46, X47, X48, X49],
          [X51, X52, X53, X54, X55, X56, X57, X58, X59],
          [X61, X62, X63, X64, X65, X66, X67, X68, X69],
          [X71, X72, X73, X74, X75, X76, X77, X78, X79],
          [X81, X82, X83, X84, X85, X86, X87, X88, X89],
          [X91, X92, X93, X94, X95, X96, X97, X98, X99]
        ]
        ).


my_member(X, [X | _]).
my_member(X, [ _ | T]) :- my_member(X, T).

my_unique([]).
my_unique([HEAD|TAIL]):-
	not(my_member(HEAD,TAIL)),
	my_unique(TAIL).
	
in_range(Low, _, Low).
in_range(Low, High, X) :- NewLow is Low + 1, NewLow =< High, in_range(NewLow, High, X).

transpose([[]|_], []):- !.
transpose(Matrix, [Row|Rows]) :- transpose_1st_col(Matrix, Row, RestMatrix),
                                 transpose(RestMatrix, Rows).
                                 
transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- transpose_1st_col(Rows, Hs, Ts).
	
my_remove_first([_|T], T).
my_remove_first_col([], []).
my_remove_first_col([H|T], [HR|Res]) :- my_remove_first(H, HR), my_remove_first_col(T, Res).	


/* Remove variables from list */
remove_vars(Acc, [], Res):- !,
	reverse(Acc, Res).
remove_vars(Acc, [VAR|TAIL], Res):- 
	var(VAR), !,
	remove_vars(Acc, TAIL, Res).
remove_vars(Acc, [HEAD|TAIL], Res):-
	remove_vars([HEAD|Acc], TAIL, Res).	

/* Get missing values for each list */
get_missing([], Acc, Acc):- !.
get_missing([HEAD|TAIL], Acc, Res):- 
	remove_vars([], HEAD, TMP1),
	subtract([1, 2, 3, 4, 5, 6, 7, 8, 9], TMP1, X),
	length(X, Num_missing),
	TMP = [Num_missing|HEAD],
	Acc1 = [TMP|Acc],
	get_missing(TAIL, Acc1, Res).
	
/* Similar to get_missing */	
my_subtract([], Acc, Res):- !,
	reverse(Acc, Res).
my_subtract([HEAD|TAIL], Acc, Res):- 
	remove_vars([], HEAD, TMP1),
	subtract([1, 2, 3, 4, 5, 6, 7, 8, 9], TMP1, X),
	Acc1 = [X|Acc],
	my_subtract(TAIL, Acc1, Res).

/* Intersect each list wih Single */	
my_intersect(Acc, [], _, Res) :- !,
	reverse(Acc, Res).
my_intersect(Acc, [HEAD|TAIL], Single, Res):-
	intersection(Single, HEAD, TMP),
	Acc1 = [TMP|Acc],
	my_intersect(Acc1, TAIL, Single, Res).		
	
/* Generate all possible values for each possition */	
all_possibilities(Rows, Cols, Sq, Poss):- 
  my_subtract(Rows, [], Rows_1),
  my_subtract(Cols, [], Cols_1),
  my_subtract(Sq,   [], Sq_1),
  
	% maplist(my_intersect([], Rows_1), Cols_1, Cols_2),
	maplist(my_intersect([], Cols_1), Rows_1, Rows_2),
	squares(Rows_2, Sq_2),
	maplist(my_intersect([]), Sq_2, Sq_1, Sq_3),
	
	squares_to_rows(Sq_3, Poss).

/* Filter variables from first list and corresponding possitions from second list */
my_var_filter([], [], Acc1, Acc2, Res1, Res2):- !,
	reverse(Acc1, Res1),
	reverse(Acc2, Res2).
my_var_filter([VAR|TAIL1], [HEAD2|TAIL2], Acc1, Acc2, Res1, Res2):-
	var(VAR),
	my_var_filter(TAIL1, TAIL2, [VAR|Acc1], [HEAD2|Acc2], Res1, Res2), !.
my_var_filter([_|TAIL1], [_|TAIL2], Acc1, Acc2, Res1, Res2):- 
	my_var_filter(TAIL1, TAIL2, Acc1, Acc2, Res1, Res2).


/* Get shortest list and its possition from list of lists */	
list_min([L|Ls], Min, Idx) :- 
	list_min(0, Ls, L, 0, Min, Idx).
list_min(_, [], Min, Idx, Min, Idx):- !.
list_min(I, [L|Ls], Min0, _, Min, Idx) :-
		length(L, Len),
		% Len \= 0,
		length(Min0, Len0),
		Len < Len0,
		I1 is I + 1, !,
    list_min(I1, Ls, L, I1, Min, Idx).
list_min(I, [_|Ls], Min0, Idx0, Min, Idx) :-
		I1 is I + 1,
    list_min(I1, Ls, Min0, Idx0, Min, Idx).

/* Get var from given row with least possibilities */
get_min_in_row(Row, Row_poss, VAR, P):-
	my_var_filter(Row, Row_poss, [], [], Row_1, Poss_1),
	length(Row_1, Len),
	Len > 0, !,
	list_min(Poss_1, P, Idx),
  nth0(Idx, Row_1, VAR).
get_min_in_row(_, _, _, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).
%get_min_in_row(_, _, _, []).
  
/* Get var from all rows with least possibilities */ 
get_min_in_rows(Rows, All_poss, VAR, P):-
	maplist(get_min_in_row, Rows, All_poss, VARS, Min_VARS_poss),
	list_min(Min_VARS_poss, P, Idx),
	nth0(Idx, VARS, VAR).	
	
/* Checks if sudoku is complete */
is_complete(Rows):-
	transpose(Rows, Columns),
  squares(Rows, Squares),
	Nums = [1, 2, 3, 4, 5, 6, 7, 8, 9],
	
	maplist(remove_vars([]), Rows, Rows_1),
	maplist(remove_vars([]), Columns, Cols_1),
	maplist(remove_vars([]), Squares, Sq_1),
  maplist(subset(Nums), Rows_1),
  maplist(subset(Nums), Cols_1),
  maplist(subset(Nums), Sq_1),
  write("HURRAY!"), nl.
	
sudoku(Rows) :- is_complete(Rows), !.
sudoku(Rows) :-
	%pp(Rows), nl,
	Size is 9,
  length(Rows, Size),
  maplist(same_length(Rows), Rows),
 	transpose(Rows, Columns),
  squares(Rows, Squares),
  
  
  %Rows = [R1, R2, R3, R4, R5, R6, R7, R8, R9],
 	%Columns = [C1, C2, C3, C4, C5, C6, C7, C8, C9],
  %Squares = [S1, S2, S3, S4, S5, S6, S7, S8, S9],

  all_possibilities(Rows, Columns, Squares, Poss_rows),
  get_min_in_rows(Rows, Poss_rows, V, P),
  %write(V), write(P), nl,
  length(P, Len),
  Len >= 1, Len =< 9,
  member(V, P), 
  sudoku(Rows). 
  

test_1 :-
  L = [
         [_,6,_,1,_,4,_,5,_],
         [_,_,8,3,_,5,6,_,_],
         [2,_,_,_,_,_,_,_,1],
         [8,_,_,4,_,7,_,_,6],
         [_,_,6,_,_,_,3,_,_],
         [7,_,_,9,_,1,_,_,4],
         [5,_,_,_,_,_,_,_,2],
         [_,_,7,2,_,6,9,_,_],
         [_,4,_,5,_,8,_,7,_]
  ],
  sudoku(L),
  pp(L).


test_2 :-
  L = [
        [_,4,3,_,8,_,2,5,7],
        [6,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,1,_,9,4],
        [9,_,_,_,_,4,_,7,_],
        [_,_,_,6,_,8,_,_,_],
        [_,1,_,2,_,_,_,_,3],
        [8,2,_,5,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,5],
        [_,3,4,_,9,_,7,1,_]
  ],
  sudoku(L),
  pp(L).


test_3 :-
  L = [
        [_,_,_,1,5,_,_,7,_],
        [1,_,6,_,_,_,8,2,_],
        [3,_,_,8,6,_,_,4,_],
        [9,_,_,4,_,_,5,6,7],
        [_,_,4,7,_,8,3,_,_],
        [7,3,2,_,_,6,_,_,4],
        [_,4,_,_,8,1,_,_,9],
        [_,1,7,_,_,_,2,_,8],
        [_,5,_,_,3,7,_,_,_]
  ],
  sudoku(L),
  pp(L).

test_4 :-
  L = [
        [_,_,_,2,6,_,7,_,1],
        [6,8,_,_,7,_,_,9,_],
        [1,9,_,_,_,4,5,_,_],
        [8,2,_,1,_,_,_,4,_],
        [_,_,4,6,_,2,9,_,_],
        [_,5,_,_,_,3,_,2,8],
        [_,_,9,3,_,_,_,7,4],
        [_,4,_,_,5,_,_,3,6],
        [7,_,3,_,1,8,_,_,_]
  ],
  sudoku(L),
  pp(L).
  
test_empty :-
  L = [
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_]
  ],
  sudoku(L),
  pp(L).
  
test_hard :-
  L = [
        [8,_,_,_,_,_,_,_,_],
        [_,_,3,6,_,_,_,_,_],
        [_,7,_,_,9,_,2,_,_],
        [_,5,_,_,_,7,_,_,_],
        [_,_,_,_,4,5,7,_,_],
        [_,_,_,1,_,_,_,3,_],
        [_,_,1,_,_,_,_,6,8],
        [_,_,8,_,_,_,_,1,_],
        [_,9,_,_,_,_,4,_,_]
  ],
  sudoku(L),
  pp(L).
  
   
pp([]).
pp([H|T]) :-
  write(H),nl,
  pp(T).
