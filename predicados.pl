:- consult('palavras').
:- consult('code').

delMember(_, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).

search_words([]).
search_words([H1|T1]):-
    palavra(H1),
    search_words(T1).

sum_char_code(_,[],[]).
sum_char_code(Code,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|_]),
    H2 is mod(H3+Code,76),
    sum_char_code(Code,T1,T2).

sub_char_code(_,[],[]).
sub_char_code(Code,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|_]),
    H2 is mod(H3-Code,76),
    sub_char_code(Code,T1,T2).

string_to_list_of_characters(String, Characters) :-
    name(String, Xs),
    maplist( number_to_character,
    Xs, Characters ).

number_to_character(Number, Character) :-
    name(Character, [Number]).

string2code([],[]).
string2code([H1|T1],[H2|T2]):-
    code(H1,H2),
    string2code(T1,T2).

increment_code(X1,X2):-
    X2 is X1 + 1.

decrement_code(X1,X2):-
    X2 is X1 - 1.

reverse([],Z,Z).

reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

copy(L,R) :- accCp(L,R).

accCp([],[]).
accCp([H|T1],[H|T2]) :- accCp(T1,T2).

organize_string(S,X):-
    remove_char(S,53,Y),
    atomics_to_string(X,' ',Y).

remove_char_aux(S,C,Y):-
    string_to_list_of_characters(S,Z),
    delMember(C,Z,X),
    atomics_to_string(X,Y).

remove_char(S,65,S).

remove_char(S,Code,X):-
    code(Char,Code),
    remove_char_aux(S,Char,Y),
    increment_code(Code,Code2),
    remove_char(Y,Code2,X).

remove_char(S,Code,X):-
    increment_code(Code,Code2),
    remove_char(S,Code2,X).