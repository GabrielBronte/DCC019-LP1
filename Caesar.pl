
:- include('palavras').
:- consult('code').

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

sum_char_code(_,[],[]).
sum_char_code(Code,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|_]),
    H2 is mod(H3+Code,102),
    sum_char_code(Code,T1,T2).

conversion(_,[],[]).
conversion(Code,[H1|T1],[H2|T2]):-
    sum_char_code(Code, [H1|T1],[H3|_]),
    code(H2,H3),
    conversion(Code,T1,T2).

deconversion(_,[],[]).
deconversion(Code,[H1|T1],[H2|T2]):-
    sub_char_code(Code, [H1|T1],[H3|_]),
    code(H2,H3),
    deconversion(Code,T1,T2).

encoding(String,Char,R):-
    string_to_list_of_characters(String,X),code(Char,Code),
    conversion(Code,X,Y), atomics_to_string(Y,R).

decoding(String,Char,R):-
    string_to_list_of_characters(R,X),code(Char,Code),
    deconversion(Code,X,Y), atomics_to_string(Y,String).

sub_char_code(_,[],[]).
sub_char_code(Code,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|_]),
    H2 is mod(H3-Code,102),
    sub_char_code(Code,T1,T2).

caesar(X,Char,T):-
    nonvar(X),
    encoding(X,Char,T),!;
    decoding(X,Char,T).

delMember(_, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).

organize_string(S,X):-
    remove_char(S,53,Y),
    atomics_to_string(X,' ',Y).

increment_code(X1,X2):-
    X2 is X1 + 1.

search_words([]).
search_words([H1|T1]):-
    write(H1),
    write(' '),
    write(T1),
    write(' '),
    palavra(H1),
    search_words(T1).

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

busca_char_aux(String, Code, Char):-
    increment_code(Code,Code2),
    deconversion(Code2,String,T),
    atomics_to_string(T,Z),
    organize_string(Z,X),!,
    search_words(X),
    code(Char, Code2).

busca_char(_,102,_):-
    !,false.

busca_char(String, Code, Char):-
    busca_char_aux(String,Code,Char).

 busca_char(String, Code, Char):-
    increment_code(Code,Code2),
    busca_char(String, Code2, Char).
    
quebra_caesar(String,Char):-
    string_to_list_of_characters(String,X),
    busca_char(X, -1, Char).
