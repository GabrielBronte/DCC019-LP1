:- consult('palavras').
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

sub_char_code(_,[],[]).
sub_char_code(Code,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|_]),
    H2 is mod(H3-Code,102),
    sub_char_code(Code,T1,T2).

conversion([],_,[],[]).
conversion([_|_],_,[],[]).
conversion([],C,[H1|T1],[H2|T2]):-
    conversion(C,C,[H1|T1],[H2|T2]).

conversion([KH1|KT1],C,[H1|T1],[H2|T2]):-
    code(KH1,Code),
    sum_char_code(Code, [H1|T1],[H3|_]),
    code(H2,H3),
    conversion(KT1,C,T1,T2).

deconversion([],_,[],[]).
deconversion([_|_],_,[],[]).

deconversion([],C,[H1|T1],[H2|T2]):-
    deconversion(C,C,[H1|T1],[H2|T2]).

deconversion([KH1|KT1],C,[H1|T1],[H2|T2]):-
    code(KH1,Code),
    sub_char_code(Code, [H1|T1],[H3|_]),
    code(H2,H3),
    deconversion(KT1,C,T1,T2).


encoding(String,Key,R):-
    string_to_list_of_characters(String,X),string_to_list_of_characters(Key,K),
    conversion(K,K,X,Y), atomics_to_string(Y,R).


decoding(String,Key,R):-
    string_to_list_of_characters(R,X),string_to_list_of_characters(Key,K),
    deconversion(K,K,X,Y), atomics_to_string(Y,String).


vigenere(X,Key,T):-
    nonvar(X),
    encoding(X,Key,T),!;
    decoding(X,Key,T).

increment_code(X1,X2):-
    X2 is X1 + 1.

decrement_code(X1,X2):-
    X2 is X1 - 1.

increment_code_up_to_n(String,1,Inicial,Result):-
    string_concat(String,'b',Result),!.

increment_code_up_to_n(String,N,Inicial,Result):-
    increment_code(Inicial,Inicial2),
    string_concat(String,'a',X),
    decrement_code(N,N2),
    increment_code_up_to_n(X,N2,Inicial2,Result).

quebra_vigenere(String,N,Final):-
    increment_code_up_to_n('',N,1,X),
    write(X),
    vigenere(String,X,Final).