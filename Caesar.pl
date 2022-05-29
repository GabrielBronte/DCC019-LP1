:- consult('palavras').
:- consult('code').
:- consult('predicados').

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

caesar(X,Char,T):-
    nonvar(X),
    encoding(X,Char,T),!;
    decoding(X,Char,T).

busca_char_aux(String, Code, Char):-
    increment_code(Code,Code2),
    deconversion(Code2,String,T),
    atomics_to_string(T,Z),
    organize_string(Z,X),!,
    search_words(X),
    code(Char, Code2).

busca_char(_,77,_):-
    !,false.

busca_char(String, Code, Char):-
    busca_char_aux(String,Code,Char).

busca_char(String, Code, Char):-
    increment_code(Code,Code2),
    busca_char(String, Code2, Char).
    
quebra_caesar(String,Char):-
    string_to_list_of_characters(String,X),
    busca_char(X, -1, Char).