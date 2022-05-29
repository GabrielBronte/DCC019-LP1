:- consult('palavras').
:- consult('code').
:- consult('predicados').




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

increment_code_up_to_n(String,1,String).

increment_code_up_to_n(String,N,Result):-
    string_concat(String, ' ', R),
    decrement_code(N, Aux),
    increment_code_up_to_n(R, Aux, Result).

vrau(String, Key,Final):-
    atomics_to_string(Key,X),
    vigenere(String2,X,String),
    organize_string(String2,Z),!,
    search_words(Z).

vrau(String, Key,Final):-
    write(Key).


    

%vrau(String,Key,Final):-
%search_key(Key,Y),
    %string_to_list_of_characters(Y,List),
    %vrau(String,List,Final).

increment_char(Code1,Code2):-
    Code2 is Code1 + 1. 


increment_string([], _).

increment_string([H1|T1], [H2|T2]):-
    ((H1 >= 3), 
    H2 is 0,
    increment_string(T1,T2)).

increment_string([H1|T1], [H2|T2]):-
    ((H1 < 3), 
    H2 is H1 + 1,
    copy(T1,T2)).
    
search_key(List,L):-
    string2code(List,X),
    reverse(X,Z),
    increment_string(Z,V),
    reverse(V,T),
    string2code(Y,T),
    atomics_to_string(Y,L).

quebra_vigenere(String,N,Final):-
    increment_code_up_to_n(' ',N,X),
    string_to_list_of_characters(X,Key),
    vrau(String, Key, Final).



