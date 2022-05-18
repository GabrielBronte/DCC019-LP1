palavra('presunto').
palavra('sobre').
palavra('sul').
palavra('fio').
palavra('carrapato').
palavra('molde').
palavra('pulsar').
palavra('golfinho').
palavra('mola').
palavra('varejo').
palavra('hospital').
palavra('motel').
palavra('diario').
palavra('foto').
palavra('aderir'). 
palavra('teste').
palavra('surpresa').
palavra('flamenco').
palavra('laringe').
palavra('discar').
palavra('aperitivo').
palavra('cumprimentar').
palavra('anestesia').
palavra('bateria').
palavra('torre').
palavra('cesta').
palavra('aguaceiro').
palavra('motor').
palavra('dezembro').
palavra('pasta').
palavra('radiante').
palavra('mascarar').
palavra('computador').
palavra('dinheiro').
palavra('recipiente').
palavra('flutuador').
palavra('defesa').
palavra('trabalho').
palavra('fuga').
palavra('lugar').
palavra('magro').
palavra('concha').
palavra('sempre').
palavra('asno').
palavra('borboleta').
palavra('pia').
palavra('lago').
palavra('atual').
palavra('infantil').
palavra('data').
palavra('pilha').
palavra('cachos').
palavra('parente').
palavra('bater').
palavra('aposta').
palavra('oceano').
palavra('cadeado').
palavra('flores').
palavra('chuva').
palavra('boca').
palavra('temporada').
palavra('para').
palavra('de').
palavra('com').
palavra('eu').
palavra('tu').
palavra('topo').
palavra('subir').
palavra('radical').
palavra('flauta').
palavra('hipnotizar').
palavra('fechado').
palavra('conflito').
palavra('gordura').
palavra('nota').
palavra('gaiola').
palavra('selo').
palavra('liberdade').
palavra('pico').
palavra('manteiga').
palavra('custo').
palavra('carro').
palavra('futebol').
palavra('pulso').
palavra('filho').
palavra('jarra').
palavra('aparar').
palavra('limpo').
palavra('antena').
palavra('cliente').
palavra('corpo').
palavra('popular').
palavra('tela').
palavra('limpo').
palavra('teclado').
palavra('tijolo').
palavra('chifre').
palavra('caderno').
palavra('luz').
palavra('jubilado').


code('a',1).
code('b',2).
code('c',3).
code('d',4).
code('e',5).
code('f',6).
code('g',7).
code('h',8).
code('i',9).
code('j',10).
code('k',11).
code('l',12).
code('m',13).
code('n',14).
code('o',15).
code('p',16).
code('q',17).
code('r',18).
code('s',19).
code('t',20).
code('u',21).
code('v',22).
code('w',23).
code('x',24).
code('y',25).
code('z',26).


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

sum3(Code,[],[]).
sum3(Code,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|T3]),
    H2 is mod(H3+Code,26),
    sum3(Code,T1,T2).

conversion(Code,[],[]).
conversion(Code,[H1|T1],[H2|T2]):-
    sum3(Code, [H1|T1],[H3|T3]),
    code(H2,H3),
    conversion(Code,T1,T2).

caesar(String,Char,R):-
    string_to_list_of_characters(String,X),code(Char,Code),
    conversion(Code,X,Y), atomics_to_string(Y,R).


list_length([], 0 ).
list_length([_|Xs] , L ) :- list_length(Xs,N) , L is N+1 .


extend_list(Number,[],[]).
extend_list(0,[H1|T1],[H2|T2]).
extend_list(Number,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|T3]),
    H2 is mod(H3+Code,26),
    sum3(Code,T1,T2).