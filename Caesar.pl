palavra("presunto").
palavra("sobre").
palavra("sul").
palavra("fio").
palavra("carrapato").
palavra("molde").
palavra("pulsar").
palavra("golfinho").
palavra("mola").
palavra("varejo").
palavra("hospital").
palavra("motel").
palavra("diario").
palavra("foto").
palavra("aderir"). 
palavra("teste").
palavra("surpresa").
palavra("flamenco").
palavra("laringe").
palavra("discar").
palavra("aperitivo").
palavra("cumprimentar").
palavra("anestesia").
palavra("bateria").
palavra("torre").
palavra("cesta").
palavra("aguaceiro").
palavra("motor").
palavra("dezembro").
palavra("pasta").
palavra("radiante").
palavra("mascarar").
palavra("computador").
palavra("dinheiro").
palavra("recipiente").
palavra("flutuador").
palavra("defesa").
palavra("trabalho").
palavra("fuga").
palavra("lugar").
palavra("magro").
palavra("concha").
palavra("sempre").
palavra("asno").
palavra("borboleta").
palavra("pia").
palavra("lago").
palavra("atual").
palavra("infantil").
palavra("data").
palavra("pilha").
palavra("cachos").
palavra("parente").
palavra("bater").
palavra("aposta").
palavra("oceano").
palavra("cadeado").
palavra("flores").
palavra("chuva").
palavra("boca").
palavra("temporada").
palavra("para").
palavra("de").
palavra("com").
palavra("eu").
palavra("tu").
palavra("topo").
palavra("subir").
palavra("radical").
palavra("flauta").
palavra("hipnotizar").
palavra("fechado").
palavra("conflito").
palavra("gordura").
palavra("nota").
palavra("gaiola").
palavra("selo").
palavra("liberdade").
palavra("pico").
palavra("manteiga").
palavra("custo").
palavra("carro").
palavra("futebol").
palavra("pulso").
palavra("filho").
palavra("cantou").
palavra("aparar").
palavra("limpo").
palavra("antena").
palavra("cliente").
palavra("corpo").
palavra("popular").
palavra("tela").
palavra("limpo").
palavra("teclado").
palavra("tijolo").
palavra("chifre").
palavra("caderno").
palavra("luz").
palavra("jubilado").

code(' ',0).
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

code('A',27).
code('B',28).
code('C',29).
code('D',30).
code('E',31).
code('F',32).
code('G',33).
code('H',34).
code('I',35).
code('J',36).
code('K',37).
code('L',38).
code('M',39).
code('N',40).
code('O',41).
code('P',42).
code('Q',43).
code('R',44).
code('S',45).
code('T',46).
code('U',47).
code('V',48).
code('W',49).
code('X',50).
code('Y',51).
code('Z',52).

code('!',53).
code('?',54).
code('.',55).
code(',',56).
code(';',57).
code(':',58).
code('(',59).
code(')',60).
code('{',61).
code('}',62).
code('[',63).
code(']',64).

code('/',65).
code('*',66).
code('-',67).
code('+',68).
code('=',69).

code('_',70).
code('&',71).
code('¨',72).

code('%',73).
code('$',74).
code('#',75).
code('@',76).
code('"',77).
code("'",78).

code('^',79).
code('~',80).
code('`',81).
code('º',82).
code('ª',83).

code('à',84).
code('á',85).
code('ã',86).
code('â',87).

code('è',88).
code('é',89).
code('ẽ',90).
code('ê',91).

code('ì',92).
code('í',93).
code('î',94).

code('ò',95).
code('ó',96).
code('õ',97).
code('ô',98).

code('ù',99).
code('ú',100).
code('û',101).

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

sum_char_code(Code,[],[]).
sum_char_code(Code,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|T3]),
    H2 is mod(H3+Code,102),
    sum_char_code(Code,T1,T2).

conversion(Code,[],[]).
conversion(Code,[H1|T1],[H2|T2]):-
    sum_char_code(Code, [H1|T1],[H3|T3]),
    code(H2,H3),
    conversion(Code,T1,T2).

deconversion(Code,[],[]).
deconversion(Code,[H1|T1],[H2|T2]):-
    sub_char_code(Code, [H1|T1],[H3|T3]),
    code(H2,H3),
    deconversion(Code,T1,T2).

encoding(String,Char,R):-
    string_to_list_of_characters(String,X),code(Char,Code),
    conversion(Code,X,Y), atomics_to_string(Y,R).

decoding(String,Char,R):-
    string_to_list_of_characters(R,X),code(Char,Code),
    deconversion(Code,X,Y), atomics_to_string(Y,String).

sub_char_code(Code,[],[]).
sub_char_code(Code,[H1|T1],[H2|T2]):-
    string2code([H1|T1],[H3|T3]),
    H2 is mod(H3-Code,102),
    sub_char_code(Code,T1,T2).

caesar(X,Char,T):-
    nonvar(X),
    encoding(X,Char,T),!;
    decoding(X,Char,T).

%quebra_caesar

delMember(X, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).

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
    code(Char,Code),
    remove_char_aux(S,Char,X),!;
    increment_code(Code,Code2),
    remove_char(S,Code2,X).

organize_string(S,X):-
    remove_char(S,53,Y),
    atomics_to_string(X,' ',Y).

increment_code(X1,X2):-
    X2 is X1 + 1.

search_words([]).
search_words([H1|T1]):-
    palavra(H1),
    search_words(T1).


busca_char_aux(String, Code, Char):-
    increment_code(Code,Code2),
    deconversion(Code2,String,T),
    atomics_to_string(T,Z),
    palavra(Z),
    code(Char, Code2).

busca_char(String,102,Char).

busca_char(String, Code, Char):-
    busca_char_aux(String, Code, Char),!;
    increment_code(Code,Code2),
    busca_char(String, Code2, Char).
    
quebra_caesar(String,Char):-
    string_to_list_of_characters(String,X), 
    busca_char(X, -1, Char).

%organize_string(Z,X),
%search_words(X),

