:- module(language, [question/4]).

question(Q0, QEnd, C0, CEnd) :-
    starter_phrase(Q0, Q1),
    query_phrase(Q1, QEnd, C0, CEnd).

starter_phrase(['What', is | L], L).
starter_phrase(['Which' | L], L).

query_phrase(L0, LEnd, C0, CEnd) :-
    det(L0, L1, C0, C1),
    req_type_phrase(L1, L2, C1, C2),
    coin_name(L2, L3, C2, C3),
    optional_phrase(L3, LEnd, C3, CEnd).

det([the | L], L, C, C).
det(L, L, C, C).

req_type_phrase([Req, of | L], L, [req_type(Req) | C], C).
req_type_phrase([Req, sell | L], L, [req_type(Req) | C], C).

coin_name([Coin | L], L, [coin(Code) | C], C) :- 
    coin_code(Coin, Code).

optional_phrase([], _, C, C).
optional_phrase([?], _, C, C).
optional_phrase([.], _, C, C).

optional_phrase([in | L], LEnd, C0, CEnd) :-
    currency_name(L, R, C0, C1),
    optional_phrase(R, LEnd, C1, CEnd).

optional_phrase([at, Market | L], LEnd, [market(Market) | C], C) :-
    optional_phrase(L, LEnd, C, C).

currency_name([Currency | L], L, [currency(Code) | C], C) :-
    currency_code(Currency, Code).

coin_code('Bitcoin', btc).
coin_code('btc', btc).
coin_code('BTC', btc).

coin_code('Ethereum', eth).
coin_code('ETH', eth).
coin_code('eth', eth).

currency_code('USD', usd).
currency_code('usd', usd).

currency_code('CAD', cad).
currency_code('cad', cad).

currency_code('EUR', eur).
currency_code('eur', eur).

currency_code('GBP', gbp).
currency_code('gbp', gbp).