:- module(language, [question/4]).

question(Q0, QEnd, C0, CEnd) :-
    starter_phrase(Q0, Q1),
    query_phrase(Q1, QEnd, C0, CEnd).

starter_phrase(['What', is | L], L).
starter_phrase(['Which' | L], L).
starter_phrase(['Which', market, has | L], L).

query_phrase(L0, LEnd, C0, CEnd) :-
    det(L0, L1, C0, C1),
    req_type_phrase(L1, L2, C1, C2),
    coin_name(L2, L3, C2, C3),
    optional_phrase(L3, LEnd, C3, CEnd).

det([the | L], L, C, C).
det(L, L, C, C).

req_type_phrase([Req, of | L], L, [req_type(Req) | C], C).
req_type_phrase([Req, sell | L], L, [req_type(Req) | C], C).
req_type_phrase([Req, price, for | L], L, [req_type(Req) | C], C).

coin_name([Coin | L], L, [coin(Code) | C], C) :- 
    downcase_atom(Coin, CoinLowerCase),
    coin_code(CoinLowerCase, Code).

optional_phrase([], _, C, C).
optional_phrase([?], _, C, C).
optional_phrase([.], _, C, C).

optional_phrase([in | L], LEnd, C0, CEnd) :-
    currency_name(L, R, C0, C1),
    optional_phrase(R, LEnd, C1, CEnd).

optional_phrase([at, Market | L], LEnd, [market(Market) | C], C) :-
    optional_phrase(L, LEnd, C, C).

currency_name([Currency | L], L, [currency(Code) | C], C) :-
    downcase_atom(Currency, CurrencyLowerCase),
    currency_code(CurrencyLowerCase, Code).

coin_code('bitcoin', btc).
coin_code('btc', btc).
coin_code('ethereum', eth).
coin_code('eth', eth).
coin_code('litecoin', ltc).
coin_code('ltc', ltc).
coin_code('peercoin', ppc).
coin_code('ppc', ppc).
coin_code('dogecoin', dode).
coin_code('dode', dode).
coin_code('ripple', xrp).
coin_code('xrp', xrp).
coin_code('nxt', nxt).
coin_code('auroracoin', aur).
coin_code('aur', aur).
coin_code('dash', dash).
coin_code('neo', neo).
coin_code('monero', xmr).
coin_code('xmr', xmr).
coin_code('verge', xvg).
coin_code('xvg', xvg).
coin_code('stellar', xlm).
coin_code('xlm', xlm).
coin_code('nano', nano).
coin_code('tether', usdt).
coin_code('usdt', usdt).
coin_code('zcash', zec).
coin_code('zec', zec).
coin_code('cardano', ada).
coin_code('ada', ada).

currency_code('usd', usd).
currency_code('cad', cad).
currency_code('eur', eur).
currency_code('gbp', gbp).
currency_code('jpy', jpy).
currency_code('aud', aud).
currency_code('rub', rub).
currency_code('brl', brl).
currency_code('idr', idr).