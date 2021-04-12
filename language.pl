:- module(language, [question/4]).

% Question is a starter phrase followed by a query phrase
question(Q0, QEnd, C0, CEnd) :-
    starter_phrase(Q0, Q1),
    query_phrase(Q1, QEnd, C0, CEnd).

% Different starter phrases for different types of questions, 
% e.g. What is the price of Bitcoin? or Which markets sell Bitcoin?
starter_phrase(['What', is | L], L).
starter_phrase([what, is | L], L).
starter_phrase(['Which' | L], L).
starter_phrase([which | L], L).
starter_phrase(['Which', market, has | L], L).
starter_phrase([which, market, has | L], L).

% Query phrase is a determinant followed by a request type, 
% coin name, and optional phrases to specify currency and markets
query_phrase(L0, LEnd, C0, CEnd) :-
    det(L0, L1, C0, C1),
    req_type_phrase(L1, L2, C1, C2),
    coin_name(L2, L3, C2, C3),
    optional_phrase(L3, LEnd, C3, CEnd).

det([the | L], L, C, C).
det(L, L, C, C).

% Request types can be: price, volume, change, markets, highest, lowest
req_type_phrase([Req, of | L], L, [req_type(Req) | C], C).
req_type_phrase([Req, sell | L], L, [req_type(Req) | C], C).
req_type_phrase([Req, price, for | L], L, [req_type(Req) | C], C).

% Coin names are converted to standard codes for the API call
coin_name([Coin | L], L, [coin(Code) | C], C) :- 
    downcase_atom(Coin, CoinLowerCase),
    coin_code(CoinLowerCase, Code).

% Optional phrases specify markets and currencies, e.g.
% What is the price of Bitcoin in USD? or What is the volume of Bitcoin at BitFinex?
optional_phrase([], _, C, C).
optional_phrase([?], _, C, C).
optional_phrase([.], _, C, C).

optional_phrase([in | L], LEnd, C0, CEnd) :-
    currency_name(L, R, C0, C1),
    optional_phrase(R, LEnd, C1, CEnd).

optional_phrase([at, Market | L], LEnd, [market(Market) | C0], CEnd) :-
    optional_phrase(L, LEnd, C0, CEnd).

% Currency names get converted to standard codes for the API call
currency_name([Currency | L], L, [currency(Code) | C], C) :-
    downcase_atom(Currency, CurrencyLowerCase),
    currency_code(CurrencyLowerCase, Code).

currency_name([Country, 'Dollars' | L], L, [currency(Code) | C], C) :-
    downcase_atom(Country, CountryLowerCase),
    currency_code([CountryLowerCase, dollars], Code).

currency_name([Country, 'dollars' | L], L, [currency(Code) | C], C) :-
    downcase_atom(Country, CountryLowerCase),
    currency_code([CountryLowerCase, dollars], Code).


% Supported cryptocurrencies
coin_code(bitcoin, btc).
coin_code(btc, btc).
coin_code(ethereum, eth).
coin_code(eth, eth).
coin_code(litecoin, ltc).
coin_code(ltc, ltc).
coin_code(peercoin, ppc).
coin_code(ppc, ppc).
coin_code(dogecoin, dode).
coin_code(dode, dode).
coin_code(ripple, xrp).
coin_code(xrp, xrp).
coin_code(nxt, nxt).
coin_code(auroracoin, aur).
coin_code(aur, aur).
coin_code(dash, dash).
coin_code(neo, neo).
coin_code(monero, xmr).
coin_code(xmr, xmr).
coin_code(verge, xvg).
coin_code(xvg, xvg).
coin_code(stellar, xlm).
coin_code(xlm, xlm).
coin_code(nano, nano).
coin_code(tether, usdt).
coin_code(usdt, usdt).
coin_code(zcash, zec).
coin_code(zec, zec).
coin_code(cardano, ada).
coin_code(ada, ada).

% Supported currencies
currency_code(usd, usd).
currency_code([us, dollars], usd).
currency_code(cad, cad).
currency_code([canadian, dollars], cad).
currency_code(eur, eur).
currency_code(euro, eur).
currency_code(gbp, gbp).
currency_code(pounds, gbp).
currency_code(jpy, jpy).
currency_code(yen, jpy).
currency_code(aud, aud).
currency_code([australian, dollars], aud).
currency_code(rub, rub).
currency_code(rubles, rub).
currency_code(brl, brl).
currency_code(reals, brl).
currency_code(idr, idr).
currency_code(rupees, idr).