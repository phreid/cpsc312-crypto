:- module(api, [fetch_answer/2]).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(language).

api_base("https://api.cryptonator.com/api/full/").

% api_call(Coin, Currency, Result) is true if Response is the result
% of an api call to https://api.cryptonator.com/api/full/[Coin]-[Currency]
api_call(Coin, Currency, Response) :- 
    api_base(BaseUrl),
    atom_concat(Coin, "-", S1),
    atom_concat(S1, Currency, S2),
    atom_concat(BaseUrl, S2, ReqUrl),
    http_get(ReqUrl, Response, []).

% process_response(Response, Dict) is true if Dict is the value of the 
% "ticker" key in Response. False if the API call was unsuccesful. 
process_response(Response, Dict) :-
    atom_json_dict(Response, Result, []),
    get_dict(ticker, Result, Dict).

% Questions get resolved in 4 steps:
%     1. Parse the question to get the list of constraints and coin/currency params
%     2. Call the Cryptonator API with params
%     3. Process the JSON response into a dictionary
%     4. Get the answer from the dictionary based on list of constraints
fetch_answer(Question, Result) :-
    build_call_from_question(Question, Coin, Currency, Constraints),
    api_call(Coin, Currency, Response),
    process_response(Response, Dict),
    get_answer_from_dict(Dict, Constraints, Result).

% build_call_from_question(Question, Coin, Currency, Constraints) is true
% if Constraints in the list of constraints parsed from Question, and 
% Coin and Currency are the API parameters from Constraints  
build_call_from_question(Question, Coin, Currency, Constraints) :-
    question(Question, _, Constraints, []),
    member(coin(Coin), Constraints),
    member(currency(Currency), Constraints).

% Default to USD if no currency provided
build_call_from_question(Question, Coin, usd, Constraints) :-
    question(Question, _, Constraints, []),
    member(coin(Coin), Constraints),
    \+ member(currency(_), Constraints).

% get_answer_from_dict(Dict, Constraints, Result) is true if Result is the value
% we get from Dict after resolving Constraints. This predictate matches if there is 
% no market constraint. 
get_answer_from_dict(Dict, Constraints, Result) :-
    \+ member(market(_), Constraints),
    \+ member(req_type(markets), Constraints),
    \+ member(req_type(highest), Constraints),
    \+ member(req_type(lowest), Constraints),
    member(req_type(Type), Constraints),
    get_dict(Type, Dict, Result).

% This predicate matches if there is a market constraint.
get_answer_from_dict(Dict, Constraints, Result) :-
    member(market(M), Constraints),
    atom_string(M, Market),
    member(req_type(Type), Constraints),
    get_dict(markets, Dict, Dicts),
    filter_markets(Dicts, Market, D1),
    get_dict(Type, D1, Result).

% This predicate matches if there is a req_type constraint of markets.
get_answer_from_dict(Dict, Constraints, Result) :-
    member(req_type(markets), Constraints),
    get_dict(markets, Dict, Dicts),
    dicts_slice([market], Dicts, Values),
    member(A, Values),
    atom_string(A.get(market), Result).

% These two predicates match if there is a req_type constraint of 
% highest or lowest
get_answer_from_dict(Dict, Constraints, Result) :-
    member(req_type(highest), Constraints),
    get_dict(markets, Dict, Dicts),
    dicts_slice([price], Dicts, P),
    dicts_slice([market], Dicts, M),
    compare_price_highest(M, P, 0, _, Result).

get_answer_from_dict(Dict, Constraints, Result) :-
    member(req_type(lowest), Constraints),
    get_dict(markets, Dict, Dicts),
    dicts_slice([price], Dicts, P),
    dicts_slice([market], Dicts, M),
    compare_price_lowest(M, P, 1000000, _, Result).

% filter_markets([Dicts], Market, Dict) is true if Dict is a dictionary
% in [Dicts] where Dict[market] is equal to Market. Market has to be a string.
filter_markets([], _, _{}).
filter_markets([D|_], Market, D) :-
    get_dict(market, D, Market).
filter_markets([D|T], Market, D1) :-
    get_dict(market, D, M1),
    dif(Market, M1),
    filter_markets(T, Market, D1).

% compare_price_highest([M], [P], Price, _, Market) is true if Market is the market name
% with the highest price.  
compare_price_highest([M], [P], Price, _, Market) :- 
    atom_string(M.get(market), Market),
    atom_number(P.get(price), Pr),
    Pr > Price.
compare_price_highest(_, [P], Price, Market, Market) :-
    atom_number(P.get(price), Pr),
    Pr < Price.
compare_price_highest([M|M1], [P|P1], Price, _, Market) :-
    atom_string(M.get(market), Ma),
    atom_number(P.get(price), Pr),
    Pr > Price,
    compare_price_highest(M1, P1, Pr, Ma, Market).
compare_price_highest([_|M1], [P|P1], Price, M, Market) :-
    atom_number(P.get(price), Pr),
    Pr < Price,
    compare_price_highest(M1, P1, Price, M, Market).

% compare_price_lowest([M], [P], Price, _, Market) is true if Market is the market name
% with the lowest price. 
compare_price_lowest([M], [P], Price, _, Market) :- 
    atom_string(M.get(market), Market),
    atom_number(P.get(price), Pr),
    Pr < Price.
compare_price_lowest(_, [P], Price, Market, Market) :-
    atom_number(P.get(price), Pr),
    Pr > Price.
compare_price_lowest([M|M1], [P|P1], Price, _, Market) :-
    atom_string(M.get(market), Ma),
    atom_number(P.get(price), Pr),
    Pr < Price,
    compare_price_lowest(M1, P1, Pr, Ma, Market).
compare_price_lowest([_|M1], [P|P1], Price, M, Market) :-
    atom_number(P.get(price), Pr),
    Pr > Price,
    compare_price_lowest(M1, P1, Price, M, Market).