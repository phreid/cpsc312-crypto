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

% filter_markets([Dicts], Market, Dict) is true if Dict is a dictionary
% in [Dicts] where Dict[market] is equal to Market. Market has to be a string.
filter_markets([], _, _{}).
filter_markets([D|_], Market, D) :-
    get_dict(market, D, Market).
filter_markets([D|T], Market, D1) :-
    get_dict(market, D, M1),
    dif(Market, M1),
    filter_markets(T, Market, D1).
