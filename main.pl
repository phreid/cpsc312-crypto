:- use_module(api).

main(Result) :-
    write("Ask a question: "),
    flush_output(current_output),
    readln(Ln),
    fetch_answer(Ln, Result).
