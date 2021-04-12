:- use_module(api).

main :-
    repeat,
        writeln("Ask a question (or type 'quit' to exit): "),
        flush_output(current_output),
        readln(Ln),
        check_answer(Ln),
        writeln("\n"),
        fail.

check_answer([quit]) :- writeln("Goodbye."), abort.

check_answer(Ln) :-
    (
        findall(A, fetch_answer(Ln, A), Answers),
        length(Answers, L),
        0 < L
    ->
        atomic_list_concat(Answers, ', ', R),
        write("The answer is: "), writeln(R)
    ;
        writeln("I'm sorry, I couldn't find an answer to that question.")
    ).
