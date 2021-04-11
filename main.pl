:- use_module(api).

main :-
    repeat,
        write("Ask a question (or type 'quit' to exit): "),
        flush_output(current_output),
        readln(Ln),
        check_answer(Ln),
        writeln("\n"),
        fail.

check_answer([quit]) :- writeln("Goodbye."), abort.

check_answer(Ln) :-
    (
        fetch_answer(Ln, Answer)
    ->
        write("The answer is: "), writeln(Answer)
    ;
        writeln("I'm sorry, I couldn't find an answer to that question.")
    ).
