% filepath: c:\Users\gamer\OneDrive\Desktop\PProlog25-IlyesMaya-OubaidBeldi-KhadraouiOussama\PProlog25-IlyesMaya-OubaidBeldi-KhadraouiOussama\InterfacePython\main.pl
:- use_module(library(statistics)).

% --- Include Algorithm Modules ---
:- use_module(dfs).
:- use_module(backtracking).
:- use_module(greedy).

% --- Run and Time Algorithms ---
run_algorithm(Algorithm, Time) :-
    format('Running ~w...~n', [Algorithm]),
    statistics(walltime, [Start|_]),
    ( call(Algorithm) -> true ; format('Error running ~w~n', [Algorithm]) ),
    statistics(walltime, [End|_]),
    Time is End - Start,
    format('~w completed in ~w ms~n', [Algorithm, Time]).

% --- Main Entry Point --- :- initialization(main).

main :-
    % Define algorithms to test
    Algorithms = [
        dfs:dfs_color_graph,          % DFS algorithm
        backtracking:backtracking_color_graph,  % Backtracking algorithm
        greedy:greedy_color_graph     % Greedy algorithm
    ],

    % Run and time each algorithm
    findall(Time-Algorithm, (
        member(Algorithm, Algorithms),
        run_algorithm(Algorithm, Time)
    ), Results),

    % Sort results by time
    sort(Results, SortedResults),

    % Display ranking
    format('Algorithm Performance Ranking:~n'),
    forall(member(Time-Algorithm, SortedResults), (
        format('~w: ~w ms~n', [Algorithm, Time])
    )),

    halt.