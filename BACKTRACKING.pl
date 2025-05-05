:- use_module(library(clpfd)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- dynamic node/1.
:- dynamic edge/2.

% --- Color Definitions ---
color(1, red).
color(2, blue).
color(3, green).
color(4, yellow).

% --- Load Graph from JSON ---
load_graph(File) :-
    ( catch(open(File, read, Stream), Error, (format('Error opening file ~w: ~w~n', [File, Error]), fail)) ->
        catch(json_read_dict(Stream, Dict), Error, (close(Stream), format('Error parsing JSON: ~w~n', [Error]), fail)),
        close(Stream),
        retractall(node(_)),
        retractall(edge(_, _)),
        ( get_dict(sommets, Dict, Nodes) -> true ; format('Error: JSON missing "sommets" key~n'), fail ),
        ( get_dict(links, Dict, Links) -> true ; format('Error: JSON missing "links" key~n'), fail ),
        ( is_list(Nodes) -> maplist(assert_node, Nodes) ; format('Error: "sommets" must be a list~n'), fail ),
        ( dict_pairs(Links, _, Pairs) -> maplist(assert_edges, Pairs) ; format('Error: "links" must be a dictionary~n'), fail )
    ; format('Failed to load graph from ~w~n', [File]), fail
    ).

assert_node(NodeStr) :-
    atom_string(Node, NodeStr),
    ( node(Node) -> true ; assertz(node(Node)) ).

assert_edges(FromStr-ToList) :-
    atom_string(From, FromStr),
    ( is_list(ToList) ->
        forall(member(ToStr, ToList), (
            atom_string(To, ToStr),
            assert_edge(From, To),
            assert_edge(To, From)  % Ensure undirected graph
        ))
    ; format('Error: Edges for node ~w must be a list~n', [From]), fail
    ).

assert_edge(From, To) :-
    ( edge(From, To) -> true ; assertz(edge(From, To)) ).

% --- Run and Show Result in Console ---
colorGraph :-
    findall(Node, node(Node), Nodes),
    ( Nodes = [] -> format('Error: No nodes in graph~n'), fail ; true ),
    color_nodes(Nodes, ColorAssignments),
    printColorList(ColorAssignments).

% --- Entry Point to Assign and Save as JSON ---
saveToJson(File) :-
    findall(Node, node(Node), Nodes),
    ( Nodes = [] -> format('Error: No nodes in graph~n'), fail ; true ),
    color_nodes(Nodes, ColorAssignments),
    convert_to_json(ColorAssignments, JsonData),
    ( catch(open(File, write, Stream), Error, (format('Error opening file ~w: ~w~n', [File, Error]), fail)) ->
        json_write(Stream, JsonData),
        close(Stream),
        format('Coloring saved to ~w~n', [File])
    ; format('Failed to save coloring to ~w~n', [File]), fail
    ).

% --- CLP(FD)-Based Coloring ---
color_nodes(Nodes, ColorAssignments) :-
    length(Nodes, N),
    findall(ColorID, color(ColorID, _), Colors),
    length(Colors, NumColors),
    length(ColorVars, N),  % One variable per node
    ColorVars ins 1..NumColors,  % Domain: available color IDs
    impose_constraints(Nodes, ColorVars),
    label(ColorVars),  % Find a solution
    pair_nodes_colors(Nodes, ColorVars, ColorAssignments).

impose_constraints(Nodes, ColorVars) :-
    maplist(impose_node_constraints(Nodes, ColorVars), Nodes).

impose_node_constraints(Nodes, ColorVars, Node) :-
    nth1(Index, Nodes, Node),
    nth1(Index, ColorVars, ColorVar),
    findall(NeighborIndex, (
        (edge(Node, Neighbor); edge(Neighbor, Node)),
        nth1(NeighborIndex, Nodes, Neighbor)
    ), NeighborIndices),
    maplist(constrain_different(ColorVar, ColorVars), NeighborIndices).

constrain_different(ColorVar, ColorVars, NeighborIndex) :-
    nth1(NeighborIndex, ColorVars, NeighborColorVar),
    ColorVar #\= NeighborColorVar.

pair_nodes_colors([], [], []).
pair_nodes_colors([Node|Nodes], [ColorID|ColorVars], [hasColor(Node, ColorName)|Rest]) :-
    color(ColorID, ColorName),
    pair_nodes_colors(Nodes, ColorVars, Rest).

% --- Convert to JSON ---
convert_to_json(ColorAssignments, json([colors=JsonColors])) :-
    maplist(color_to_json, ColorAssignments, JsonColors).

color_to_json(hasColor(Node, ColorName), json([node=Node, color=ColorName])).

% --- Result Printer ---
printColorList([]).
printColorList([hasColor(Node, ColorName)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, ColorName]),
    printColorList(Rest).

% --- Example Usage ---
:- initialization((
    load_graph('graphs.json'),
    colorGraph,
    saveToJson('BACKTRACKING_coloring.json')
)).