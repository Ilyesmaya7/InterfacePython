% filepath: c:\Users\gamer\OneDrive\Desktop\PProlog25-IlyesMaya-OubaidBeldi-KhadraouiOussama\PProlog25-IlyesMaya-OubaidBeldi-KhadraouiOussama\InterfacePython\GREEDY.pl
:- module(greedy, [greedy_color_graph/0]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- dynamic node/1.
:- dynamic edge/2.

% --- Color Definitions (ID, Name) ---
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

% --- Entry Point: Greedy Coloring ---
colorGraph :-
    findall(Node, node(Node), Nodes),
    ( Nodes = [] -> format('Error: No nodes in graph~n'), fail ; true ),
    greedyColorNodes(Nodes, [], ColorList),
    printColorList(ColorList).

% --- Greedy Coloring Implementation ---
greedyColorNodes([], Acc, Acc).
greedyColorNodes([Node|Rest], Acc, ColorList) :-
    findall(ColorID-ColorName, color(ColorID, ColorName), Colors),
    exclude(adjacentHasColor(Node, Acc), Colors, ValidColors),
    ValidColors = [ChosenColorID-ChosenColorName|_],  % Take the first valid color
    greedyColorNodes(Rest, [hasColor(Node, ChosenColorName)|Acc], ColorList).

% --- Check if a Color is Invalid Due to a Neighbor Using It ---
adjacentHasColor(Node, ColorList, ColorID-_ColorName) :-
    ( edge(Node, Neighbor) ; edge(Neighbor, Node) ),
    member(hasColor(Neighbor, NeighborColorName), ColorList),
    color(ColorID, NeighborColorName).

% --- Print Coloring Result to Terminal ---
printColorList([]).
printColorList([hasColor(Node, ColorName)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, ColorName]),
    printColorList(Rest).

% --- Convert Result to JSON-Compatible Format ---
convert_to_json(ColorList, json([colors=JsonColors])) :-
    maplist(color_to_json, ColorList, JsonColors).

color_to_json(hasColor(Node, ColorName), json([node=Node, color=ColorName])).

% --- Save to JSON File ---
saveJson(File) :-
    findall(Node, node(Node), Nodes),
    ( Nodes = [] -> format('Error: No nodes in graph~n'), fail ; true ),
    greedyColorNodes(Nodes, [], ColorList),
    convert_to_json(ColorList, Json),
    ( catch(open(File, write, Stream), Error, (format('Error opening file ~w: ~w~n', [File, Error]), fail)) ->
        json_write(Stream, Json),
        close(Stream),
        format('Saved coloring result to ~w~n', [File])
    ; format('Failed to save coloring to ~w~n', [File]), fail
    ).

% --- Main Predicate for Greedy ---
greedy_color_graph :-
    load_graph('graphs.json'),
    colorGraph,
    saveJson('GREEDY_coloring.json').