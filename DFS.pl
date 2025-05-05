:- use_module(library(clpfd)).
:- use_module(library(http/json)).

% --- Color list (1-Red,2-Blue,3-Green,4-Yellow) ---
color(1). color(2). color(3). color(4).

% --- Nodes in graph ---
node(a). node(b). node(c). node(d). node(e).
node(f). node(g). node(h).

% --- Edges (undirected) ---
edge(a,b). edge(a,c).
edge(b,c). edge(b,d).
edge(c,d). edge(c,e).
edge(d,f).
edge(e,f). edge(e,g).
edge(f,h).

% --- Main predicate to color the graph ---
colorGraph(ColorList) :-
    findall(X, node(X), Nodes),
    colorGraphDFS(Nodes, [], ColorList).

% --- DFS-Based Graph Coloring ---
colorGraphDFS([], Acc, Acc).
colorGraphDFS([Node|Rest], Acc, ColorList) :-
    color(Color),
    \+ adjacentWithSameColor(Node, Color, Acc),
    colorGraphDFS(Rest, [hasColor(Node, Color)|Acc], ColorList).

% --- Neighbor with same color check ---
adjacentWithSameColor(Node, Color, ColorList) :-
    edge(Node, Neighbor),
    member(hasColor(Neighbor, Color), ColorList).
adjacentWithSameColor(Node, Color, ColorList) :-
    edge(Neighbor, Node),
    member(hasColor(Neighbor, Color), ColorList).

% --- Print coloring result to terminal ---
printColorList([]).
printColorList([hasColor(Node, Color)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, Color]),
    printColorList(Rest).

% --- Convert result to JSON-compatible format ---
convert_to_json(ColorList, json([colors=JsonColors])) :-
    maplist(color_to_json, ColorList, JsonColors).

color_to_json(hasColor(Node, Color), json([node=Node, color=Color])).

% --- Save to JSON file ---
saveJson(File) :-
    colorGraph(ColorList),
    convert_to_json(ColorList, Json),
    open(File, write, Stream),
    json_write(Stream, Json),
    close(Stream),
    format('Saved coloring result to ~w~n', [File]).
