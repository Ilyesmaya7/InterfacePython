:- use_module(library(clpfd)).
:- use_module(library(http/json)).

% --- Colors ---
color(1).  % Red
color(2).  % Blue
color(3).  % Green
color(4).  % Yellow

% --- Nodes ---
node(a). node(b). node(c). node(d). node(e).
node(f). node(g). node(h).

% --- Edges (undirected) ---
edge(a,b). edge(a,c).
edge(a,d). edge(a,e).
edge(b,c). edge(c,d).
edge(b,d).

edge(c,d). edge(c,e).
edge(d,f).
edge(e,f). edge(e,g).
edge(f,h).

% --- Run and Show Result in Console ---
% Takes a list of nodes and colors them.
colorGraph(Nodes) :-
    assignColors(Nodes, [], ColorList),
    printColorList(ColorList).

% --- Entry Point to Assign and Save as JSON ---
% Takes a list of nodes and saves the coloring to a JSON file.
saveToJson(File) :-
    findall(Node, node(Node), Nodes),     % Get all nodes
    assignColors(Nodes, [], ColorList),   % Color the nodes
    convert_to_json(ColorList, JsonData), % Convert to JSON
    open(File, write, Stream),             % Open the file for writing
    json_write(Stream, JsonData),          % Write JSON data to file
    close(Stream),                         % Close the stream
    format('Coloring saved to ~w~n', [File]).

% --- Backtracking Assignment ---
% Base case: No nodes left, return accumulated color assignments.
assignColors([], ColorList, ColorList).
% Recursive case: Try assigning colors to remaining nodes.
assignColors([Node|Rest], AssignedSoFar, ColorList) :-
    color(Color),                                      % Pick a color
    \+ hasConflict(Node, Color, AssignedSoFar),         % Check no conflict with neighbors
    assignColors(Rest, [hasColor(Node, Color)|AssignedSoFar], ColorList).  % Assign color

% --- Conflict Checker ---
% Checks if the current node conflicts with neighbors (i.e., has the same color).
hasConflict(Node, Color, ColorList) :-
    edge(Node, Neighbor),
    member(hasColor(Neighbor, Color), ColorList).
hasConflict(Node, Color, ColorList) :-
    edge(Neighbor, Node),
    member(hasColor(Neighbor, Color), ColorList).

% --- Convert to JSON ---
convert_to_json(ColorList, json([colors=JsonColors])) :-
    maplist(color_to_json, ColorList, JsonColors).

color_to_json(hasColor(Node, Color), json([node=Node, color=Color])).
    
% --- Result Printer (for console use) ---
% Prints the color assignment for each node in the console.
printColorList([]).  
printColorList([hasColor(Node, Color)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, Color]),
    printColorList(Rest).
