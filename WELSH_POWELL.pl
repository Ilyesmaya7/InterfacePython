% this to be fixed or to be checked need more studies 

% --- Color List (1-Red, 2-Blue, 3-Green, 4-Yellow) ---
color(1).
color(2).
color(3).
color(4).

% --- Nodes in the Graph ---
node(a). node(b). node(c). node(d). node(e).
node(f). node(g). node(h).

% --- Edges in the Graph ---
edge(a,b). edge(a,c). edge(a,e). edge(a,h).
edge(b,c). edge(b,d). edge(c,d). edge(c,e).
edge(d,f). edge(e,f). edge(e,g). edge(f,h).

% --- Main Entry Point for Graph Coloring ---
colorGraph(ColorList) :-
    findall(Node, node(Node), Nodes),             % Find all nodes in the graph
    welsh_powell_algorithm(Nodes, [], ColorList).  % Apply Welsh-Powell algorithm for coloring

% --- Welsh-Powell Algorithm ---
welsh_powell_algorithm([], Colored, Colored).  % Base case: no more nodes to color
welsh_powell_algorithm([Node|Rest], ColoredSoFar, Result) :-
    node_degree(Node, Degree),                % Get the degree of the current node
    available_color(Node, ColoredSoFar, Color), % Find a valid color for the node
    welsh_powell_algorithm(Rest, [hasColor(Node, Color) | ColoredSoFar], Result). % Recur on the rest of the nodes

% --- Node Degree ---
% Calculates the degree of a node, i.e., the number of edges connected to it
node_degree(Node, Degree) :-
    findall(Neighbor, edge(Node, Neighbor), Neighbors), % Get all neighbors
    length(Neighbors, Degree).                         % The degree is the number of neighbors

% --- Available Color ---
% Finds the smallest color not already used by adjacent nodes
available_color(Node, ColoredSoFar, Color) :-
    color(Color),                                % Iterate through available colors
    \+ member(hasColor(Node, Color), ColoredSoFar),   % Make sure the node doesnt have this color
    \+ adjacent_with_same_color(Node, Color, ColoredSoFar). % Check that no adjacent node has the same color

% --- Adjacent with Same Color ---
% Checks if any adjacent node has the same color
adjacent_with_same_color(Node, Color, ColoredSoFar) :-
    edge(Node, Neighbor),                              % Check if Node is adjacent to Neighbor
    member(hasColor(Neighbor, Color), ColoredSoFar).   % If Neighbor has the same color, return true
adjacent_with_same_color(Node, Color, ColoredSoFar) :-
    edge(Neighbor, Node),                              % Also check the reverse (undirected graph)
    member(hasColor(Neighbor, Color), ColoredSoFar).

% --- Print Colored Graph ---
% Helper function to print each node with its assigned color
printColorList([]).  % Base case: nothing to print
printColorList([hasColor(Node, Color)|Rest]) :- 
    format('Node ~w has color ~w~n', [Node, Color]),  % Print current node-color pair
    printColorList(Rest).                             % Recurse on the rest
