% --- 1. Déclaration du module et importation des bibliothèques ---
% Cette section définit le module 'dfs' et importe les bibliothèques nécessaires pour la lecture et l'écriture de fichiers JSON, ainsi que pour la gestion dynamique des faits.
:- module(dfs, [dfs_color_graph/0]).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% Déclaration des prédicats dynamiques pour permettre la modification des nœuds et arêtes pendant l'exécution.
:- dynamic node/1.
:- dynamic edge/2.

% --- 2. Définition des couleurs ---
% Cette section associe des identifiants numériques à des noms de couleurs pour la coloration des nœuds.
color(1, red).
color(2, blue).
color(3, green).
color(4, yellow).

% --- 3. Chargement du graphe depuis un fichier JSON ---
% Cette section lit un fichier JSON contenant les nœuds ("sommets") et les arêtes ("links"), gère les erreurs et stocke les données dans la base de faits.
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

% --- 4. Gestion des nœuds ---
% Cette section ajoute les nœuds du graphe à la base de faits, en évitant les doublons.
assert_node(NodeStr) :-
    atom_string(Node, NodeStr),
    ( node(Node) -> true ; assertz(node(Node)) ).

% --- 5. Gestion des arêtes ---
% Cette section crée des arêtes bidirectionnelles pour un graphe non orienté, en vérifiant la validité des données JSON.
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

% Ajoute une arête à la base de faits, en évitant les doublons.
assert_edge(From, To) :-
    ( edge(From, To) -> true ; assertz(edge(From, To)) ).

% --- 6. Coloration du graphe et affichage ---
% Cette section applique l’algorithme de coloration basé sur DFS et affiche les résultats dans la console.
colorGraph :-
    findall(Node, node(Node), Nodes),
    ( Nodes = [] -> format('Error: No nodes in graph~n'), fail ; true ),
    colorGraphDFS(Nodes, [], ColorList),
    printColorList(ColorList).

% --- 7. Algorithme de coloration DFS ---
% Cette section implémente la coloration du graphe en utilisant un parcours en profondeur (DFS) pour assigner des couleurs.
colorGraphDFS([], Acc, Acc).
colorGraphDFS([Node|Rest], Acc, ColorList) :-
    findall(ColorID, color(ColorID, _), Colors),
    member(ColorID, Colors),
    color(ColorID, ColorName),
    \+ adjacentWithSameColor(Node, ColorID, Acc),
    colorGraphDFS(Rest, [hasColor(Node, ColorName)|Acc], ColorList).

% --- 8. Vérification des nœuds adjacents ---
% Cette section vérifie si un nœud adjacent a la meme couleur, pour éviter les conflits dans la coloration.
adjacentWithSameColor(Node, Color, ColorList) :-
    edge(Node, Neighbor),
    member(hasColor(Neighbor, ColorName), ColorList),
    color(Color, ColorName).
adjacentWithSameColor(Node, Color, ColorList) :-
    edge(Neighbor, Node),
    member(hasColor(Neighbor, ColorName), ColorList),
    color(Color, ColorName).

% --- 9. Affichage des résultats ---
% Cette section affiche les couleurs assignées à chaque nœud dans la console.
printColorList([]).
printColorList([hasColor(Node, ColorName)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, ColorName]),
    printColorList(Rest).

% --- 10. Conversion en JSON ---
% Cette section convertit les résultats de la coloration en un format JSON pour la sauvegarde.
convert_to_json(ColorList, json([colors=JsonColors])) :-
    maplist(color_to_json, ColorList, JsonColors).

color_to_json(hasColor(Node, ColorName), json([node=Node, color=ColorName])).

% --- 11. Sauvegarde des résultats dans un fichier JSON ---
% Cette section sauvegarde la coloration du graphe dans un fichier JSON, en ne prenant que la première solution.
saveJson(File) :-
    findall(Node, node(Node), Nodes),
    ( Nodes = [] -> format('Error: No nodes in graph~n'), fail ; true ),
    once(colorGraphDFS(Nodes, [], ColorList)),  % Only take the first solution!
    convert_to_json(ColorList, Json),
    ( catch(open(File, write, Stream), Error, (format('Error opening file ~w: ~w~n', [File, Error]), fail)) ->
        json_write(Stream, Json),
        close(Stream),
        format('Saved coloring result to ~w~n', [File])
    ; format('Failed to save coloring to ~w~n', [File]), fail
    ).

% --- 12. Point d’entrée principal ---
% Cette section est le point d’entrée du programme, qui charge le graphe, le colore avec DFS et sauvegarde les résultats.
dfs_color_graph :-
    load_graph('graphs.json'),
    colorGraph,
    saveJson('DFS_coloring.json').