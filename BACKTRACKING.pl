% --- 1. Déclaration du module et importation des bibliothèques ---
% Cette section définit le module et importe les bibliothèques nécessaires pour la gestion des contraintes, la lecture/écriture JSON et la manipulation dynamique des faits.
:- module(backtracking, [backtracking_color_graph/0]).

% Importation de modules de clpfd qui est requis pour la gestion de contraintes logiques sur les entiers
:- use_module(library(clpfd)). 

% --- Importation de 2 modules de json qui sont requis pour la lecture et l écriture de fichiers json
:- use_module(library(http/json)). 
:- use_module(library(http/json_convert)).  

% nodes dynamic pour informer l utilisateur que ces predicats peuvent changer pendant l execution du programme source : https://www.swi-prolog.org/pldoc/man?predicate=dynamic/1
:- dynamic node/1.
:- dynamic edge/2.

% --- 2. Définition des couleurs ---
% Cette section définit les couleurs disponibles pour colorer les nœuds du graphe, associant un identifiant numérique à un nom de couleur.
color(1, red).
color(2, blue).
color(3, green).
color(4, yellow).

% --- 3. Chargement du graphe depuis un fichier JSON ---
% Cette section lit un fichier JSON contenant les nœuds ("sommets") et les arêtes ("links") du graphe, gère les erreurs et stocke les données dans la base de faits.
load_graph(File) :-
    % catch pour gerer les erreur en le process de la lecture du fichier Json source : https://www.swi-prolog.org/pldoc/man?predicate=catch/3
    ( catch(open(File, read, Stream), Error, (format('Error opening file ~w: ~w~n', [File, Error]), fail)) ->
        catch(json_read_dict(Stream, Dict), Error, (close(Stream), format('Error parsing JSON: ~w~n', [Error]), fail)),
        close(Stream),  
        retractall(node(_)), % initialisation de nodes 
        retractall(edge(_, _)), % initialisation de edges 
        ( get_dict(sommets, Dict, Nodes) -> true ; format('Error: JSON missing "sommets" key~n'), fail ),
        ( get_dict(links, Dict, Links) -> true ; format('Error: JSON missing "links" key~n'), fail ),
        ( is_list(Nodes) -> maplist(assert_node, Nodes) ; format('Error: "sommets" must be a list~n'), fail ),
        ( dict_pairs(Links, _, Pairs) -> maplist(assert_edges, Pairs) ; format('Error: "links" must be a dictionary~n'), fail )
    ; format('Failed to load graph from ~w~n', [File]), fail % alternative error message dans le cas ou le fichier n existe pas ou problem de lecture
    ).

% --- 4. Gestion des nœuds ---
% Cette section ajoute les nœuds du graphe à la base de faits, en assurant qu’il n’y a pas de doublons.
assert_node(NodeStr) :- % ajouter un node dans le base de faits 
    atom_string(Node, NodeStr), % conversion de NodeStr en atom source : https://www.swi-prolog.org/pldoc/man?predicate=atom_string/2
    ( node(Node) -> true ; assertz(node(Node)) ). % ce ligne pour verifier l unicite de nodes avant de l ajouter dans le base def faits

% --- 5. Gestion des arêtes ---
% Cette section ajoute les arêtes du graphe à la base de faits, en créant des connexions bidirectionnelles pour un graphe non orienté.
assert_edges(FromStr-ToList) :- % ajouter un edge dans le base de faits
    atom_string(From, FromStr), % conversion de FromStr en atom source 
    % ajout des aretes entre les nodes connecter dans une liste dans le json file 
    ( is_list(ToList) ->
        forall(member(ToStr, ToList), (
            atom_string(To, ToStr),
            assert_edge(From, To),
            assert_edge(To, From)  % Ensure undirected graph
        ))
    ; format('Error: Edges for node ~w must be a list~n', [From]), fail
    ).

% ajouter le lien entre 2 nodes utiliser dans le predicat precedent 
assert_edge(From, To) :-
    ( edge(From, To) -> true ; assertz(edge(From, To)) ).

% --- 6. Coloration du graphe et affichage ---
% Cette section applique l’algorithme de coloration et affiche les résultats à l’écran.
colorGraph :-
    findall(Node, node(Node), Nodes),
    ( Nodes = [] -> format('Error: No nodes in graph~n'), fail ; true ),
    color_nodes(Nodes, ColorAssignments),
    printColorList(ColorAssignments).

% --- 7. Sauvegarde des résultats dans un fichier JSON ---
% Cette section sauvegarde la coloration du graphe dans un fichier JSON, en ne prenant que la première solution trouvée.
saveToJson(File) :-
    findall(Node, node(Node), Nodes),
    ( Nodes = [] -> format('Error: No nodes in graph~n'), fail ; true ),
    once(color_nodes(Nodes, ColorAssignments)),  % Only take the first solution!
    convert_to_json(ColorAssignments, JsonData),
    ( catch(open(File, write, Stream), Error, (format('Error opening file ~w: ~w~n', [File, Error]), fail)) ->
        json_write(Stream, JsonData),
        close(Stream),
        format('Coloring saved to ~w~n', [File])
    ; format('Failed to save coloring to ~w~n', [File]), fail
    ).

% --- 8. Algorithme de coloration avec CLP(FD) ---
% Cette section implémente la coloration du graphe à l’aide de contraintes logiques (CLP(FD)) pour assigner des couleurs aux nœuds.
color_nodes(Nodes, ColorAssignments) :-
    length(Nodes, N),
    findall(ColorID, color(ColorID, _), Colors),
    length(Colors, NumColors),
    length(ColorVars, N),  % One variable per node
    ColorVars ins 1..NumColors,  % Domain: available color IDs
    impose_constraints(Nodes, ColorVars),
    label(ColorVars),  % Find a solution
    pair_nodes_colors(Nodes, ColorVars, ColorAssignments).

% --- 9. Application des contraintes ---
% Cette section définit les contraintes pour assurer que les nœuds adjacents ont des couleurs différentes.
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

% --- 10. Association des nœuds aux couleurs ---
% Cette section associe les identifiants de couleurs aux noms de couleurs pour produire le résultat final.
pair_nodes_colors([], [], []).
pair_nodes_colors([Node|Nodes], [ColorID|ColorVars], [hasColor(Node, ColorName)|Rest]) :-
    color(ColorID, ColorName),
    pair_nodes_colors(Nodes, ColorVars, Rest).

% --- 11. Conversion en JSON ---
% Cette section convertit les résultats de la coloration en un format JSON pour la sauvegarde.
convert_to_json(ColorAssignments, json([colors=JsonColors])) :-
    maplist(color_to_json, ColorAssignments, JsonColors).

color_to_json(hasColor(Node, ColorName), json([node=Node, color=ColorName])).

% --- 12. Affichage des résultats ---
% Cette section affiche les couleurs assignées à chaque nœud dans la console.
printColorList([]).
printColorList([hasColor(Node, ColorName)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, ColorName]),
    printColorList(Rest).

% --- 13. Point d’entrée principal ---
% Cette section est le point d’entrée du programme, qui charge le graphe, le colore et sauvegarde les résultats.
backtracking_color_graph :-
    load_graph('graphs.json'),
    colorGraph,
    saveToJson('BACKTRACKING_coloring.json').