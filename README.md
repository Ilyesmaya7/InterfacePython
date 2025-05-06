Graph Coloring Application
This project provides a graphical tool to create, edit, and visualize undirected graphs, along with three algorithms—Backtracking, DFS (Depth-First Search), and Greedy—to color the graph such that no adjacent nodes share the same color. The visualization is built in Python using Tkinter, while the coloring algorithms are implemented in Prolog.
Features

Graph Editor: Create and edit graphs by adding nodes and edges, drag nodes to reposition, and save graphs.
Graph Visualization: Display graphs with nodes as colored circles and edges as lines, updating colors after algorithm execution.
Coloring Algorithms: Solve the graph coloring problem using Backtracking, DFS, or Greedy algorithms, with results saved to JSON files.
Input/Output: Graphs are saved in JSON format, and coloring results are both displayed and saved.

Requirements

Python: Install tkinter (usually included with Python) and pyswip (pip install pyswip).
Prolog: SWI-Prolog must be installed for the coloring algorithms.
Files:
main.py: The Python GUI for graph creation and visualization.
dfs.pl, backtracking.pl, greedy.pl: Prolog files implementing the coloring algorithms.
Input: graphs.json (simple graph structure) and graphs_with_pos.json (graph with node positions).
Output: DFS_coloring.json, BACKTRACKING_coloring.json, GREEDY_coloring.json.



Graph Format
The GUI generates two JSON files:

graphs.json (for Prolog algorithms):
"sommets": List of node names (strings).
"links": Dictionary where each key is a node, and its value is a list of adjacent nodes.

{
  "sommets": ["A", "B", "C"],
  "links": {
    "A": ["B", "C"],
    "B": ["A", "C"],
    "C": ["A", "B"]
  }
}


graphs_with_pos.json (for visualization):
Includes nodes, edges, node positions, and colors.

{
  "nodes": ["A", "B", "C"],
  "edges": [["A", "B"], ["B", "C"], ["A", "C"]],
  "positions": {"A": [100, 100], "B": [150, 150], "C": [200, 100]},
  "colors": {"A": "red", "B": "blue", "C": "green"}
}



Available colors: red, blue, green, yellow (default: lightgray before coloring).
Using the Application

Run the GUI:
Execute python main.py.
The window shows a canvas (left) for graph visualization and a sidebar (right) for controls.


Create/Edit Graph:
Add Node: Click "Add Node" and enter a unique name. Nodes appear as circles.
Add Edge: Click "Add Edge" and enter two node names to connect them with a line.
Move Nodes: Click and drag nodes to reposition them.
Save Graph: Click "Save Graph" to update graphs.json and graphs_with_pos.json.
Clear Graph: Click "Clear Graph" to reset everything.


Solve Graph:
Click "Solve Graph" to open a dialog.
Select an algorithm (DFS, Backtracking, Greedy) and click "Solve".
The graph is colored, results are printed to the console, and colors are saved to graphs_with_pos.json and the algorithm’s output file (e.g., DFS_coloring.json).


View Results:
The canvas updates with colored nodes.
Check the output JSON file for the coloring (e.g., {"colors": [{"node": "A", "color": "red"}, ...]}).



Algorithms
1. Backtracking

Description: Tries all possible color combinations, backtracking if a conflict (adjacent nodes with the same color) occurs.
Process: Assigns a color to a node, moves to the next, and backtracks to try another color if no valid option exists.
Pros: Finds a solution if one exists; can explore all possible colorings.
Cons: Slow for large graphs due to exhaustive search.
Prolog File: backtracking.pl
Output: BACKTRACKING_coloring.json

2. DFS (Depth-First Search)

Description: Colors nodes using a depth-first traversal, assigning the first valid color that doesn’t conflict with neighbors.
Process: Picks a node, assigns a color, and moves to the next node in DFS order, trying different colors if needed.
Pros: Faster than Backtracking; simple to implement.
Cons: May not find optimal solutions; depends on node order.
Prolog File: dfs.pl
Output: DFS_coloring.json

3. Greedy

Description: Sequentially assigns the first valid color to each node based on neighbors’ colors, without backtracking.
Process: Takes nodes in order, picks the first non-conflicting color, and continues.
Pros: Very fast; works well for sparse graphs.
Cons: May use more colors than necessary; sensitive to node order.
Prolog File: greedy.pl
Output: GREEDY_coloring.json

Notes

The graph is undirected (edges are bidirectional).
Ensure Prolog files are in the same directory as main.py.
If the graph is empty or invalid, the GUI shows an error.
Modify the color/2 facts in Prolog files to change available colors.
For large graphs, Greedy is fastest, DFS is moderate, and Backtracking is slowest but thorough.

