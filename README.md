Graph Coloring Algorithms
This project implements three graph coloring algorithms—Backtracking, DFS, and Greedy—to assign colors to nodes in an undirected graph such that no adjacent nodes share the same color. The algorithms use a JSON file (graphs.json) for input, process the graph, and output the coloring results to the console and a JSON file.
Input Format
The input file graphs.json should contain:

"sommets": A list of node names (strings).
"links": A dictionary where each key is a node, and its value is a list of adjacent nodes.

Example:
{
  "sommets": ["A", "B", "C"],
  "links": {
    "A": ["B", "C"],
    "B": ["A", "C"],
    "C": ["A", "B"]
  }
}

Available colors are red, blue, green, and yellow.
Algorithms
1. Backtracking
Description: The Backtracking algorithm tries all possible color combinations for the nodes, ensuring no adjacent nodes have the same color. It assigns a color to a node, moves to the next, and backtracks if a conflict arises, trying different colors until a valid solution is found or all options are exhausted.
How It Works:

Start with the first node and assign a color.
Move to the next node, picking a color that doesn't conflict with adjacent nodes.
If no valid color is found, backtrack to the previous node and try a different color.
Repeat until all nodes are colored or no solution exists.

Usage:

Run the Backtracking algorithm (e.g., backtrack_color_graph/0 in a Prolog implementation).
Output: Colors printed to the console and saved to backtrack_coloring.json.
Pros: Guarantees a solution if one exists; can find all possible colorings.
Cons: Can be slow for large graphs due to exhaustive search.

2. DFS (Depth-First Search)
Description: The DFS-based algorithm colors the graph by exploring nodes in a depth-first manner. It assigns the first valid color to each node, checking adjacent nodes to avoid conflicts, and continues until all nodes are colored.
How It Works:

Pick a node and assign the first color that doesn't conflict with its neighbors.
Move to the next node in the graph, following a depth-first traversal.
If a conflict is found, try the next available color for the current node.
Continue until all nodes are colored.

Usage:

Run the DFS algorithm (e.g., dfs_color_graph/0 in the provided Prolog code).
Output: Colors printed to the console and saved to DFS_coloring.json.
Pros: Faster than Backtracking for many graphs; simple to implement.
Cons: May not find all solutions; solution quality depends on node order.

3. Greedy
Description: The Greedy algorithm assigns colors to nodes sequentially, choosing the first valid color for each node based on the colors of its already-colored neighbors. It doesn't backtrack or explore alternative solutions.
How It Works:

Take nodes in a specific order (e.g., as listed in the input).
For each node, assign the first color that doesn't conflict with its neighbors.
Move to the next node and repeat until all nodes are colored.

Usage:

Run the Greedy algorithm (e.g., greedy_color_graph/0 in a Prolog implementation).
Output: Colors printed to the console and saved to greedy_coloring.json.
Pros: Very fast, even for large graphs; simple and efficient.
Cons: May use more colors than necessary; solution depends heavily on node order.

Running the Algorithms

Ensure graphs.json is in the working directory with the correct format.
Load the Prolog program for the desired algorithm.
Call the main predicate (e.g., dfs_color_graph/0 for DFS).
Check the console for color assignments and the output JSON file (e.g., DFS_coloring.json) for results.

Notes

The algorithms assume an undirected graph (edges are bidirectional).
If the graph has no nodes or invalid input, an error message is displayed.
The number of colors is fixed (red, blue, green, yellow), but you can modify the color definitions in the code.
For large graphs, Greedy is fastest, followed by DFS, while Backtracking may be slower but more thorough.

