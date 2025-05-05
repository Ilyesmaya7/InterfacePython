import tkinter as tk
from tkinter import simpledialog, messagebox, ttk, filedialog
import json
import os
from pyswip import Prolog

GRAPH_FILE = "graphs_with_pos.json"
ALGO_OUTPUT_FILE = "algo_output.json"
PROLOG_FILE = "main.pl"
SIMPLE_GRAPH_FILE = "graphs.json"

class GraphEditor(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Graph Creator & Solver")
        self.geometry("900x600")
        self.nodes = []
        self.edges = []
        self.node_positions = {}
        self.selected_node = None
        self.canvas = tk.Canvas(self, bg="white")
        self.canvas.pack(fill=tk.BOTH, expand=True, side=tk.LEFT)
        self.sidebar = tk.Frame(self, width=200)
        self.sidebar.pack(fill=tk.Y, side=tk.RIGHT)
        self.create_sidebar()
        self.load_graph()
        self.canvas.bind("<Button-1>", self.on_canvas_click)
        self.canvas.bind("<B1-Motion>", self.on_drag)
        self.dragging_node = None

    def create_sidebar(self):
        tk.Label(self.sidebar, text="Graph Menu", font=("Arial", 14)).pack(pady=10)
        tk.Button(self.sidebar, text="Add Node", command=self.add_node_dialog).pack(fill=tk.X, pady=5)
        tk.Button(self.sidebar, text="Add Edge", command=self.add_edge_dialog).pack(fill=tk.X, pady=5)
        tk.Button(self.sidebar, text="Save Graph", command=self.save_graph).pack(fill=tk.X, pady=5)
        tk.Button(self.sidebar, text="Solve Graph", command=self.solve_graph_dialog).pack(fill=tk.X, pady=20)
        tk.Button(self.sidebar, text="Clear Graph", command=self.clear_graph).pack(fill=tk.X, pady=5)

    def add_node_dialog(self):
        name = simpledialog.askstring("Node Name", "Enter node name:")
        if name and name not in self.nodes:
            x, y = 100 + 50 * len(self.nodes), 100 + 50 * len(self.nodes)
            self.nodes.append(name)
            self.node_positions[name] = (x, y)
            self.draw_graph()
            self.save_graph()
        elif name in self.nodes:
            messagebox.showerror("Error", "Node already exists.")

    def add_edge_dialog(self):
        if len(self.nodes) < 2:
            messagebox.showerror("Error", "At least two nodes required.")
            return
        node1 = simpledialog.askstring("Edge", "Enter first node name:")
        node2 = simpledialog.askstring("Edge", "Enter second node name:")
        if node1 in self.nodes and node2 in self.nodes and node1 != node2:
            edge = (node1, node2)
            if edge not in self.edges and (node2, node1) not in self.edges:
                self.edges.append(edge)
                self.draw_graph()
                self.save_graph()
            else:
                messagebox.showerror("Error", "Edge already exists.")
        else:
            messagebox.showerror("Error", "Invalid node names.")

    def on_canvas_click(self, event):
        for node, (x, y) in self.node_positions.items():
            if (event.x - x) ** 2 + (event.y - y) ** 2 < 400:
                self.dragging_node = node
                return
        self.dragging_node = None

    def on_drag(self, event):
        if self.dragging_node:
            self.node_positions[self.dragging_node] = (event.x, event.y)
            self.draw_graph()
            self.save_graph()

    def draw_graph(self, coloring=None):
        self.canvas.delete("all")
        # Draw edges
        for n1, n2 in self.edges:
            x1, y1 = self.node_positions[n1]
            x2, y2 = self.node_positions[n2]
            self.canvas.create_line(x1, y1, x2, y2, width=2)
        # Draw nodes
        for node in self.nodes:
            x, y = self.node_positions[node]
            color = "lightgray"
            if coloring and node in coloring:
                color = coloring[node]
            self.canvas.create_oval(x-20, y-20, x+20, y+20, fill=color, outline="black", width=2)
            self.canvas.create_text(x, y, text=node, font=("Arial", 12, "bold"))

    def save_graph(self):
        # Save detailed format (with positions)
        data = {
            "nodes": self.nodes,
            "edges": self.edges,
            "positions": self.node_positions,
            "colors": {node:color for node in self.nodes for color in ["lightgray"]},
        }
        with open(GRAPH_FILE, "w") as f:
            json.dump(data, f, indent=2)

        # Save simple format for Prolog
        sommets = self.nodes
        links = {}
        for edge in self.edges:
            a, b = edge
            links.setdefault(a, []).append(b)
            links.setdefault(b, []).append(a)
        # Remove duplicates in adjacency lists
        for k in links:
            links[k] = list(sorted(set(links[k])))

        simple_data = {
            "sommets": sommets,
            "links": links
        }
        with open(SIMPLE_GRAPH_FILE, "w") as f:
            json.dump(simple_data, f, indent=2)

    def load_graph(self):
        if os.path.exists(GRAPH_FILE):
            with open(GRAPH_FILE, "r") as f:
                data = json.load(f)
                self.nodes = data.get("nodes", [])
                self.edges = [tuple(e) for e in data.get("edges", [])]
                self.node_positions = {k: tuple(v) for k, v in data.get("positions", {}).items()}
                coloring = data.get("coloring", None)
                self.draw_graph(coloring=coloring)

    def clear_graph(self):
        self.nodes = []
        self.edges = []
        self.node_positions = {}
        self.draw_graph()
        self.save_graph()

    def solve_graph_dialog(self):
        if not self.nodes or not self.edges:
            messagebox.showerror("Error", "Graph is empty.")
            return
        algo_win = tk.Toplevel(self)
        algo_win.title("Choose Algorithm")
        tk.Label(algo_win, text="Select Algorithm:").pack(pady=10)
        algo_var = tk.StringVar(value="DFS")
        for algo in ["DFS", "Backtracking", "Greedy"]:
            tk.Radiobutton(algo_win, text=algo, variable=algo_var, value=algo).pack(anchor=tk.W)
        def run_algo():
            algo = algo_var.get()
            algo_win.destroy()
            self.run_algorithm(algo)
        tk.Button(algo_win, text="Solve", command=run_algo).pack(pady=10)

    def run_algorithm(self, algo_name):
        self.save_graph()
        prolog = Prolog()
        if algo_name == "DFS":
            prolog.consult("dfs.pl") 
            output_file = "DFS_coloring.json"
            predicate = "dfs:dfs_color_graph"
        elif algo_name == "Backtracking":
            prolog.consult("backtracking.pl")
            output_file = "BACKTRACKING_coloring.json"
            predicate = "backtracking:backtracking_color_graph"
        else:
            prolog.consult("greedy.pl")
            output_file = "GREEDY_coloring.json"
            predicate = "greedy:greedy_color_graph"
        try:
            list(prolog.query(predicate))
        except Exception as e:
            messagebox.showerror("Prolog Error", str(e))
            return
        if not os.path.exists(output_file):
            messagebox.showerror("Error", f"Algorithm output '{output_file}' not found.")
            return
        # Read the new color format
        with open(output_file, "r") as f:
            result = json.load(f)
        colors = result.get("colors", [])
        # Load the current graph file
        with open(GRAPH_FILE, "r") as f:
            graph_data = json.load(f)
        # Update only the colors field
        if "colors" not in graph_data:
            graph_data["colors"] = {}
        for item in colors:
            node = item["node"]
            color = item["color"]
            graph_data["colors"][node] = color
        with open(GRAPH_FILE, "w") as f:
            json.dump(graph_data, f, indent=2)
        # Redraw using new colors
        self.draw_graph(coloring=graph_data["colors"])

if __name__ == "__main__":
    app = GraphEditor()
    app.mainloop()