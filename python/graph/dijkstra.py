import math


class DijkstraSSP:

    def __init__(self, G, src):
        # Parent graph
        self.G = G
        self.G_age = G.age()
        # Starting vertex
        self.start = src
        # Who's the parent of whom in the shortest path
        # Following this backwards provides us our path
        self.parents = {}
        # Distance from source
        self.dist = {}
        # Track which nodes are closest by using a priority queue
        # The best priQ depends on the sparsity of the graph
        self.q = self.select_pri_q(G)
        self.dijkstra(G, src)

    def dijkstra(self, G, src):
        for v in G.V:  # All nodes begin infinitely far away
            self.dist[v] = math.inf
        self.dist[src] = 0  # Source is, of course, no self.distance away
        self.q.put(src)  # Start with our source
        while not self.q.empty():
            self.relax(self.q.extract_min())

    def relax(self, v):
        for e in self.G.edges[v]:
            w = e.weight
            u = e.dst
            if self.dist[u] > self.dist[v] + w:
                self.parents[u] = v
                self.dist[u] = self.dist[v] + w
                # The performance of testing for inclusion and decrease_key
                # depend heavily on your PriQ implementation
                if u in self.q:
                    self.q.decrease_key(u, self.dist[u])
                else:
                    self.q.put(u, self.dist[u])
