import enum


class DFS:

    class Status(enum.Enum):
        Unknown = 0
        Discovered = 1
        Processed = 2

    def __init__(self, v):
        self.start = v
        self.status = {}
        self.parents = {}
        self.topo = []
        self.dfs(v)

    def dfs(self, v):
        self.status[v] = self.Status.Discovered
        self.pre_process(v)

        for w in v.neighbors():
            status = self.status.get(w, self.Status.Unknown)
            if status == self.Status.Discovered:
                raise Exception("Goddamn cycle!")
            elif status == self.Status.Processed:
                print("Cross/Forward edge: {}->{}".format(v, w))
                continue
            else:
                assert status == self.Status.Unknown
                self.parents[w] = v
                self.process_edge(v, w)
                self.dfs(w)

        self.status[v] = self.Status.Processed
        self.post_process(v)
