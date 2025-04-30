class UnionFind:
    def __init__(self, num_elements):
        self.representative = [i for i in range(num_elements)]
        self.size = [1] * num_elements

    # find the representative element
    def find(self, i):
        if self.representative[i] == i:
            return i
        else:
            root = self.find(self.representative[i])
            self.representative[i] = root # path compression
            return root

    # merge the sets that i and j belong to
    def union(self, i, j):
        i = self.find(i)
        j = self.find(j)
        if i == j:
            return

        # smaller-to-larger merging
        if self.size[i] > self.size[j]:
            larger, smaller = i, j
        else:
            larger, smaller = j, i
        self.representative[smaller] = larger
        self.size[larger] += self.size[smaller]

    # create a new set with a single element.
    def make(self):
        new_id = len(self.representative)
        self.representative.append(new_id)
        self.size.append(1)
        return new_id

uf = UnionFind(5) # [[0], [1], [2], [3], [4]]
uf.union(2, 3) # [[0], [1], [2, 3], [4]]
uf.union(0, 4) # [[0, 4], [1], [2, 3]]
uf.union(0, 3) # [[0, 4, 2, 3], [1]]

# find(a) == find(b) <=> a,b belong to the same set
assert uf.find(4) == uf.find(3) # 4 and 3 belong to the same set.
assert uf.find(4) != uf.find(1) # 4 and 1 belong to different sets.



