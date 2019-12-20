import string
import sortedcollections
import collections
import math
import re
import sys
from typing import Tuple, List, Dict, Set

def p1_adj(p:Tuple[int, int]) -> List[Tuple[int, int]]:
    return [(p[0] + 1, p[1]), (p[0] - 1, p[1]), (p[0], p[1] + 1), (p[0], p[1] - 1)]

def p1_reach(grid:List[str], starts:List[Tuple[int, int]]) -> Dict[str, int]:
    out  = {}
    for start in starts:
        dist = {start: 0}
        bfs  = collections.deque([start])
        while bfs:
            t = bfs.popleft()
            for p in p1_adj(t):
                if (p[0] < 0 or p[0] >= len(grid) or p[1] < 0 or p[1] >= len(grid[0])):
                    continue
                c = grid[p[0]][p[1]]
                if (c == '#' or p in dist):
                    continue
                dist[p] = dist[t] + 1
                if ('A' <= c <= 'Z'):
                    for (a, b) in p1_adj(p):
                        if (a < 0 or a >= len(grid) or b < 0 or b >= len(grid[0])):
                            continue
                        if ('A' <= grid[a][b] <= 'Z'):
                            n = ("".join(sorted([c, grid[a][b]])))
                            if n in out:
                                out[n] = min(dist[p], out[n])
                            else:
                                out[n] = dist[p]
                    continue
                bfs.append(p)
    return out

def p1_get_portals(grid:List[str]) -> Dict[str, List[Tuple[int, int]]]:
    l = {}
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if (grid[i][j] == '.'):
                for (x, y) in p1_adj((i, j)):
                    if (x < 0 or x >= len(grid) or y < 0 or y >= len(grid[0])):
                        continue
                    if ('A' <= grid[x][y] <= 'Z'):
                        for (a, b) in p1_adj((x, y)):
                            if (a < 0 or a >= len(grid) or b < 0 or b >= len(grid[0])):
                                continue
                            if ('A' <= grid[a][b] <= 'Z'):
                                n = ("".join(sorted([grid[x][y], grid[a][b]])))
                                if n in l:
                                    l[n].append((i, j))
                                else:
                                    l[n] = [(i, j)]
    return l

def p1_run(grid:List[str], start:Tuple[int, int]) -> int:
    keypath = {k:p1_reach(grid, p1_get_portals(grid)[k]) for k in p1_get_portals(grid)}
    print(keypath)
    return p1_shortest(keypath, set(), 'AA')

def p1_shortest(keypath:Dict[str, Dict[str, int]], p:Set[str], k:str) -> int:
    min_dist = 0
    for opt in keypath[k]:
        d = 9999999999999999
        if opt == 'ZZ':
            d = keypath[k][opt] - 1
        elif opt not in p and opt != k:
            # print(k, opt, keypath[k][opt], p)
            pn = p.copy()
            pn.add(opt.upper())
            d = p1_shortest(keypath, pn, opt) + keypath[k][opt]
        if min_dist == 0:
            min_dist = d
        else:
            min_dist = min(min_dist, d)
    return min_dist

def p2_reach(grid:List[str], starts:List[Tuple[int, int]]) -> Dict[str, int]:
    out  = {}
    for start in starts:
        dist = {start: 0}
        bfs  = collections.deque([start])
        while bfs:
            t = bfs.popleft()
            for p in p1_adj(t):
                if (p[0] < 0 or p[0] >= len(grid) or p[1] < 0 or p[1] >= len(grid[0])):
                    continue
                c = grid[p[0]][p[1]]
                if (c == '#' or p in dist):
                    continue
                dist[p] = dist[t] + 1
                if ('A' <= c <= 'Z'):
                    for (a, b) in p1_adj(p):
                        if (a < 0 or a >= len(grid) or b < 0 or b >= len(grid[0])):
                            continue
                        if ('A' <= grid[a][b] <= 'Z'):
                            if (a < len(grid) / 5 or a >= 4 * len(grid) / 5 or b < len(grid[0]) / 5 or b >= 4 * len(grid[0]) / 5):
                                n = "$".join(sorted([grid[p[0]][p[1]], grid[a][b]]))
                            else:
                                n = "^".join(sorted([grid[p[0]][p[1]], grid[a][b]]))
                            if n in out:
                                out[n] = min(dist[p], out[n])
                            else:
                                out[n] = dist[p]
                    continue
                bfs.append(p)
    return out

def p2_get_portals(grid:List[str]) -> Dict[str, Tuple[int, int]]:
    l = {}
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if (grid[i][j] == '.'):
                for (x, y) in p1_adj((i, j)):
                    if (x < 0 or x >= len(grid) or y < 0 or y >= len(grid[0])):
                        continue
                    if ('A' <= grid[x][y] <= 'Z'):
                        for (a, b) in p1_adj((x, y)):
                            if (a < 0 or a >= len(grid) or b < 0 or b >= len(grid[0])):
                                continue
                            if ('A' <= grid[a][b] <= 'Z'):
                                if (a < len(grid) / 5 or a >= 4 * len(grid) / 5 or b < len(grid[0]) / 5 or b >= 4 * len(grid[0]) / 5):
                                    n = "$".join(sorted([grid[x][y], grid[a][b]]))
                                else:
                                    n = "^".join(sorted([grid[x][y], grid[a][b]]))
                                l[n] = ((i, j))
    return l

def p2_run(grid:List[str], start:Tuple[int, int]) -> int:
    keypath = {k:p2_reach(grid, [p2_get_portals(grid)[k]]) for k in p2_get_portals(grid)}
    print(keypath)
    return p2_shortest(keypath, set(), 'A$A')

def p2_invert(s:str) -> str:
    return s.replace('$', '^') if '$' in s else s.replace('^', '$')

depth = 0
def p2_shortest(keypath:Dict[str, Dict[str, int]], p:Set[str], k:str) -> int:
    global depth
    min_dist = 0
    for opt in keypath[k]:
        d = 999999999999999999
        ndepth = depth + 1 if '^' in opt else depth - 1
        # print(depth, k, opt, keypath[k][opt], p)
        if (depth == 0 and '$' in opt and opt != 'Z$Z') or (depth != 0 and opt == 'Z$Z') or opt == 'A$A':
            continue
        if depth == 0 and opt == 'Z$Z':
            d = keypath[k][opt] - 1
        elif (opt, ndepth) not in p and opt != k and depth < 30:
            tdepth = depth
            depth = ndepth
            pn = p.copy()
            pn.add((opt.upper(), depth))
            d = p2_shortest(keypath, pn, p2_invert(opt)) + keypath[k][opt]
            depth = tdepth
        if min_dist == 0:
            min_dist = d
        else:
            min_dist = min(min_dist, d)
    return min_dist

with open(sys.argv[1]) as file:
    grid = [l.rstrip('\n') for l in file]

start = (0, 0)
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if grid[i][j] == '@':
            start = (i, j)

print(p1_run(grid, start))

print(p2_run(grid, start))
