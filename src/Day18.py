import string
import sortedcollections
import collections
import math
import re
import sys
from typing import Tuple, List, Dict, Set

def p1_adj(p:Tuple[int, int]) -> List[Tuple[int, int]]:
    return [(p[0] + 1, p[1]), (p[0] - 1, p[1]), (p[0], p[1] + 1), (p[0], p[1] - 1)]

def p1_reach(grid:List[str], start:Tuple[int, int]) -> Dict[str, Tuple[int, Set[str]]]:
    out  = {}
    dist = {start: 0}
    obst = {start: set()}
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
            obst[p] = obst[t].copy()
            if ('A' <= c <= 'Z'):
                obst[p].add(c)
            if ('a' <= c <= 'z'):
                out[c] = (dist[p], obst[p].copy())
                obst[p].add(c)
            bfs.append(p)
    return out

def p1_get_keys(grid:List[str]) -> List[Tuple[Tuple[int, int], str]]:
    l = []
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if ('a' <= grid[i][j] <= 'z' or grid[i][j] == '@'):
                l.append(((i, j), grid[i][j]))
    return l

def p1_run(grid:List[str], start:Tuple[int, int]) -> int:
    keypath = {k:p1_reach(grid, (x, y)) for ((x, y), k) in p1_get_keys(grid)}
    return p1_shortest(keypath, set(), '@')

cache = {}
def p1_shortest(keypath:Dict[str, Dict[str, Tuple[int, Set[str]]]], p:Set[str], k:str) -> int:
    min_dist = 0
    for opt in keypath[k]:
        if keypath[k][opt][1].issubset(p) and opt not in p:
            pn = p.copy()
            pn.add(opt)
            pn.add(opt.upper())
            if (opt, str(sorted(pn))) in cache:
                d = cache[(opt, str(sorted(pn)))] + keypath[k][opt][0]
            else:
                d = p1_shortest(keypath, pn, opt) + keypath[k][opt][0]
            if min_dist == 0:
                min_dist = d
            else:
                min_dist = min(min_dist, d)
    cache[(k, str(sorted(p)))] = min_dist
    return min_dist

def p2_get_keys(grid:List[str]) -> List[Tuple[Tuple[int, int], str]]:
    l = []
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if ('a' <= grid[i][j] <= 'z'):
                l.append(((i, j), grid[i][j]))
    return l

def p2_run(grid:List[str], starts:List[Tuple[int, int]]) -> int:
    keypath = {k:p1_reach(grid, (x, y)) for ((x, y), k) in p2_get_keys(grid)}
    state = {}
    for i in starts:
        keypath[str(i)] = p1_reach(grid, i)
        state[str(i)] = str(i)
    # print(keypath)
    return p2_shortest(keypath, set(), state)

def p2_shortest(keypath:Dict[str, Dict[str, Tuple[int, Set[str]]]], p:Set[str], r:Dict[str, str]) -> int:
    min_dist = 0
    for k in r:
        for opt in keypath[r[k]]:
            # print(r, r[k], opt, keypath[r[k]][opt], p)
            if keypath[r[k]][opt][1].issubset(p) and opt not in p:
                pn = p.copy()
                pn.add(opt)
                pn.add(opt.upper())
                rn = r.copy()
                rn[k] = opt
                if (str(rn), str(sorted(pn))) in cache:
                    d = cache[(str(rn), str(sorted(pn)))] + keypath[r[k]][opt][0]
                    # print("Cache hit " + str(rn) + " " + str(sorted(pn)) + " " + str(cache[(str(rn), str(sorted(pn)))]))
                else:
                    d = p2_shortest(keypath, pn, rn) + keypath[r[k]][opt][0]
                if min_dist == 0:
                    min_dist = d
                else:
                    min_dist = min(min_dist, d)
    cache[(str(r), str(sorted(p)))] = min_dist
    return min_dist

with open(sys.argv[1]) as file:
    grid = [l.rstrip('\n') for l in file]

start = (0, 0)
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if grid[i][j] == '@':
            start = (i, j)

print(p1_run(grid, start))

with open(sys.argv[2]) as file:
    grid = [l.rstrip('\n') for l in file]

starts = []
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if grid[i][j] == '@':
            starts.append((i, j))

print(p2_run(grid, starts))