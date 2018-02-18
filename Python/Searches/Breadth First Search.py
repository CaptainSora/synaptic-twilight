# -*- coding: utf-8 -*-
"""
Created on Tue Feb 21 22:09:41 2017

@author: Joseph
"""

graph = {'A': set(['B', 'D']),
         'B': set(['A', 'C', 'G']),
         'C': set(['B', 'I', 'J']),
         'D': set(['A', 'E', 'J']),
         'E': set(['D', 'F']),
         'F': set(['E', 'J', 'K']),
         'G': set(['B']),
         'H': set(['I', 'L']),
         'I': set(['C', 'H']),
         'J': set(['C', 'D', 'F']),
         'K': set(['F', 'L']),
         'L': set(['H', 'K'])}


def bfs(graph, start):
    visited, queue = set(), [start]
    while queue:
        vertex = queue.pop(0)
        if vertex not in visited:
            visited.add(vertex)
            queue.extend(graph[vertex] - visited)
    print(visited)

bfs(graph, 'A')


def bfs_paths(graph, start, goal):
    queue = [(start, [start])]
    while queue:
        (vertex, path) = queue.pop(0)
        for next in graph[vertex] - set(path):
            if next == goal:
                yield path + [next]
            else:
                queue.append((next, path+[next]))

print(list(bfs_paths(graph, 'A', 'L')))
