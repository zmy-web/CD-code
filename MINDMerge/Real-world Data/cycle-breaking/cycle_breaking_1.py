import json

import networkx as nx
from pgmpy.base import DAG
from pgmpy.models import BayesianModel
from pgmpy.inference import VariableElimination
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from collections import deque


def dag_to_martix(dag, feature):
    edges_martix = pd.DataFrame(np.zeros((len(feature), len(feature))), columns=feature, index=feature)
    for i in dag.edges:
        edges_martix.iloc[feature.index(i[0]), feature.index(i[1])] = 1
    return edges_martix


def martix_to_dag(martix, feature):
    dag = DAG()
    for idx in feature:
        for col in feature:
            if martix[col][idx] > 0:
                dag.add_edge(idx, col)
    return dag

def remove_cycle(model):
    
    def topological_sort(model):
        visited = set()
        sorted_nodes = deque()

        def visit(node):
            if node not in visited:
                visited.add(node)
                for child in model.successors(node):
                    visit(child)
                sorted_nodes.appendleft(node)

        for node in model.nodes():
            if node not in visited:
                visit(node)

        return list(sorted_nodes)

    
    def find_cycle(model, sorted_nodes):
        for index, node in enumerate(sorted_nodes):
            for child in model.successors(node):
                if sorted_nodes.index(child) < index:
                    return node, child
        return None, None

    
    sorted_nodes = topological_sort(model)
    print(sorted_nodes)
    node1, node2 = find_cycle(model, sorted_nodes)

    if node1 and node2:
        model.remove_edge(node1, node2)
        print(f"Removed edge: {node1} -> {node2}")

        return 1
    return 0

alarm = 'alarm_200_500'
dict1 = {}
# build a  cycle BNs
for dataNo in range(1,11):
    result_k2 = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_1.csv',index_col=0)
    names = list(result_k2.columns)
    length = len(names)
    graph = np.zeros([length,length])
    total_result = pd.DataFrame(graph,index=names,columns=names)

    # dataFrame = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\select\\{alarm}_{dataNo}.csv')
    print(dataNo)
    # for NO in range(1,51):
    #     tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_{NO}.csv',index_col=0)
    #     total_result += tmp
    # total_result = total_result/10
    #
    #hom-het
    result_k2 = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_1.csv', index_col=0)
    names = list(result_k2.columns)
    length = len(names)
    graph = np.zeros([length, length])
    total_result = pd.DataFrame(graph, index=names, columns=names)

    for i in range(1, 6):
        tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_{i}.csv', index_col=0)
        total_result += tmp
    total_result = total_result / 10
    total_result[total_result < 0.85] = 0

    model = martix_to_dag(total_result, names)
    while True:
        flag = remove_cycle(model)
        if(flag == 0):
            break
    mat = dag_to_martix(model, names)
    count = 0
    for index, row in mat.iterrows():
        for i in range(len(names)):
            if (row[i] > 0):
                count += 1
    print(count)


    matrix = mat
    names = matrix.columns
    PCset = dict()
    modelString = ''
    for i in names:
        tmp = matrix[i]
        PCset[i] = []
        for index, row in tmp.iteritems():
            if (row > 0):
                PCset[i].append(index)
    for key in PCset:
        flag = 0
        if (len(PCset[key]) == 1):
            flag = 1
        if (len(PCset[key]) > 1):
            flag = 2
        if (flag == 0):
            modelString = modelString + f'[{key}]'
        if (flag == 1):
            tmp = f'[{key}|{PCset[key][0]}]'
            modelString = modelString + tmp
        if (flag == 2):
            start = f'[{key}|'
            end = ']'
            for i in PCset[key]:
                start = start + f'{i}:'
            start = start[:-1]
            modelString = modelString + start + end

    print(modelString)
    dict1[dataNo] = modelString

with open(f'C:\\Users\\riyang\\Documents\\BNdata\\{alarm}_top_file.json', 'w', encoding='utf-8')as file: 
    json.dump(dict1, file,
              ensure_ascii=False)  
    file.write('\n')
