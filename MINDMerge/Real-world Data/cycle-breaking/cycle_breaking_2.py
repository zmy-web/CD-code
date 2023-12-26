import itertools
import json

from sklearn.metrics import mutual_info_score
import networkx as nx
from pgmpy.base import DAG
from pgmpy.models import BayesianModel
from pgmpy.inference import VariableElimination
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from collections import deque
import networkx as nx

def find_cycles(graph):
    
    G = nx.DiGraph(graph)
    
    cycles = nx.simple_cycles(G)
    print(cycles)
    
    directed_cycles = []
    for cycle in cycles:
        directed_cycle = []
        for i in range(len(cycle) - 1):
            directed_cycle.append((cycle[i], cycle[i+1]))
        directed_cycle.append((cycle[-1], cycle[0]))
        directed_cycles.append(directed_cycle)
    # 
    return directed_cycles

def martix_to_dag(martix, feature):
    dag = nx.DiGraph()
    for idx in feature:
        for col in feature:
            if martix[col][idx] > 0:
                dag.add_edge(idx, col)
    return dag
def dag_to_martix(model, feature):
    edges_martix = pd.DataFrame(np.zeros((len(feature), len(feature))), columns=feature, index=feature)
    for i in model.edges:
        edges_martix.iloc[feature.index(i[0]), feature.index(i[1])] = 1
    return edges_martix
dataset = "alarm_500"
alarm = 'alarm_200_ad_500'
dict1 = {}
# Create a directed graph
for dataNo in range(1,11):
    # result_k2 = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_1.csv',index_col=0)
    # names = list(result_k2.columns)
    # length = len(names)
    # graph = np.zeros([length,length])
    # total_result = pd.DataFrame(graph,index=names,columns=names)
    #
    # dataFrame = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\select\\{alarm}_{dataNo}.csv')
    # print(dataNo)
    # for NO in range(1,51):
    #     tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_{NO}.csv',index_col=0)
    #     total_result += tmp
    # total_result = total_result/10

    #hom-het
    dataFrame = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\select\\{dataset}_{dataNo}.csv')
    result_k2 = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_1.csv', index_col=0)
    names = list(result_k2.columns)
    length = len(names)
    graph = np.zeros([length, length])
    total_result = pd.DataFrame(graph, index=names, columns=names)

    for i in range(1, 6):
        tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_{i}.csv', index_col=0)
        total_result += tmp
    total_result = total_result/10
    total_result[total_result < 0.85] = 0

    model = martix_to_dag(total_result, names)
    print(1)
    while True:
        cycleList = find_cycles(model)
        if (len(cycleList) == 0):
            break
        miList = []
        for edge in cycleList[0]:
            fromNode = edge[0]
            toNode = edge[1]
            miList.append(mutual_info_score(dataFrame[fromNode], dataFrame[toNode]))
        min_index = miList.index(min(miList))
        fromNode = cycleList[0][min_index][0]
        toNode = cycleList[0][min_index][1]
        model.remove_edge(fromNode, toNode)
        print(model.edges)
    count = 0
    mat = dag_to_martix(model, names)
    print(mat)
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

with open(f'C:\\Users\\riyang\\Documents\\BNdata\\{alarm}_mi_file.json', 'w', encoding='utf-8')as file: 
    json.dump(dict1, file,
              ensure_ascii=False)  
    file.write('\n')





