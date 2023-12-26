import json
import pandas as pd
from queue import Queue
from graphviz import Digraph
import numpy as np
from itertools import *
from circleList import circleNum
import scipy.linalg as slin
import scipy.optimize as sopt
from scipy.special import expit as sigmoid
import copy
from eicuProcess.utilsA import utils


dict1 = {}
hospitalID = 435
for dataNo in range(1,11):
    result_k2 = pd.read_csv(f'C:\\Users\\riyang\\Documents\local\\result\\{hospitalID}_10_result_1_{dataNo}.csv',index_col=0)
    names = result_k2.columns
    length = len(names)
    print(names)
    graph = np.zeros([length,length])
    total_result = pd.DataFrame(graph,index=names,columns=names)


    print(dataNo)
    for NO in range(1,51):
        tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\local\\result\\{hospitalID}_10_result_{NO}_{dataNo}.csv',index_col=0)

        for index, row in tmp.iterrows():
            for i in range(len(names)):
                if (row[i] > 2):
                    row[i] = 1
                else:
                    row[i] = 0
        total_result += tmp
    count=0
    for index, row in total_result.iterrows():
        for i in range(len(names)):
            if (row[i] > 25):
                row[i] = 1
                count+=1
            else:
                row[i] = 0
    print(count)
    import pydot
    graph = pydot.Dot("my_graph", graph_type="digraph",fontsize=18)
    result = total_result #
    names = result.columns.tolist()
    for index,row in result.iterrows():
        for i in range(len(names)):
            if (row[i] > 0):
                graph.add_node(pydot.Node(names[i], shape="oval"))
                graph.add_node(pydot.Node(index, shape="oval"))
                graph.add_edge(pydot.Edge(index, names[i], color="black"))

    graph.write_png(f"output_{dataNo}.png")

    print(total_result)
    matrix = total_result
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

with open(f'C:\\Users\\riyang\\Documents\\local\\{hospitalID}_hethom_vote_file.json', 'w', encoding='utf-8')as file:  
    json.dump(dict1, file,
              ensure_ascii=False) 
    file.write('\n')

#

dict1 = {}
for dataNo in range(1,11):
    result_k2 = pd.read_csv(f'C:\\Users\\riyang\\Documents\local\\result\\{hospitalID}_10_ad_result_{dataNo}_1.csv',index_col=0)
    names = result_k2.columns
    length = len(names)
    graph = np.zeros([length,length])
    total_result = pd.DataFrame(graph,index=names,columns=names)


    print(dataNo)
    for NO in range(1,6):
        tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\local\\result\\{hospitalID}_10_ad_result_{dataNo}_{NO}.csv',index_col=0)

        for index, row in tmp.iterrows():
            for i in range(len(names)):
                if (row[i] > 25):
                    row[i] = 1
                else:
                    row[i] = 0
        total_result += tmp
    count=0
    for index, row in total_result.iterrows():
        for i in range(len(names)):
            if (row[i] > 2):
                row[i] = 1
                count+=1
            else:
                row[i] = 0
    print(count)
    print(total_result)
    matrix = total_result
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

with open(f'C:\\Users\\riyang\\Documents\\local\\{hospitalID}_homhet_vote_file.json', 'w', encoding='utf-8')as file:  
    json.dump(dict1, file,
              ensure_ascii=False)  
    file.write('\n')