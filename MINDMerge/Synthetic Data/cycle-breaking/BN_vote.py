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

alarm = 'child_5000'
dict1 = {}
for dataNo in range(1,11):
    result_k2 = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_result_1.csv',index_col=0)
    names = result_k2.columns
    length = len(names)
    graph = np.zeros([length,length])
    total_result = pd.DataFrame(graph,index=names,columns=names)


    print(dataNo)
    for NO in range(1,51):
        tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_result_{NO}.csv',index_col=0)

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

with open(f'C:\\Users\\riyang\\Documents\\BNdata\\{alarm}_vote_file.json', 'w', encoding='utf-8')as file:  
    json.dump(dict1, file,
              ensure_ascii=False)  
    file.write('\n')

#
alarm = 'child_ad_5000'
dict1 = {}
for dataNo in range(1,11):
    result_k2 = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_result_1.csv',index_col=0)
    names = result_k2.columns
    length = len(names)
    graph = np.zeros([length,length])
    total_result = pd.DataFrame(graph,index=names,columns=names)


    print(dataNo)
    for NO in range(1,6):
        tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_result_{NO}.csv',index_col=0)

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

with open(f'C:\\Users\\riyang\\Documents\\BNdata\\{alarm}_vote_file.json', 'w', encoding='utf-8')as file: 
    json.dump(dict1, file,
              ensure_ascii=False)  
    file.write('\n')