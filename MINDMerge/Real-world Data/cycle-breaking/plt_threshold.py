import json

import pandas as pd
import numpy as np

def modelString(df,No):
    matrix =df
    names = matrix.columns
    PCset = dict()
    modelString = ''
    for i in names:
        tmp = matrix[i]
        PCset[i] = []
        for index,row in tmp.iteritems():
            if(row>0):
                PCset[i].append(index)
    for key in PCset:
        flag = 0
        if(len(PCset[key])==1):
            flag =1
        if (len(PCset[key]) > 1):
            flag = 2
        if(flag ==0):
            modelString = modelString + f'[{key}]'
        if(flag ==1):
            tmp = f'[{key}|{PCset[key][0]}]'
            modelString = modelString + tmp
        if(flag ==2):
            start = f'[{key}|'
            end = ']'
            for i in PCset[key]:
                start = start+f'{i}:'
            start = start[:-1]
            modelString = modelString+start+end

    dict1[No] = modelString
    print(modelString)


dict1 = {}

res = pd.read_csv(f'asia_res.csv',index_col=0)
names = res.columns
length = len(names)
count = 0
for index, row in res.iterrows():
    for i in range(len(names)):
        if (row[i] > 0):
            count += 1
print(count)
for index, row in res.iterrows():
    for i in range(len(names)):
        if (abs(row[i]) >3.5):
            row[i] = 1
        else:
            row[i] = 0
count = 0
print(res)
for index, row in res.iterrows():
    for i in range(len(names)):
        if (row[i] > 0):
            count += 1
print(count)

import pydot
graph = pydot.Dot("my_graph", graph_type="digraph",fontsize=11)
result = res #0-1matrix
names = result.columns.tolist()
fontSize =16
for index,row in result.iterrows():
    for i in range(len(names)):
        if (row[i] > 0):
            if('COM' in names[i]):
                nd = pydot.Node(names[i], shape="oval",color = '#FAEBD7',style='filled',fontsize=fontSize,fontname="Calibri bold")

            elif('PRO' in names[i]):
                nd = pydot.Node(names[i], shape="oval", color='#00C2F2', style='filled', fontsize=fontSize,fontname="Calibri bold")
            elif('LAB' in names[i]):
                nd = pydot.Node(names[i], shape="oval", color='#C7F4C7', style='filled', fontsize=fontSize,fontname="Calibri bold")
            elif('label' in names[i]):
                nd = pydot.Node(names[i], shape="oval", color='#FF4747', style='filled', fontsize=fontSize,fontname="Calibri bold")
            else:
                nd = pydot.Node(names[i], shape="oval", color='#CC99FF', style='filled', fontsize=fontSize,fontname="Calibri bold")
            graph.add_node(nd)
            if('COM' in index):
                nd = pydot.Node(index, shape="oval",color = '#FAEBD7',style='filled',fontsize=fontSize,fontname="Calibri bold")

            elif('PRO' in index):
                nd = pydot.Node(index, shape="oval", color='#00C2F2', style='filled', fontsize=fontSize,fontname="Calibri bold")
            elif('LAB' in index):
                nd = pydot.Node(index, shape="oval", color='#C7F4C7', style='filled', fontsize=fontSize,fontname="Calibri bold")
            elif('label' in index):
                nd = pydot.Node(index, shape="oval", color='#FF4747', style='filled', fontsize=fontSize,fontname="Calibri bold")
            else:
                nd = pydot.Node(index, shape="oval", color='#CC99FF', style='filled', fontsize=fontSize,fontname="Calibri bold")
            graph.add_node(nd)
            if(names[i] == 'label'):
                continue
            graph.add_edge(pydot.Edge(index, names[i], color="#011F64"))

print(modelString(res,1))

graph.add_edge(pydot.Edge('COM11', 'label', color="#011F64",penwidth=2))
graph.add_edge(pydot.Edge('COM10', 'label', color="#011F64",penwidth=2))
graph.add_edge(pydot.Edge('COM8', 'label', color="#011F64",penwidth=2))
graph.add_edge(pydot.Edge('PRO3', 'label', color="#011F64",penwidth=2))
graph.add_edge(pydot.Edge('LAB19', 'label', color="#011F64",penwidth=2))


graph.write_png(f"output_1.png")

# for i in range(1,11):
#     print(i)
#
#     dataset = pd.read_csv(f"data/full_new_result{i}.csv",index_col=0)
#     modelString(dataset,i)
