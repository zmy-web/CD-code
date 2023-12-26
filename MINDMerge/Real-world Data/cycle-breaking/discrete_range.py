import json
from time import sleep

import pandas as pd
from sklearn.model_selection import train_test_split, KFold
from scipy.stats import chi2_contingency

dataset = pd.read_csv('data/site435disc.csv')
names = list(dataset.columns)
discreate_range = pd.read_csv('discreate_range.csv')

names1 = discreate_range.iloc[:,2].values.tolist()

ret = list(set(names).intersection(set(names1)))
print(ret)
ranges = {}
for i in ret:
    ranges[i] = [-1000]
    for index, row in discreate_range.iterrows():
        if (row.loc['name'] == i):
            ranges[i].append(row.loc['low'])
            ranges[i].append(row.loc['up'])
    ranges[i].append(10000)

ranges['bmi'] = [-1000,18,25,30,10000]
ranges['age'] = [18,35,50,65,80,1000]
ranges['respiration'] = [-1000,12,16,10000]
ranges['heartrate'] = [-1000,60,100,10000]
No = 0

names = list(dataset.columns)
features = names
for i in range(len(features)):
    if(features[i] == 'LAB6'):
        features.pop(i)
        break
for i in range(len(features)):
    if(features[i] == 'LAB43'):
        features.pop(i)
        break
for i in range(len(features)):
    if(features[i] == 'LAB44'):
        features.pop(i)
        break

print(features)
print(len(features))
for i in range(len(features)):
    if(features[i] in ranges.keys()):
        dataset[features[i]] = pd.cut(x=dataset[features[i]], bins=ranges[features[i]],labels=False)
print(dataset.head(5))
dataset.to_csv('data/16data.csv',index=False)

dataset = pd.read_csv('data/16data.csv')
for index,row in dataset.iterrows():

    if(row['sex'] == 1):
        if(row['LAB6'] <4.2):
            row['LAB6'] = 0
        elif(row['LAB6']>=4.2 and row['LAB6']<=5.4):
            row['LAB6'] = 1
        else:
            row['LAB6'] = 2
        if(row['LAB43'] <36):
            row['LAB43'] = 0
        elif(row['LAB43']>=36 and row['LAB43']<=48):
            row['LAB43'] = 1
        else:
            row['LAB43'] = 2
        if(row['LAB44'] <12.1):
            row['LAB44'] = 0
        elif(row['LAB44']>=12.1 and row['LAB44']<=15.1):
            row['LAB44'] = 1
        else:
            row['LAB44'] = 2
    if(row['sex'] == 2):
        if(row['LAB6'] <4.7):
            row['LAB6'] = 0
        elif(row['LAB6']>=4.7 and row['LAB6']<=6.1):
            row['LAB6'] = 1
        else:
            row['LAB6'] = 2
        if(row['LAB43'] <41):
            row['LAB43'] = 0
        elif(row['LAB43']>=41 and row['LAB43']<=50):
            row['LAB43'] = 1
        else:
            row['LAB43'] = 2
        if(row['LAB44'] <13.2):
            row['LAB44'] = 0
        elif(row['LAB44']>=13.2 and row['LAB44']<=17.2):
            row['LAB44'] = 1
        else:
            row['LAB44'] = 2
    dataset.iloc[index] = row
No += 1
dataset.fillna(0,inplace=True)
dataset.to_csv(f'data/435disc.csv', index=False)
    # dataset.to_csv(f"/home/daimingyang/eicu/data/discrete_hospital_No{i}_med.csv",index=False)

print(1)
sleep(5)


# Kfold
kf = KFold(n_splits=10, shuffle=True, random_state=1)

i = 1
dict_train = {}
dict_test = {}
for train_index, test_index in kf.split(dataset):
    list_train = train_index.tolist()
    dict_train[i] = [a+1 for a in list_train]
    list_test = test_index.tolist()
    dict_test[i] = [a+1 for a in list_test]
    dataset_train, dataset_test = dataset.iloc[train_index], dataset.iloc[test_index]
    # dataset_train.to_csv(f'C:\\Users\\riyang\\Documents\local\\hospital\\122_m_train_{i}.csv', index=False, sep=',')
    # dataset_test.to_csv(f'C:\\Users\\riyang\\Documents\local\\hospital\\122_m_test_{i}.csv', index=False, sep=',')
    fs_list = []
    features = dataset_train.columns[1:]
    for feature in features:
        cross_table = pd.crosstab(dataset_train['label'], dataset_train[feature], margins=True)
        res = chi2_contingency(cross_table)
        if (res[1] < 0.05):
            fs_list.append(feature)

    fs_list.append('label')
    if(len(fs_list) > 0):
        dataset_train = dataset_train.loc[:,fs_list]
        dataset_test = dataset_test.loc[:, fs_list]

        dataset_train.to_csv(f'C:\\Users\\riyang\\Documents\local\\hospital\\435_10_train{i}.csv', index=False, sep=',')

        num = 1
        for j in range(100, 150):
            #sampling。

            groupSub = dataset_train.sample(frac=1.0,replace = True,random_state=j)
            groupSub.to_csv(f'C:\\Users\\riyang\\Documents\local\\hospital\\435_10_train{i}_{num}.csv',index=False)
            num+=1

        #
        dataset_test.to_csv(f'C:\\Users\\riyang\\Documents\local\\hospital\\435_10_test{i}.csv', index=False, sep=',')
        i += 1
# print(dict_test)
# with open(f'C:\\Users\\riyang\\Documents\\local\\122_train_file.json', 'w', encoding='utf-8')as file: 
#     json.dump(dict_train, file,
#               ensure_ascii=False) 
#     file.write('\n')
#
# with open(f'C:\\Users\\riyang\\Documents\\local\\122_test_file.json', 'w', encoding='utf-8')as file: 
#     json.dump(dict_test, file,
#               ensure_ascii=False) 
#     file.write('\n')