
import numpy as np
import pandas as pd
from scipy.stats import chi2_contingency

dataset = pd.read_csv('data/Imp_data_122.csv')
fs_list = []
features = dataset.columns[2:]

cross_table_res = pd.crosstab(dataset['age'], dataset['label'], margins=True)
cross_table_res.loc[:,'pValue'] = chi2_contingency(cross_table_res)[1]
cross_table_res.loc[:,'feature'] = 'age'

cross_table_res = pd.crosstab(dataset['sex'], dataset['label'], margins=True)
cross_table_res.loc[:,'pValue'] = chi2_contingency(cross_table_res)[1]
cross_table_res.loc[:,'feature'] = 'sex'

for feature in features:
    cross_table = pd.crosstab(dataset[feature], dataset['label'], margins=True)
    res = chi2_contingency(cross_table)

    if (res[1] < 0.05):
        fs_list.append(feature)

        cross_table.loc[:, 'pValue'] = res[1]
        cross_table.loc[:, 'feature'] = feature
        cross_table_res = cross_table_res.append(cross_table, ignore_index=False)

print(cross_table_res.head(20))
cross_table_res.to_csv('cross_table_res.csv',index=True)
fs_list.append('label')
print(len(fs_list))
print(fs_list)
fs_list.append('sex')
fs_list.append('age')
dataset = dataset.loc[:,fs_list]
dataset.to_csv(f'C:\\Users\\riyang\\Documents\local\\hospital\\122_total.csv', index=False, sep=',')
num = 1
for j in range(300, 350):
    # sampling。

    groupSub = dataset.sample(frac=1.0, replace=True, random_state=j)
    groupSub.to_csv(f'C:\\Users\\riyang\\Documents\local\\hospital\\122_total{num}.csv', index=False)
    num += 1