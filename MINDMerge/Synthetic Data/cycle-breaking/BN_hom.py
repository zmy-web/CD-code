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

def notears_linear(X, lambda1,lambda2, loss_type, max_iter=100, h_tol=1e-8, rho_max=1e+16, w_threshold=0.5):
    """Solve min_W L(W; X) + lambda1 ‖W‖_1 s.t. h(W) = 0 using augmented Lagrangian.

    Args:
        X (np.ndarray): [n, d] sample matrix
        lambda1 (float): l1 penalty parameter
        loss_type (str): l2, logistic, poisson
        max_iter (int): max num of dual ascent steps
        h_tol (float): exit if |h(w_est)| <= htol
        rho_max (float): exit if rho >= rho_max
        w_threshold (float): drop edge if |weight| < threshold

    Returns:
        W_est (np.ndarray): [d, d] estimated DAG
    """
    def _loss(W):
        """Evaluate value and gradient of loss."""
        M = W
        if loss_type == 'l2':
            R = X - M
            loss = 0.5 * lambda2 * (R ** 2).sum()
            G_loss = -1 * lambda2 * R
        elif loss_type == 'logistic':
            loss = 1.0 / X.shape[0] * (np.logaddexp(0, M) - X * M).sum()
            G_loss = 1.0 / X.shape[0] * X.T @ (sigmoid(M) - X)
        elif loss_type == 'poisson':
            S = np.exp(M)
            loss = 1.0 / X.shape[0] * (S - X * M).sum()
            G_loss = 1.0 / X.shape[0] * X.T @ (S - X)
        else:
            raise ValueError('unknown loss type')
        return loss, G_loss

    def _h(W):
        """Evaluate value and gradient of acyclicity constraint."""
        E = slin.expm(W * W)  # (Zheng et al. 2018)
        h = np.trace(E) - d
        #     # A different formulation, slightly faster at the cost of numerical stability
        #     M = np.eye(d) + W * W / d  # (Yu et al. 2019)
        #     E = np.linalg.matrix_power(M, d - 1)
        #     h = (E.T * M).sum() - d
        G_h = E.T * W * 2
        return h, G_h

    def _adj(w):
        """Convert doubled variables ([2 d^2] array) back to original variables ([d, d] matrix)."""
        return (w[:d * d] - w[d * d:]).reshape([d, d])

    def _func(w):
        """Evaluate value and gradient of augmented Lagrangian for doubled variables ([2 d^2] array)."""
        W = _adj(w)
        loss, G_loss = _loss(W)
        h, G_h = _h(W)
        obj = loss + 0.5 * rho * h * h + alpha * h +lambda1 * w.sum()
        G_smooth = G_loss + (rho * h + alpha) * G_h
        g_obj = np.concatenate((G_smooth + lambda1, - G_smooth + lambda1), axis=None)
        return obj, g_obj

    n, d = X.shape
    w_est, rho, alpha, h = np.zeros(2 * d * d), 1.0, 0.0, np.inf  # double w_est into (w_pos, w_neg)
    bnds = [(0, 0) if i == j else (0, None) for _ in range(2) for i in range(d) for j in range(d)]
    for _ in range(max_iter):
        w_new, h_new = None, None
        while rho < rho_max:
            sol = sopt.minimize(_func, w_est, method='L-BFGS-B', jac=True, bounds=bnds)
            w_new = sol.x
            h_new, _ = _h(_adj(w_new))
            if h_new > 0.25 * h:
                rho *= 10
            else:
                break
        w_est, h = w_new, h_new
        alpha += rho * h
        if h <= h_tol or rho >= rho_max:
            break
    W_est = _adj(w_est)
    W_est[np.abs(W_est) < w_threshold] = 0
    return W_est
alarm = 'cancer_ad_500'
dict1 = {}
for dataNo in range(1,11):
    for NO in range(1,6):
        tmp = pd.read_csv(f'C:\\Users\\riyang\\Documents\\BNdata\\result\\{dataNo}_{alarm}_efbn_{NO}.csv', index_col=0)
        n, d, s0, graph_type, sem_type = 100, 20, 20, 'ER', 'gauss'
        dataset = tmp
        names = dataset.columns
        length = len(names)
        tmp = tmp / 2

        tmp[tmp < 0.85] = 0
        X = tmp.values
        W_est = notears_linear(X, lambda1=0.5, lambda2=1, loss_type='l2')

        res = pd.DataFrame(W_est, columns=names, index=names)

        for i in range(length):
            for j in range(length):
                if (res.iloc[i, j] > res.iloc[j, i]):
                    res.iloc[j, i] = 0
                elif (res.iloc[i, j] < res.iloc[j, i]):
                    res.iloc[i, j] = 0
        print(res)
        for index, row in res.iterrows():
            for i in range(len(names)):
                if (row[i] > 0):
                    row[i] = 1
                else:
                    row[i] = 0

        count = 0

        for index, row in res.iterrows():
            for i in range(len(names)):
                if (row[i] > 0):
                    count += 1
        print(count)

        # for index, row in result.iterrows():
        #     for i in range(len(names)):
        #         if (row[i] > 0):
        #             row[i] = 1
        #         else:
        #             row[i] = 0
        #
        #
        # result.to_csv('nb_ad_result.csv',index=True)
        matrix = res
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
        dict1[NO] = modelString
    #
    with open(f'C:\\Users\\riyang\\Documents\BNdata\\{dataNo}_{alarm}_file.json', 'w', encoding='utf-8')as file: 
        json.dump(dict1, file,
                  ensure_ascii=False) 
        file.write('\n')