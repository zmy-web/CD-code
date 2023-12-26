# Title     : TODO
# Objective : TODO
# Created by: riyang
# Created on: 2022/8/29

r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
library(pROC)
library(PRROC)
library(infotheo)
library(ROCR)
library(caret)
library('bnlearn')
library(modEvA)
library(PRROC) 
set.seed(100)

hospital_list = c(122,141,142,188,420,435)



train_net <- function(train,bl,method,hospitalID,NO){
  names = colnames(train)
  
  graph = matrix(0,length(names),length(names))
  
  VoteDataFrame = as.data.frame(graph)
  colnames(VoteDataFrame) = names
  
  rownames(VoteDataFrame) = names
  
  if(method == 1){
    xval=tabu(train,blacklist = bl,score = 'bde')
  }
  if(method == 2){
    xval=tabu(train,blacklist = bl,score = 'bic')
  }
  if(method == 3){
    xval=tabu(train,blacklist = bl,score = 'bdla')
  }
  if(method == 4){
    xval=h2pc(train,blacklist = bl)
  }
  if(method == 5){
    xval=mmhc(train,blacklist = bl)
  }
  arcs = arc.strength(xval,train,criterion = 'mi')
  print(score(xval,train,type='bic'))
  print(arcs[(arcs$to == 'label'), ])
  i= length(arcs[,1]) #边的数量，懒得改变量名了
  arcsNum = i
  for(j in 1:i){
    from = arcs[j,1]
    to = arcs[j,2]
    VoteDataFrame[from,to] = 1
  }
  filepath2 = paste('./result/',hospitalID,'_result_',sep = '')
  filepath1 = paste(filepath2,NO,'_',sep='')
  filepath = paste(filepath1,method,'.csv',sep = '')
  write.csv(VoteDataFrame,filepath,row.names = TRUE,col.names =TRUE,sep='')
  
  
  
  filepath1 = paste('./hospital/',hospitalID,'_10_test',sep = '')
  filepath = paste(filepath1,num,'.csv',sep = '')
  test = read.csv(filepath,header = TRUE)
  cols <- sapply(test, is.numeric)
  listA = which(colSums(test)==0)
  
  for(z in listA){
    A = names[z]
    test[,A]<- factor(test[,A],levels = c(0,1))
  }
  test[,cols] <- lapply(test[,cols], as.factor)
  
  #训练集和测试集的rbind因子化
  len_train = dim(train)[1]
  len_test = dim(test)[1]

  complete <- rbind(train, test)
  
  complete[] <- lapply(complete, as.factor)
  train = complete[1:len_train, ]
  l = len_train+1
  lf = len_train + len_test
  test = complete[l:lf, ]
  
  
  
  fitted = bn.fit(xval,train)
  
  imputed = predict(fitted,test,method = 'bayes-lw',node='label',prob = TRUE)
  features = attr(imputed,"prob")[2,]
  rocobj <- roc(test$label, features)
  
  example <-caret::confusionMatrix(imputed,test$label,positive = '1')
  print(example)
  raw_F1 = 2/(1/example$byClass[['Sensitivity']] + 1/example$byClass[['Pos Pred Value']])
  raw_recall=example$byClass[['Sensitivity']]
  raw_precision = example$byClass[['Pos Pred Value']]
  
  roc_result <- coords(rocobj, "best")
  # 计算在最佳阈值下混淆矩阵各项的值
  TP <- dim(test[as.numeric(test$label)==2 & features > roc_result$threshold, ])[1]
  FP <- dim(test[as.numeric(test$label)==1 & features > roc_result$threshold, ])[1]
  TN <- dim(test[as.numeric(test$label)==1 & features <= roc_result$threshold, ])[1]
  FN <- dim(test[as.numeric(test$label)==2 & features <= roc_result$threshold, ])[1]
  
  TPR <- TP / (TP + FN)
  TNR <- TN / (TN + FP)
  ACC <- (TP + TN) / (TP + TN + FP + FN)
  precision = TP / (TP + FP)
  F1 = 2*TPR*precision/(TPR + precision)
  print(rocobj$auc)
  print(TPR)
  print(ACC)
  print(F1)
  result = list(method,score(xval,train,type='bde'),arcsNum,rocobj$auc,ACC,TPR,precision,F1,raw_F1,raw_recall,raw_precision)
  return(result)
  
}


get_result <- function(hospitalID,num){
  methods = c('bde','mbde','bic','bdla','qnml')
  filepath1 = paste('./hospital/',hospitalID,'_10_train',sep = '')
  filepath = paste(filepath1,num,'.csv',sep = '')
  Train = read.csv(filepath,header = TRUE)
  names = colnames(Train)
  cols <- sapply(Train, is.numeric)
  listA = which(colSums(Train)==0)
  for(i in listA){
    A = names[i]
    Train[,A]<- factor(Train[,A],levels = c(0,1))
  }
  Train[,cols] <- lapply(Train[,cols], as.factor)
  
  
  
  
  
  #setting blacklist
  ofn <- setdiff(colnames(Train),c('sex','label'))
  blsex = tiers2blacklist(list('sex',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  bl_s=rbind(blsex,bllabel,c('label','sex'))
  
  ofn <- setdiff(colnames(Train),c('age','label'))
  blsex = tiers2blacklist(list('age',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  bl_a=rbind(blsex,bllabel,c('label','age'))
  
  ofn <- setdiff(colnames(Train),c('race','label'))
  blsex = tiers2blacklist(list('race',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  bl_r=rbind(blsex,bllabel,c('label','race'),c('PRO3','label'))
  
  ofn <- setdiff(colnames(Train),c('age','race','label'))
  blage = tiers2blacklist(list('age',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  blrace = tiers2blacklist(list('race',ofn))
  bl_ar=rbind(blage,blrace,bllabel,c('label','age'),c('race','age'),c('age','race'),c('label','race'))
  
  ofn <- setdiff(colnames(Train),c('age','sex','label'))
  blage = tiers2blacklist(list('age',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  blrace = tiers2blacklist(list('sex',ofn))
  bl_as=rbind(blage,blrace,bllabel,c('label','age'),c('sex','age'),c('age','sex'),c('label','sex'))
  
  ofn <- setdiff(colnames(Train),c('race','sex','label'))
  blage = tiers2blacklist(list('race',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  blrace = tiers2blacklist(list('sex',ofn))
  bl_rs=rbind(blage,blrace,bllabel,c('label','race'),c('sex','race'),c('race','sex'),c('label','sex'))
  
  ofn <- setdiff(colnames(Train),c('race','sex','age','label'))
  blrace = tiers2blacklist(list('race',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  blsex = tiers2blacklist(list('sex',ofn))
  blage = tiers2blacklist(list('age',ofn))
  bl_ars=rbind(blage,blrace,blsex,bllabel,c('label','race'),c('label','age'),c('age','sex'),c('age','race'),c('sex','race'),c('race','sex'),c('race','age'),c('sex','age'),c('label','sex'))
  
  
  ofn <- setdiff(colnames(Train),c('label'))
  bl_ =tiers2blacklist(list(ofn,'label'))
  
  if(num %in% c(10)){
    blackList = bl_s
  }
  if(num %in% c(4)){
    blackList = bl_r
  }
  if(num %in% c(2)){
    blackList = bl_rs
  }
  if(num %in% c(1,3,5,6,7,8,9)){
    blackList = bl_
  }
  # blackList = bl_r
  blackList = bl_
  print(blackList)
  graph = matrix(0,5,11)
  netSets = as.data.frame(graph)
  colnames(netSets) = c('method','scores','arcNums','AUC','ACC','recall','precision','f1','raw_F1','raw_recall','raw_precision')
  rownames(netSets) = c(1:5)
  i = 1
  for(method in 1:5){
    result <- train_net(Train,blackList,method,hospitalID,1)
    netSets[i,] = result[1:11]
    i = i+1
  }
  filepath = paste('bn_result_',num,'.csv',sep = '')
  write.csv(netSets,filepath,row.names = TRUE,col.names =TRUE,sep='')
}

hosiptalID = 435
for(num in 1:10){
  print(num)
  print(num)
  get_result(hosiptalID,num)
}