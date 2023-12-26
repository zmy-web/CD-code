
library(bnlearn)
library(Rmpfr)
library(pROC)
library(caret)
library(rjson)
set.seed(100)
options(digits=3)
nodenames = c('fusion')

graph = matrix(0,4,12)
# recall
recallFrame = as.data.frame(graph)
col = c()

hospitalID = 435

jsonModel = fromJSON(file="mimic_het_score_file.json")
string1 = as.data.frame(jsonModel)
nodeNames = colnames(string1)
# recall
recallFrame = as.data.frame(graph)
col = c()
rowname = c('AUC','recall','ACC','F1')
for (i in 1:5){
  col = append(col,i)
}
col = append(col,'mean')
col = append(col,'test')
colnames(recallFrame) = col
rownames(recallFrame) = rowname

sumRecall = 0
sumAUC = 0
recall = c()
auc = c()
sumAcc = 0
ACC = c()
F1score = c()
scoreList = c()
raw_F1=c()
raw_recall=c()
raw_precision=c()
raw_acc=c()
sumf1 = 0
NO = 1
for(X in nodeNames){
  print(X)
  res = model2network(string1[[X]])
  
  # if(NO>5){
  #   NO = NO-5
  # }
  num = NO
  NO= NO+1
  
  
  filepath1 = paste('./hospital/',hospitalID,'_train_',sep = '')
  filepath = paste(filepath1,num,'.csv',sep = '')
  filepath = paste('dataset/mimic_train_',num,'.csv',sep = '')
  
  dataset1 = read.csv(filepath,header = TRUE)
  names = colnames(dataset1)
  print(names)
  cols <- sapply(dataset1, is.numeric)
  listA = which(colSums(dataset1)==0)
  
  for(z in listA){
    A = names[z]
    dataset1[,A]<- factor(dataset1[,A],levels = c(0,1))
  }
  dataset1[,cols] <- lapply(dataset1[,cols], as.factor)
  
  filepath1 = paste('./hospital/',hospitalID,'_test_',sep = '')
  filepath = paste(filepath1,num,'.csv',sep = '')
  filepath = paste('dataset/mimic_test_',num,'.csv',sep = '')
  dataset = read.csv(filepath,header = TRUE)
  names = colnames(dataset)
  cols <- sapply(dataset, is.numeric)
  listA = which(colSums(dataset)==0)
  for(i in listA){
    A = names[i]
    dataset[,A]<- factor(dataset[,A],levels = c(0,1))
  }
  dataset[,cols] <- lapply(dataset[,cols], as.factor)
  scoreList = append(scoreList,score(res,dataset1,type='bic'))

  
  fitted = bn.fit(res,dataset1)
  #print(bf.strength(res, dataset, score = "bds", prior = "marginal"))
  arcs = arc.strength(res,dataset,criterion = 'bde')
  i= length(arcs[,1]) 
  imputed =predict(fitted,dataset,method = 'parents',node='label',prob = TRUE)
  example <-caret::confusionMatrix(imputed,dataset$label,positive = '1')
  print(example)
  raw_F1 = append(raw_F1,2/(1/example$byClass[['Sensitivity']] + 1/example$byClass[['Pos Pred Value']]))
  raw_recall=append(raw_recall,example$byClass[['Sensitivity']])
  raw_acc = append(raw_acc,example$overall['Accuracy'])
  raw_precision = append(raw_precision,example$byClass[['Pos Pred Value']])
  print(raw_precision)
  print(raw_recall)
  print(raw_F1)
  print(i)
  

  
  
  
  features = attr(imputed,"prob")[2,]
  rocobj <- roc(dataset$label, features)
  
  roc_result <- coords(rocobj, "best")
  
  TP <- dim(dataset[as.numeric(dataset$label)==2 & features > roc_result$threshold, ])[1]
  FP <- dim(dataset[as.numeric(dataset$label)==1 & features > roc_result$threshold, ])[1]
  TN <- dim(dataset[as.numeric(dataset$label)==1 & features <= roc_result$threshold, ])[1]
  FN <- dim(dataset[as.numeric(dataset$label)==2 & features <= roc_result$threshold, ])[1]
  
  TPR <- TP / (TP + FN)
  TNR <- TN / (TN + FP)
  acc <- (TP + TN) / (TP + TN + FP + FN)
  precision = TP / (TP + FP)
  F1 = 2/(1/TPR + 1/precision)

  
  recall = append(recall,TPR)
  ACC =append(ACC,acc)
  auc = append(auc,as.numeric(rocobj$auc))
  F1score = append(F1score,F1)
  
  print(rocobj$auc,digits = 4)
  print("recall:")
  print(TPR,digits = 4)
  print("precision:")
  print(precision,digits = 4)
  print("F1:")
  print(F1,digits = 4)
  print("ACC:")
  print(acc,digits = 4)
  print(length(res$arcs))
  #aupr=AUC(obs=dataset$label,pred=as.numeric(features),curve = "PR", main = "PR curve")
}
print('recall')
i = 1
for(re in recall){
  recallFrame['recall',i] = re
  i = i+1
  #cat(paste(re,'\n'))
}
print('meanRecall:')
print(sumRecall/10)

j = 1
print(ACC)
for(acc in ACC){
  recallFrame['ACC',j] = acc
  j = j+1
  #cat(paste(ac,'\n'))
}
j = 1
for(ac in auc){
  recallFrame['AUC',j] = ac
  j = j+1
  #cat(paste(ac,'\n'))
}
print('F1')
j = 1
for(f in F1score){
  recallFrame['F1',j] = f
  j = j+1
  #cat(paste(ac,'\n'))
}
print(scoreList)
write.csv(recallFrame,'3recall.csv',row.names = TRUE,col.names =TRUE,sep='')