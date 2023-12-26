library(bnlearn)
library(pROC)
library(PRROC)
library(infotheo)
library(ROCR)
library(caret)
library('bnlearn')
library(modEvA)
library(PRROC) 

hospitalID=435
for(NO in 1:5){
  print('\n')
  graph = matrix(0,5,4)
  resultFrame = as.data.frame(graph)
  colnames(resultFrame) = c('TP','recall','F1','HD')
  rownames(resultFrame) = c('bde','bdla','bic','h2pc','mmhc')
  filepath1 = paste('./hospital/',hospitalID,'_train_',sep = '')
  filepath = paste(filepath1,NO,'.csv',sep = '')
  
  print(filepath)
  dataset = read.csv(filepath,sep=',',header = TRUE)
  names = colnames(dataset)
  cols <- sapply(dataset, is.numeric)
  listA = which(colSums(dataset)==0)
  for(i in listA){
    A = names[i]
    dataset[,A]<- factor(dataset[,A],levels = c(0,1))
  }
  dataset[,cols] <- lapply(dataset[,cols], as.factor)
  
  dataset = na.omit(dataset)
  
  
  
  
  # setting blacklist
  ofn <- setdiff(colnames(dataset),c('sex','label'))
  blsex = tiers2blacklist(list('sex',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  bl_s=rbind(blsex,bllabel,c('label','sex'))
  
  ofn <- setdiff(colnames(dataset),c('age','label'))
  blage = tiers2blacklist(list('age',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  bl_a=rbind(blage,bllabel,c('label','age'))
  
  ofn <- setdiff(colnames(dataset),c('race','label'))
  blsex = tiers2blacklist(list('race',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  bl_r=rbind(blsex,bllabel,c('label','race'))
  
  ofn <- setdiff(colnames(dataset),c('age','race','label'))
  blage = tiers2blacklist(list('age',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  blrace = tiers2blacklist(list('race',ofn))
  bl_ar=rbind(blage,blrace,bllabel,c('label','age'),c('race','age'),c('age','race'),c('label','race'))
  
  ofn <- setdiff(colnames(dataset),c('age','sex','label'))
  blage = tiers2blacklist(list('age',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  blrace = tiers2blacklist(list('sex',ofn))
  bl_as=rbind(blage,blrace,bllabel,c('label','age'),c('sex','age'),c('age','sex'),c('label','sex'),c('PRO3','label'))
  
  ofn <- setdiff(colnames(dataset),c('race','sex','label'))
  blage = tiers2blacklist(list('race',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  blrace = tiers2blacklist(list('sex',ofn))
  bl_rs=rbind(blage,blrace,bllabel,c('label','race'),c('sex','race'),c('race','sex'),c('label','sex'))
  
  ofn <- setdiff(colnames(dataset),c('race','sex','age','label'))
  blrace = tiers2blacklist(list('race',ofn))
  bllabel =tiers2blacklist(list(ofn,'label'))
  blsex = tiers2blacklist(list('sex',ofn))
  blage = tiers2blacklist(list('age',ofn))
  bl_ars=rbind(blage,blrace,blsex,bllabel,c('label','race'),c('label','age'),c('age','sex'),c('age','race'),c('sex','race'),c('race','sex'),c('race','age'),c('sex','age'),c('label','sex'))
  
  
  # if(num %in% c(10)){
  #   blackList = bl_s
  # }
  # if(num %in% c(4)){
  #   blackList = bl_r
  # }
  # if(num %in% c(2)){
  #   blackList = bl_rs
  # }
  print(NO)
  print(NO)
  if(NO %in% c(1,2,3,5)){
    bl = bl_a
  }
  if(NO %in% c(2)){
    bl = bl_a
  }
  if(NO %in% c(4)){
    bl = bl_as
  }
  
  ofn <- setdiff(colnames(dataset),c('label'))
  bl_ =tiers2blacklist(list(ofn,'label'))
  
  bl = bl_
  print(bl)
  graph = matrix(0,length(names),length(names))
  matrix_sum_de = as.data.frame(graph)
  colnames(matrix_sum_de) = names
  rownames(matrix_sum_de) = names
  
  matrix_sum_one= as.data.frame(graph)
  colnames(matrix_sum_one) = names
  rownames(matrix_sum_one) = names
  
  
  for(method in 4:5){
    
    print(method)
    
    if(method == 1){
      xval=tabu(dataset,blacklist = bl,score = 'bde')
    }
    if(method == 2){
      xval=tabu(dataset,blacklist = bl,score = 'bic')
    }
    if(method == 3){
      xval=tabu(dataset,blacklist = bl,score = 'bdla')
    }
    if(method == 4){
      xval=h2pc(dataset,blacklist = bl)
    }
    if(method == 5){
      xval=mmhc(dataset,blacklist = bl)
    }
    
    arcs = arc.strength(xval,dataset,criterion = 'mi')
    print(arcs[(arcs$to == 'label'), ])
    print(score(xval,dataset,type='bic'))
    i= length(arcs[,1]) 
    arcsNum = i
    
    
    
    
    filepath1 = paste('./hospital/',hospitalID,'_test_',sep = '')
    filepath = paste(filepath1,NO,'.csv',sep = '')
    
    test = read.csv(filepath,header = TRUE)
    cols <- sapply(test, is.numeric)
    listA = which(colSums(test)==0)
    
    for(z in listA){
      A = names[z]
      test[,A]<- factor(test[,A],levels = c(0,1))
    }
    test[,cols] <- lapply(test[,cols], as.factor)
    

    len_train = dim(dataset)[1]
    len_test = dim(test)[1]
    
    complete <- rbind(dataset, test)
    print(12)
    complete[] <- lapply(complete, as.factor)
    dataset = complete[1:len_train, ]
    l = len_train+1
    lf = len_train + len_test
    test = complete[l:lf, ]
    
    
    
    fitted = bn.fit(xval,dataset)
    
    imputed = predict(fitted,test,method = 'bayes-lw',node='label',prob = TRUE)
    features = attr(imputed,"prob")[2,]
    rocobj <- roc(test$label, features)
    
    example <-caret::confusionMatrix(imputed,test$label,positive = '1')
    print(example)
    raw_F1 = 2/(1/example$byClass[['Sensitivity']] + 1/example$byClass[['Pos Pred Value']])
    raw_recall=example$byClass[['Sensitivity']]
    raw_precision = example$byClass[['Pos Pred Value']]
    
    roc_result <- coords(rocobj, "best")

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
    result = list(method,score(xval,dataset,type='bde'),arcsNum,rocobj$auc,ACC,TPR,precision,F1,raw_F1,raw_recall,raw_precision)
    # return(result)
    
    matrix<-as.data.frame(graph)  
    rownames(matrix) <- names
    colnames(matrix)<-names
    
    matrix_bansui<-as.data.frame(graph) 
    rownames(matrix_bansui) <- names
    colnames(matrix_bansui)<-names
    
    for( i in 1:(arcsNum)){
      matrix[arcs[i,1],arcs[i,2]]<-sqrt(length(names)/arcsNum);
      matrix_bansui[arcs[i,1],arcs[i,2]]<-1;
    }
    matrix_sum_de+matrix->matrix_sum_de;  
    matrix_sum_one+matrix_bansui->matrix_sum_one
    
  }
  
  np = 1
  matrix_process<-matrix_sum_one*(1/2);
  matrix_result<-matrix_sum_de*matrix_process;
  
  filepath2 = paste('./result/',hospitalID,'_het_result_',sep='')
  filepath1 = paste(filepath2,NO,'.csv',sep='')
  write.csv(matrix_sum_one,filepath1,row.names = TRUE,col.names =TRUE,sep='')
  filepath2 = paste('./result/',hospitalID,'_het_mix_efbn_',sep='')
  filepath1 = paste(filepath2,NO,'.csv',sep='')
  write.csv(matrix_result,filepath1,row.names = TRUE,col.names =TRUE,sep='')
  
  # filepath1 = paste('_10000_',NO,'.csv',sep='')
  # write.csv(resultFrame,filepath1,row.names = TRUE,col.names =TRUE,sep='')
}
