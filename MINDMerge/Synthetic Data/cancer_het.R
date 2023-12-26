library(bnlearn)


data_net<-read.net('data/cancer.net')

dag = model2network(modelstring(data_net))

print(dag)
for(NO in 1:10){
  graph = matrix(0,5,4)
  resultFrame = as.data.frame(graph)
  colnames(resultFrame) = c('TP','recall','F1','HD')
  rownames(resultFrame) = c('bde','bdla','bic','h2pc','mmhc')
  
  
  filepath = paste('select/cancer_500_',NO,'.csv',sep='')
  dataset = read.csv(filepath,sep=',',header = TRUE)
  cols <- sapply(dataset, is.character)
  dataset[,cols] <- lapply(dataset[,cols], as.factor)
  cols <- sapply(dataset, is.logical)
  dataset[,cols] <- lapply(dataset[,cols], as.factor)
  
  dataset = na.omit(dataset)
  
  names = colnames(dataset)
  
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
      xval=tabu(dataset,score = 'bde',tabu=10)
    }
    if(method == 2){
      xval=tabu(dataset,score = 'bdla')
    }
    if(method == 3){
      xval=tabu(dataset,score = 'bic',tabu=10)
    }
    if(method == 4){
      xval=tryCatch({
        h2pc(dataset,maximize.args = list(score = 'bde'))
      },warning = function(w){
        print('warning')
        h2pc(dataset,maximize.args = list(score = 'bde'))
      },error =function(e){
        h2pc(dataset)
      })
      
    }
    if(method == 5){
      xval=mmhc(dataset,maximize.args = list(score = 'bde'))
    }
    res = compare(dag, xval, arcs = FALSE)
    arcs = arc.strength(xval,dataset,criterion = 'bde')
    i= length(arcs[,1]) 
    arcsNum = i
    
    
    matrix<-as.data.frame(graph) 
    rownames(matrix) <- names
    colnames(matrix)<-names
    
    matrix_bansui<-as.data.frame(graph) 
    rownames(matrix_bansui) <- names
    colnames(matrix_bansui)<-names
    if(arcsNum != 0){
      for( i in 1:(arcsNum)){
        matrix[arcs[i,1],arcs[i,2]]<-sqrt(length(names)/arcsNum);
        matrix_bansui[arcs[i,1],arcs[i,2]]<-1;
      }
      
    }
    #print(matrix_bansui)
    matrix_sum_de+matrix->matrix_sum_de;  
    matrix_sum_one+matrix_bansui->matrix_sum_one
    
    TP = res$tp
    FP = res$fp
    FN = res$fn
    recall = TP/(TP+FN)
    precision = TP/(TP+FP)
    HD = FP+FN
    print(recall)
    print(precision)
    F1 = 2/(1/recall + 1/precision)
    print(HD)
    
    resultFrame[method,] = c(TP,recall,F1,HD)
    
  }
  matrix_process<-matrix_sum_one*(1/2);
  matrix_result<-matrix_sum_de*matrix_process;
  
  filepath2 = paste('./result/',NO,'_cancer_500_mix_al_result_',sep='')
  filepath1 = paste(filepath2,method,'.csv',sep='')
  write.csv(matrix_sum_one,filepath1,row.names = TRUE,col.names =TRUE,sep='')
  filepath2 = paste('./result/',NO,'_cancer_500_mix_al_efbn_',sep='')
  filepath1 = paste(filepath2,method,'.csv',sep='')
  write.csv(matrix_result,filepath1,row.names = TRUE,col.names =TRUE,sep='')
  
  
  filepath1 = paste('result_cancer_1000_',NO,'.csv',sep='')
  write.csv(resultFrame,filepath1,row.names = TRUE,col.names =TRUE,sep='')
}
