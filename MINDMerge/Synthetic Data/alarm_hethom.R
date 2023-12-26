library(bnlearn)
for(dataNO in 1:10){
  print(dataNO)
filepath = paste('alarm_10000_',dataNO,'.csv',sep='')
data = read.csv(filepath,sep=',',header = TRUE)
names = colnames(data)


for(NO in 1:50){
  print(NO)
  set.seed(NO)
  size = dim(data)[1]
  dataset = data[sample(1:nrow(data), size,
                        replace=TRUE),]
  
  
  cols <- sapply(dataset, is.character)
  dataset[,cols] <- lapply(dataset[,cols], as.factor)
  
  
  cols <- sapply(dataset, is.logical)
  dataset[,cols] <- lapply(dataset[,cols], as.factor)
  dataset$ANAPHYLAXIS  <- factor(dataset$ANAPHYLAXIS  , levels=c('FALSE', 'TRUE'))
  dataset$PULMEMBOLUS   <- factor(dataset$PULMEMBOLUS   , levels=c('FALSE', 'TRUE'))
  dataset = na.omit(dataset)
  graph = matrix(0,length(names),length(names))
  matrix_sum_de = as.data.frame(graph)
  colnames(matrix_sum_de) = names
  rownames(matrix_sum_de) = names
  
  matrix_sum_one= as.data.frame(graph)
  colnames(matrix_sum_one) = names
  rownames(matrix_sum_one) = names
  
  for(method in 1:5){
    graph = matrix(0,length(names),length(names))
    VoteDataFrame = as.data.frame(graph)
    colnames(VoteDataFrame) = names
    rownames(VoteDataFrame) = names
    cchl_hr = tiers2blacklist(list('HR','CCHL'))
    if(method == 1){
      xval=tabu(dataset,score = 'bde',tabu=10)
    }
    if(method == 2){
      xval=tabu(dataset,score = 'bdla',tabu=10)
    }
    if(method == 3){
      xval=tabu(dataset,score = 'bic',tabu=10)
    }
    if(method == 4){
      xval=tryCatch({
        h2pc(dataset,maximize.args = list(score = 'k2'))
      },warning = function(w){
        print('warning')
        h2pc(dataset,maximize.args = list(score = 'k2'))
      },error =function(e){
        h2pc(dataset)
      })
      
    }
    if(method == 5){
      xval=mmhc(dataset,maximize.args = list(score = 'k2'))
    }
    arcs = arc.strength(xval,dataset,criterion = 'bde')
    i= length(arcs[,1]) #
    arcsNum = i
    
    matrix<-as.data.frame(graph)  # 
    rownames(matrix) <- names
    colnames(matrix)<-names
    
    matrix_bansui<-as.data.frame(graph) #
    rownames(matrix_bansui) <- names
    colnames(matrix_bansui)<-names
    
    for( i in 1:(arcsNum)){
      matrix[arcs[i,1],arcs[i,2]]<-sqrt(length(names)/arcsNum);
      matrix_bansui[arcs[i,1],arcs[i,2]]<-1;
    }
    #print(matrix_bansui)
    matrix_sum_de+matrix->matrix_sum_de;  #
    matrix_sum_one+matrix_bansui->matrix_sum_one
    
}
# 
  np = 1
  matrix_process<-matrix_sum_one*(1/5);
  matrix_result<-matrix_sum_de*matrix_process;
    
  filepath2 = paste('./result/',dataNO,'_alarm_10000_result_',sep='')
  filepath1 = paste(filepath2,NO,'.csv',sep='')
  write.csv(matrix_sum_one,filepath1,row.names = TRUE,col.names =TRUE,sep='')
  filepath2 = paste('./result/',dataNO,'_alarm_10000_efbn_',sep='')
  filepath1 = paste(filepath2,NO,'.csv',sep='')
  write.csv(matrix_result,filepath1,row.names = TRUE,col.names =TRUE,sep='')

}
}