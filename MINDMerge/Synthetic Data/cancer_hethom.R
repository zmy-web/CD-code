library(bnlearn)
for(dataNO in 1:10){
  print(dataNO)
  filepath = paste('select/cancer_500_',dataNO,'.csv',sep='')
  data = read.csv(filepath,sep=',',header = TRUE)
  print(data)
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
    # dataset$AppDtGnTm <- factor(dataset$AppDtGnTm  , levels=c('Fast_Enough', 'Too_Long'))
    dataset$Cancer<- factor(dataset$Cancer , levels=c('False', 'True'))
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
      if(method == 1){
        xval=tabu(dataset,score = 'bde')
      }
      if(method == 2){
        xval=tabu(dataset,score = 'bdla')
      }
      if(method == 3){
        xval=tabu(dataset,score = 'bic')
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
      arcs = arc.strength(xval,dataset,criterion = 'bde')
      i= length(arcs[,1]) #number of arc，
      arcsNum = i
      matrix<-as.data.frame(graph)  # 
      rownames(matrix) <- names
      colnames(matrix)<-names
      
      matrix_bansui<-as.data.frame(graph) # 0-1matrix
      rownames(matrix_bansui) <- names
      colnames(matrix_bansui)<-names
      if(arcsNum != 0){
        for( i in 1:(arcsNum)){
          matrix[arcs[i,1],arcs[i,2]]<-sqrt(length(names)/arcsNum);
          matrix_bansui[arcs[i,1],arcs[i,2]]<-1;
        }
        
      }
      
      #print(matrix_bansui)
      matrix_sum_de+matrix->matrix_sum_de;  # weighted matrxi 
      matrix_sum_one+matrix_bansui->matrix_sum_one
      
    }
    # n is the number of algorithm,np is the proportion of sample
    np = 1
    matrix_process<-matrix_sum_one*(1/5);
    matrix_result<-matrix_sum_de*matrix_process;
    
    filepath2 = paste('./result/',dataNO,'_cancer_500_result_',sep='')
    filepath1 = paste(filepath2,NO,'.csv',sep='')
    write.csv(matrix_sum_one,filepath1,row.names = TRUE,col.names =TRUE,sep='')
    filepath2 = paste('./result/',dataNO,'_cancer_500_efbn_',sep='')
    filepath1 = paste(filepath2,NO,'.csv',sep='')
    write.csv(matrix_result,filepath1,row.names = TRUE,col.names =TRUE,sep='')
    
  }
}