
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

hospitalID=435
num = 1


sigmoid = function(x,a=1){1/(1+exp(-a*x))}
maxmin<-function(matrix,ncol){
  for( q in 1:ncol){
    for(p in 1:ncol){
      matrix[q,p]<-(matrix[q,p]-min(matrix))/(max(matrix)-min(matrix))
    }
  }
  return(matrix)
}
methods = c('k2','bds','fnml','mbde','bdla')
graph = matrix(0,10,7)
wsFrame = as.data.frame(graph)
colnames(wsFrame) = methods
rownames(wsFrame) = c(1:10)
for (num in 1:10) {
  for(method in 1:5){
    
    filepath1 = paste('./hospital/',hospitalID,'_10_train',sep = '')
    filepath = paste(filepath1,num,'.csv',sep = '')
    dataset = read.csv(filepath,header = TRUE)
    names = colnames(dataset)
    print(names)
    graph = matrix(0,100,11)
    netSets = as.data.frame(graph)
    colnames(netSets) = c('method','scores','arcNums','AUC','ACC','recall','precision','f1','raw_F1','raw_recall','raw_precision')
    rownames(netSets) = c(1:100)
    
    
    graph = matrix(0,length(names),length(names))
    matrix_sum_de = as.data.frame(graph)
    colnames(matrix_sum_de) = names
    rownames(matrix_sum_de) = names
    
    matrix_sum_one= as.data.frame(graph)
    colnames(matrix_sum_one) = names
    rownames(matrix_sum_one) = names
    
    for(NO in 1:50){
      print(NO)
      filepath1 = paste('./hospital/',hospitalID,'_10_train',sep = '')
      filepath2 = paste(filepath1,num,'_',sep = '')
      filepath = paste(filepath2,NO,'.csv',sep = '')
      dataset = read.csv(filepath,header = TRUE)
      names = colnames(dataset)
      
      ofn <- setdiff(colnames(dataset),c('sex','label'))
      blsex = tiers2blacklist(list('sex',ofn))
      bllabel =tiers2blacklist(list(ofn,'label'))
      bl_s=rbind(blsex,bllabel,c('label','sex'))
      
      ofn <- setdiff(colnames(dataset),c('age','label'))
      blsex = tiers2blacklist(list('age',ofn))
      bllabel =tiers2blacklist(list(ofn,'label'))
      bl_a=rbind(blsex,bllabel,c('label','age'))
      
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
      
      
      ofn <- setdiff(colnames(dataset),c('label'))
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
      
      blackList = bl_
      #
      cols <- sapply(dataset, is.numeric)
      listA = which(colSums(dataset)==0)
      for(i in listA){
        A = names[i]
        dataset[,A]<- factor(dataset[,A],levels = c(0,1))
      }
      dataset[,cols] <- lapply(dataset[,cols], as.factor)
      
      
      graph = matrix(0,length(names),length(names))
      VoteDataFrame = as.data.frame(graph)
      colnames(VoteDataFrame) = names
      rownames(VoteDataFrame) = names
      
      if(method == 1){
        xval=tabu(dataset,blacklist = blackList,score = 'bde')
      }
      if(method == 2){
        xval=tabu(dataset,blacklist = blackList,score = 'bdla')
      }
      if(method == 3){
        xval=tabu(dataset,blacklist = blackList,score = 'bic')
      }
      if(method == 4){
        xval=tryCatch({
          h2pc(dataset,blacklist = blackList)
        },warning = function(w){
          print('warning')
          h2pc(dataset,blacklist = blackList)
        },error =function(e){
          h2pc(dataset)
        })
        
      }
      if(method == 5){
        xval=mmhc(dataset,blacklist = blackList)
      }
      #
      arcs = arc.strength(xval,dataset,criterion = 'bde')
      print(arcs)
      i= length(arcs[,1]) #
      arcsNum = i
      print(i)
      for(j in 1:i){
        from = arcs[j,1]
        to = arcs[j,2]
        VoteDataFrame[from,to] = 1
      }
      
      
      strengthDataFrame = as.data.frame(graph)
      colnames(strengthDataFrame) = names
      rownames(strengthDataFrame) = names
      
      
      sampleNums = dim(dataset)[1]
      
      x<-c(0)
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
      #print(matrix_bansui)
      matrix_sum_de+matrix->matrix_sum_de;  
      matrix_sum_one+matrix_bansui->matrix_sum_one
      
    }
    np = 1
    matrix_process<-matrix_sum_one*(1/50);
    matrix_result<-matrix_sum_de*matrix_process;
    
 
    # matrix_update<-maxmin(matrix_result,length(names));
    filepath2 = paste('./result/',hospitalID,'_10_ad_result_',sep='')
    filepath1 = paste(filepath2,num,'_',sep='')
    filepath = paste(filepath1,method,'.csv',sep = '')
    write.csv(matrix_sum_one,filepath,row.names = TRUE,col.names =TRUE,sep='')
    filepath2 = paste('./result/',hospitalID,'_10_ad_efbn_',sep='')
    filepath1 = paste(filepath2,num,'_',sep='')
    filepath = paste(filepath1,method,'.csv',sep = '')
    write.csv(matrix_result,filepath,row.names = TRUE,col.names =TRUE,sep='')
    
  }
}






