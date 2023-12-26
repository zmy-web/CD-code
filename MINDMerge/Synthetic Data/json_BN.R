library(bnlearn)
library(rjson)
# 
# graph = matrix(0,1,4)
# resultFrame = as.data.frame(graph)
# colnames(resultFrame) = c('TP','recall','F1','HD')
# rownames(resultFrame) = nodeNames
# 
# for (i in 1:10) {
  # filename = paste(i,'_alarm_200_ad_500_file.json',sep='')
  filename = '10_asia_500_mix_al_file.json'
  jsonModel = fromJSON(file=filename)
  string1 = as.data.frame(jsonModel)
  
  
  # dag = model2network('[BirthAsphyxia][Disease|BirthAsphyxia][Age|Disease:Sick][LVH|Disease][DuctFlow|Disease][CardiacMixing|Disease][LungParench|Disease][LungFlow|Disease][Sick|Disease][HypDistrib|DuctFlow:CardiacMixing][HypoxiaInO2|CardiacMixing:LungParench][CO2|LungParench][ChestXray|LungParench:LungFlow][Grunting|LungParench:Sick][LVHreport|LVH][LowerBodyO2|HypDistrib:HypoxiaInO2][RUQO2|HypoxiaInO2][CO2Report|CO2][XrayReport|ChestXray][GruntingReport|Grunting]
  # ')
  data_net<-read.net('data/asia.net')
  
  dag = model2network(modelstring(data_net))
  
  # dag = model2network(modelstring)
  nodeNames = colnames(string1)
  
  graph = matrix(0,length(nodeNames),4)
  resultFrame = as.data.frame(graph)
  colnames(resultFrame) = c('recall','precision','F1','SHD')
  rownames(resultFrame) = nodeNames
  for(X in nodeNames){
    print('\n')
    current = model2network(string1[[X]])
    res = compare(dag, current, arcs = FALSE)
    shdRES = shd(current,dag)
    print(111111)
    print(shdRES)
    TP = res$tp
    FP = res$fp
    FN = res$fn
    recall = TP/(TP+FN)
    precision = TP/(TP+FP)
    HD = FP+FN
    F1 = 2/(1/recall + 1/precision)
    
    print(recall)
    print(F1)
    print(HD)
    SHD = shd(current,dag)
    
    resultFrame[X,] = c(recall,precision,F1,SHD)
  }
  filepath = paste(filename,'_result.csv',sep='')
  write.csv(resultFrame,filepath,col.names = T,row.names = T)
# }
# write.csv(resultFrame,'result.csv',col.names = T,row.names = T)