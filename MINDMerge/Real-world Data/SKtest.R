# 安装并加载ScottKnott包
library(ScottKnott)

# 创建数据
data(CRD2)
data(sorghum)
CRD2 = read.csv('122_AUC.csv')
## From: formula
sk1 <-SK(y ~ r+x,data=CRD2,
               which='x',sig.level=0.1)

plot.SK(sk1,title='AUC SK test',result=F,replicates=F,
     disp='ci',col=c('red','red','red','green','green','green','green','green','green','green'),
     d.col=c('red','red','red','green','green','green','green','green','green','green'))

