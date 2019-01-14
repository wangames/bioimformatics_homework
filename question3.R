#  question3
#  author:jbwang
#  3.1
library(data.table)
singlecell<-fread("~/data_set/singlecell_data.txt",sep = "\t",header = T)

#  3.2
cell_Y<-singlecell[singlecell$chr == "Y"]
cell_Y.1<-as.data.frame(cbind(colnames(cell_Y[,-c(1,2)]),as.data.frame(t(cell_Y[,-c(1,2)]))))
colnames(cell_Y.1)<-c("cell_name","Y_express")
cell_Y.1$Y_express<-as.numeric(cell_Y.1$Y_express)
cell_Y.1$sex<-"femina"
for (i in 1:nrow(cell_Y.1)) {
  if(cell_Y.1[i,2] > 0) {cell_Y.1[i,3] = "male"}
  else {next}
}

#  3.3
cell_X<-singlecell[singlecell$chr == "X"]
medians<-function(a)
{
  a<-a[a>0]
  return(median(a))
}
X_med<-apply(cell_X[,-c(1,2)],2,medians)
#X_med1<-as.data.frame(cbind(colnames(cell_X[,-c(1,2)]),t(X_med)))
cell_X.1<-cell_X[,-c(1,2)]
cell_X.1[cell_X.1>0]<-1
percent1<-function(a)
{
  return(sum(a) / length(a))
}
X_per<-round(apply(cell_X.1, 2, percent1), 3)


normal_chr <- as.data.frame(singlecell[singlecell$chr!="X" 
                                      & singlecell$chr!="Y"])
normal_chr_2 <- rbind(X_per,normal_chr[,-c(1,2)])
Ma<-function(a)
{
  a1 = quantile(a[-1],1-a[1])
  a2 = a[-1]
  return(median(a2[a2>a1]))
}
nor_med<-round(apply(normal_chr_2,2,Ma),3)
ratio<-X_med/nor_med
ratio_2<-as.data.frame(cbind(colnames(cell_X[,-c(1,2)]),
                                   as.data.frame(ratio)))
colnames(ratio_2)<-c("cell_name","Mx/Ma")
ratio_2$`Mx/Ma`<-round(ratio_2$`Mx/Ma`,4)
ratio_2$"Mx/Ma %"<-sprintf("%1.2f%%",100*ratio_2$`Mx/Ma`)

#  3.4
library(ggplot2)
ratio_2$sex<-cell_Y.1$sex
ratio_2$gro<-paste(ratio_2$cell_name,ratio_2$sex,sep = "_")
plot1<-ggplot(data = ratio_2,aes(x=gro, y=`Mx/Ma`,group=gro,
                          color=gro))+geom_boxplot()
