Time<-(rep(c(0),10))
Num<-1:10
RES<-double(10)
t0<-strptime(Sys.time(),"%Y%m%d%H%M%S")

# 1 вектор XA от 100 до 200 с шагом 5
xA<-seq(100,200, by=5)
sum(xA)
RES[1]=sum(xA)
t1<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[1]=t1-t0
#2 
length(xA)
RES[2]=length(xA)
t2<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[2]=t2-t1
#3
mean(xA)
RES[3]=mean(xA)
t3<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[3]=t3-t2
#4
col=length(xA)
xnorm=rnorm(col+7,5)
RES[4]=round(sd(xnorm))
t4<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[4]=t4-t3
#4
arr=array(xA,dim = c(5, round(col/5)))
RES[5]=round(sum(sin(arr)),4)
t5<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[5]=t5-t4
#5

xA
matr=matrix(xA[1:20],nrow=5)
matr=matr[-c(2,5),]
nrow(matr)
ncol(matr)
RES[6]=nrow(matr)+ncol(matr)
t6<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[6]=t6-t5
#6
list=list(rep(c(TRUE,FALSE),5),rep(c(TRUE,FALSE),5),rep(c(TRUE,FALSE),5))
list[[1]]||list[[2]]||list[[3]]

RES[7]=list[[1]]||list[[2]]||list[[3]]
t7<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[7]=t7-t6
#7
identical(matr,arr)
RES[8]=identical(matr,arr)
t8<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[8]=t8-t7
#8
arr=arr[-c(2,5),]
identical(matr,arr)
RES[9]=identical(matr,arr)
t9<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[9]=t9-t8
arr
matr
#9
result=data.frame(num=Num,res=RES,time=Time)
t10<-strptime(Sys.time(),"%Y%m%d%H%M%S")
Time[10]=t10-t9

View(result)
