#13 GooglePlay

#Выполнить дескриптивный анализ данных
app.01 <- read.csv("C:/Users/Кристина/Desktop/все/R/lab6/13_GooglePlay/googleplaystore.csv")

#удаляем одинаковые строки
app.01 <- app.01[!duplicated(app.01),]

#анализ данных по различным критериям
pie(table(app.01$Type), labels=c("Free", "Paid"), col=c("lightblue", "orange"), main="Оплата приложений")
barplot(table(app.01$Content.Rating), las=1, col="lightgreen", main="Возраст", ylab="Count")

#удаляем ненужные столбцы(оставляем Rating, Reviews, Size и Installs.)
app.01 <- app.01[,-2]
#7 раз
app.01 <- app.01[,-7]
app.01 <- app.01[,-6]


#преобразуем неверные значения и удаляем эти строки 
app.01$Size <- gsub("M", "", app.01$Size)  # удаление буквенного обозначения Мб
app.01$Size <- gsub("k", "000", app.01$Size)  # преобразование Кб в числовой формат
app.01$Size[app.01$Size=='Varies with device'] <- NA
app.01 <- na.omit(app.01)
app.01$Size <- as.numeric(app.01$Size)  # преобразование столбца Size в числовой формат


app.01$Installs <- gsub("\\+", "", app.01$Installs)  # удаление символа '+'
app.01$Installs <- gsub(",", "", app.01$Installs)  # удаление символа ','
app.01$Installs[app.01$Installs == "Free"] <- NA  # замена значения "Free" на NA
app.01 <- na.omit(app.01)
app.01$Installs <- as.numeric(app.01$Installs)  # преобразование столбца Installs в числовой формат
options(scipen = 999)

app.01$Reviews <- as.numeric(app.01$Reviews)  # преобразование столбца Installs в числовой формат




df3<-app.01
App_name<-app.01$App


#   Шаг 3.  Стандартизация переменных.
#   В данной задаче переменные существенно различны.
#   Стандартизировать надо.

app.02 <- scale(app.01[,2:5], center = TRUE, scale = TRUE)

#   Исключим колонку "Страна"
app.02<-app.01,-1]
maxs <- apply(app.02, 2, max)
mins <- apply(app.02, 2, min)

maxs
mins

app.02 <- scale(app.02, center = mins, scale = maxs - mins)
app.02
#   Вернем колонку "Страна"

app.02<-data.frame(App_name,app.02)

#   Создаем матрицу попарных расстояний (по умолчанию - Евклидово расстояние)
dist.app <- dist(app.02 [,2:5])
#dist.app
#   Проводим кластерный анализ, 
#   результаты записываем в список clust.protein
#   hclust ожидает матрицу расстояния, а не исходные данные.

clust.app <- hclust(dist.app, "ward.D")

#  Смотрим краткую сводку результатов анализа
clust.app$height


#  Шаг 4.  Построение дендрограммы
clust.app

# повернуть названия приложений на 90 градусов
par(mar=c(5,10,5,5)) # увеличиваем отступы между графиком и границами окна
plot(clust.app, labels = app.02$App_name, main = "Дендрограмма", ylab = "Cходство", xlab = "fdd", cex = 0.2)


k = 4
rect.hclust(clust.app, k = 4, border="red")
clust.app$height
plot(clust.app$height, type='b',xlab="Номер компоненты",ylab = "Собственное значение") # каменная осыпь

#  Разделим приложения на 4 кластера
#  Вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.app, k) 
# Разрезает дерево, например, полученное в результате hclust, на несколько групп 
# путем указания желаемого количества групп или высоты среза.
groups

dend <- as.dendrogram(clust.app)
#if(!require(dendextend)) install.packages("dendextend"); 
library(dendextend)
dend <- color_branches(dend, k) 
plot(dend)
#dev.off() 


#  Выведем страны соответсвенно сформированным кластерам 
app.01[groups==1, 1]
app.01[groups==2, 1]
app.01[groups==3, 1]
app.01[groups==4, 1]




#  Для каждого столбц определяем, 
#  какая доля стран в среднем кластере приобретала этот столбец

#   в 1-ом кластере
g1<-colMeans(app.01[groups==1, 2:5])
g1
#   во 2-ом кластере
g2<-colMeans(app.01[groups==2, 2:5])
#   в 3-ем кластере
g3<-colMeans(app.01[groups==3, 2:5])
#   во 4-ом кластере
g4<-colMeans(app.01[groups==4, 2:5])


g11<-colMeans(app.02[groups==1, 2:5])
g11
#   во 2-ом кластере
g12<-colMeans(app.02[groups==2, 2:5])
g12
#   в 3-ем кластере
g13<-colMeans(app.02[groups==3, 2:5])
#   во 4-ом кластере
g14<-colMeans(app.02[groups==4, 2:5])


#   делаем дата фрейм из векторов групп кластеров


df2<-data.frame(g11,g12,g13,g14);
df<-data.frame(g1,g2,g3,g4); df
df1<-t(df2); df1

barplot(as.matrix(df2), col=c("magenta","red","yellow","blue","green","orange")) 
legend("topright",cex=0.6, rownames(df2),fill=c("magenta","red","yellow","blue","green","orange") )
barplot(df1[,1], ylim=range(pretty(c(0,max(df1[,1])))), 
        main="Рейтинг", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))

barplot(df1[,2], ylim=range(pretty(c(0,max(df1[,2])))), 
        main="Отзывы", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))

barplot(df1[,3], ylim=range(pretty(c(0,max(df1[,3])))), 
        main="Размер", 
        col=c("magenta","red","yellow","blue","green"))
legend("topleft",rownames(df1),fill=c("magenta","red","yellow","blue","green","orange") )
barplot(df1[,4], ylim=range(pretty(c(0,max(df1[,4])))), 
        main="Скачивание", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))


library (lattice)
library("scatterplot3d")


df3
df3["Group"]<-groups
# удаляем значения
#выведем график рассеяния с минимальным количеством параметров с выделением имени
xyplot(Rating ~ Installs,group = Group, data = df3,auto.key = TRUE,pch = 20,cex = 1.5)

boxplot(Rating~Group , data =df3, ylab = "Рейтинг", frame = FALSE, col = rainbow(4))
# График, классифицирующий типы согласно их полей
xyplot(Size~Rating|Group,data=df3, grid = T, auto.key=TRUE,pch = 20,cex = 1.5)

#Построим трехмерный график наших классов
cloud(Rating~Reviews*Installs, group = Group, data = df3, auto.key = TRUE,pch = 20,cex = 1.5) 
packages <- c('ggplot2', 'dplyr', 'tidyr', 'tibble')
#install.packages(packages)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
df3 %>%
  ggplot(aes(Reviews, Installs, color=Group))+geom_point()




#_________________ЧАСТЬ 2

install.packages("vctrs")
install.packages('tibble', repos = 'http://cran.rstudio.com/', type = 'source')
install.packages('klaR', dependencies = TRUE)

library(klaR)

app.01 <- read.csv("C:/Users/Кристина/Desktop/все/R/lab6/13_GooglePlay/googleplaystore.csv")

#удаляем одинаковые строки
app.01 <- app.01[!duplicated(app.01),]

#удаляем ненужные столбцы(оставляем Rating, Reviews, Size и Installs.)
app.01 <- app.01[,-2]
#7 раз
app.01 <- app.01[,-7]
app.01 <- app.01[,-6]


#преобразуем неверные значения и удаляем эти строки 
app.01$Size <- gsub("M", "", app.01$Size)  # удаление буквенного обозначения Мб
app.01$Size <- gsub("k", "000", app.01$Size)  # преобразование Кб в числовой формат
app.01$Size[app.01$Size=='Varies with device'] <- NA
app.01 <- na.omit(app.01)
app.01$Size <- as.numeric(app.01$Size)  # преобразование столбца Size в числовой формат


app.01$Installs <- gsub("\\+", "", app.01$Installs)  # удаление символа '+'
app.01$Installs <- gsub(",", "", app.01$Installs)  # удаление символа ','
app.01$Installs[app.01$Installs == "Free"] <- NA  # замена значения "Free" на NA
app.01 <- na.omit(app.01)
app.01$Installs <- as.numeric(app.01$Installs)  # преобразование столбца Installs в числовой формат
options(scipen = 999)

app.01$Reviews <- as.numeric(app.01$Reviews)  # преобразование столбца Installs в числовой формат


my_data<-app.01[1:1000,]
my_data
groups[1:1000]
my_data$Group<- c(as.factor(groups[1:1000]))

naive_df <- NaiveBayes(my_data$Group ~ ., data = my_data) 
naive_df$tables 
naive_df




#делаем графики по байсу
opar=par() 
opar
layout(matrix(1:6, nrow = 2, ncol = 3))


plot(naive_df, lwd = 2, legendplot = TRUE,cex=0.3)
legend(x = "topright", y = "top", lty = 1:3, cex = 0.8)
#восстановление
par=opar

# Классификация Decision Tree


set.seed(1234)
ind <- sample(2, nrow(my_data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- my_data[ind==1,]
testData <- my_data[ind==2,] 
nrow(trainData)
nrow(testData)
nrow(my_data)
install.packages('party')
library(party)
my_data
myFormula <- Group ~ Rating + Reviews + Size + Installs 
df_ctree <- ctree(myFormula, data=trainData)
df_ctree
table(predict(df_ctree), trainData$Group) 
predict(df_ctree)
plot(predict(df_ctree))
plot(df_ctree)

#Алгоритм Random Forest 
install.packages("randomForest")
library(randomForest) 
rf <- randomForest(Group ~ .,data=trainData, ntree=500, proximity=TRUE)
table(predict(rf), trainData$Group)
print(rf)


