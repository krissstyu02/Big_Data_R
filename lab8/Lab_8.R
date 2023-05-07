install.packages(pkgs=c("ellipse"))
install.packages("car")
library(car)
library(ellipse)

df <- read.csv("C:/Users/Кристина/Desktop/все/R/Lab8/country.csv", sep = "," , header = TRUE)

years=1989:2018


data_set=df[ df$Country.Name=="Belarus",]

#подчищаю таблицу
for (i in 1:23){
  for (j in 5:34){
    if (data_set[i,j]!=".."){
      data_set[i,j]= as.numeric(data_set[i,j])
    }
    else{
      data_set[i,j]= NA
    }
    
  }
}


# a. Прирост ВВП
VVP<-rep(0,30)

for (i in 5:34) {
  VVP[i-4]=data_set[2,i]
  
}
plot(years, VVP, xlab='Год', ylab='Прирост, %', main='Прирост ВВП в процентах',type='b', lty=1, pch=1,  lwd=2)




#####################################################################


# b. Рост ВВП и прирост населения
Population_growth<-rep(0,30)

for (i in 5:34) {
  Population_growth[i-4]=as.numeric(data_set[14,i])
}

VVP_growth<-rep(0,30)

for (i in 5:34) {
  VVP_growth[i-4]=as.numeric(data_set[1,i])
}

plot(
  years,
  VVP_growth,
  xlab='Год',
  ylab='Прирост ВВП',
  main='Прирост ВВП',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)


#Немного про выборки

mean(VVP_growth,na.rm=TRUE)
median(VVP_growth,na.rm=TRUE)

mean(Population_growth,na.rm=TRUE)
median(Population_growth,na.rm=TRUE)

shapiro.test(Population_growth)
shapiro.test(VVP_growth)


#Корреляции

for (i in 1:30){
  if (is.na(VVP_growth[i])){
    #VVP_growth[i]=0
    VVP_growth[i]= median(VVP_growth,na.rm=TRUE)
  }
  if (is.na(Population_growth[i])){
    #Population_growth[i]=0
    Population_growth[i]= median(Population_growth,na.rm=TRUE)
  }
  
}

cor(VVP_growth,Population_growth , method= "spearman")
cor.test(VVP_growth,Population_growth )

cor(VVP_growth,Population_growth , method= "pearson")
# Рост ВВП и прирост населения - обратная корреляция(-0,95..)

##########################################################################

# Задание 2.Изменения расходов на медицину и увеличения продолжительности жизни и смертность.

Changes_medical_cost<-rep(0,30)

for (i in 5:34) {
  Changes_medical_cost[i-4]=as.numeric(data_set[12,i])
}

Average_life<-rep(0,30)

for (i in 5:34) {
  Average_life[i-4]=as.numeric(data_set[13,i])
}

Death_rate<-rep(0,30)

for (i in 5:34) {
  Death_rate[i-4]=as.numeric(data_set[18,i])
}


#Немного про выборки

mean(Changes_medical_cost,na.rm=TRUE)
median(Changes_medical_cost,na.rm=TRUE)

mean(Average_life,na.rm=TRUE)
median(Average_life,na.rm=TRUE)

mean(Death_rate,na.rm=TRUE)
median(Death_rate,na.rm=TRUE)

shapiro.test(Changes_medical_cost)
shapiro.test(Death_rate)
shapiro.test(Average_life)

#Корреляции
for (i in 1:30){
  if (is.na(Changes_medical_cost[i])){
    #Changes_medical_cost[i]=0
    Changes_medical_cost[i]= median(Changes_medical_cost,na.rm=TRUE)
  }
  if (is.na(Average_life[i])){
    #Average_life[i]=0
    Average_life[i]= median(Average_life,na.rm=TRUE)
  }
  if (is.na(Death_rate[i])){
    #Death_rate[i]=0
    Death_rate[i]= median(Death_rate,na.rm=TRUE)
  }
}

cor(Changes_medical_cost,Average_life, method= "spearman")
cor.test(Changes_medical_cost,Average_life )

cor(Changes_medical_cost,Average_life, method= "pearson")


cor(Changes_medical_cost,Death_rate, method= "spearman")
cor.test(Changes_medical_cost,Death_rate )

cor(Changes_medical_cost,Death_rate, method= "pearson")

df3=data.frame(Average_life,Death_rate,Changes_medical_cost)

library(ellipse)
plotcorr(cor(df3))




##########################################################################

# Задание 2.Прирост людей с высшим образованием на рост экспорта товаров и на прирост
#высокотехнологичного производства

Higher_education_growth<-rep(0,30)

for (i in 5:34) {
  Higher_education_growth[i-4]=as.numeric(data_set[19,i])
}

Growth_exports_goods<-rep(0,30)

for (i in 5:34) {
  Growth_exports_goods[i-4]=as.numeric(data_set[17,i])
}

Growth_technical_production<-rep(0,30)

for (i in 5:34) {
  Growth_technical_production[i-4]=as.numeric(data_set[21,i])
}


#Немного про выборки

mean(Higher_education_growth,na.rm=TRUE)
median(Higher_education_growth,na.rm=TRUE)

mean(Growth_exports_goods,na.rm=TRUE)
median(Growth_exports_goods,na.rm=TRUE)

mean(Growth_technical_production,na.rm=TRUE)
median(Growth_technical_production,na.rm=TRUE)

shapiro.test(Higher_education_growth)
shapiro.test(Growth_technical_production)
shapiro.test(Growth_exports_goods)

#Корреляции
for (i in 1:30){
  if (is.na(Higher_education_growth[i])){
    #Higher_education_growth[i]=0
    Higher_education_growth[i]= median(Higher_education_growth,na.rm=TRUE)
  }
  if (is.na(Growth_exports_goods[i])){
    #Growth_exports_goods[i]=0
    Growth_exports_goods[i]= median(Growth_exports_goods,na.rm=TRUE)
  }
  if (is.na(Growth_technical_production[i])){
    #Growth_technical_production[i]=0
    Growth_technical_production[i]= median(Growth_technical_production,na.rm=TRUE)
  }
}

#Higher_education_growth - 1 значение в таблице, нет смысла
cor(Higher_education_growth,Growth_exports_goods, method= "spearman")
cor.test(Higher_education_growth,Growth_exports_goods )

cor(Higher_education_growth,Growth_exports_goods, method= "pearson")


cor(Higher_education_growth,Growth_technical_production, method= "spearman")
cor.test(Higher_education_growth,Growth_technical_production )

cor(Higher_education_growth,Growth_technical_production, method= "pearson")


df4=data.frame(Growth_exports_goods,Growth_technical_production,Higher_education_growth)

library(ellipse)
plotcorr(cor(df4))


##########################################################################

# Задание 2.Расходов на образование на – кумулятивный прирост бакалавров среди женщин

Edication_cost<-rep(0,30)

for (i in 5:34) {
  Edication_cost[i-4]=as.numeric(data_set[15,i])
}

Female_bachelors<-rep(0,30)

for (i in 5:34) {
  Female_bachelors[i-4]=as.numeric(data_set[20,i])
}


#Немного про выборки

mean(Edication_cost,na.rm=TRUE)
median(Edication_cost,na.rm=TRUE)

mean(Female_bachelors,na.rm=TRUE)
median(Female_bachelors,na.rm=TRUE)


shapiro.test(Edication_cost)
shapiro.test(Female_bachelors)


#Корреляции

#Female_bachelors - 1 значение, нет смысла
for (i in 1:30){
  if (is.na(Edication_cost[i])){
    #Edication_cost[i]=0
    Edication_cost[i]= median(Edication_cost,na.rm=TRUE)
  }
  if (is.na(Female_bachelors[i])){
    #Female_bachelors[i]=0
    Female_bachelors[i]= median(Female_bachelors,na.rm=TRUE)
  }
}


cor(Edication_cost,Female_bachelors, method= "spearman")
cor.test(Edication_cost,Female_bachelors )

cor(Edication_cost,Female_bachelors, method= "pearson")



##########################################################################

# Задание 2.Прирост людей с высшим образованием на развитие высоких технологий (прирост статей в научных журналах)

Article<-rep(0,30)

for (i in 5:34) {
  Article[i-4]=as.numeric(data_set[23,i])
}


#Немного про выборки

mean(Higher_education_growth,na.rm=TRUE)
median(Higher_education_growth,na.rm=TRUE)

mean(Article,na.rm=TRUE)
median(Article,na.rm=TRUE)


shapiro.test(Higher_education_growth)
shapiro.test(Article)


#Корреляции

#Higher_education_growth - 1 значение в таблице, нет смысла

for (i in 1:30){
  
  if (is.na(Article[i])){
    #Article[i]=0
    Article[i]= median(Article,na.rm=TRUE)
  }
}


cor(Higher_education_growth,Article, method= "spearman")
cor.test(Higher_education_growth,Article )

cor(Higher_education_growth,Article, method= "pearson")



##########################################################################



df2=data.frame(VVP_growth,Population_growth,Growth_exports_goods,Article,Growth_technical_production,Female_bachelors,Changes_medical_cost,Death_rate)


plotcorr(cor(df2))







states <- as.data.frame(df2[,c('VVP_growth','Population_growth',
                               'Growth_exports_goods','Article',
                               'Growth_technical_production',
                               'Changes_medical_cost',
                               'Death_rate')])
cor(states)



states2 <- as.data.frame(df2[,c('VVP_growth','Population_growth',
                               'Growth_exports_goods',
                               'Growth_technical_production')])
library(ellipse)
library(car)

scatterplotMatrix(states2, spread=FALSE, lty.smooth=2, main="Матрица диаграмм рассеивания")

# На сколько ВВП зависимая от всех остальных
fit <- lm(VVP_growth ~ Population_growth + Growth_exports_goods
          + Growth_technical_production, 
          data=states)

fit

