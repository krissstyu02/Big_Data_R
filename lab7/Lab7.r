# Задание 1

df<-read.csv("C:/R/athlete_events.csv", sep=",", header=TRUE)

all_medal<- read.csv("C:/R/LR4_all.csv", sep=";",header=T, fileEncoding="utf8")
woman_medal<- read.csv("C:/R/LR4_f.csv", sep=";",header=T, fileEncoding="utf8")
man_medal<- read.csv("C:/R/LR4_m.csv", sep=";",header=T, fileEncoding="utf8")


v<-c("Name", "Sex", "Weight", "Sport")

df1<-df[which(df[, "Sport"]=="Cross Country Skiing"), v]
df1<-df1[-which(is.na(df1[, "Weight"])),]
x<-df1[1:nrow(df1), "Weight"]


# Тест Стьюдента
t.test(x, mu=mean(x), conf.int=TRUE)
t.test(x, mu=75, conf.int=TRUE)

# Тест Уилкоксона
wilcox.test(x, mu=mean(x), conf.int=TRUE)
wilcox.test(x, mu=30, conf.int=TRUE)


x<-x[1:5000]
# Проверка на нормальность распределения.
# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(x)


par(mar=c(5,5,4,2)+0.1, pin=c(6,4))
# Графический способ.
# Гистограмма с линией плотности.
x2<-seq(min(x), max(x), length=length(x))
fun<-dnorm(x2, mean=mean(x), sd=sd(x))
y_range <- diff(range(density(x)$y))

# установка параметров для увеличения длины оси y
hist(x, freq=FALSE, col="gray", ylim=c(0, max(density(x)$y) + y_range*0.2))
lines(x2, fun, col=2, lwd=2)


# Квантильно-квантильный график.
qqnorm(x)
qqline(x, col=4, lwd=2)

# проверка 2 вида спорта- плаванья

df3<-df[which(df[, "Sport"]=="Speed Skating"), v]
df3<-df3[-which(is.na(df3[, "Weight"])),]
df3<-df3[which(df3[, "Sex"]=="M"),]
x3<-df3[1:nrow(df3), "Weight"]

# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(x3)
# Квантильно-квантильный график.
qqnorm(x3)
qqline(x3, col=4, lwd=2)


#Задание 2
#необходимо проверить a) нормальность распределения и b) равенство дисперсий:


df2<-df[which(df[, "Sport"]%in%c("Speed Skating", "Cross Country Skiing")), v]
df2<-df2[-which(is.na(df2[, "Weight"])),]
df2<-df2[which(df2[, "Sex"]=="F"),]
df2$Sport<-droplevels(df2$Sport)
x<-df2[1:nrow(df2), "Weight"]

x<-x[1:5000]
shapiro.test(x)
qqnorm(x)
qqline(x, col=4, lwd=2)

# Сравнение двух независимых выборок.
tapply(df2$Weight, df2$Sport, mean)


fligner.test(df2$Weight~df2$Sport, df2)
#различаются ли эти средние значения статистически, проверим на 
#основе гипотезы об отсутствии разницы при помощи t-теста:

t.test(df2$Weight~df2$Sport)
t.test(df2$Weight~df2$Sport, paired=FALSE, var.equal=TRUE)


