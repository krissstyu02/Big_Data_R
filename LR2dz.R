getwd()
df<-read.csv("Любимый сериал.csv", sep=";", header=T, fileEncoding="cp1251")
View(df)
print(df)

#1
min(df$Шерлок)
max(df$Шерлок)
mean(df$Шерлок)

min(df$Триггер)
max(df$Триггер)
mean(df$Триггер)

min(df$Хрустальный)
max(df$Хрустальный)
mean(df$Хрустальный)

min(df$Как.избежать.наказание.за.убийство)
max(df$Как.избежать.наказание.за.убийство)
mean(df$Как.избежать.наказание.за.убийство)

min(df$Самка.богомола)
max(df$Самка.богомола)
mean(df$Самка.богомола)

min(df$Нулевой.пациент)
max(df$Нулевой.пациент)
mean(df$Нулевой.пациент)

min(df$Корона)
max(df$Корона)
mean(df$Корона)

min(df$Убивая.Еву)
max(df$Убивая.Еву)
mean(df$Убивая.Еву)

min(df$Красная.королева)
max(df$Красная.королева)
mean(df$Красная.королева)

max(df$Мост,na.rm=TRUE)
min(df$Мост,na.rm=TRUE)
mean(df$Мост,na.rm=TRUE)

#2
df[14,9]=7
vec=c()
ivec=3:12
jvec=1:16
for(i in ivec)
{
  k=0
  for(j in jvec)
  {
    #print(df[j,i])
    if(df[j,i]>3)
    {
      k=k+1
    }
  
  }
  vec=c(vec,k)
      
}

vec
vec2=c()
for(i in ivec)
{
  k=0
  for(j in jvec)
  {
    #print(df[j,i])
    if(df[j,i]<7)
    {
      k=k+1
    }
    
  }
  vec2=c(vec,k)
  
}
vec2
df[3,3]
is.integer(df[3,3])
#3
x=c(mean(df$Шерлок),mean(df$Триггер),mean(df$Хрустальный),mean(df$Как.избежать.наказание.за.убийство),
   mean(df$Самка.богомола),mean(df$Нулевой.пациент),mean(df$Мост,na.rm=TRUE),
   mean(df$Корона),mean(df$Убивая.Еву),mean(df$Красная.королева))
x
names(x) <- c("Шерлок", "Триггер", "Хрустальный","Как избежать наказание за убийство","Самка богомола",
              "Нулевой пациент","Мост","Корона","Убивая Еву","Красная королева")
sort(x, decreasing = TRUE)

#4
hist(x)
barplot(x)
