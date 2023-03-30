all_medal<- read.csv("C:/lab4/LR4_all.csv", sep=";",header=T, fileEncoding="utf8")
woman_medal<- read.csv("C:/lab4/LR4_f.csv", sep=";",header=T, fileEncoding="utf8")
man_medal<- read.csv("C:/lab4/LR4_m.csv", sep=";",header=T, fileEncoding="utf8")


opar <- par(no.readonly=TRUE)      
par(mfrow=c(1,3))

sum_all <- sapply(all_medal[,-1], sum)
sum_man <- sapply(man_medal[,-1], sum)
sum_woman <- sapply(woman_medal[,-1], sum)
barplot(sum_man, names=c(1:8), col=heat.colors(8), xlab="Место", ylab="Количество",ylim=c(0,30), main="Мужчины (за последние 30 лет)")
barplot(sum_woman, names=c(1:8), col=heat.colors(8), xlab="Место", ylab="Количество",ylim=c(0,30), main="Женщины (за последние 30 лет)")
barplot(sum_all, names=c(1:8), col=heat.colors(8), xlab="Место", ylab="Количество",ylim=c(0,30), main="Все (за последние 30 лет)")


#золотые медали
first_all <- all_medal[,c(1:2)][all_medal$N1 > 0, ]
first_man <- man_medal[,c(1:2)][man_medal$N1 > 0, ]
first_woman <- woman_medal[,c(1:2)][woman_medal$N1 > 0, ]

par(mfrow=c(1,3))
pie(first_all$N1, labels=first_all$N1, col=terrain.colors(length(first_all$N1)), main = "Количество золотых \nмедалей всего\nза последние 30 лет")
legend(-1,4, legend=first_all$Год, cex = 1.3, fill=terrain.colors(length(first_all$Год)))

pie(first_man$N1, labels=first_man$N1, col=terrain.colors(length(first_man$N1)), main = "Количество золотых \nмедалей среди мужчин\nза последние 30 лет")
legend(-1, 4, first_man$Год, cex = 1.3, fill=terrain.colors(length(first_man$Год)))

pie(first_woman$N1, labels=first_woman$N1, col=terrain.colors(length(first_woman$N1)), main = "Количество золотых \nмедалей среди женщин\nза последние 30 лет")
legend(-1, 4, first_woman$Год, cex = 1.3, fill=terrain.colors(length(first_woman$Год)))

par(opar)

#призовые места
prize_man <- data.frame(Год=man_medal$Год, "Количество призовых"=rowSums(man_medal[, 2:4]))
prize_woman <- data.frame(Год=woman_medal$Год, "Количество призовых"=rowSums(woman_medal[, 2:4]))


plot(prize_man, type="b", pch=19, col="#76EEC6", xaxt="n", ylim=c(0,10), main="Призовые места Австрии по горнолыжному спорту за  последние 30 лет")
lines(prize_woman, type="o", pch=19, col="#68228B")
legend(x="topleft",min(man_medal$Год), c("Мужчины", "Женщины"), fill=c("#76EEC6", "#68228B"))
axis(side=1, at=prize_man$Год)


#по 7 странам

gold<- read.csv("C:/lab4/gold.csv", sep=";",header=T, fileEncoding="cp1251")
silver<- read.csv("C:/lab4/silver.csv", sep=";",header=T, fileEncoding="cp1251")
bronze<- read.csv("C:/lab4/bronze.csv", sep=";",header=T, fileEncoding="cp1251")

#золото
plot(gold$Год, gold$Россия, type="b", pch=16, col="darkseagreen4", xaxt="n", ylim=c(0,60), xlab="Год", ylab="Золотых медалей", main="Золотые медали за 6 последних олимпиад")
lines(gold$Год, gold$США, type="o", pch=16, col="#CD1076")
lines(gold$Год, gold$Австрия, type="o", pch=16, col="#8B3A3A")
lines(gold$Год, gold$Канада, type="o", pch=16, col="#CD69C9")
lines(gold$Год, gold$Великобритания, type="o", pch=16, col="#2C70F7")
lines(gold$Год, gold$Швеция, type="o", pch=16, col="#CD8162")
lines(gold$Год, gold$Франция, type="o", pch=16, col="#90EE90")

axis(side=1, at=gold$Год)
legend(x="topleft", horiz = TRUE,cex=0.39,x.intersp=1, 
       legend=c("Россия", "США", "Австрия", "Канада", "Великобритания", "Швеция", "Франция"), 
       fill=c("darkseagreen4", "#CD1076", "#8B3A3A", "#CD69C9", "#2C70F7", "#CD8162", "#90EE90"))


#все 3 места по всем странам
all_countr=gold[2:8]+silver[2:8]+bronze[2:8] 

plot(gold$Год, all_countr$Россия, type="b", pch=16, col="#3be8b0", xaxt="n", ylim=c(0,140), xlab="Год", ylab="Количество призовых мест", main="Призовые места за 6 последних олимпиад")
lines(gold$Год, all_countr$США, type="o", pch=16, col="#23d211")
lines(gold$Год, all_countr$Австрия, type="o", pch=16, col="#fae234")
lines(gold$Год, all_countr$Канада, type="o", pch=16, col="#ff3867")
lines(gold$Год, all_countr$Великобритания, type="o", pch=16, col="#2C70F7")
lines(gold$Год, all_countr$Швеция, type="o", pch=16, col="#EC9D00")
lines(gold$Год, all_countr$Франция, type="o", pch=16, col="#9933FF")

axis(side=1, at=gold$Год)
legend(x="topleft",cex=0.5, horiz = TRUE, legend=c("США", "Австрия", "Россия", "Канада", "Великобритания", "Швеция", "Франция"), fill=c("#23d211", "#fae234", "#3be8b0", "#ff3867", "#2C70F7", "#EC9D00", "#9933FF"))


#мужчины и женщины за последние 30 лет
par(mfrow=c(1,3))


plot(prize_man, type="b", pch=16, col="#87CEFA", xaxt="n", ylim=c(0,10), main="Призовые места за 30 лет")
lines(prize_woman, type="o", pch=16, col="hotpink")
legend("topleft",cex=0.7, c("Мужчины", "Женщины"), fill=c("#87CEFA", "hotpink"))
axis(side=1, at=prize_man$Год)

sum_all_prize<- data.frame(Год=prize_man$Год,"Призовые_м"=prize_man$Количество.призовых, 
                           "Призовые_ж"=prize_woman$Количество.призовых)
sum_all_prize<-sum_all_prize[order(sum_all_prize$Год),]
barplot(height=t(as.matrix(sum_all_prize[2:3])), beside=TRUE, xlab="Год", ylab="Количество", 
        names.arg=sum_all_prize$Год, col=c("#87CEFA", "hotpink"), main="Количество призовых мест Австрии")

prize_all_for_pie <-sapply(sum_all_prize[2:3], sum)
pie(prize_all_for_pie, labels=c(prize_all_for_pie["Призовые_м"], prize_all_for_pie["Призовые_ж"]), col=c("#87CEFA", "hotpink"), main="Количество призовых мест Австрии ")
legend("topleft",cex=0.7, c("Мужчины", "Женщины"), fill=c("#87CEFA", "hotpink"))

