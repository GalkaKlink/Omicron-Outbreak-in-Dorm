library("ggplot2")
common = theme_classic()+
        theme(axis.text.x = element_text(size =20,color="black"),
        axis.title.y = element_text(size =20,color="black"),
        axis.text.y = element_text(size =20,color="black"), 
        axis.title.x = element_text(size =20,color="black"),
        legend.text = element_text(size =20,color="black"),
        legend.title = element_blank(),
        plot.title = element_text(size=20,hjust=0.5,color="black"))


setwd("D:/Omicron")
data=read.table(file="DormSubtree.DistanceTable.tsv",header=TRUE) #table that contains, each all pair of samples, their rooms, floors and phylogenetic distance  
data$room = ifelse(data$room1==data$room2,"yes","no")
data$floor = ifelse(data$floor1==data$floor2,"yes","no")
data$group = ifelse(data$room=="yes" & data$floor=="yes","same_room","other")
data$group = ifelse(data$room=="no" & data$floor=="yes","same_floor",data$group)

################################## calculating statistic for rooms for real data

dat1 = data[,c(1,2,7)]
dat2 = data[,c(2,1,7)]
colnames(dat1) = c("name1","name2","dist")
colnames(dat2) = c("name1","name2","dist")
dat = rbind(dat1,dat2)

first = data[,c(1,3,5)]
second = data[,c(2,4,6)]
colnames(first) = c("name","room","floor")
colnames(second) = c("name","room","floor")
all = rbind(first,second)
samples = all[!duplicated(all), ]

rooms = unique(samples$room)
dists = vector()
for (rm in rooms) {
    sub = subset(samples,samples$room == rm)
    humns = sub$name
    if (length(humns) > 1) {
        chisl = 0
        znam = 0
        for (i in 1:(length(humns)-1)) {
            for (j in (i+1):length(humns)) {
                hum1 = humns[i]
                hum2 = humns[j]
                str = subset(dat,dat$name1==hum1 & dat$name2==hum2)        
                dist = as.numeric(str[1,3])
                chisl = chisl+dist
                znam = znam+1
                }
        }
        mean_dist = chisl/znam
        dists = c(dists,mean_dist)    
    }
}
d_stat = sum(dists)
rooms_number = length(dists)

#######################Shuffling rooms of the same floor:
d_null = vector()
for (N in 1:10000) {
    floors = unique(samples$floor)
    samples1 = data.frame()
    for (fl in floors) {
        sub_floor = subset(samples,samples$floor==fl)
        shuffled_room = sample(sub_floor$room)
        sub_floor1 = as.data.frame(cbind(sub_floor$name,shuffled_room))
        samples1 = rbind(samples1,sub_floor1)
    }
    colnames(samples1) = c("name","room")
    null_dists = vector()
    for (rm in rooms) {
        sub = subset(samples1,samples1$room == rm)
        humns = sub$name
        if (length(humns) > 1) {
            chisl = 0
            znam = 0
            for (i in 1:(length(humns)-1)) {
                for (j in (i+1):length(humns)) {
                    hum1 = humns[i]
                    hum2 = humns[j]
                    str = subset(dat,dat$name1==hum1 & dat$name2==hum2)        
                    dist = as.numeric(str[1,3])
                    chisl = chisl+dist
                    znam = znam+1
                }
        }
        mean_dist = chisl/znam
        null_dists = c(null_dists,mean_dist)    
    }
}
null_d_stat = sum(null_dists)
d_null = c(d_null,null_d_stat)
}

small = d_null[d_null <= d_stat]
length(small)

pdf("D:/Omicron/ShufledRooms10kTimes.WithinFloor.pdf")
hist(d_null,main="",ylab="",xlab="m",cex.lab=1.5, cex.axis=1.5)
abline(v=d_stat,col="red")
text(22,12000,paste("p=",round(length(small)/length(d_null),digits=3),sep=""),cex=1.5)
dev.off()

d_stat1 = d_stat/rooms_number
d_null1 = d_null/rooms_number

pdf("D:/Omicron/ShufledRooms10kTimes.WithinFloor.MeanStat.pdf")
hist(d_null1,main="",ylab="",xlab="m",cex.lab=1.5, cex.axis=1.5)
abline(v=d_stat1,col="red")
text(1.7,23000,paste("p=",round(length(small)/length(d_null),digits=3),sep=""),cex=1.5)
dev.off()


############shuffling rooms independent of the floor
d_null = vector()
for (N in 1:10000) {
    shuffled_room = sample(samples$room)
    samples1 = as.data.frame(cbind(samples$name,shuffled_room))
    colnames(samples1) = c("name","room")
    null_dists = vector()
    for (rm in rooms) {
        sub = subset(samples1,samples1$room == rm)
        humns = sub$name
        if (length(humns) > 1) {
            chisl = 0
            znam = 0
            for (i in 1:(length(humns)-1)) {
                for (j in (i+1):length(humns)) {
                    hum1 = humns[i]
                    hum2 = humns[j]
                    str = subset(dat,dat$name1==hum1 & dat$name2==hum2)        
                    dist = as.numeric(str[1,3])
                    chisl = chisl+dist
                    znam = znam+1
                }
        }
        mean_dist = chisl/znam
        null_dists = c(null_dists,mean_dist)    
    }
}
null_d_stat = sum(null_dists)
d_null = c(d_null,null_d_stat)
}

small = d_null[d_null <= d_stat]
length(small)

pdf("D:/Omicron/ShufledRooms10kTimes.pdf")
hist(d_null,main="",ylab="",xlab="m",cex.lab=1.5, cex.axis=1.5)
abline(v=d_stat,col="red")
text(30,20000,paste("p=",round(length(small)/length(d_null),digits=3),sep=""),cex=1.5)
dev.off()

d_stat1 = d_stat/rooms_number
d_null1 = d_null/rooms_number

pdf("D:/Omicron/ShufledRooms10kTimes.MeanStat.pdf")
hist(d_null1,main="",ylab="",xlab="m",cex.lab=1.5, cex.axis=1.5)
abline(v=d_stat1,col="red")
text(2,20000,paste("p=",round(length(small)/length(d_null),digits=3),sep=""),cex=1.5)
dev.off()




################################################for floors
floors = unique(samples$floor)
dists = vector()
for (fl in floors) {
    sub = subset(samples,samples$floor == fl)
    humns = sub$name
    if (length(humns) > 1) {
        chisl = 0
        znam = 0
        for (i in 1:(length(humns)-1)) {
            for (j in (i+1):length(humns)) {
                hum1 = humns[i]
                hum2 = humns[j]
                str = subset(dat,dat$name1==hum1 & dat$name2==hum2)        
                dist = as.numeric(str[1,3])
                chisl = chisl+dist
                znam = znam+1
                }
        }
        mean_dist = chisl/znam
        dists = c(dists,mean_dist)    
    }
}
d_stat = sum(dists)
floors_number = length(dists)

d_null = vector()
for (N in 1:10000) {
    shuffled_floor = sample(samples$floor)
    samples1 = as.data.frame(cbind(samples$name,shuffled_floor))
    colnames(samples1) = c("name","floor")
    null_dists = vector()
    for (fl in floors) {
        sub = subset(samples1,samples1$floor == fl)
        humns = sub$name
        if (length(humns) > 1) {
            chisl = 0
            znam = 0
            for (i in 1:(length(humns)-1)) {
                for (j in (i+1):length(humns)) {
                    hum1 = humns[i]
                    hum2 = humns[j]
                    str = subset(dat,dat$name1==hum1 & dat$name2==hum2)        
                    dist = as.numeric(str[1,3])
                    chisl = chisl+dist
                    znam = znam+1
                }
        }
        mean_dist = chisl/znam
        null_dists = c(null_dists,mean_dist)    
    }
}
null_d_stat = sum(null_dists)
d_null = c(d_null,null_d_stat)
}

small = d_null[d_null <= d_stat]
length(small)


pdf("D:/Omicron/ShuffledFloors10kTimes.pdf")
hist(d_null,main="",ylab="",xlab="m",cex.lab=1.5, cex.axis=1.5)
abline(v=d_stat,col="red")
text(13,1700,paste("p=",round(length(small)/length(d_null),digits=3),sep=""),cex=1.5)
dev.off()

d_stat1 = d_stat/floors_number
d_null1 = d_null/floors_number

pdf("D:/Omicron/ShuffledFloors10kTimes.MeanStat.pdf")
hist(d_null1,main="",ylab="",xlab="m",cex.lab=1.5, cex.axis=1.5)
abline(v=d_stat1,col="red")
text(1.7,1300,paste("p=",round(length(small)/length(d_null),digits=3),sep=""),cex=1.5)
dev.off()




