
setwd("C:/Users/achintya/Downloads/Compressed/r/r/Cases")
data <- read.csv("flag.data_dataset.txt",header = F)
View(data)
#colnames(data)<- c("name","landmass","zone","area","population","language","religion","bars","stripes","colours")
#Find most common color among flags from all countries
data.frame(table(data$colours))
b<-table(data$colours)
View(b)
#Find the 7th largest country in terms of area
q2 <- sort(data$area,decreasing = T)
q2 <- subset(data, data$area==q2[7], select = c(name,area))
View(q2)
#Find out largest county in terms of area in NE zone
q3<-subset(data,data$zone == 1,select = c(name,area,zone))
q3<-q3[order(q3$area,decreasing = T),]
q3<-q3[1,]
View(q3)
q3 <- subset(data, data$V3 == 1, select = c(V1,V4,V3))
q3 <- q3[order(q3$V4,decreasing = T),]
q3 <- q3[1,]

#Find out least populated country in S. America landmass
q4<-subset(data,data$landmass==2,select=c(name,area,population))
q4<-q4[order(q4$population),]
q4<-q4[q4$population==0,]
View(q4)
q4 <- subset(data,data$V2 == 2,select = c(V1,V5))
q4 <- q4[order(q4$V5),]
q4 <- q4[q4$V5==0,]

#How many countries have same top left and bottom right color in flag
a1 <- as.character(data$V29)==as.character(data$V30)
q5 <- data[a1==T,c(1,29,30)]
View(q5)
q5<-nrow(subset(data,data$V29==data$V30))
View(q5)
#Find count of number of countries based on landmass.
for (i in min(data$V2):max(data$V2)) {
  command <- paste0("Landmass",i," <- nrow(subset(data,data$V2==",i,"))")
  eval(parse(text = command))
}
summary(as.factor(data$landmass))
q6 <- cbind(Landmass1,Landmass2,Landmass3,Landmass4,Landmass5,Landmass6)
a<-table(data$V2)
View(a)
table(data$landmass==1)
table(data$landmass==2)
table(data$landmass==3)
table(data$landmass==4)
table(data$landmass==5)
table(data$landmass==6)
data.frame(table(data$colour))
#How many countries have icon in their flag
d <- data$V27==1
q7 <- nrow(data[d==T,])
View(q7)
table(data$V27==1)
#Which zone has maximum countries also specify number

for (i in 1:4) {
  command <- paste0("zone",i," <- sum(data$V3==",i,")")
  eval(parse(text = command))
}

zone1 <- table(data$V3 == 1,exclude = F)
View(zone1)
#Find largest speaking language among all countries

for (i in 1:10) {
  command <- paste0("lang",i," <- sum(data$V6==",i,")")
  eval(parse(text = command))
}
Lang1<-data.frame(table(data$V6))
colnames(Lang1)<-c("v1","v2")
View(Lang1)
#Find top 5 countries with sum of bars and stripes in a flag
a <- data.frame(as.numeric(data$V8),as.numeric(data$V9))
q10 <- data.frame(data$V1,rowSums(a))
q10 <- q10[order(q10$rowSums.a.,decreasing = T),]
q10 <- q10[1:5,]
View(q10)
data11<-subset(data,select=c(v8,v9))
colnames(data11)<=c("v8","v9")
a2<-rowsum(data11)
View(a2)
a3<-a2[order(a2)]
a3<-a3[1:5]
View(a3)
#Find sum of all circles present in all country flags

q11 <- sum(data$V19)

#Count of countries which have both icon and text in flag

q12 <- nrow(subset(data, data$V26 ==1 & data$V28 ==1))
View(q12)




