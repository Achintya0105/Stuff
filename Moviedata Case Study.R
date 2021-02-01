#Moviedata <- read.csv("C:/Users/18030242023/Downloads/moviesdataset.txt", header = F, sep = ",")
Moviedata<-read.csv("C:/Users/achintya/Downloads/Compressed/r/r/Cases/moviesdataset.txt",header=F)
colnames(Moviedata) <- c("MovieID","MovieName","YearofRelease","RatingofMovie","Moviedurationinseconds")
dim(Moviedata)
str(Moviedata)
View(Moviedata)

a <- subset(Moviedata,YearofRelease >= 1950 & YearofRelease <= 1960 )
View(a)
b <- subset(Moviedata,RatingofMovie > 4 )
View(b)
c <- subset(Moviedata,RatingofMovie >= 3 &  RatingofMovie <= 4)
View(c)
d <- subset(Moviedata,Moviedurationinseconds > 7200 )
View(d)
e <- data.frame(table(Moviedata$YearofRelease))
colnames(e) <- c("year","movieisreleased")
View(e)
f <- dim(Moviedata)



g <- e[e$movieisreleased==max(e$movieisreleased),]
h <- e[e$movieisreleased==min(e$movieisreleased),]


i <- subset(Moviedata, RatingofMovie==max(Moviedata$RatingofMovie,na.rm = T))
j <- subset(Moviedata, RatingofMovie==min(Moviedata$RatingofMovie,na.rm = T))



