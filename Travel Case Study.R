install.packages("readxl")
library(readxl)

setwd("C:/Users/achintya/Downloads/Compressed/r/r/Cases")
data12<- read_xls("TRAVEL.XLS",header = F)
View(data12)
names <- t(data[16:26,2])
View(names)

parameters <- data[14:nrow(data),3]
parameters <- na.omit(parameters)
n1 <- 11
nr <- nrow(parameters) 
split_para <- split(parameters, rep(1:ceiling(nr/n1), each=n1, length.out=nr)) #Split data
split_para <- t(as.data.frame(split_para))
split_para <- as.data.frame(split_para)
colnames(split_para) <- names
rownames(split_para) <- 1:nrow(split_para)

a <- as.character(split_para$`HolidayType:`)
split_para$`HolidayType:` <- substr(split_para$`HolidayType:`,1,nchar(a)-1)

                                    