fav.data<-read.csv("favorite_food_data.csv",na.string = c(""," "),fileEncoding = "UTF-8-BOM")
stud.data<-read.csv("student_data.csv",na.strings = c(""," "),fileEncoding = "UTF-8-BOM")

fav.data
stud.data

data <- merge(fav.data,stud.data,by.x ="Student.Name", by.y = "Student" )
data

fav.food.freq<-table(data$Favorite.Food)
fav.food.prec<-round(fav.food.freq/sum(fav.food.freq)*100,0)
fav.food.prec


#a
pie(
  fav.food.freq,
  main = "Food Type Appeal",
  fav.food.prec,
  col = rainbow(length(fav.food.freq))
)



#b
#ganti semua education grade yang awalnya hanya sd smp sma , menajdi sesuai kelas , seperti 
#sd 1 , sd 2 ... sd 6 , smp1 ... smp3 , sma1....sma3

data.edu <- stud.data
data.frame(data.edu$Education.Grade,data.edu$Education.Year)
data.edu
data.edu$Education.Grade <- ifelse(data.edu$Education.Year=="1"& data.edu$Education.Grade =="SD","SD1",
                                   ifelse(data.edu$Education.Year=="2"& data.edu$Education.Grade =="SD","SD2",
                                          ifelse(data.edu$Education.Year=="3"& data.edu$Education.Grade =="SD","SD3",
                                                 ifelse(data.edu$Education.Year=="4"& data.edu$Education.Grade =="SD","SD4",
                                                        ifelse(data.edu$Education.Year=="5"& data.edu$Education.Grade =="SD","SD5",
                                                               ifelse(data.edu$Education.Year=="6"& data.edu$Education.Grade =="SD","SD6",
                                                                      ifelse(data.edu$Education.Year=="1"& data.edu$Education.Grade =="SMP","SMP1",
                                                                             ifelse(data.edu$Education.Year=="2"& data.edu$Education.Grade =="SMP","SMP2",
                                                                                    ifelse(data.edu$Education.Year=="3"& data.edu$Education.Grade =="SMP","SMP3",
                                                                                           ifelse(data.edu$Education.Year=="1"& data.edu$Education.Grade =="SMA","SMA1", 
                                                                                                  ifelse(data.edu$Education.Year=="2"& data.edu$Education.Grade =="SMA","SMA2",
                                                                                                         ifelse(data.edu$Education.Year=="3"& data.edu$Education.Grade =="SMA","SMA3",data.edu$Education.Grade))))))))))))


barplot(table(data.edu$Education.Grade))



#c

data.height <- data[stud.data$Age<=12,]
data.height <- data[stud.data$Education.Grade=="SD",]
na.omit(data.height)

data.height

grade1 <- mean(data.height[stud.data$Education.Year==1,]$Height)
grade2 <- mean(data.height[stud.data$Education.Year==2,]$Height)
grade3 <- mean(data.height[stud.data$Education.Year==3,]$Height)
grade4 <- mean(data.height[stud.data$Education.Year==4,]$Height)
grade5 <- mean(data.height[stud.data$Education.Year==5,]$Height)
grade6 <- mean(data.height[stud.data$Education.Year==6,]$Height)

gradeAvg <- c(grade1,grade2,grade3,grade4,grade5,grade6)
gradeAvg

plot(
    gradeAvg,
    type = "b",
    main = "Primary School Average Height"
)

#4
data.clean <- data.edu
data.clean <- data.clean[!data.clean$Student.Name=="",]
data.clean <- data.clean[!data.clean$Education.Grade=="SD",]
data.clean <- data.clean[!duplicated(data.clean),]

data.clean.split <- split(data.clean$Favorite.Food,data.clean$Id)

data.clean.split
library(arules)
hasil <- apriori(data.clean.split,parameter = list(support = 0.25 ,target = "freq"))
inspect(freq)
