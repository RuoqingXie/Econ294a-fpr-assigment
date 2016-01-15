# Econ294a-fgr-assigment
#Econ 294 Assigment1 data:Jan15

#0.print:
firstName <- "Ruoqing(Sherry)"
lastName  <- "xie"
studentID <- "1504995"
print(paste(firstName,lastName,studentID))

#1.Load Data:
library(foreign) 
df.dta <- read.dta(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta"
)

df.csv <- read.csv(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv"
)

df.td <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt"
)

load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))
print("name of Rdata is 'NHIS_2007_RData.RData'")

#2. 
print("df.dta is 452.31KB; df.td is 506.39KB; df.csv is 188.52KB;Rdara is 188.52KB;
      THe structurs within the data influences their variability.")

#3:
df.RData<-NHIS_2007_RData
typeof(df.RData) 
print("Ans: list")
class(df.RData)  
print("Ans: data.frame")

#4:
library(foreign) 
df<- read.dta(
  file = "http://people.ucsc.edu/~aspearot/Econ_217_Data/org_example.dta"
)
str(df) 
print("Ans:1119754 observations and 30 variables")
summary(df$rw)
print("Ans:Min=1.8;Median=15.9;Mean=19.8; Max=354.8;NA=521279 ")

#5:
vector<-c(1, 2, 3, 4, 5, 6, 7, 4, "NULL", "NA")
vector
summary(vector) 
print("Ans: the length of vector is 10; because not all the factor in the  vector is number.")
mean(as.numeric(vector),na.rm=TRUE) 
print("Ans: 4")

#6:
MaxT<-matrix(1:9, ncol=3, nrow=3, byrow=TRUE)
MaxT
#find the transpose:
Maxtrans<-matrix(1:9, ncol=3, nrow=3, byrow=FALSE)
#find the eigenvalue and eigenvector:
eigen(MaxT)

Y<-matrix(c(1,2,3,3,2,1,2,3,0), ncol=3, nrow=3, byrow=TRUE)
Y
#finf the inverse of Y:
Yinv<-solve(Y)
Yinv
I<- Y %*% solve(Y)
I  
print("Ans: I is identity matrix")

#7:1.creat a table:
diamonds<-data.frame(carat=c(5,2,0.5,1.5,5,"NA",3), 
                     cut=c("fair","good","very good","good","fair","Ideal","fair"),
                     clarity=c("SI1","I1","VI1","VS1","IF","VVS2","NA"),
                     price=c(850,450,450,NA,750,980,420))
diamonds
#mean price:
mean(diamonds$price,na.rm = T) 
print("Ans:650")
#mean price of "fair":
diamonds2<-subset(diamonds,(cut=="fair"))
mean(diamonds2$price)
print("Ans:673.33")
#mean price of "non-fair":
diamonds3<-subset(diamonds,(cut!="fair"))
mean(diamonds3$price,na.rm=T)
print("Ans:626.67")
#median price:
diamonds4<-subset(diamonds,(carat>2 & cut=="Ideal"& cut=="very good"))
diamonds4
print("Ans:no data fits")
