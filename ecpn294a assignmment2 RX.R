#Econ294a Assignment2 Jan 22

#0.
RuoqingXieAssignment2 <- list(
  firstName = "Ruoqing",
  lastName  = "Xie",
  email     = "rxie4@ucsc.edu",
  studentID = 1504995
)


#1.a
library(foreign) 
diamonds <- read.csv(  
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.CSV"
)
RuoqingXieAssignment2$s1a <- nrow(diamonds)
RuoqingXieAssignment2$s1b <- ncol(diamonds)
RuoqingXieAssignment2$s1c <- names(diamonds)
RuoqingXieAssignment2$s1d <- summary(diamonds$price)

#2
df.txt <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",
  header = TRUE
)
RuoqingXieAssignment2$s2a <- nrow(df.txt)
RuoqingXieAssignment2$s2b <- ncol(df.txt)
RuoqingXieAssignment2$s2c <- names(df.txt)
RuoqingXieAssignment2$s2d <- mean(df.txt$weight)
RuoqingXieAssignment2$s2e <- median(df.txt$weight)
#histogram
hist(df.txt$weight,col=blues9)
table(df.txt$weight)
#new colum
df.txt$new_weight <- ifelse(df.txt$weight>=996, yes = NA, no=df.txt$weight)
RuoqingXieAssignment2$s2f <- mean(df.txt$new_weight)
RuoqingXieAssignment2$s2g <- median(df.txt$new_weight)
df.txt$weight_f <- ifelse(df.txt$SEX==2,yes=df.txt$weight, no=NA)
RuoqingXieAssignment2$s2h <- summary(df.txt$weight_f)
df.txt$weight_m <- ifelse(df.txt$SEX==1,yes=df.txt$weight, no=NA)
RuoqingXieAssignment2$s2i <- summary(df.txt$weight_m)

#3
vec <- c(letters,LETTERS)
x <- seq(2,52,2)
RuoqingXieAssignment2$s3a <- vec[c(x)]
RuoqingXieAssignment2$s3b <- paste(vec[c(44,21,15)], collapse="")

arr <- array(c(letters,LETTERS), dim = c(3,3,3))
RuoqingXieAssignment2$s3c <- arr[,1,2]
RuoqingXieAssignment2$s3d <- paste(arr[5],arr[2,2,2],arr[2,2,3])
RuoqingXieAssignment2$s3e <- paste(arr[3,3,2],arr[3,1,3],arr[3,2,2], sep = "")

#4
df.dta <- read.dta(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)