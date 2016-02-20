#0
print("First name: Ruoqing")
print("Last name: Xie")
print("ID:1504995")

#1
library(foreign)
install.packages("dplyr")
library(dplyr)

flight <- read.csv(
  file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/flights.csv",
 stringsAsFactors = F
)
airpot <- read.csv(
  file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/airports.csv",
  stringsAsFactors = F
)
plane <- read.csv(
  file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/planes.csv",
  stringsAsFactors = F
)
weather <- read.csv(
  file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/weather.csv",
  stringsAsFactors = F
)

#2.as.Data()
flight$date <- as.Date(flight$date, "%Y-%m-%d %H:%M:%S")
class(flight$date) #"Date"
weather$date <- as.Date(weather$date, "%Y-%m-%d")
class(weather$date) #"Date"

#3.
#sfo&oak
flight2a <- flight %>%
  dplyr::filter(
    dest == "SFO" | dest == "OAK"
  )
print(nrow(flight2a))
#delay as least 1 h
flight2b <- flight %>%
  dplyr::filter(
    dep_delay >= 60 | arr_delay >= 60
  )
print(nrow(flight2b))
#arr_delay vs dep_delay
flight2c <- flight %>%
  dplyr::filter(
    (arr_delay > 2*as.numeric(dep_delay)) & (dep_delay>0)
  )
print(nrow(flight2c))

#4.select(3 ways)
####1
flight3a <- flight %>%
  select(arr_delay,dep_delay)
####2
flight3b <- flight %>%
  select(contains("delay"))
####3
flight3c <- flight %>%
  select(ends_with("delay"))

#5.arrange
###a.most dep_delay
print(
  arrange(flight,-dep_delay)%>%
  head(5)
)
###b.
print(flight5b<-flight%>%
  mutate(catup=dep_delay-arr_delay)%>%
  arrange(-catup)%>%
  select(time,dep_delay,arr_delay,plane,catup)%>%
  head(5)
)

#6.mutate
flight <- mutate(flight,speed=dist/time)
View(flight$speed)
flight <- mutate(flight, delta=dep_delay-arr_delay)
View(flight$delta)
##a
print(
  flight%>%
    arrange(speed)%>%
    select(time,plane,speed)%>%
    head(5)
)
##b
print(
  flight%>%
    arrange(delta)%>%
    select(time,plane,dep_delay,arr_delay,delta)%>%
    head(5)
)

#7.Group-by & summarize
####a
flight7a <- flight %>%
  group_by(carrier) %>%               
  summarise(
    cancel = sum(cancelled),
    total = n(),
    ratio = cancel/total, 
    min_d = min(delta, na.rm = T),
    quarter1_d = quantile(delta, .25, na.rm = T),
    quarter2_d = quantile(delta, .75, na.rm = T),
    median_d = median(delta,na.rm = T),
    mean_d = mean(delta, na.rm = T),
    quantile3_d = quantile(delta, .90, na.rm = T),
    max_d = max(delta, na.rm = T)
  )
####b
######original:
day_delay1 <- dplyr::filter(
  summarize(
    group_by(
      dplyr::filter(flight,!is.na(dep_delay)), date
      ),
    delay = mean(dep_delay),n = n()
  ),
n > 10
)
######transform:
day_delay <- flight %>%
  dplyr::filter(!is.na(dep_delay))%>%
  group_by(date)%>%
  summarise(delay = mean(dep_delay),
            n = n()
            )%>%
  filter(n>10)

#8. lag.x
print(
  mutate(
  day_delay, differ = delay-lag(delay)
  )%>%
  arrange(
    -differ
    )%>%
  head(5)
)

#9.table
####
dest_delay<-flight %>% 
  group_by(dest) %>%
  summarise (
    mean_ad = mean(arr_delay, na.rm = T),
    number=n()
  )
airports<-select(airpot,
                 dest = iata, 
                 name = airport , 
                 city,state, lat, long
                 )
####a. left_join
df.9a<- dest_delay%>% 
  left_join(airports,by="dest")
  
print(df.9a%>%
        select(city,state,mean_ad)%>%
        head(5)
    )
print(nrow(df.9a)) #116 OBS
####b. inner_join
df.9b<- dest_delay%>% 
  inner_join(airports,by="dest")

print(df.9b%>%
        select(city,state,mean_ad)%>%
        head(5)
)
print(nrow(df.9b)) #114 OBS
##NO the same, because in left join, rows in dest_delay with no matchi in y will have NA
##and keep show in df.9a, but the inner join only show all the match value. So we know
##there are 2 rows not matching in airports.

#c. right_join
df.9c<- dest_delay%>% 
  right_join(airports,by="dest")
print(nrow(df.9c)) #3376 obs
##this time we have 3376 obs, because airports dataframe ahs more obs than des_delay.
##and all the unmatched rows in airports will keep and show in df.9c
##and we have NA in arr_delay,because mean_ad is a unique variable in dest_delay and 
## data in airports can not match this,so it show NA.

#d. full_join
df.9d <- dest_delay%>% 
  full_join(airports,by="dest")
print(nrow(df.9d)) #3378
##this one has 3378 obs. Because it combines and keep rows in both dataframe. 
##we don,t have NA in arr_delay. because it keep all the colunmes and rows, not
##just those matching.

#10.join
hourly_delay<- filter(flight,!is.na(dep_delay))%>%
  group_by(date,hour)%>%
  summarise(
    delay = mean(dep_delay),
    n = n()
    )%>%
  filter(n>10)
####merge:
df.10<- hourly_delay%>% 
  inner_join(weather)

print(df.10%>%
  group_by(conditions)%>%
  summarise(
    max_dl=max(delay,na.rm=T)
    )
)

#11.
install.packages("tidyr")
install.packages("dplyr")
require(tidyr)
require(dplyr)
##a
df <- data.frame(
  treatment = c("a", "b"), 
  subject1 = c(3, 4), 
  subject2 = c(5, 6)
  )
##
df.11a <- df %>% 
  gather(demo,value,
         ...= -treatment) %>%
  rename(subject=demo)%>%
  select(subject,treatment,value)

df.11a[1,"subject"]<- 1
df.11a[2,"subject"]<- 1
df.11a[3,"subject"]<- 2
df.11a[4,"subject"]<- 2
df.11a

##b
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
###
df.11b <-df %>%
  arrange(treatment,subject)%>%
  spread(
    key = subject,
    value = value
  ) %>%
  rename(subject1=`1`,subject2=`2`)

##c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df.11c <- df %>%
  separate(
    demo, 
    c("sex", "age", "state"), #name of new column(s)
    "_"
  )

##d
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
##
df11.d <- df %>%
  unite(
    col = demo, 
    ... = sex, age, city,
    sep = "."
  )
df11.d[4,2] = "<NA>"

