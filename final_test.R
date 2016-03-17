#################Econ294a final exam##################
####Ruoqing XIE####

library(nycflights13)
library(dplyr)
library(RSQLite)
require(tidyr)
library(stargazer)
library(ggplot2)
library(scales)
#weather<-weather
#planes <- planes
#flights <- flights
#airports <- airports


my_db <- src_sqlite("my_db.sqlite1", create = T)
nycflights13_sqlite()

flights_sqlite <- copy_to(
  my_db, flights, temporary = FALSE, 
  indexes = list(
    c("year", "month", "day", "hour"), 
    "arr_delay","dest",
    "carrier", 
    "tailnum")
)

weather_sqlite <- copy_to(
  my_db, weather, temporary = FALSE, 
  indexes = list(
    c("year", "month","day", "hour"),
    "wind_dir","wind_speed",
    "dewp","precip",
    "visib",
    "pressure"
    )
)

planes_sqlite <- copy_to(
  my_db, planes, temporary = FALSE, 
  indexes = list(c("tailnum"),
                 "type"
                 )
)

airlines_sqlite <- copy_to(
  my_db, airlines, temporary = FALSE, 
  indexes = list("carrier")
)

airports_sqlite <- copy_to(
  my_db, airports, temporary = FALSE, 
  indexes = list("faa")
)

my_db
nycflights13_sqlite()

flights_sqlite <- tbl(my_db, "flights")
weather_sqlite <- tbl(my_db, "weather")
airports_sqlite <- tbl(my_db,"airports")
planes_sqlite <- tbl(my_db,"planes")
airlines_sqlite <- tbl(my_db,"airlines")

##########part a###########
flights = tbl(my_db, "flights") %>% 
  collect() %>%
  mutate(canceled = is.na(arr_time))%>%
  mutate(canceled = as.numeric(canceled))

flights <- flights %>%
  unite(
    col = date, 
    ... = year, month, day, hour,
    sep = "-"
  )

weather <- tbl(my_db,"weather")%>%
  collect()%>%
  select(year:hour, temp,dewp,wind_speed,precip,visib)

weather<-weather%>%
  unite(
    col = date, 
    ... = year, month, day, hour,
    sep = "-"
  )

flight_weather <- weather%>%
  inner_join(flights,by="date")

flight_weather1 <- flight_weather%>%
  group_by(canceled)%>%
  arrange(dep_delay)%>%
  select(dewp,wind_speed,visib,precip,canceled,dep_delay)

rela <- lm(dep_delay~visib+wind_speed+dewp+precip,flight_weather1)
summary(rela)
stargazer(rela,type="text",title="Regression of weather on dep_delay",out="final_weather1.doc")

relation1<-glm(canceled~dep_delay+visib+wind_speed+dewp+precip,flight_weather1,family=binomial(link = "logit"))
summary(relation1)
stargazer(relation1,type="text",title="Regression of weather on cancel",out="final_weather.doc")

gweather <- ggplot(
  data = flight_weather1,
  aes(x=dep_delay,fill=factor(dewp))
) + 
  geom_histogram(aes(x=dep_delay, y=..density..))+
  facet_grid("canceled ~ .")+ylab("cancel")

###########part b###########
flight_time <- flights%>%
  select(date,minute,contains("time"),canceled,dep_delay)

flight_time1 <- flight_time %>%
  separate(
    date, 
    c("year", "month", "day","hour"), #name of new column(s)
    "-"
  )%>%
  group_by(canceled)%>%
  arrange(dep_time,dep_delay)%>%
  filter(!is.na(dep_delay))

lmrel<- lm(dep_delay~dep_time+month+day+hour+minute, flight_time1)
summary(lmrel)
stargazer(lmrel,type="text",title="Regression of time on dep_delay",out="final_time1.doc")

relation2 <- glm(canceled~dep_delay+dep_time+month+day+hour+minute, flight_time1,family=binomial(link = "logit"))
stargazer(relation2,type="text",title="Regression of time on cancel",out="final_time2.doc")

gtime <- ggplot(
  data = flight_time1,
  aes(x=dep_delay, fill = month)
) + 
  geom_histogram(aes(x=dep_delay, y=..density..))+
  facet_grid("canceled ~ .")+ylab("density")

##########part c###########
airports <- tbl(
  my_db,sql("SELECT name, alt, faa as dest
        FROM airports")
  )%>%
  collect()

flight_dest <- airports%>%
  inner_join(flights,by="dest")

relc <- lm(dep_delay~dest,flight_dest)
summary(relc)
relation3 <- glm(canceled~dep_delay+dest-1,flight_dest,family=binomial(link = "logit"))
summary(relation3)
stargazer(relation3,type="text",title="Regression of destination on cancel",out="final_time2.doc")

gdest <- ggplot(
  data = flight_dest,
  aes(x=dep_delay, fill = dest)
) + 
  geom_histogram(aes(x=dep_delay, y=..density..))+
  facet_grid("canceled ~ .")+ylab("cancel")

##############part d################
planes <- tbl(
  my_db,sql("planes")
  )%>%
  collect()

flight_plane <- planes%>%
  inner_join(flights,by="tailnum")%>%
  select(canceled,dep_delay,year,type,manufacturer)%>%
  group_by(canceled) ##dep_delay od cancel is NA?

reld <- lm(dep_delay~year+type+manufacturer, flight_plane)
summary(reld)
stargazer(reld,type="text",title="Regression of plane on dep_delay",out="final_plane1.doc")
relation4 <- lm(canceled~dep_delay+year+type+manufacturer, flight_plane)
summary(relation4)
stargazer(relation4,type="text",title="Regression of plane on cancel",out="final_plane2.doc")

gplane <- ggplot(
  data = flight_plane,
  aes(x=dep_delay, fill = factor(year))
) + 
  geom_histogram(aes(x=dep_delay, y=..density..))+
  facet_grid("canceled ~ .")+ylab("density")



