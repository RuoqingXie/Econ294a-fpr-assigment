
#Econ294a assingment3 Feb 5

# 0 
print("Ruoqing Xie")
print(1504995)
print("rxie4@ucsc.edu")
#1
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)

# 2
install.packages("dplyr")
library(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(df.ex.2))

df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(df.ex.2))

# 3
df.ex.3 <- df.ex %>%
  arrange(year,month)

# 4
df.ex.4a <- df.ex %>%
  select(year:age)

df.ex.4b <- df.ex %>%
  select(year,month,starts_with("i"))
 ##distinct
  names(df.ex)
  unique(df.ex$state)
  print(distinct(select(df.ex,state)))

# 5
stndz <- function(x){
    (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}
nrmlz <- function(x){
  (x-min(x, na.rm = T)) / (max(x, na.rm = T)-min(x, na.rm = T))
}

df.ex.5a <- df.ex %>%
  mutate(
    rw.stndz=stndz(rw),
    rw_nrmlz=nrmlz(rw)
  ) %>%
  select(rw.stndz,rw_nrmlz)

df.ex.5b <- df.ex %>%
  group_by(year,month) %>%
    mutate(
      rw.stndz=stndz(rw),
      rw_nrmlz=nrmlz(rw),
      count=n()
    ) %>%
  select(rw.stndz,rw_nrmlz,count)

# 6
df.ex.6 <- df.ex %>%
  group_by(year,month,state) %>%
  summarise(
    rw.min = min(rw,na.rm=T),
    rw.1stq = quantile(rw,0.25,na.rm=T),
    rw.mean = mean(rw,na.rm=T),
    rw.median = median(rw,na.rm=T),
    rw.3rdq = quantile(rw,0.75,na.rm=T),
    rw.max = max(rw, na.rm=T),
    count = n()
  )
print(nrow(df.ex.6))

#
print(
  df.ex.6 %>%  
  ungroup() %>%
  arrange(desc(rw.mean))%>%
  select(year,month,state)%>%
  head(1)
)

# 7
df.ex.7a <- df.ex %>%
  group_by(year,month,state)%>%
  ungroup()%>%
  arrange(year,month,as.character(state))
  
print(
  df.ex.7a %>%
  select(year,month,state)%>%
  head(1)
)
  