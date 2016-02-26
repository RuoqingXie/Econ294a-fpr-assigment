#0
print("First name: Ruoqing")
print("Last name: Xie")
print("ID:1504995")

install.packages("ggplot2")
library(ggplot2)
library(scales)

###########QUESTION 1##########
##a##
a1 <- ggplot(diamonds,
            aes(x=x*y*z, y=price, size=carat)
)+ 
  geom_point(aes(colour = factor(clarity)),alpha=0.2
)+ 
  scale_x_log10() + scale_y_log10() + scale_size(range = c(2.5,9))
a1
##b##
b1 <- ggplot(
    data = diamonds,
    aes(x=carat, fill = clarity)
) + 
  geom_histogram(aes(x=carat, y=..density..))+
  facet_grid("cut ~ .")
b1

##c##
c1 <- ggplot(
              data = diamonds,
              aes(x=cut, y=price)
 )+
     geom_violin(
       colour = "black"
 )+
     geom_jitter(alpha=0.02)
c1

############QUESTION 2#############
library(foreign)
library(dplyr)
df <- read.dta(
      file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)

##a##
df1 <- df%>%
     group_by(year,month)%>%
     summarise(
       quarter1_d = quantile(rw, .25, na.rm = T,type=7),
       quarter3_d = quantile(rw, .75, na.rm = T,type=7),
       deciles1_d = quantile(rw, .1, na.rm = T,type=5),
       deciles3_d = quantile(rw, .9, na.rm = T,type=5),
       median_d = median(rw, na.rm = T)
     ) %>%
     mutate(
       date = paste(year, month, "01", sep = "-"),
       date = as.Date(date, format = "%Y-%m-%d")
     )

a2 <- ggplot(
    data=df1,
    aes(x=date)
)+
  geom_line(
    aes(y = median_d)
)+
  geom_ribbon(
    aes(ymin = quarter1_d, ymax = quarter3_d), fill = "grey50",
    alpha=0.6
)+
  geom_ribbon(
    aes(ymin = deciles1_d, ymax = deciles3_d), fill = "grey70",
    alpha=0.4
)+
  ylim(0,50)
a2

##b##
df2 <- df%>%
  group_by(year,month,educ)%>%
  summarise(
    median_d = median(rw, na.rm = T)
  ) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )

b2 <- ggplot(
  data=df2,
  aes(x=date,
      color=educ)
)+
  geom_line(
    aes(y = median_d))+ylim(5,35)
b2

