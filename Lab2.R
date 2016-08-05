library(statsr)
library(dplyr)
library(ggplot2)

data(nycflights)
str(nycflights) #To see a quick view of the variables #

# Dplyr Functions #

ggplot(data= nycflights, aes(x= dep_delay)) +
  geom_histogram()

ggplot(data= nycflights, aes(x= dep_delay)) +
  geom_histogram(binwidth = 15)
ggplot(data= nycflights, aes(x= dep_delay)) +
  geom_histogram(binwidth = 150)

rdu_flights<- nycflights %>%
  filter (dest == "RDU")
ggplot(data=rdu_flights, aes(x=dep_delay))+
  geom_histogram()

rdu_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

sfo_feb_flights <-nycflights %>% 
  filter (dest == "SFO", month ==2)

sfo_feb_flights # 68 Observations #
ggplot(data=sfo_feb_flights, aes(x=dep_delay))+
  geom_histogram()   # one #

sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(mean_ad = mean(arr_delay), sd_ad = sd(arr_delay), n = n(), iqd_ad = IQR(arr_delay))
# American Airlines #

nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay))%>%
  arrange(desc(mean_dd))

#July #

nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay))%>%
  arrange(desc(median_dd))

#december #


#Boxplot #

ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()

nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type =="on time")/n()) %>%
  arrange(desc(ot_dep_rate))

#LGA #

nycflights<- nycflights %>%
  mutate(avg_speed = distance/hour)

nycflights2<- nycflights %>%
  select(avg_speed,tailnum)%>%
  arrange(desc(avg_speed))

#N779JB #

# 9 #
#there is a overall positive association between distance and average speed #

ggplot(data = nycflights, aes(x = avg_speed, y = distance)) +
  geom_point()


#10 #

nycflights <- nycflights %>%
  mutate(arr_type = ifelse(dep_delay < 5, "on time", "delayed"))


