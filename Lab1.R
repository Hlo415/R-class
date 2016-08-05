install.packages("devtools")
install.packages("dplyr")
install.packages("rmarkdown")
install.packages("ggplot2")
install.packages("broom")
install.packages("gridExtra")
install.packages("shiny")
install.packages("cubature")
library(devtools)
install_github("StatsWithR/statsr")


library(dplyr)
library(ggplot2)
library(statsr)
data(arbuthnot)
arbuthnot
arbuthnot$boys


ggplot(data = arbuthnot, aes(x = year, y = girls)) +
  geom_point()
?ggplot

arbuthnot$boys + arbuthnot$girls
arbuthnot <- arbuthnot %>%
  mutate(total = boys + girls)

ggplot(data = arbuthnot, aes(x = year, y = total)) +
  geom_line()

#DataSet 2 #

data(present)
present
present <- present %>%
  mutate(total = boys + girls)

propboys<- present$boys/present$total
ggplot(data = present, aes(x = year, y = propboys)) +
  geom_line()


present <- present %>%
  mutate(more_boys = boys > girls)

present$prop_boy_girl<- present$boys/present$girls

ggplot(data = present, aes(x = year, y = prop_boy_girl)) +
  geom_line()

ggplot(data = present, aes(x = year, y = total)) +
  geom_line()
