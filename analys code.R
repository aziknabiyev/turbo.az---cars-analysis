library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

cars <- read.csv("cars.csv")

cars <- mutate(cars,price =
                  case_when(str_detect(price,"AZN")==FALSE ~ as.numeric(parse_number(gsub(" ", "", price, fixed = TRUE)))*1.7,
                            str_extract(price, "AZN")=="AZN" ~ as.numeric(parse_number(gsub(" ", "", price, fixed = TRUE)))
                            ))

crat <- group_by(cars,marka,model) %>% summarise(n = n(),price=mean(price))
View(crat)
crat <- subset(crat, n>4)
crat <- subset(crat, marka=="Toyota")

ggplot(data=crat, mapping=aes(x=model,y=price,fill=model))+
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=model), position=position_dodge(width=0.2), vjust=-0.50)+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab('Toyota')



#cars <- subset(cars, type=="Sedan")

# cr <- group_by(cars,marka) %>% summarise(mean_price = mean(price),
#                                          mean_view=mean(view),n=n())
#cr <- subset(cr, n>10)



#View(cr)

# cr <- group_by(cars,marka) %>% summarise(n = n())
# cr <- subset(cr, n>10)
# 
# ggplot(data=cr, mapping=aes(x=marka,y=n,fill=marka))+
#   geom_bar(position = 'dodge', stat='identity') +
#   geom_text(aes(label=marka), position=position_dodge(width=0.2), vjust=-0.50)+
# 
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# View(cars)

# cars <- cars[order(cars$year, decreasing = TRUE),] 

# cars <- mutate(cars,price =
#                   case_when(str_detect(price,"AZN")==FALSE ~ as.numeric(parse_number(gsub(" ", "", price, fixed = TRUE)))*1.7,
#                             str_extract(price, "AZN")=="AZN" ~ as.numeric(parse_number(gsub(" ", "", price, fixed = TRUE)))
#                             ))
# 



