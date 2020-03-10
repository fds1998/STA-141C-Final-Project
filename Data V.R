library(tidyverse)
data = read.csv('Metro_Interstate_Traffic_Volume.csv')

install.packages('corrplot')
install.packages('RColorBrewer')


library(corrplot)
library(RColorBrewer)

data$hours <- hour(data$date_time)

M <-cor(data[,c(2,3,4,5,10,9)])
corrplot(M, type="upper",method="color",addCoef.col = "black",
         col=brewer.pal(n=8, name="RdBu"))

data_cate <- data[,c(1,6,7)]
data_cate <- data_cate %>% mutate_if(is.factor, as.numeric)

M2 <-cor(data_cate[,c(2,3)])
corrplot(M2, type="upper",method="color",addCoef.col = "black",
         col=brewer.pal(n=8, name="RdBu"))

library(ggplot2)

ggplot(data= data, aes(x = traffic_volume)) + 
  geom_histogram(color="darkblue", fill="lightblue")

ggplot(data = data[data$holiday!='None',], aes(group = holiday, y=traffic_volume,color=holiday )) +
  geom_boxplot()

ggplot(data = data, aes(group = weather_main, y=traffic_volume,color=weather_main )) +
  geom_boxplot()

table(data$weather_main)

factor(data$weather_main)
lm(data$traffic_volume ~ data$temp) %>% summary()

ggplot(data[data$temp != 0, ], aes(x=temp, y=traffic_volume)) + 
  geom_point()+
  geom_smooth(method=lm)

boxplot(data$temp)$out

#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

