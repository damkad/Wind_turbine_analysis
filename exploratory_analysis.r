library(tidyverse)
library(lubridate)
data <- read.csv("T1.csv", stringsAsFactors = F)
data$Date.Time <- dmy_hm(data$Date.Time)
data$month <- month(data$Date.Time)
data$month <- as.factor(data$month)
data$`Theoritical Power (in 1hr) - Active Power` <- data$Theoretical_Power_Curve..KWh.-data$LV.ActivePower..kW.

data_gather <- gather(data, key=Color, value=Power, c(2,7))
data_gather$Color <- as.factor(data_gather$Color)
#data1 <- read.csv("Dataset.csv", stringsAsFactors = F)
summary(data)
#a plot of the active power (in KW) over time.
#this shows the yearly periodicity of active power
#length(which(is.na(data$LV.ActivePower..kW.)))
#data$active_power <- log10(data$LV.ActivePower..kW.)
glimpse(data)
#exploring dataset.
#check for empty data
sum(is.na(data))

#graph of distribution of the variables
ggplot(data, aes(x=`LV.ActivePower..kW.`))+geom_histogram()
ggplot(data, aes(x=`Wind.Speed..m.s.`))+geom_histogram()
ggplot(data, aes(x=`Theoretical_Power_Curve..KWh.`))+geom_histogram()
ggplot(data, aes(x=`Wind.Direction....`))+geom_histogram()


#E=pAV^3/2
#from the wind power equation, we can see that the power is 
#a function of the cubic power of the speed, and density.
#the values changes all through the year. However, due to the
#fact that the power is a function of the cubic power of speed, 
  #this makes speed the most significant variable in energy production
  #as such, three zones are established based on the magnitude of speed

#zone 1;Low speed; this is usually between 1.0 - 3.5m/s;  
#turbines do not usually operate in this range because it generatess
#a low power, which translates to a non-economically profitable operation
#the minimum operating speed is btw 3-3.5m/s

#zone 2; mid speed; this is usually between 3.5 to 14.5 m/s, the power generated 
#is dependent on the speed
#at 14.5m/s the max power is generated

#zone 3; high speed; this is usually between 14.5-25m/s, 
#the maximum power generated in the curve corresponds to the design
#speed of the turbine. a further increase in speed leads to no
#increase in power generated. moreover, this means an increase
#in mechanical loads that the sturcture must withstand, as such  
#safety measures are taking to avoid such (limit of below 25m/s).

#creating a new column for the speed zone
data$speed_zone = ifelse(data$Wind.Speed..m.s. < 3.5, "low", 
                         ifelse(data$Wind.Speed..m.s. >= 3.5  | data$Wind.Speed..m.s. < 14.5, "mid", "high"))
  
data$speed_zone <- as.factor(data$speed_zone)

#plot to see the speed zone distribution
ggplot(data, aes(x=`Wind.Speed..m.s.`, fill=speed_zone))+geom_histogram()
#as seen fron the graph, the low and mid speed zone is observed
#for the year 2018. however, the mid speed zone recorded is about 311% more than the
#low speed zone.
#graph of theoretical power curve by the wind speed zone
#low speed zone
ggplot(subset(data, speed_zone=="low")  , aes(x=`Wind.Speed..m.s.`, y= `Theoretical_Power_Curve..KWh.`))+geom_line()

#mid speed zone
ggplot(subset(data, speed_zone=="mid")  , aes(x=`Wind.Speed..m.s.`, y= `Theoretical_Power_Curve..KWh.`))+geom_line()

#combined speed zone
t1.rec <- data.frame(xmin = 0, xmax = 3.5, ymin=0, ymax=Inf)
t2.rec <- data.frame(xmin = 3.5, xmax = Inf, ymin=0, ymax=Inf)
p = ggplot(data)+
geom_point(aes(x=`Wind.Speed..m.s.`, y= `LV.ActivePower..kW.`, color="Active Power")) + 
geom_point(aes(x=`Wind.Speed..m.s.`, y= `Theoretical_Power_Curve..KWh.`, color="Theoretical Power"))
p + geom_rect(data= t1.rec, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data= t2.rec, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="blue", alpha=0.1, inherit.aes = FALSE) +
  theme(legend.position = "top", legend.direction = "horizontal")
  
#The following graph shows the power generated per number of hours. 
#This allows to establish a context on the productivity of the turbine. 
#Thus, we see that in 25% of the time, the turbine was able to generate a value equal to or 
#greater than 2360 kW; 760 kW, 50% of the time and 34 kW, 75% of the time. 
#From the graph it is clear how the turbine was inoperative for approximately 25% of the time 
#in the year.
#plot graph
summary(data_gather)
levels(data_gather$Color)
ggplot(data_gather, aes(x=month, y=Power, fill=factor(Color, levels = c("Theoritical Power (in 1hr) - Active Power", "LV.ActivePower..kW." ))))+
  geom_bar(stat="identity", position = "stack")+
  labs(fill="Color") +ylab("Power (KW)") +theme(legend.position = "top", legend.direction = "horizontal")

ggplot(data, aes(x=`LV.ActivePower..kW.`, y=`diff`))+geom_bar(stat="identity")


data_x <- data
data_x <- data_x %>% arrange(desc(`LV.ActivePower..kW.`))
f.rec <- data.frame(xmin = 39500, xmax = Inf, ymin=0, ymax=Inf)

q = ggplot(data_x, aes(x=1:nrow(data_x), y=`LV.ActivePower..kW.`))+geom_line()
q + geom_rect(data= f.rec, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)


#cleaning dataset

#prediction
#visualizing the data all through the year
ggplot(data, aes(x=1:nrow(data), y=`LV.ActivePower..kW.`))+geom_line()

ggplot(data, aes(x=Date.Time, y=`LV.ActivePower..kW.`))+geom_line()

#from the plot we can see the periodicity which is as a result of periodicity
#diving deeper for the first 10 days
ggplot(data[1:1440,], aes(x=1:1440, y=`LV.ActivePower..kW.`))+geom_line()
#from the plot we can see the periodicity especially for the last 4days.
#as such due to the relablility of the periodicity of the yearly time scale of the
#the data, it might be possible to predict few more days.
