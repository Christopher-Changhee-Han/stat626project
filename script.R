#STAT 626 GROUP 8 Project
#code by Christopher Han

#install.packages("astsa")
#install.packages("lubridate")

library(astsa)
library(stats)
library(lubridate) #package needed for date manipulation
library(dplyr) #package makes easier for manipulating data frames

file <- "C:/Users/Chris Han/Desktop/STAT/STAT 626/PROJECT/2.csv" # change this to your file path

ercot <- read.csv(file, header = TRUE, skip = 4)
names(ercot)[2] <- "mwh"
ercot$Category <- as.character(ercot$Category)
ercot$Category <- gsub(" -0[56]00", "", ercot$Category)
ercot$Category <- mdy_hm(ercot$Category)

#Sum hourly to daily
daily_ercot <- ercot %>%
    mutate(day = as.Date(Category, format="%m/%d/%y")) %>%
    group_by(day) %>% # group by the day column
    summarise(daily_mwh=sum(mwh)) %>%  # calculate the SUM of all electricity consumed each day
    na.omit()

#Yearly graph
start <- mdy("06/01/2019")
end <- mdy("05/31/2020")
oneyear <- daily_ercot[daily_ercot$day > start & daily_ercot$day < end,]

par(mfrow= c(1,1)) 
tsplot(oneyear$day, 
       y = oneyear$daily_mwh, 
       col = "blue", 
       gg = TRUE, 
       main = "Daily Electricity Demand (MWH) in Texas 05/31/2019 - 06/01/2020",
       ylab = "MWH (Megawatt-Hour)", 
       las = 0 # graphical parameter to make y-axis label not overlap with the scale
       )

#Monthly graph
start <- mdy("07/01/2019")
end <- mdy("08/01/2019")
onemonth <- daily_ercot[daily_ercot$day > start & daily_ercot$day < end,]

par(mfrow= c(1,1)) 
tsplot(onemonth$day, 
       y = onemonth$daily_mwh, 
       col = "blue", 
       gg = TRUE, 
       main = "Daily Electricity Demand (MWH) in Texas 07/2019",
       ylab = "MWH (Megawatt-Hour)", 
       las = 0 # graphical parameter to make y-axis label not overlap with the scale
)
