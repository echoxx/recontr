setwd("~/Google_Drive2/Reconsider/2016-07_Risk_Ter")


#Pre-processing
library(data.table)
library(dplyr) 
library(ggplot2)
library(reshape2)

#Load terrorist attacks
attacks <- read.csv("attacks_data.csv")
names(attacks) <- c("ID", "date", "country", "city", "killed", "injured", "description")
attacks$date <- as.Date(attacks$date)
attacks <- data.table(attacks)

#load other deaths
otherdeaths <- read.table("Deaths_1999-2014.txt", sep = "\t", header = T)
otherdeaths <- data.table(otherdeaths)
otherdeaths <- otherdeaths[otherdeaths$Notes != "Total",]
otherdeaths[, c("Notes", "Year.Code") := NULL]
names(otherdeaths) <- c("cause.death", "death.code", "year", "deaths", "population", "crude.rate")

#Attacks by country
attacks.bycountry <- attacks[order(attacks[,c(country,date)]), .(ID, date, country, city, killed, injured)]
attacks.bycountry <- attacks.bycountry[, .(killed = sum(killed), injured = sum(injured)), by = country]

#US attacks
attacks.US <- attacks[which(country == "USA"),]
#attacks.US.ex911 <- attacks.US[format.Date(attacks.US$date, "%y") != "01",]

#Create data table with yearly metrics
US.fat <- numeric()
US.inj <- numeric()
tempcount <- 2001
for(i in 1:16) {
  US.fat[i] <- sum(attacks.US[format.Date(attacks.US$date, "%Y") == tempcount,5])
  US.inj[i] <- sum(attacks.US[format.Date(attacks.US$date, "%Y") == tempcount,6])
  tempcount <- tempcount + 1
}

#for bar charts
attacks.US.bar <- data.table(year = 2001:2016,
                             killed = US.fat,
                             injured = US.inj)

attacks.US.bar.ex911 <-  US.attacks.bar[year > 2001]

##Plots
#US incl 911
ggplot(attacks.US.bar, aes(x = year, y = killed)) + geom_bar(stat = "identity", width=0.7, fill="steelblue") + 
  geom_text(aes(label=killed), vjust=-0.3, size=3.5)

#US, excl 911
ggplot(attacks.US.bar.ex911, aes(x = year, y = killed)) + geom_bar(stat = "identity", width=0.7, fill="steelblue") + 
  geom_text(aes(label=killed), vjust=-0.3, size=3.5) + 
  theme_minimal() 

#Injured & Killed
attacks.US.bar.ex911.melt <- melt(attacks.US.bar.ex911, id = "year", variable.name = "inj_kill", value.name = "count" )

ggplot(attacks.US.bar.ex911.melt, aes(x = year, y = count, fill = inj_kill)) + 
  geom_bar(stat = "identity", width=0.7, position = position_dodge()) + 
  theme_minimal() +
  geom_text(aes(label=count), vjust=-0.2, hjust = 0.6, color="black", position = position_dodge(0.9), size=3) + 
  scale_fill_brewer(palette = "Paired")
