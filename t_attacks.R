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

#Load other deaths
otherdeaths <- read.table("Deaths_1999-2014.txt", sep = "\t", header = T, stringsAsFactors = F)
otherdeaths <- data.table(otherdeaths)
otherdeaths <- otherdeaths[otherdeaths$Notes != "Total",]
otherdeaths[, c("Notes", "Year.Code") := NULL]
names(otherdeaths) <- c("cause.death", "death.code", "year", "deaths", "population", "crude.rate")
keep_codes <- c("X23", "X33", "X40", "X41", "X42", "X44", "X45", "X46", "X47", "X49")
otherdeaths <- otherdeaths[death.code %in% c("X23", "X33", "X40", "X41", "X42", "X44", "X45", "X46", "X47", "X49")]
otherdeaths <- otherdeaths[year %in% 2001:2014]

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
  US.fat[i] <- sum(attacks.US[format.Date(attacks.US$date, "%Y") == tempcount, killed ])
  US.inj[i] <- sum(attacks.US[format.Date(attacks.US$date, "%Y") == tempcount, injured])
  tempcount <- tempcount + 1
}

#for bar charts
attacks.US.bar <- data.table(year = 2001:2016,
                             killed = US.fat,
                             injured = US.inj)

attacks.US.bar.ex911 <-  attacks.US.bar[year > 2001]

##Plots
#Terrorism deaths - US incl 911
ggplot(attacks.US.bar, aes(x = year, y = killed)) + geom_bar(stat = "identity", width=0.7, fill="steelblue") + 
  geom_text(aes(label=killed), vjust=-0.3, size=3.5)

#Terrorism deaths - US, excl 911
ggplot(attacks.US.bar.ex911, aes(x = year, y = killed)) + geom_bar(stat = "identity", width=0.7, fill="steelblue") + 
  geom_text(aes(label=killed), vjust=-0.3, size=3.5) + 
  theme_minimal() 

#Terrorism deaths & injured - US, excl 911
attacks.US.bar.ex911.melt <- melt(attacks.US.bar.ex911, id = "year", variable.name = "inj_kill", value.name = "count" )

ggplot(attacks.US.bar.ex911.melt, aes(x = year, y = count, fill = inj_kill)) + 
  geom_bar(stat = "identity", width=0.7, position = position_dodge()) + 
  theme_minimal() +
  geom_text(aes(label=count), vjust=-0.2, hjust = 0.6, color="black", position = position_dodge(0.9), size=3) + 
  scale_fill_brewer(palette = "Paired")

#Lightning v terrorism
setkey(otherdeaths, death.code)
lightning <- otherdeaths["X33"]
lightning.ex911 <- lightning[2:14]
attacks.US.bar.ex911.2014 <- attacks.US.bar.ex911[1:13,]

terr.v.lightning <- merge(attacks.US.bar.ex911.2014, lightning.ex911, by = "year", all.x = T)
terr.v.lightning <- select(terr.v.lightning, year, killed, deaths)
names(terr.v.lightning) <- c("year", "terror_deaths", "lightning_deaths")
terr.v.lightning.melt <- melt(terr.v.lightning, id = "year", variable.name = "death_type", value.name = "count")

#Lightning v bees v terrorism
setkey(otherdeaths, death.code)
hornwaspbees <- otherdeaths["X23"]
hornwaspbees.ex911 <- hornwaspbees[2:14]

terr.v.lightning.bees <- merge(terr.v.lightning, hornwaspbees.ex911, by = "year", all.x = T)
terr.v.lightning.bees <- select(terr.v.lightning.bees, year, terror_deaths, lightning_deaths, deaths)
names(terr.v.lightning.bees) <- c("year", "terror_deaths", "lightning_deaths", "bee_deaths")
terr.v.lightning.bees.melt <- melt(terr.v.lightning.bees, id = "year", variable.name = "death_type", value.name = "count")
  
##Deaths over time
ggplot(terr.v.lightning.bees.melt, aes(x = year, y = count, fill = death_type)) + 
  geom_bar(stat = "identity", width=0.7, position = position_dodge()) + 
  theme_minimal() +
  geom_text(aes(label=count), vjust=-0.2, hjust = 0.6, color="black", position = position_dodge(0.9), size=3) + 
  scale_fill_brewer(palette = "Paired")

##Death sums
terr.lightning.bees.sumdeath <- terr.v.lightning.bees.melt[,sum(count), by = death_type]
names(terr.lightning.bees.sumdeath) <- c("death_type", "count")

##Reorders so that order in chart is correct
terr.lightning.bees.sumdeath <- terr.lightning.bees.sumdeath[order(terr.lightning.bees.sumdeath$count)]


ggplot(terr.lightning.bees.sumdeath[order(terr.lightning.bees.sumdeath$count)], aes(x = death_type, y = count, order = death_type, fill = death_type)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) + 
  theme_minimal() + 
  geom_text(aes(label=count), vjust=-0.2, hjust = 0.6, color="black", position = position_dodge(0.9), size=3) + 
  scale_fill_brewer(palette = "Paired")

#Facet wrap all other deaths
ggplot(otherdeaths, aes(x = year, y = deaths)) + geom_bar(stat = "identity") + 
  facet_wrap(~death.code)



