setwd("~/Google_Drive2/Reconsider/2016-07_Risk_Ter")


### 1. Pre-processing ####
library(data.table)
library(dplyr) 
library(ggplot2)
library(reshape2)
library(scales)


### 2. Load & clea process data ###
#Load terrorist attacks
attacks <- read.csv("attacks_data.csv")
names(attacks) <- c("ID", "date", "country", "city", "killed", "injured", "description")
attacks$date <- as.Date(attacks$date)
attacks <- data.table(attacks)

#Terrorism - Attacks by country
attacks.bycountry <- attacks[order(attacks[,c(country,date)]), .(ID, date, country, city, killed, injured)]
attacks.bycountry <- attacks.bycountry[, .(killed = sum(killed), injured = sum(injured)), by = country]

#US attacks
attacks.US <- attacks[which(country == "USA"),]
#attacks.US.ex911 <- attacks.US[format.Date(attacks.US$date, "%y") != "01",]

#Terrorism - Create data table with yearly metrics
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

#Load other deaths
otherdeaths <- read.table("Deaths_1999-2014.txt", sep = "\t", header = T, stringsAsFactors = F)
otherdeaths <- data.table(otherdeaths)
otherdeaths <- otherdeaths[otherdeaths$Notes != "Total",]
otherdeaths[, c("Notes", "Year.Code") := NULL]
names(otherdeaths) <- c("cause.death", "death.code", "year", "deaths", "population", "crude.rate")
keep_codes <- c("X23", "X33", "X40", "X41", "X42", "X44", "X45", "X46", "X47", "X49")
otherdeaths <- otherdeaths[death.code %in% c("X23", "X33", "X40", "X41", "X42", "X44", "X45", "X46", "X47", "X49")]
otherdeaths <- otherdeaths[year %in% 2001:2014]

#Load homicides
homicides <- read.table("homicides.txt", sep = "\t", header = T, stringsAsFactors = F)
homicides <- data.table(homicides)
homicides[, c("Notes", "Year.Code") := NULL]
names(homicides) <- c("cause.death", "death.code", "year", "deaths", "population", "crude.rate")
setkey(homicides, death.code)
homicides <- homicides[!c("U01.1", "U01.2", "U01.4")]   #Remove terrorist attacks
setkey(homicides, year)
homicides.02.14 <- homicides[J(2002:2014), sum(deaths), by = year]


#Load top causes of death
topcauses <- data.table(read.csv("topcausesUS.csv", stringsAsFactors = FALSE))
topcauses[,AADR:=NULL]
setkey(topcauses, CAUSE_NAME)
top5causes <- topcauses[c("Diseases of Heart", "Cancer", "CLRD", "Unintentional Injuries", "Stroke")]
top5causes.0214 <- top5causes[YEAR > 2001]

### 3. PLOTS ###

#Terrorism deaths - US incl 911
ggplot(attacks.US.bar, aes(x = year, y = killed)) + geom_bar(stat = "identity", width=0.7, fill="steelblue") + 
  geom_text(aes(label=comma(killed) ), vjust=-0.3, size=3.5) +
  theme_minimal() + 
  ylab("Deaths") + xlab("Year") +
  scale_x_continuous(breaks = c(seq(from = 2002, to = 2014, by = 1))) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())

#Figure 1 - Terrorism deaths - US, excl 911
ggplot(attacks.US.bar.ex911, aes(x = year, y = killed)) + geom_bar(stat = "identity", width=0.7, fill="steelblue") + 
  geom_text(aes(label=killed), vjust=-0.2, hjust = 0.5, color="black", size = 3) + 
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") + 
  ylab("Deaths") + xlab("Year") +
  scale_x_continuous(breaks = c(seq(from = 2002, to = 2014, by = 1))) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  ggtitle("Deaths from terrorism")
  

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
names(terr.v.lightning.bees) <- c("year", "Terrorism", "Lightning", "Bee Stings")
terr.v.lightning.bees.melt <- melt(terr.v.lightning.bees, id = "year", variable.name = "death_type", value.name = "count")
  
##Deaths over time
ggplot(terr.v.lightning.bees.melt, aes(x = year, y = count, fill = death_type)) + 
  geom_bar(stat = "identity", width=0.7, position = position_dodge()) + 
  theme_minimal() +
  geom_text(aes(label=count), vjust=-0.2, hjust = 0.5, color="black", position = position_dodge(0.9), size=3) + 
  scale_fill_brewer(palette = "Paired", name = "Death Type") + 
  ylab("Deaths") + xlab("Year") +
  scale_x_continuous(breaks = c(seq(from = 2002, to = 2014, by = 1))) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())


##Death sums
terr.lightning.bees.sumdeath <- terr.v.lightning.bees.melt[,sum(count), by = death_type]
names(terr.lightning.bees.sumdeath) <- c("death_type", "count")
terr.lightning.bees.sumdeath.2016 <- terr.lightning.bees.sumdeath
terr.lightning.bees.sumdeath.2016$count[1] <- sum(attacks.US.bar.ex911$killed)

##Death sums terror, lightning, bees (Excludes 2015-2016)
terr.lightning.bees.sumdeath$death_type <- c("Terrorism", "Lightning", 
                                                  "Bee Stings")
terr.lightning.bees.sumdeath$death_type <- factor(terr.lightning.bees.sumdeath$death_type, 
                                                       levels = c("Terrorism", "Lightning", "Bee Stings"))

ggplot(terr.lightning.bees.sumdeath[order(terr.lightning.bees.sumdeath$count)], aes(x = death_type, y = count, order = death_type, fill = death_type)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) + 
  theme_minimal() + 
  geom_text(aes(label=count), vjust=-0.2, hjust = 0.6, color="black", position = position_dodge(0.9), size=3) + 
  ylab("Death Type") + xlab("Deaths") +
  scale_fill_brewer(palette = "Paired", name = "Death Type") +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
          axis.text.x = element_text(angle = 90), 
          panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  

##Death sums terror, lightning, bees (Includes 2015-2016)
terr.lightning.bees.sumdeath.2016$death_type <- c("Terrorism", "Lightning", 
                                                      "Bee Stings")
terr.lightning.bees.sumdeath.2016$death_type <- factor(terr.lightning.bees.sumdeath.2016$death_type, 
                                                           levels = c("Terrorism", "Lightning", "Bee Stings"))

ggplot(terr.lightning.bees.sumdeath.2016[order(terr.lightning.bees.sumdeath.2016$count)], aes(x = death_type, y = count, order = death_type, fill = death_type)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) + 
  theme_minimal() + 
  geom_text(aes(label=count), vjust=-0.2, hjust = 0.6, color="black", position = position_dodge(0.9), size=3) + 
  ylab("Deaths") + xlab("Death Type") + 
  scale_fill_brewer(palette = "Paired", name = "Death Type") + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())


#########Death sums terror, lightning, bees, homicide (Includes 2015-2016)
terr.v.lightning.bees.hom <- merge(terr.v.lightning.bees, homicides.02.14, by = "year", all.x = T)
names(terr.v.lightning.bees.hom)  <- c("year", "terror_deaths", "lightning_deaths", "bee_deaths", "homicides")
terr.v.lightning.bees.hom.melt <- melt(terr.v.lightning.bees.hom, id = "year", variable.name = "death_type", value.name = "count")
terr.lightning.bees.hom.sumdeath <- terr.v.lightning.bees.hom.melt[,sum(count), by = death_type]
names(terr.lightning.bees.hom.sumdeath) <- c("death_type", "count")

#########One year of homicide
terr.lightning.bees.hom.sumdeath.2014 <- terr.lightning.bees.hom.sumdeath
terr.lightning.bees.hom.sumdeath.2014$count[4] <- terr.v.lightning.bees.hom$homicides[13]

terr.lightning.bees.hom.sumdeath.2014$death_type <- c("Terrorism", "Lightning", 
                                                      "Bee Stings", "Homicides")
terr.lightning.bees.hom.sumdeath.2014$death_type <- factor(terr.lightning.bees.hom.sumdeath.2014$death_type, 
                                 levels = c("Terrorism", "Lightning", "Bee Stings", "Homicides"))

ggplot(terr.lightning.bees.hom.sumdeath.2014[order(terr.lightning.bees.hom.sumdeath$count)], 
       aes(x = death_type, y = count, order = death_type, fill = death_type)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) + 
  theme_minimal() + 
  geom_text(aes(label= comma ( count )), vjust=-0.2, hjust = 0.6, color="black", 
            position = position_dodge(0.9), size=3) + 
  ylab("Deaths") + xlab("Death Type") + 
  scale_fill_brewer(palette = "Paired", name = "Death Type") + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())

#########All years of homicides
terr.lightning.bees.hom.sumdeath$death_type <- c("Terrorism", "Lightning", 
                                                      "Bee Stings", "Homicides")
terr.lightning.bees.hom.sumdeath$death_type <- factor(terr.lightning.bees.hom.sumdeath$death_type, 
                                                           levels = c("Terrorism", "Lightning", "Bee Stings", "Homicides"))

ggplot(terr.lightning.bees.hom.sumdeath[order(terr.lightning.bees.hom.sumdeath$count)], aes(x = death_type, y = count, order = death_type, fill = death_type)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) + 
  theme_minimal() + 
  geom_text(aes(label = comma ( count )), vjust=-0.2, hjust = 0.6, 
            color="black", position = position_dodge(0.9), size=3) + 
  ylab("Deaths") + xlab("Death Type") + 
  scale_fill_brewer(palette = "Paired", name = "Death Type") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
      axis.text.x = element_text(angle = 90), 
      panel.grid.minor = element_blank(), panel.grid.major = element_blank())


#########Top causes of death (1 yr top 5 vs 12 yrs others)
top5.14 <- top5causes[,tail(DEATHS, n = 1), by = CAUSE_NAME]
names(top5.14) <- c("death_type", "count")
top5.14 <- top5.14[order(top5.14$count)]
all.top5.14 <- full_join(terr.lightning.bees.hom.sumdeath, top5.14)
all.top5.14$death_type <- c("Terrorism", "Lightning", "Bee Stings", "Homicides", "Stroke",
                              "Accidents", "CLRD", "Cancer", "Heart Disease")

all.top5.14$death_type <- factor(all.top5.14$death_type, 
                                   levels = c("Heart Disease", "Cancer", "CLRD", 
                                              "Accidents", "Stroke", "Homicides", 
                                              "Bee Stings", "Lightning", "Terrorism"))

ggplot(all.top5.14, aes(x = death_type, y = count, 
                          order = death_type, fill = death_type)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) + 
  theme_minimal() + 
  geom_text(aes(label = comma(count) ), vjust=-0.2, hjust = 0.5, color="black", 
            position = position_dodge(0.9), size=3) + 
  ylab("Deaths") + xlab("Death Type") + 
  scale_fill_brewer(palette = "Paired", name = "Death Type") + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())

#########Top causes of death (12 yrs all)
top5.0214 <- top5causes.0214[,sum(DEATHS), by = CAUSE_NAME]
names(top5.0214) <- c("death_type", "count")
all.top5.0214 <- full_join(terr.lightning.bees.hom.sumdeath, top5.0214)
all.top5.0214 <- all.top5.0214[order(all.top5.0214$count),]
all.top5.0214$death_type <- c("Terrorism", "Lightning", "Bee Stings", "Homicides", "Accidents",
                              "CLRD", "Stroke", "Cancer", "Heart Disease")

all.top5.0214$death_type <- factor(all.top5.0214$death_type, 
                                 levels = c("Heart Disease", "Cancer", "Stroke", 
                                            "CLRD", "Accidents", "Homicides", 
                                            "Bee Stings", "Lightning", "Terrorism"))


ggplot(all.top5.0214, aes(x = death_type, y = count, 
                                                   order = death_type, fill = death_type)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) + 
  theme_minimal() + 
  geom_text(aes(label = comma(count) ), vjust=-0.2, hjust = 0.5, color="black", 
            position = position_dodge(0.9), size=3) + 
  ylab("Deaths") + xlab("Death Type") + 
  scale_fill_brewer(palette = "Paired", name = "Death Type") + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())


#########All causes in 2001 compared to terrorism
attacks.US.01 <- attacks.US.bar[year == 2001]
attacks.US.01[,c("death_type", "injured") := .("Terrorism", NULL)]
names(attacks.US.01) <- c("year", "deaths", "death_type")
setcolorder(attacks.US.01, c("year", "death_type",  "deaths"))

###hornwaspbees###
bees.01 <- hornwaspbees[year == 2001]
bees.01 <- select(bees.01, year, cause.death, deaths)
names(bees.01) <- c("year", "death_type", "deaths")
bees.01$death_type[1] <- "Bee Stings"

lightning.01 <- lightning[year == 2001]
lightning.01 <- select(lightning.01, year, cause.death, deaths)
names(lightning.01) <- c("year", "death_type", "deaths")
lightning.01$death_type[1] <- "Lightning"


homicides.01 <- homicides[.(2001), sum(deaths), by = year]
homicides.01[,death_type := "death_type"]
names(homicides.01) <- c("year", "deaths", "death_type")
setcolorder(homicides.01, c("year", "death_type", "deaths"))
homicides.01$death_type <- "Homicides"

top5causes.01 <- top5causes[YEAR == 2001]
top5causes.01 <- select(top5causes.01, YEAR, CAUSE_NAME, DEATHS)
names(top5causes.01) <- c("year", "death_type", "deaths")
all.01 <- rbind(top5causes.01, homicides.01, bees.01, lightning.01, attacks.US.01)
all.01 <- all.01[order(all.01$deaths),]
all.01$death_type <- factor(all.01$death_type, 
                                   levels = c("Heart Disease", "Cancer", "Stroke", 
                                              "CLRD", "Accidents", "Homicides", 
                                              "Bee Stings", "Lightning", "Terrorism"))
ggplot(all.01, aes(x = death_type, y = deaths, order = death_type, color = death_type)) + 
  geom_bar(stat = "identity")

ggplot(all.01, aes(x = death_type, y = deaths, 
                          order = death_type, fill = death_type)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) + 
  theme_minimal() + 
  geom_text(aes(label = comma(deaths) ), vjust=-0.2, hjust = 0.5, color="black", 
            position = position_dodge(0.9), size=3) + 
  ylab("Deaths") + xlab("Death Type") + 
  scale_fill_brewer(palette = "Paired", name = "Death Type") + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())

#########Top 5 causes over time
top5causes.ts <- select(top5causes, YEAR, CAUSE_NAME, DEATHS)
names(top5causes.ts) <- c("year", "death_type", "deaths")
top5causes.ts[death_type == "Diseases of Heart", death_type := "Heart Disease"] 
top5causes.ts[death_type == "Unintentional Injuries", death_type := "Accidents"] 


ggplot(top5causes.ts, aes(x = year, y = deaths, color = death_type )) + 
  geom_line() + guides(guide = guide_legend(title = "TEST")) +
  scale_y_log10(labels = comma,
                              breaks = trans_breaks("log10", function(x) 10^x)) + 
  
  scale_fill_brewer(palette = "Paired") + 
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  ylab("Deaths (logarithmic)") + xlab("Year") 

### scale_fill_brewer options available here: http://docs.ggplot2.org/current/scale_brewer.html








