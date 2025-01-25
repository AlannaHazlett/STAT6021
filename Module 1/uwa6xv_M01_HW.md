library(tidyverse)
USCovid.df<-read.csv("UScovid.csv",header=TRUE)
view(USCovid.df)
# Get subset of data from June 3, 2021, remove date and flip columns
USCovid1.df<-USCovid.df[c(1381437:1384683),c(2,3,5,6)]
view(USCovid1.df)
# Remove rows with Unknown counties 
UnknownCounty<-which(USCovid1.df$county=="Unknown")
USCovid2.df<-USCovid1.df[-c(91,181,316,321,386,535,551,692,884,1177,1199,1224,1241,1322,1405,1757,1791,1812,2054,2141,2222,2402,2414,2615,2902,2919),]
# ordered by county and then state alphabetically and name 'latest'
latest<-USCovid2.df[order(USCovid2.df$county,USCovid2.df$state),]
head(latest)
# County case fatality rate (numeric variable to numeric variable)
death.rate<-(latest$deaths / latest$cases) * 100
death.rate<-round(death.rate,2)
# Add death.rate to latest and display first 6 rows
latest<-data.frame(latest,death.rate)
head(latest)
# Display 10 largest cases
LargeCases<-latest[order(-latest$cases),]
LargeCases<-LargeCases[c(1:10),]
view(LargeCases)
# Display 10 largest deaths
LargeDeaths<-latest[order(-latest$deaths),]
LargeDeaths<-LargeDeaths[c(1:10),]
view(LargeDeaths)
# Display 10 largest case fatality rates
LargeDeathRate<-latest[order(-latest$death.rate),]
LargeDeathRate<-LargeDeathRate[c(1:10),]
view(LargeDeathRate)
# Display the counties with the 10 highest case fatality rates among counties with at lease 100,000 cases
LargeCountyLargeRate<-latest[which(latest$cases>99999),]
LargeCountyLargeRate<-LargeCountyLargeRate[order(-LargeCountyLargeRate$death.rate),]
LargeCountyLargeRate<-LargeCountyLargeRate[c(1:10),]
view(LargeCountyLargeRate)
# Display these rows: Albemarle, Virginia Charlottesville city, Virginia 
Albemarle<-latest[c(35),]
view(Albemarle)
Charlottesville<-latest[c(474),]
view(Charlottesville)
# Problem 2 
# Select rows with June 3, 2021 date, select columns state, # of cases, # of deaths
USCovid3.df<-USCovid.df[c(1381437:1384683),c(3,5,6)]
# Create state.level with sums of cases and sums of deaths by state
# Received help from Karunya Iyappan on Piazza to resolve state column 
state.names<-unique(USCovid3.df$state)
state.cases<-tapply(USCovid3.df$cases,USCovid3.df$state,sum)
state.deaths<-tapply(USCovid3.df$deaths,USCovid3.df$state,sum)
state.level<-data.frame(state.names,state.cases,state.deaths)
# Remove row labels (non-working column) received help from Karunya Iyappan on Piazza
rownames(state.level)<-c()
# Renaming columns of state.level 
names(state.level)[c(1,2,3)]<-c("state","cases","deaths")
head(state.level)
# State case fatality rate (numeric variable to numeric variable)
state.rate<-(state.level$deaths / state.level$cases) * 100
state.rate<-round(state.rate,2)
# Add state.rate to state.level and display first 6 rows
state.level<-data.frame(state.level,state.rate)
head(state.level)
# Case fatality for Virginia
Virginia<-state.level[c(51),]
view(Virginia)
# Case fatality for Puerto Rico
Puerto_Rico<-state.level[c(42),]
view(Puerto_Rico)
# Display 10 largest state case fatality rates
LargeStateRate<-state.level[order(-state.level$state.rate),]
LargeStateRate<-LargeStateRate[c(1:10),]
view(LargeStateRate)
# Display 10 lowest state case fatality rates
SmallStateRate<-state.level[order(state.level$state.rate),]
SmallStateRate<-SmallStateRate[c(1:10),]
view(SmallStateRate)
# Create csv called stateCovid.csv
write.csv(state.level,file="stateCovid.csv",row.names=TRUE)

