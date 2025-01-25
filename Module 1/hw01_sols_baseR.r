########################
##traditional approach##
########################

Covid<-read.csv("UScovid.csv", header=TRUE)

#######
##Q1a##
#######

##create data frame by criteria
latest<-Covid[which(Covid$date =="2021-06-03" 
                    & Covid$county != "Unknown"), 
              -c(1,4)]

latest<-latest[order(latest$county, latest$state),]

head(latest)

#######
##Q1b##
#######

##calculate case fatality rate, convert to %, round to 2dp
death.rate<-round(latest$deaths/latest$cases * 100, 2)

##add case fatality rate to data frame
latest<-data.frame(latest, death.rate)

head(latest)

#######
##Q1c##
#######

##find counties with 10 highest number of cases
latest<-latest[order(-latest$cases),]
latest[1:10,]

#######
##Q1d##
#######

##find counties with 10 highest number of deaths
latest<-latest[order(-latest$deaths),]
latest[1:10,]

#######
##Q1e##
#######

##find counties with 10 highest case fatality rates
latest<-latest[order(-latest$death.rate),]
latest[1:10,]

#######
##Q1f##
#######

##consider counties with at least 100,000 cases
most.cases<-latest[which(latest$cases >= 100000),]

##find counties with 10 highest case fatality rates, with at least 100,000 cases
most.cases<-most.cases[order(-most.cases$death.rate),]
most.cases[1:10,]

#######
##Q1g##
#######

##find numbers for Albemarle county, VA
latest[which(latest$county=="Albemarle" & latest$state=="Virginia"),]

##find numbers for Charlottesville, VA
latest[which(latest$county=="Charlottesville city" & latest$state=="Virginia"),]

#######
##Q2a##
#######

##since we are interested in state level data, we can 
##include counties which are unknown since the states
##are known
counties.latest<-Covid[which(Covid$date =="2021-06-03"), -c(1,4)]

##total cases by state
cases.state<-tapply(counties.latest$cases, counties.latest$state, sum, na.rm=T)

state1<-data.frame(State=names(cases.state), Cases=cases.state)

##total deaths by state
deaths.state<-tapply(counties.latest$deaths, counties.latest$state, sum, na.rm=T)

state2<-data.frame(State=names(deaths.state), Deaths=deaths.state)

##merge cases and deaths into a data frame 
state.level<-merge(state1,state2, by="State")

state.level<-state.level[order(state.level$State),]

head(state.level)

#######
##Q2b##
#######

##calculate case fatality rate
state.rate<-round(state.level$Deaths/state.level$Cases * 100, 2)

##add case fatality rate to data frame
state.level<-data.frame(state.level,state.rate)
head(state.level)

#######
##Q2c##
#######

##VA's case fatality rate
state.level[which(state.level$State=="Virginia"),4]

#######
##Q2d##
#######

##PR's case fatality rate
state.level[which(state.level$State=="Puerto Rico"),4]

#######
##Q2e##
#######

##10 highest case fatality rates
state.level<-state.level[order(-state.level$state.rate),]
state.level[1:10,]

#######
##Q2f##
#######

##10 lowest case fatality rates
state.level<-state.level[order(state.level$state.rate),]
state.level[1:10,]

#######
##Q2g##
#######

write.csv(state.level, file="stateCovid.csv", row.names = TRUE)
