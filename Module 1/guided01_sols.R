########################
##traditional approach##
########################

students<-read.table("students.txt", header=TRUE) 

##############
##Question 1##
##############

##remove first column
students.df<-students[,-1]

##############
##Question 2##
##############

##number of students
nrow(students.df)

##############
##Question 3##
##############

##number of students with missing data
missing<-students.df[!complete.cases(students.df),]
nrow(missing)

##############
##Question 4##
##############

##find medians of numeric variables
apply(students.df[,c(5:8)],2,median,na.rm=T)

##############
##Question 5##
##############

##mean of studyhrs by gender
tapply(students.df$StudyHrs, students.df$Gender, mean, na.rm=T)

##SD of studyhrs by gender
tapply(students.df$StudyHrs, students.df$Gender, sd, na.rm=T)

##############
##Question 6##
##############

##new variable PartyAnimal
PartyAnimal<-ifelse(students.df$PartyNum>8, "yes", "no")

##############
##Question 7##
##############

##new variable GPA.cat
GPA.cat<-cut(students.df$GPA, breaks = c(-Inf, 3.0, 3.5, Inf), right = FALSE, labels = c("low", "moderate", "high"))

##############
##Question 8##
##############

##number of students with the 3 criteria
fun.times<-students.df[which(students.df$GPA<3.0 & students.df$PartyNum>8 & students.df$StudyHrs<15),]
nrow(fun.times)

##############
##Question 9##
##############

##add newly created variables to data frame
students.df<-data.frame(students.df,PartyAnimal,GPA.cat)

##export data as .csv
write.csv(students.df, file="new_students.csv", row.names = TRUE)

