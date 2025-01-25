library(tidyverse)
students<-read.table("students.txt" , header=TRUE)
view(students)
# Question 1 Remove variables not needed for analysis
students_df<-students[,-1]

#Question 2 Number of students in data frame
nrow(students_df)

# Question 3 How many students have at least one missing entry - 12
missing<-students_df[!complete.cases(students_df),]
nrow(missing)

# Question 4 Median values of numeric variables
apply(students_df[,c(5:8)],2,median,na.rm=T)

# Question 5 Report mean and st. dev. for females and males
# Base R answer
tapply(students_df$StudyHrs, students_df$Gender, mean, na.rm=T)
tapply(students_df$StudyHrs, students_df$Gender, sd, na.rm=T)
#tidyverse answer
#####
#students_df%>%
#  group_by(Gender) %>% 
# summarize(AvgStudyHrs=mean(StudyHrs,na.rm=T))
#students_df%>%
#  group_by(Gender) %>% 
#  summarize(Gender_StDev=sd(StudyHrs,na.rm=T))
#####
# Question 6 Create PartyAnimal if student parties more than 8 days/month
PartyAnimal<-ifelse(students_df$PartyNum>8,"yes","no")

# Question 7 create GPA.cat variable categorize numerical value
GPA.cat<-cut(students_df$GPA,breaks=c(-Inf,3.0,3.5,Inf), right = FALSE, labels=c("low","moderate","high"))
# right = False changes the inclusion of values 

# Q8 Students who have low GPAs (below 3.0), party a lot
#(more than 8 days a month), and study little (less than 15 hours a week). Create a
#data frame that contains these students. How many such students are there?
new_df<-students_df[which(students_df$GPA<3.0 & students_df$PartyNum>8 & students_df$StudyHrs<15),]

# Question 9 add GPA.cat and PartyAnimal to df
new_students<-data.frame(students_df,PartyAnimal,GPA.cat)
write.csv(new_students, file="new_students.csv", row.names = TRUE)


