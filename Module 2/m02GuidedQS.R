library(tidyverse)
students<-read.csv("new_students.csv",header=TRUE)
# PROBLEM 1: Produce frequency table number of students in each level of GPA.cat
# Single Categorical Variable
table(students$GPA.cat)
# Change order of categories to be logical 
students$GPA.cat<-factor(students$GPA.cat, levels=c("low","moderate","high"))
levels(students$GPA.cat)
mytab<-table(students$GPA.cat)
mytab
# PROBLEM 2: Produce bar chart summarizing number of students in each level of GPA.cat
students %>% 
  filter(!is.na(GPA.cat)) %>% 
    ggplot(aes(x=GPA.cat))+
    geom_bar()+
    labs(x="GPA Category",y="Number of Students",title="Number of students in each GPA category")
# PROBLEM 3: Proportion bar chart summarizing number of students in each level of GPA.cat
# create new data frame where each row represents a category, add proportion of each category to a new column 
newStudents<-students %>% 
  group_by(GPA.cat) %>% 
  summarize(counts=n()) %>% 
  mutate(percent=counts/nrow(students))
newStudents
# proportion bar chart
newStudents %>% 
  filter(!is.na(GPA.cat)) %>% 
    ggplot(aes(x=GPA.cat,y=percent))+
    geom_bar(stat="identity")+
    labs(x="GPA Category",y="Proportion of Students",title="Proportion of students in each GPA category")
# PROBLEM 4: Produce a two-way table for the number of female and male students and the GPA category
tab3<-table(students$Gender,students$GPA.cat)
tab3
# PROBLEM 5: Produce a table for the percentage of GPA category for each gender. For the percentage round to 2 decimal places. Comment on the relationship between gender and GPA.cat
tab4<-round(prop.table(tab3,1) * 100, 2)
tab4
# There is a slightly higher percentage of males in the low category and a more significant higher percentage of females in the high category.
# PROBLEM 6: Create a proportion bar chart of GPA categories for female and male students. Be sure to remove the bar corresponding to the missing values.
#GenderGPA<-students %>% 
  #group_by(GPA.cat,Gender) %>% 
  #summarize(counts=n()) %>% 
  #mutate(percent=counts/nrow(students))
#GenderGPA
#GenderGPA %>% 
  #filter(!is.na(GPA.cat)) %>% 
  #filter(!is.na(Gender)) %>% 
    #ggplot(aes(x=GPA.cat,y=percent,fill=Gender))+
    #geom_bar(position="dodge",stat="identity")+
    #labs(x="GPA Categories by Gender",y="Proportion of Students",title="Number of students in each GPA category based on Gender")
# DID NOT NEED TO MAKE TABLE FIRST 
students %>% 
  filter(!is.na(GPA.cat)) %>% 
  ggplot(aes(x=Gender, fill=GPA.cat))+
  geom_bar(position="dodge")+
  labs(x="Gender",y="Proportion",title="GPA Categories by Gender")
# PROBLEM 7: Create bar chart with GPA categories, gender, and split based on smoking status
students %>% 
  filter(!is.na(GPA.cat)) %>% 
  filter(!is.na(Gender)) %>% 
    ggplot(aes(x=Gender,fill=GPA.cat))+
    geom_bar(position="dodge")+
    facet_wrap(~Smoke)+
    labs(x="Gender",y="Proportion",title="GPAs by Gender and Smoking Status",subtitle="Smoking Status")+
    theme(plot.subtitle=element_text(hjust=0.5))
# These trends, there is a slightly higher percentage of males in the low category and a more significant higher percentage of females in the moderate and high categories, remain consistent among smokers and non-smokers.
# PROBLEM 8: Create a scatterplot of GPA against the amount of hours spent studying a week. How would you describe the relationship between GPA and amount of time spent studying?
    ggplot(students, aes(x=StudyHrs,y=GPA))+
    geom_point(alpha=0.5)+
    labs(x="Study Hours",y="GPA",title="GPA Against Study Hours")
# As study hours increase, so does GPA category.
    #professors: There appears to be some relationship between GPA and the amount of time spent studying.
    #Generally, the more time spent studying, the higher the GPA, although this relationship is
    #not very strong. The absence of data points in the bottom right quadrant does inform us
    #that students who study a lot (more than 40 hours) almost always have a GPA higher than
    #3.25
# PROBLEM 9: Add the number of days the student parties in a month.
students %>% 
  ggplot(aes(x=StudyHrs,y=GPA,size=PartyNum))+
  geom_point(alpha=0.5)+
  labs(x="Study Hours",y="GPA",title="Effect of Study Hours and Party Hours on GPA")+
  scale_size(range=c(0.1,5))
  #Professors:Looking at the top right quadrant, we see an individual who parties between 20 and 30 times
  #a week, but studies 40 hours a week and has a 4.0 GPA. The rest of the students in this
  #quadrant party between 10 and 20 times a month.
  #The picture in the left half of the plot is less clear, we see plots of all sizes that seem to be
  #randomly scattered with little apparent pattern
# PROBLEM 10: Add whether the student smokes or not to scatterplot in problem 9.
students %>% 
  ggplot(aes(x=StudyHrs,y=GPA,size=PartyNum,color=Smoke))+
  geom_point(alpha=0.5)+
  labs(x="Study Hours",y="GPA",title="Effect of Study Hours and Party Hours on GPA")+
  scale_size(range=c(0.1,5))
#Professors: The red plots represent non-smokers, and they seem to have higher GPAs than the blue plots (smokers).

  
  