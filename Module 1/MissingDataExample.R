# Load tidyverse or dyplr
library(tidyverse)
# Read in data set, store, open
height_weight<-read.csv("missing.csv", header=TRUE)
view(height_weight)
# View standard missing data
missing<-is.na(height_weight)
###Only rows 5 & 6 from Height column show, even though there are missing values in rows 7,8,9 in Weight
#Update data set to replace non-standard missing values with standard missing value terms
height_weight<-height_weight %>% 
  mutate(Weight=replace(Weight,Weight=="",NA)) %>% 
  mutate(Weight=replace(Weight,Weight=="na",NA)) %>% 
  mutate(Weight=replace(Weight,Weight=="N/A",NA))
##change weight by replacing weight value when it equals x with NA 
# Check your work - rerun missing 
missing<-is.na(height_weight)
# Now TRUE is in Rows 5&6 for Height and 7,8,9 for Weight