library(dplyr)
library(lubridate)

#PIPE IT ALL TOGETHER! Praise Hadely
test <- read.csv("folderstartsample.csv",stringsAsFactors = F) %>%
          mutate(instdate=lubridate::mdy(instdate), #convert dates
                 remdatefill=lubridate::mdy(remdatefill),
                 folderdateend=lubridate::mdy(folderdateend))


output <- test%>%
          group_by(site) %>%
          distinct(folderdateend) %>%
          mutate(folderstart = lag(folderdateend)) %>%
          right_join(., test)%>%
          mutate(folderstart = ifelse(is.na(folderstart), instdate, folderstart),
                 folderstart = as.Date(folderstart, origin = "1970-01-01"),
                 folderstarttest = ifelse(folderstart < instdate, instdate, folderstart),
                 folderstarttest = as.Date(folderstarttest, origin = "1970-01-01")) %>%
          ungroup()%>%data.frame()


print("Praise Hadley")

#create a data frame of unique values per site
# test1 <- test %>%
#   group_by(site) %>%
#   distinct(folderdateend) %>%
#   data.frame()
# 
# #lag the unique date data frame by 1L
# test2 <- test1 %>%
#   group_by(site) %>%
#   mutate(folderstart = lag(folderdateend)) %>%
#   data.frame()
# 
# #join the lagged data frame to the main data frame to fill in folder start dates
# test3 <- left_join(test, test2)
# 
# #If folderstart is NA, then insert the site install date, convert output to a date using starting origin
# test4 <- test3 %>%
#   group_by(site) %>%
#   mutate(folderstart = ifelse(is.na(folderstart), instdate, folderstart)) %>%
#   mutate(folderstart = as.Date(folderstart, origin = "1970-01-01")) %>%
#   data.frame()
# 
# #If folderstart < instdate, then replace with instdate, convert output to a date using starting origin
# test5 <- test4 %>%
#   group_by(site) %>%
#   mutate(folderstarttest = ifelse(folderstart < instdate, instdate, folderstart)) %>%
#   mutate(folderstarttest = as.Date(folderstarttest, origin = "1970-01-01")) %>%
#   data.frame()