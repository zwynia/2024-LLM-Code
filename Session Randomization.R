library(readxl)
library(tidyverse)
library(ggplot2)

PRT <- read_excel("/Users/zanwynia/Desktop/Datasets/PRT_Rand.xlsx")


results_prt <- PRT %>% 
  #Split record ID & session #
  separate(RecordID_SessionNum, into = c("RecordID", "SessionNum"), sep="-") %>% 
  #Group by record ID
  group_by(RecordID) %>% 
  #Randomly sample one row per group 
  slice_sample(n = 1) %>% 
  #Ungroup data 
  ungroup() %>% 
  #Randomly select 20 rows 
  slice_sample(n = 20) %>% 
  #Recombine record ID and session num 
  mutate(RecordID_SessionNum = paste(RecordID, SessionNum, sep="-"))


#Same thing for CBT now 

CBT <- read_excel("/Users/zanwynia/Desktop/Datasets/CBT_Rand.xlsx")

results_cbt <- CBT %>% 
  #Split record ID & session #
  separate(RecordID_SessionNum, into = c("RecordID", "SessionNum"), sep="-") %>% 
  #Group by record ID
  group_by(RecordID) %>% 
  #Randomly sample one row per group 
  slice_sample(n = 1) %>% 
  #Ungroup data 
  ungroup() %>% 
  #Randomly select 20 rows 
  slice_sample(n = 20) %>% 
  #Recombine record ID and session num 
  mutate(RecordID_SessionNum = paste(RecordID, SessionNum, sep="-"))
