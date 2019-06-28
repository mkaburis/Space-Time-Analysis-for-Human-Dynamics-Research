library(dplyr)
library(tidyr)
#loading in files
sqf03 = read.csv(file = "/Users/sam/Desktop/csv_files/2003.csv", stringsAsFactors = F)
sqf04 = read.csv(file = "/Users/sam/Desktop/csv_files/2004.csv")
sqf05 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf05.csv")
sqf06 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf06.csv")
sqf07 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf07.csv")
sqf08 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf08.csv")
sqf09 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf09.csv")
sqf10 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf10.csv")
sqf11 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf11.csv")
sqf12 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf12.csv")
sqf13 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf13.csv")
sqf14 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf14.csv")
sqf15 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf15.csv")
sqf16 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf16.csv")
sqf17 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf17.csv")
sqf18 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf18.csv")

# Precinct, Searched, Race, Create Col. Merge Contraband Columns
sqf03mod = sqf03 %>%
  select(pct, searched, contrabn, pistol, riflshot, asltweap, knifcuti, machgun, othrweap, race)
sqf03mod = sqf03mod %>%
  mutate(contraband_flag = if_else((sqf03mod$contrabn == "Y" | sqf03mod$pistol == "Y" |
                                      sqf03mod$riflshot == "Y" | sqf03mod$asltweap == "Y" | 
                                      sqf03mod$knifcuti == "Y" | sqf03mod$machgun == "Y" | 
                                      sqf03mod$othrweap == "Y"), T, F))
#formatting 2003 data into the format of the NC data
sqf03mod = sqf03mod %>% 
  select(pct, searched, contraband_flag, race) 
#turning searched and contraband columns into 0s and 1s to add them
sqf03mod$searched[sqf03mod$searched == "N" ] = as.integer(0)
sqf03mod$searched[sqf03mod$searched == "Y" ] = as.integer(1)
sqf03mod$contraband_flag[sqf03mod$contraband_flag == F ] = as.integer(0)
sqf03mod$contraband_flag[sqf03mod$contraband_flag == T ] = as.integer(1)
#finding total stops, searches, and hits per race per precinct
sqf03mod = sqf03mod %>% 
  group_by(pct, race) %>%
  summarize(numstops = length(searched), 
            numsearches = sum(as.numeric(searched)), numhits = sum(contraband_flag))


# iterate through the table
for(i in 1:nrow(dfmod)) {
  row <- dfmod[i, ]
  print(row)
}






