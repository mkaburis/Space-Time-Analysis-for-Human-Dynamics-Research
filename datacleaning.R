library(dplyr)
library(tidyr)
#loading in files
sqf03 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf03.csv", stringsAsFactors = F)
sqf04 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf04.csv")
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

#data = list(sqf03,sqf04)
sqf03mod = sqf03
sqf03mod$searched[sqf03mod$searched == "N" ] = as.integer(0)
sqf03mod$searched[sqf03mod$searched == "Y" ] = as.integer(1)
sqf03mod$anycontra[sqf03mod$anycontra == F ] = as.integer(0)
sqf03mod$anycontra[sqf03mod$anycontra == T ] = as.integer(1)
#formatting 2003 data into the format of the NC data
sqf03mod = sqf03mod %>% 
  select(pct, searched, anycontra, race) %>% 
  group_by(pct, race) %>%
  summarize(numstops = length(searched), 
            numsearches = sum(as.numeric(searched)), numhits = sum(anycontra))

#sqf03mod$timestop[sqf03mod$timestop >= 7:00 && sqf03mod$timestop <= 19:00] == "Day"
#sqf03mod$timestop[sqf03mod$timestop > 19:00 || sqf03mod$timestop < 7:00] == "Night"





