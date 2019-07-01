library(dplyr)
library(plyr)
#library(tidyr)
#loading in files
sqf03 = read.csv(file = "/Users/sam/Desktop/csv_files/2003.csv", stringsAsFactors = F)
sqf04 = read.csv(file = "/Users/sam/Desktop/csv_files/2004.csv", stringsAsFactors = F)
sqf05 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf05.csv", stringsAsFactors = F)
sqf06 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf06.csv", stringsAsFactors = F)
sqf07 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf07.csv", stringsAsFactors = F)
sqf08 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf08.csv", stringsAsFactors = F)
sqf09 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf09.csv", stringsAsFactors = F)
sqf10 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf10.csv", stringsAsFactors = F)
sqf11 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf11.csv", stringsAsFactors = F)
sqf12 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf12.csv", stringsAsFactors = F)
sqf13 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf13.csv", stringsAsFactors = F)
sqf14 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf14.csv", stringsAsFactors = F)
sqf15 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf15.csv", stringsAsFactors = F)
sqf16 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf16.csv", stringsAsFactors = F)
sqf17 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf17.csv", stringsAsFactors = F)
sqf18 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf18.csv", stringsAsFactors = F)


sqf031 = sqf03
sqf041 = sqf04
sqf051 = sqf05
sqf061 = sqf06
sqf071 = sqf07
sqf081 = sqf08
sqf091 = sqf09
sqf101 = sqf10
sqf111 = sqf11
sqf121 = sqf12
sqf131 = sqf13
sqf141 = sqf14
sqf151 = sqf15
sqf16 = sqf16
sqf171 = sqf17
sqf181 = sqf18

data = list(sqf03, sqf04, sqf05,sqf06,sqf07,sqf08,sqf09,sqf10,sqf11,sqf12,sqf13,sqf14,sqf15,sqf16)

practice = function(dataset)
{
dataset = select(dataset, pct, searched, contrabn, pistol, riflshot, asltweap, knifcuti, 
       machgun, othrweap, race) %>%
mutate(contraband_flag = if_else((dataset$contrabn == "Y" | dataset$pistol == "Y" |
                                    dataset$riflshot == "Y" | dataset$asltweap == "Y" | 
                                    dataset$knifcuti == "Y" | dataset$machgun == "Y" | 
                                    dataset$othrweap == "Y"), 
                                   as.integer(1), as.integer(0))) %>%
  mutate(searched = if_else((dataset$searched == "N"), as.integer(0), as.integer(1)))
  dataset$race[dataset$race == "P"] = "H"
  dataset$race[dataset$race == "Q"] = "H"
  dataset = dataset %>%
    select(pct, searched, contraband_flag, race) %>%
    filter(race != "A") %>%
    filter(race != "I") %>%
    filter(race != "Z") %>%
    filter(race != "X") %>%
    filter(race != " ") %>%
    group_by(pct, race) %>%
    dplyr::summarize(numstops = length(searched), numsearches = sum(as.numeric(searched)), 
              numhits = sum(as.numeric(contraband_flag)),
              searchrate = numsearches/numstops, hitrate = numhits/numstops)
}

data = lapply(data, practice)
sqf031 = data[[1]]
sqf041 = data[[2]]
sqf051 = data[[3]]
sqf061 = data[[4]]
sqf071 = data[[5]]
sqf081 = data[[6]]
sqf091 = data[[7]]
sqf101 = data[[8]]
sqf111 = data[[9]]
sqf121 = data[[10]]
sqf131 = data[[11]]
sqf141 = data[[12]]
sqf151 = data[[13]]
sqf161 = data[[14]]


# sqf03mod = sqf03
# 
# # Precinct, Searched, Race, Create Col. Merge Contraband Columns
# sqf03mod = sqf03mod %>%
#   select(pct, searched, contrabn, pistol, riflshot, asltweap, knifcuti, machgun, othrweap, race) %>%
#   mutate(contraband_flag = if_else((sqf03mod$contrabn == "Y" | sqf03mod$pistol == "Y" |
#                                       sqf03mod$riflshot == "Y" | sqf03mod$asltweap == "Y" | 
#                                       sqf03mod$knifcuti == "Y" | sqf03mod$machgun == "Y" | 
#                                       sqf03mod$othrweap == "Y"), T, F))
# 
# #turning searched and contraband columns into 0s and 1s to add them
# sqf03mod$searched[sqf03mod$searched == "N" ] = as.integer(0)
# sqf03mod$searched[sqf03mod$searched == "Y" ] = as.integer(1)
# sqf03mod$contraband_flag[sqf03mod$contraband_flag == F ] = as.integer(0)
# sqf03mod$contraband_flag[sqf03mod$contraband_flag == T ] = as.integer(1)
# 
# #grouping black hispanic and white hispanic as hispanic
# sqf03mod$race[sqf03mod$race == "P"] = "H"
# sqf03mod$race[sqf03mod$race == "Q"] = "H"
# 
# #finding total stops, searches, and hits per race per precinct
# #formatting 2003 data into the format of the NC data. removing extraneous races 
# sqf03mod = sqf03mod %>% 
#   select(pct, searched, contraband_flag, race) %>%
#   filter(race != "A") %>%
#   filter(race != "I") %>%
#   filter(race != "Z") %>%
#   filter(race != "X") %>%
#   filter(race != " ") %>% 
#   group_by(pct, race) %>%
#   dplyr::summarize(numstops = length(searched), 
#             numsearches = sum(as.numeric(searched)), numhits = sum(contraband_flag),
#             searchrate = numsearches/numstops, hitrate = numhits/numstops)
