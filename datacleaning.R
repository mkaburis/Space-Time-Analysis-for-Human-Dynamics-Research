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

#creating lists of older years and newer years
data = list(sqf03, sqf04, sqf05,sqf06,sqf07,sqf08,sqf09,sqf10,sqf11,sqf12,sqf13,sqf14,sqf15,sqf16)
data2 = list(sqf17, sqf18)

#cleaning function for older years
clean = function(dataset)
{
  #Precinct, Searched, Race, Create Col. Merge Contraband Columns
  dataset = select(dataset, pct, searched, contrabn, pistol, riflshot, asltweap, knifcuti, 
       machgun, othrweap, race) %>%
  #turning searched and contraband columns into 0s and 1s to add them
  mutate(contraband_flag = if_else((dataset$contrabn == "Y" | dataset$pistol == "Y" |
                                    dataset$riflshot == "Y" | dataset$asltweap == "Y" | 
                                    dataset$knifcuti == "Y" | dataset$machgun == "Y" | 
                                    dataset$othrweap == "Y"), 
                                   as.integer(1), as.integer(0))) %>%
  mutate(searched = if_else((dataset$searched == "N"), as.integer(0), as.integer(1)))
  #grouping black hispanic and white hispanic as hispanic
  dataset$race[dataset$race == "P"] = "H"
  dataset$race[dataset$race == "Q"] = "H"
  #formatting 2003 data into the format of the NC data. removing extraneous races 
  dataset = dataset %>%
    select(pct, searched, contraband_flag, race) %>%
    filter(race != "A") %>%
    filter(race != "I") %>%
    filter(race != "Z") %>%
    filter(race != "X") %>%
    filter(race != " ") %>%
    filter(pct <= 123) %>%
    group_by(pct, race) %>%
    #finding total stops, searches, and hits per race per precinct
    dplyr::summarize(numstops = length(searched), numsearches = sum(as.numeric(searched)), 
              numhits = sum(as.numeric(contraband_flag)),
              searchrate = numsearches/numstops, hitrate = numhits/numstops)
    na.omit(dataset)
}

#cleaning function for newer years
clean2 = function(dataset)
{
  #Precinct, Searched, Race, Create Col. Merge Contraband Columns
  dataset = select(dataset, STOP_LOCATION_PRECINCT, SEARCHED_FLAG, OTHER_CONTRABAND_FLAG, 
                   WEAPON_FOUND_FLAG, SUSPECT_RACE_DESCRIPTION) 
    names(dataset)[1] = "pct"
    names(dataset)[2] = "searched"
    names(dataset)[5] = "race"
  #turning searched and contraband columns into 0s and 1s to add them
  dataset = dataset %>%
    mutate(contraband_flag = if_else((dataset$OTHER_CONTRABAND_FLAG == "Y" | 
                                        dataset$WEAPON_FOUND_FLAG == "Y"), 
                                     as.integer(1), as.integer(0))) %>%
    mutate(searched = if_else((dataset$searched == "N"), as.integer(0), as.integer(1)))
  #grouping black hispanic and white hispanic as hispanic. renaming races
  dataset$race[dataset$race == "BLACK HISPANIC"] = "H"
  dataset$race[dataset$race == "WHITE HISPANIC"] = "H"
  dataset$race[dataset$race == "WHITE"] = "W"
  dataset$race[dataset$race == "BLACK"] = "B"
  #formatting 2003 data into the format of the NC data. removing extraneous races 
  dataset = dataset %>%
    select(pct, searched, contraband_flag, race) %>%
    filter(race != "ASIAN/PAC.ISL") %>%
    filter(race != "ASIAN / PACIFIC ISLANDER") %>%
    filter(race != "AMERICAN INDIAN/ALASKAN NATIVE") %>%
    filter(race != "AMER IND") %>%
    filter(race != "(null)") %>%
    filter(pct <= 123) %>%
    group_by(pct, race) %>%
    #finding total stops, searches, and hits per race per precinct
    dplyr::summarize(numstops = length(searched), numsearches = sum(as.numeric(searched)), 
                     numhits = sum(as.numeric(contraband_flag)),
                     searchrate = numsearches/numstops, hitrate = numhits/numstops)
    na.omit(dataset)
}

#applying functions to older and newer years and extracting the info
data = lapply(data, clean)
data2 = lapply(data2, clean2)
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
sqf171 = data2[[1]]
sqf181 = data2[[2]]
