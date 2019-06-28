# Designed by Mihail Kaburis, Samantha Kamath, and Jasmine Medlock
# NJIT/HU REU Summer 2019

# Use dplyr library
library(dplyr)
library(plyr)

df = read.csv(file='~/Documents/SQF_Data/sqf-2016.csv', sep=',', header=T, stringsAsFactors = F)
# Precinct, Searched, Race, Create Col. Merge Contraband Columns
dfmod1 = df %>%
  select(pct, searched, contrabn, pistol, riflshot, asltweap, knifcuti, machgun, othrweap, race)

# Create a contraband_flag column based off of contraband found on searched suspect
dfmod2 = dfmod1 %>%
  mutate(contraband_flag = if_else((dfmod$contrabn == "Y" | dfmod$pistol == "Y" |
                                dfmod$riflshot == "Y" | dfmod$asltweap == "Y" | dfmod$knifcuti == "Y" |
                                dfmod$machgun == "Y" | dfmod$othrweap == "Y"), T, F))

# Create a new table that only shows pct, searched, race, contraband_flag
dfmodcontraband = dfmod2 %>%
  select(pct, searched, race, contraband_flag)

# iterate through the table
for(i in 1:nrow(dfmod)) {
  row <- dfmod[i, ]
  print(row)
}


