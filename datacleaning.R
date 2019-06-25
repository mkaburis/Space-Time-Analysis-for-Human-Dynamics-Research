library(dplyr)
#data cleaning
sqf03 = read.csv(file = "/Users/sam/Desktop/csv_files/sqf03.csv")
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

sqf03 = select(sqf03, 2, 4:5, 10, 24, 25, 82)
sqf04 = select(sqf04, 2, 4:5, 10, 24, 25, 82)



