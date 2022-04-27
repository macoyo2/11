### Preamble ###
# Purpose: Clean the Daily Shelter occupancy data downloaded from Toronto Open Data
# Author: 
  - Yitian Li 
# Date: April 27 2022
# Pre-req: None

### Workspace Set-Up ###
# install.packages("tidyverse")
# install.packages("kableExtra")
# install.packages("ggpubr")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("FrF2")
library(tidyverse)
library(kableExtra)
library(ggpubr)
library(ggplot2)
library(data.table)
library(FrF2)


### Import the dataset ###
raw_data <-read.csv("inputs/data/sports.csv")

### Clean data ###
# registered program enrollment data frame
reg <- raw_data[2:7, 1:5]
names(reg)[1] = "Year"
names(reg)[2] = "Winter"
names(reg)[3] = "Spring"
names(reg)[4] = "Summer"
names(reg)[5] = "Fall"

# drop-in program usage data frame
dr <- raw_data[12:17, 1:5]
names(dr)[1] = "Year"
names(dr)[2] = "Winter"
names(dr)[3] = "Spring"
names(dr)[4] = "Summer"
names(dr)[5] = "Fall"

# permit activity data frame
per <- raw_data[22:27, 1:5]
names(per)[1] = "Year"
names(per)[2] = "Winter"
names(per)[3] = "Spring"
names(per)[4] = "Summer"
names(per)[5] = "Fall"

# combined data frame
res <- raw_data[2:28, 1:5]
res <- res[-c(8,9,10,18,19,20), ]
names(res)[1] = "Year"
names(res)[2] = "Winter"
names(res)[3] = "Spring"
names(res)[4] = "Summer"
names(res)[5] = "Fall"
res <- as.data.frame(sapply(res,as.numeric))
res$Total = rowSums(res[c(2,3,4,5)])
res <- na.omit(res)
res <-
  res |>
  mutate(`Summer/Spring Percentage` = round((Spring + Summer)/Total*100, 2)) |>
  mutate(`Fall/Winter Percentage` = round((Winter + Fall)/Total*100, 2))
rownames(res) <- NULL

###Plots and tables###
# data summary table
kbl(res, align = "c", caption = "Sports and recreational activities participation numbers in four seasons from 2009 to 2014", booktabs = T, linesep = '') |>
  kable_styling() |>
  pack_rows("Registered Program Enrollment",1,6) |>
  pack_rows("Attendance at Drop-in Programs",7,12) |>
  pack_rows("Permit Activity - Number of Bookings",13,18)
  
 # histogram plots for registered program in four seasons
f1 <- 
  reg |>
  ggplot(aes(x=Year, y=Winter)) +
  geom_bar(stat="identity", fill="lightblue") +
  theme_classic() +
  labs(x = "Year", 
       y = "Erollment Number")

f2 <- 
  reg |>
  ggplot(aes(x=Year, y=Spring)) +
  geom_bar(stat="identity", fill="darkgreen") +
  theme_classic() +
  labs(x = "Year", 
       y = "Erollment Number")

f3 <- 
  reg |>
  ggplot(aes(x=Year, y=Summer)) +
  geom_bar(stat="identity", fill="darkred") +
  theme_classic() +
  labs(x = "Year", 
       y = "Enrollment Number")

f4 <- 
  reg |>
  ggplot(aes(x=Year, y=Fall)) +
  geom_bar(stat="identity", fill="orange") +
  theme_classic() +
  labs(x = "Year", 
       y = "Erollment Number")

figure <- ggarrange(f1, f2, f3, f4,
                    labels = c("Winter", "Spring", "Summer", "Fall"),
                    ncol = 2, nrow = 2)
figure

# histogram plots for drop-in program in four seasons
f5 <- 
  dr |>
  ggplot(aes(x=Year, y=Winter)) +
  geom_bar(stat="identity", fill="lightblue") +
  theme_classic() +
  labs(x = "Year", 
       y = "Attendance")

f6 <- 
  dr |>
  ggplot(aes(x=Year, y=Spring)) +
  geom_bar(stat="identity", fill="darkgreen") +
  theme_classic() +
  labs(x = "Year", 
       y = "Attendance")

f7 <- 
  dr |>
  ggplot(aes(x=Year, y=Summer)) +
  geom_bar(stat="identity", fill="darkred") +
  theme_classic() +
  labs(x = "Year", 
       y = "Attendance")

f8 <- 
  dr |>
  ggplot(aes(x=Year, y=Fall)) +
  geom_bar(stat="identity", fill="orange") +
  theme_classic() +
  labs(x = "Year", 
       y = "Attendance")

figure1 <- ggarrange(f5, f6, f7, f8,
                    labels = c("Winter", "Spring", "Summer", "Fall"),
                    ncol = 2, nrow = 2)
figure1

# histogram plots for permit activity in four seasons
f9 <- 
  per |>
  ggplot(aes(x=Year, y=Winter)) +
  geom_bar(stat="identity", fill="lightblue") +
  theme_classic() +
  labs(x = "Year", 
       y = "Number of bookings")

f10 <- 
  per |>
  ggplot(aes(x=Year, y=Spring)) +
  geom_bar(stat="identity", fill="darkgreen") +
  theme_classic() +
  labs(x = "Year", 
       y = "Number of bookings")

f11 <- 
  per |>
  ggplot(aes(x=Year, y=Summer)) +
  geom_bar(stat="identity", fill="darkred") +
  theme_classic() +
  labs(x = "Year", 
       y = "Number of bookings")

f12 <- 
  per |>
  ggplot(aes(x=Year, y=Fall)) +
  geom_bar(stat="identity", fill="orange") +
  theme_classic() +
  labs(x = "Year", 
       y = "Number of bookings")

figure2 <- ggarrange(f9, f10, f11, f12,
                    labels = c("Winter", "Spring", "Summer", "Fall"),
                    ncol = 2, nrow = 2)
figure2
