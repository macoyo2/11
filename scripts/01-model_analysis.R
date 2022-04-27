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


###Creating model design tables###
# build up table for the first factorial design
run <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
p <- c(5326469,770231,4848638,477831,4556238,5034069,292400,200000,
                   5574422,767006,5098539,475883,4807416,5283299,291123,210000)
r <- c("+1","+1",-1,"+1",-1,"+1",-1,-1,"+1","+1",-1,"+1",-1,"+1",-1,-1)
d <- c("+1",-1,"+1",-1,"+1","+1",-1,-1,"+1",-1,"+1",-1,"+1","+1",-1,-1)
pa <- c("+1","+1","+1",-1,-1,-1,"+1",-1,"+1","+1","+1",-1,-1,-1,"+1",-1)
fac_t <- data.frame(run,r,d,pa,p)
names(fac_t)[1] = "Run"
names(fac_t)[2] = "Registered Program"
names(fac_t)[3] = "Drop-in Program"
names(fac_t)[4] = "Permit Activity"
names(fac_t)[5] = "Participation Number"
kbl(fac_t, align = "c", caption = "$2^3$ factorial experiment design of participation methods", booktabs = T, linesep = '') |>
  kable_styling() |>
  pack_rows("Replicated Runs",9,16)
 
# build up table for the second factorial design
runs <- c(1,2,3,4,5,6,7,8)
re <- c(1742157,792074,950083,700000,
              1858141,810529,1047612,710000)
s <- c("+1",-1,"+1",-1,"+1",-1,"+1",-1)
f <- c("+1","+1",-1,-1,"+1","+1",-1,-1)
fac1_t <- data.frame(runs,s,f,re)
names(fac1_t)[1] = "Run"
names(fac1_t)[2] = "Summer/Spring"
names(fac1_t)[3] = "Winter/Fall"
names(fac1_t)[4] = "Participation Number"
kbl(fac1_t, align = "c", caption = "$2^2$ factorial experiment design of seasonal factors", booktabs = T, linesep = '') |>
  kable_styling() |>
  pack_rows("Replicated Runs",5,8)
  
###Model analysis###
# first factorial regression analysis
participation <- c(5326469,770231,4848638,477831,4556238,5034069,292400,200000,
                   5574422,767006,5098539,475883,4807416,5283299,291123,210000)
registered <- c(1,1,-1,1,-1,1,-1,-1,1,1,-1,1,-1,1,-1,-1)
drop <- c(1,-1,1,-1,1,1,-1,-1,1,-1,1,-1,1,1,-1,-1)
permit <- c(1,1,1,-1,-1,-1,1,-1,1,1,1,-1,-1,-1,1,-1)
fac <- data.frame(participation,registered,drop,permit)
setDT(fac)

fit = lm(participation~registered*drop*permit, data = fac)

# build up first factorial regression results table
coe <- c("registered","drop","permit","registerd:drop","registered:permit","drop:permit","registered:drop:permit")
es <- c(212804,2315289,120256,25625,25625,25625,-25625)
pval <- c("0.000135***","1.21e-12***","0.004850**","0.435304","0.435304","0.435304","0.435304")
es_t <- data.frame(coe,es,pval)
names(es_t)[1] = "Coefficients"
names(es_t)[2] = "Estimates"
names(es_t)[3] = "P-values"
kbl(es_t, align = "c", caption = "Summary table of factorial regression model of participation methods", booktabs = T, linesep = '') |>
  kable_styling(full_width = F) |>
  footnote(symbol = "*p < 0.1, **p < 0.05, ***p < 0.01")
  
# second factorial regression analysis
response <- c(1742157,792074,950083,700000,
              1858141,810529,1047612,710000)
ss <- c(1,-1,1,-1,1,-1,1,-1)
fw <- c(1,1,-1,-1,1,1,-1,-1)
fac1 <- data.frame(response,ss,fw)
setDT(fac1)

fit1 <- lm(response~ss*fw, data = fac1)

# build up second factorial regression results table
coe1 <- c("Summer/Spring","Fall/Winter","Summer/Spring:Fall/Winter")
es1 <- c(323174,224401,176250)
pval1 <- c("5.97e-07***","0.000302***","0.000770***")
es_t1 <- data.frame(coe1,es1,pval1)
names(es_t1)[1] = "Coefficients"
names(es_t1)[2] = "Estimates"
names(es_t1)[3] = "P-values"
kbl(es_t1, align = "c", caption = "Summary table of factorial regression model of seasons", booktabs = T, linesep = '') |>
  kable_styling(full_width = F) |>
  footnote(symbol = "*p < 0.1, **p < 0.05, ***p < 0.01")
