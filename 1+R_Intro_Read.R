# ---R packages
library(readxl)
library(haven)  
library(tidyverse)
library(expss)
library(gtsummary)
library(gt)
library(cards)
library(psych)

#--Import data ---from ---various sources--Excel, Stata, SPSS, CSv

A1 <- read_excel('P:/abc/data2.xlsx')      #--Excel--

A2 <- read_dta('P:/abc/data2.dta')         #---Stata

A3 <- read_sav('P:/abc/data2.sav')         #---SPSS------

A4 <- read.csv('P:/abc/data2.csv')         #---CSV------