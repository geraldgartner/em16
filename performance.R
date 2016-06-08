#Loading necessary packages
library(rvest)
library(dplyr)
library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)

spielerdaten <- read.csv("~/Google Drive/dStd.at/em16/rskripte/performanceanalysis/spielerdaten.csv")
marktwerte <- read.csv("~/Google Drive/dStd.at/em16/rskripte/performanceanalysis/marktwerte.csv")