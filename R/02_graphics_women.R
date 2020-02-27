# Script to analyze data of female representativity in science
# reading additional information of female researchers
# by Sara Mortara, first version 2020-02-27

## loading packages
library(ggplot2)
library(dplyr)
library(stringr)
library(wesanderson)

## color pallete for graphics
cores <-  wes_palette("Darjeeling1", 2)[2:1]
ver <- rgb(255, 0, 0, maxColorValue = 255, alpha = 150)
cin <- rgb(106, 126, 133, maxColorValue = 255)

## loading files
fa <- read.csv("outputs/female_area_2.csv")
fs <- read.csv("outputs/female_science_2.csv")

head(fa)
head(fs)

table(fa$cor)

table(fs$cor)

table(fa$continente)/nrow(fa)
table(fs$continente)/nrow(fa)
