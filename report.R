library(rworldmap)
library(ggplot2)
library(tidyverse)
library(scales)

setwd("C:/Users/flare/Documents/Research")
getwd()

df <- read.csv(file="finaldata.csv", header=TRUE, sep="," )

# Correct the first column name as Cname
colnames(df)[1] <- "Cname"
head(df)
df <- select(df, Cname, ISO_code, Region, Vaccine, Year, value)
head(df)

#Graph DTP3 from 1980-2017 by WHO region
ggplot() +
  geom_bar(data=global, aes(Year, CoverageRate), stat = "identity") +
  geom_line(data=region, aes(Year, CoverageRate, color=factor(Region), group=Region), size=2) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45),legend.position="bottom") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot() +
  geom_bar(data=X, aes(Year, CoverageRate, fill = ToHighlight), stat = "identity") +
  geom_line(data=region, aes(Year, CoverageRate, color=factor(Region), group=Region), size=1) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45),legend.position="bottom") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 







