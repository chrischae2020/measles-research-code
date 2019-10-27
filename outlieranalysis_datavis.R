#detach("package:plyr", unload=TRUE) 
#library(dplyr) 
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


#Exchange and Vacinne are same so drop Exchange from further analysis


df <- select(df, Cname, ISO_code, Region, Vaccine, Year, value)
head(df)



sapply(df, class)



df[is.na(df)] <- 0
str(df)



#how many rows are in the dataset?
nrow(df)



summary(df)



# check missing values
colSums(is.na(df))


length(unique(df$Cname))




# count unique values for each column
rapply(df,function(x)length(unique(x)))

# print unique values for each column
unique(df$Region)
unique(df$ISO_code)  # sames as Cname (or country name)
unique(df$Vaccine) 



df %>%
  group_by(Region) %>%
  summarize(n())



df %>%
  group_by(Region) %>%
  summarize(coverage_rate = mean(value, na.rm = TRUE))



df %>%
  group_by(Vaccine, Region) %>%
  summarize(coverage_rate = mean(value, na.rm = TRUE)) %>%
  arrange(Vaccine, coverage_rate)



df %>%
  group_by(Vaccine, Region, Cname) %>%
  summarize(coverage_rate = mean(value, na.rm = TRUE)) %>%
  arrange(Vaccine, Region, coverage_rate) 



df %>%
  filter(Vaccine == "DTP3") %>% 
  group_by(Region, Cname) %>%
  summarize(coverage_rate = mean(value, na.rm = TRUE)) %>%
  arrange(Region, coverage_rate) 


# Identifying Outliers: Countries with very low coverage rate


country <- df %>%
  group_by(Vaccine, Cname) %>%
  dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))

ggplot(country, aes(x = Vaccine, y = CoverageRate)) +
  geom_boxplot() +
  ggtitle("Boxplot of country coverage rate by vaccine type") +
  geom_point(aes(color=Vaccine), alpha=0.2, position='jitter') + 
  geom_boxplot(outlier.size=5, alpha=0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")


#To find out the outlier countries, let's use boxplot()

get_countryoutliers <- function(vaccine_type) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)
  
  country <- vaccine_name %>%
    group_by(ISO_code) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  outliers <- boxplot(country$CoverageRate, plot=FALSE)$out
  
  print(vaccine_type)
  print(country[country$CoverageRate %in% outliers,])
}

for (i in unique(df$Vaccine)) {
  get_countryoutliers(i)
} 




get_outliers <- function(vaccine_type) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)
  
  country <- vaccine_name %>%
    group_by(Year, ISO_code) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  out <- ggplot(country, aes(x = factor(Year), y = CoverageRate)) +
            geom_boxplot() +
            ggtitle(vaccine_type) +
            geom_point(aes(color=Year), alpha=0.2, position='jitter') + 
            geom_boxplot(outlier.size=5, alpha=0.1) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(legend.position="none")
    
  print(out)
}

for (i in unique(df$Vaccine)) {
  get_outliers(i)
} 






# Visualizing coverage rate on global maps

# Function to visualize coverage rate in a global map

create_map <- function(vaccine_type) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)
  
  country <- vaccine_name %>%
    group_by(ISO_code) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  sPDF <- joinCountryData2Map( country
                               ,joinCode = "ISO3"
                               ,nameJoinColumn = "ISO_code")
  
  mapCountryData(sPDF, 
                 nameColumnToPlot='CoverageRate',
                 mapTitle=vaccine_type)
  
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
 
}

for (i in unique(df$Vaccine)) {
  create_map(i)
} 




create_trend_barplot <- function(vaccine_type) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)

  #global trend in barplot
  global <- vaccine_name %>%
                  group_by(Year) %>%
                  dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  abc <- ggplot(global, aes(Year, CoverageRate)) + geom_bar(stat = "identity") +
          ggtitle(vaccine_type) +
          theme(plot.title = element_text(hjust = 0.5)) +
          geom_rect(aes(xmin=2008, xmax=2018, ymin=0,
          ymax=100), color="transparent", fill="orange", alpha=0.01)
  
  print(abc)
  
}

for (i in unique(df$Vaccine)) {
  create_trend_barplot(i)
} 



create_trend_barplot <- function(vaccine_type) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)

  #global trend in barplot
  global <- vaccine_name %>%
                  group_by(Year) %>%
                  dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  ## Add a column indicating whether the category should be highlighted
  X <- global %>% mutate( ToHighlight = ifelse( Year >= 2008 & Year <= 2018,
                                                "yes", "no" ) )
  
  abc <- ggplot(X, aes(Year, CoverageRate, fill = ToHighlight)) + 
            geom_bar(stat = "identity") + theme(legend.position="none") +
            ggtitle(vaccine_type) +
            theme(plot.title = element_text(hjust = 0.5)) 
  
  print(abc)
  
}

for (i in unique(df$Vaccine)) {
  create_trend_barplot(i)
} 




create_barplot_lineplot <- function(vaccine_type) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)
  
  global <- vaccine_name %>%
                  group_by(Year) %>%
                  dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  ## Add a column indicating whether the category should be highlighted
  X <- global %>% mutate( ToHighlight = ifelse( Year >= 2008 & Year <= 2018,
                                                "yes", "no" ) )
  region <- vaccine_name %>%
    group_by(Region, Year) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  abc <- ggplot() +
          geom_bar(data=X, aes(Year, CoverageRate, fill = ToHighlight), stat = "identity") +
  geom_line(data=region, aes(Year, CoverageRate, color=factor(Region), group=Region), size=1) +
              scale_y_continuous(limits = c(0, 100)) +
          theme(axis.text.x = element_text(angle = 45),legend.position="bottom") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
          ggtitle(vaccine_type) +
          theme(plot.title = element_text(hjust = 0.5)) +
          guides(fill=FALSE)
  
  print(abc)
  
}

for (i in unique(df$Vaccine)) {
  create_barplot_lineplot(i)
} 



### More outliter analysis

get_outliers <- function(vaccine_type, year) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)
  
  country <- vaccine_name %>%
    filter(Year == year) %>%
    group_by(ISO_code) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  outliers <- boxplot(country$CoverageRate, plot=FALSE)$out
  
  print(paste(vaccine_type, year, sep=" in "))
  print(country[country$CoverageRate %in% outliers,])
}

for (i in unique(df$Vaccine)) {
  for (j in unique(df$Year)) {
    get_outliers(i, j)
  }
} 



get_outliers <- function(vaccine_type, region) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)
  
  country <- vaccine_name %>%
    filter(Region == region) %>%
    group_by(Year, ISO_code) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  out <- ggplot(country, aes(x = factor(Year), y = CoverageRate)) +
    geom_boxplot() +
    ggtitle((paste(vaccine_type, region, sep=" in "))) +
    geom_point(aes(color=Year), alpha=0.2, position='jitter') + 
    geom_boxplot(outlier.size=5, alpha=0.1) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position="none")
  
  print(out)
}

for (i in unique(df$Vaccine)) {
  for (j in unique(df$Region)) {
    get_outliers(i, j)
  } 
}




get_outliers <- function(vaccine_type, region) {
  
  vaccine_name <- filter(df, Vaccine == vaccine_type)
  
  country <- vaccine_name %>%
    filter(Region == region) %>%
    group_by(Year, ISO_code) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  mybp <- boxplot(CoverageRate ~ Year, data=country, 
                  main=(paste(vaccine_type, region, sep=" in ")))
  print(country[country$CoverageRate %in% mybp$out,])
  
}

for (i in unique(df$Vaccine)) {
  for (j in unique(df$Region)) {
    get_outliers(i, j)
  } 
}