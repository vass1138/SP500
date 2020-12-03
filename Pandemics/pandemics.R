## ---------------------------
##
## Script name: pandemics.R
##
## Purpose of script: Explore S&P500 against pandemics break outs
##
##
## Author: Dr. Emanuel Vassiliadis
##
## Date Created: 2020-12-03
##
## LinkedIn: https://www.linkedin.com/in/evassiliadis/
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

rm(list = ls(all = TRUE))

library(here)
getwd()

library(tidyverse)

install_this_library <- function(libname) {

  if (!(libname %in% installed.packages())) {
    install.packages(libname)
  } else {
    print(paste0(libname," already installed"))
  }
}

install_this_library("rvest")
library(tidyverse)
library(lubridate)

library(ggplot2)

get_data <- function(data,start_date,year_range,label) {
  
  d1 <- ymd(start_date)
  # d2 <- as.Date(end_date)
  d2 <- d1 + years(year_range)
  dm <- d1 + years(year_range/2)

  # filter by date range
  df <- data %>%
    filter(between(Date,d1,d2)) %>%
    mutate(Age=as.numeric(Date-dm,unit="days"))
  
  # find row index closest to midpoint date
  idx <- as.numeric(which.min(abs(df$Date-dm)))
  
  # normalise closing price to midpoint
  df <- df %>%
    mutate(NClose=Close/Close[idx]) 
  
  # determine year for labelling facets
  zero_year <- year(df$Date[idx])
  
  df <- df %>%
    mutate(Src=label) %>%
    mutate(Facet.Title=paste0(zero_year,": ",label))
  
  return(df)
}

df <- readxl::read_xlsx("sp500_historical.xlsx",sheet="^GSPC")

df$Date <- as.Date(df$Date)

events <- c("SARS-CoV-1", "H1N1", "Ebola", "SARS-CoV-2")

d1 <- get_data(df,"2002-02-01",2,"SARS-CoV-1")
d2 <- get_data(df,"2008-02-01",2,"H1N1")
d3 <- get_data(df,"2013-03-01",2,"Ebola")
d4 <- get_data(df,"2019-03-01",2,"SARS-CoV-2")

d0 <- data.frame()
d0 <- rbind(d1,d0)
d0 <- rbind(d2,d0)
d0 <- rbind(d3,d0)
d0 <- rbind(d4,d0)

d0$Facet.Title <- as.factor(d0$Facet.Title)

# plot each event, add offset
d0 %>%
  ggplot(aes(Age,NClose)) +
  geom_line() +
  xlim(-405,405) +
  facet_wrap(~ Facet.Title) +
  theme(legend.position="none",
        plot.title=element_text(size=12, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=8, hjust=1, face="italic", color="grey")) +
  labs(x="Days Since Outbreak",
       y="Normalised Close Price",
       title="S&P500 Epidemic/Pandemic Performance",
       subtitle="",
       caption="www.linkedin.com/in/evassiliadis")

ggsave("sp500_pandemic.png",width=8,height=6)
  