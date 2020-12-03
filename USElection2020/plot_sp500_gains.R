## ---------------------------
##
## Script name: plot_sp500_gains.R
##
## Purpose of script: Plot S&P500 data before and after US election results
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

library(ggplot2)
df <- readxl::read_xlsx("data\\usa_electoral_votes.xlsx",sheet="Elections")

ds <- df %>%
  filter(Year > 1928) %>%
  mutate(PartyName = ifelse(Party=="D","Democrat","")) %>%
  mutate(PartyName = ifelse(Party=="R","Republican",PartyName))

ds$LastName <- as.character((lapply(strsplit(ds$Name,split=" "),tail,1)))

ggplot(ds,aes(factor(Year),SP500_GAIN_PERCENT,fill=PartyName)) +
  geom_col() +
  scale_fill_manual(name="",values = setNames(c('blue','red'),c("Democrat", "Republican"))) +
  coord_flip() +
  ylim(-5.5,5.5) + 
  geom_text(aes(label = Name),y=2.2,hjust=0) +
  geom_text(aes(label = Difference, y=5.2, hjust=0)) +
  theme(legend.position="top",
        plot.title=element_text(size=12, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=8, hjust=1, face="italic", color="grey")) +
  labs(x="Year",
       y="S&P500 percentage gain (relative to previous day)",
       title="S&P500 Index Percentage Gain After US Presidential Election",
       subtitle="Winners and Electoral Vote Margin shown at right",
       caption="www.linkedin.com/in/evassiliadis")

                     
ggsave("images\\sp500_enchanced.png")
