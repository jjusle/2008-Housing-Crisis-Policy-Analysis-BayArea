# Final Project Code 
# Justin Le
# Econ 114
# Professor Julian Martinez-Iriarte
# TA: Thinkling Li
rm(list = ls())

# Multiple sections of this file will be about cleaning and working with the data set to generate causal inference
# for the Final Project.

# Sections will be labeled properly to describe what I am doing with the data.

# Packages needed for this project
library(readxl)
library(dplyr)
library(stargazer)
library(ggplot2)
library(ggrepel)
library(gplots)


# Loading the data
# URL to dataset: https://data.ca.gov/dataset/percent-of-household-overcrowding-1-0-persons-per-room-and-severe-overcrowding-1-5-persons-per-
# This project will focus on "HCI Housing Overcrowding-Data
# Description and data dictionary included in download file

# setting working directory to open data set and export figures and tables
setwd("C:/Users/Justin/Desktop/ECON 114/Final Project")


over_crowding_data <- read_excel("overcrowding data.xlsx")
# Might take a minute or so to open the data set
# I am not too sure as to why the excel file opens up like that, with many variables/columns created.
# So I have to transfer the data into a new data frame.


# Cleaning data, creating new data frame with only the variables of interest
over_crowding_data_clean <- data.frame(over_crowding_data$reportyear,over_crowding_data$race_eth_code,
                                       over_crowding_data$race_eth_name,over_crowding_data$county_name,
                                       over_crowding_data$region_name, over_crowding_data$region_code, 
                                       over_crowding_data$numerator, over_crowding_data$denominator,
                                       over_crowding_data$estimate, over_crowding_data$CA_RR)

# Cleaning the data, removing all observations with missing values
over_crowding_data_omitted <- na.omit(over_crowding_data_clean)

# Some of the data had "NA" as a string
oc_data <- over_crowding_data_omitted[over_crowding_data_omitted$over_crowding_data.reportyear != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.race_eth_code != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.race_eth_name != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.county_name != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.region_name != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.region_code != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.numerator != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.denominator != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.estimate != "NA", ]
oc_data <- oc_data[oc_data$over_crowding_data.CA_RR != "NA", ]

# Renaming columns
colnames(oc_data)[1] = "report_year"
colnames(oc_data)[2] = "race_eth_code"
colnames(oc_data)[3] = "race_eth_name"
colnames(oc_data)[4] = "county_name"
colnames(oc_data)[5] = "region_name"
colnames(oc_data)[6] = "region_code"
colnames(oc_data)[7] = "numerator"
colnames(oc_data)[8] = "denominator"
colnames(oc_data)[9] = "estimate"
colnames(oc_data)[10] = "CA_RR"

# Turn categorical data into factors
oc_data$race_eth_code <- factor(oc_data$race_eth_code)
oc_data$region_code <- factor(oc_data$region_code)
oc_data$race_eth_name <- factor(oc_data$race_eth_name)




# Diff-In-Diff Process

# Will similarly replicate this tutorial
# https://rpubs.com/phle/r_tutorial_difference_in_differences



# Only want to look at the Bay Area, specifically Santa Clara and San Francisco
oc_data <- oc_data[oc_data$region_name == "Bay Area", ]
oc_data <- oc_data[oc_data$county_name == "Santa Clara" | oc_data$county_name == "San Francisco", ]


# Summary Statistics of Percentage of Population ages 25 and up with a 4 year college degree or higher, and California Ratio
stargazer(oc_data, type = "html", 
          title = "Table 1: Summary Statistics of Estimate Variable and California Ratio",
          out = "table1.doc")
# Removed CA_RR statistic


# Mean of estimates from both counties before, during, and after treatments
sc_0610 <- mean(oc_data[oc_data$county_name == "Santa Clara" & oc_data$report_year == "2006-2010", "estimate"])
sf_0610 <- mean(oc_data[oc_data$county_name == "San Francisco" & oc_data$report_year == "2006-2010", "estimate"])

sc_0913 <- mean(oc_data[oc_data$county_name == "Santa Clara" & oc_data$report_year == "2009-2013", "estimate"])
sf_0913 <- mean(oc_data[oc_data$county_name == "San Francisco" & oc_data$report_year == "2009-2013", "estimate"])

sc_1115 <- mean(oc_data[oc_data$county_name == "Santa Clara" & oc_data$report_year == "2011-2015", "estimate"])
sf_1115 <- mean(oc_data[oc_data$county_name == "San Francisco" & oc_data$report_year == "2011-2015", "estimate"])


# Table of estimate of both counties based on years
means_data_frame <- data.frame("Years" = c("2006-2010", "2006-2010", "2009-2013", "2009-2013", "2011-2015", "2011-2015"),
                               "County" = c("Santa Clara", "San Francisco", "Santa Clara", "San Francisco", "Santa Clara", "San Francisco"),
                               "Estimate Means" = c(sc_0610, sf_0610, sc_0913, sf_0913, sc_1115, sf_1115))

stargazer(means_data_frame,
          summary = FALSE,
          type = "html",
          title="Table 2: Summary of Estimates Based On Report Year and County",
          out = "table2.doc")


# Deducing by means of differencing the mean values of estimates between the counties.
differences <- oc_data %>%
  group_by(report_year, county_name) %>%
  summarise(estimate = mean(estimate, na.rm = TRUE))


# First differences
# Treatment group (San Francisco), before treatment
sf2006_2010 <- differences[1,3]

# Control group (Santa Clara), before treatment
sc2006_2010 <- differences[2,3]

# Treatment group (San Francisco), after treatment
sf2011_2015 <- differences[5,3]

# Control group (Santa Clara), after treatment
sc2009_2013 <- differences[4,3]


# Average Treatment Effect
diff <- (sf2011_2015-sf2006_2010)-(sc2009_2013-sc2006_2010)
diff

diff2 <- (sf2011_2015-sc2009_2013)-(sf2006_2010-sc2006_2010)
diff2

# Counterfactual outcome
sf_counterfactual <- tibble(
  report_year = c("2006-2015", "2011-2015"),
  county_name = c("San Francisco (Counterfactual)", "San Francisco (Counterfactual)"),
  estimate = as.numeric(c(sf2006_2010, sf2006_2010-(sc2006_2010-sc2009_2013)))
)

# Data points for treatment event
intervention <- tibble(
  report_year = c("Intervention", "Intervention", "Intervention"),
  county_name = c("San Francisco", "Santa Clara", "San Francisco (Counterfactual)"),
  estimate = c(6.32, 8.82, 6.66)
)

# Combine data
did_plotdata <- bind_rows(differences,
                          sf_counterfactual,
                          intervention)


# Line plot to illustrate DID
did_plotdata %>%
  mutate(label = if_else(report_year == "2011-2015", as.character(county_name), NA_character_)) %>%
  ggplot(aes(x=report_year, y=estimate, group=county_name)) +
  geom_line(aes(color=county_name), size=1.2) +
  geom_vline(xintercept = "2009-2013", linetype="dotted",
             color = "black", size=1.1) +
  scale_color_brewer(palette = "Accent") +
  scale_y_continuous(limits = c(3, 10)) +
  ggrepel::geom_label_repel(aes(label = label),
                            nudge_x = 0.5, nudge_y = -0.5,
                            na.rm = TRUE) +
  guides(color=FALSE) +
  labs(x="", y = "Estimate (mean)") +
  annotate(
    "text",
    x = "2011-2015",
    y = 6.3,
    label = "{Differences-in-Differences}",
    angle = 90,
    size = 1.5 
  )


# Creating dummy variables
oc_data <- mutate(oc_data,
                  Treat = ifelse(report_year == "2009-2013", 1, 0),
                  Post = ifelse(county_name == "San Francisco", 1, 0)
                  )


# DID estimation
did_model <- lm(estimate ~ Treat + Post + Treat:Post, data = oc_data)

# Regression Results
stargazer(did_model, type = "html", 
          title="Table 3: Regression Results", 
          out = "table3.doc",
          align=TRUE)
# I had to add <0.1 and <0.05 to the Note at the bottom for p-values

