# Bevan Vollenhoven (3840572) 
# PCA of World Health Organization data on progress towards attaining SDGs
# 26 July 2021 

# The worksheet deals with the method and interpretation of World Health Organization
# data on progress towards attaining SDG's. The agenda for sustainable development and lists 
# 17 development goals to achieve by 2030 called the Sustainable Development Goals (SDGs).
# Indicators (or measures) are collected and used to track the progress towards the
# ultimate goal: "a world free of poverty, hunger, disease and want". 

library(tidyverse)
library(vegan)
library(missMDA) # to impute missing values
library(ggcorrplot) # for the correlations

# 1. Code interpretations ----------
# Define and load the data ------------

# SDG 1.a Domestic general government health expenditure (GGHE-D) as percentage of general government expenditure 
# (GGE) (%)-as-percentage-of-general-government-expenditure-(gge)). 
SDG1.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG1.a_domestic_health_expenditure.csv") %>% # loads data into R studio from a url. 
  filter(Period == 2016) %>% # Only retain data that occurred in the year 2016. 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>% # Retain only the variables stipualted. 
  mutate(SDG = "SDG1.a") # Create a new column labeled SDG with rows all having the label specified in "". 
# With the above changes made to the data set, we are able to view our desired
# variables for oyr desired time period. 

# SDG 3.1 Maternal mortality ratio (per 100 000 live births))
SDG3.1_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_maternal_mort.csv") %>% 
  filter(Period == 2016,
         Indicator == "Maternal mortality ratio (per 100 000 live births)") %>% # Only filter for the rows in the indicator variable that conatin the data in "". 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>% # Retains data from these desired varibales. 
  mutate(SDG = "SDG3.1_1") # Create a new column labed SDG with rows all having the label specified in "". 
# With the modifactions made to the dataset, we are retain the data regarding our 
# desired variables regarding Maternal mortality ratio (per 100 000 live births) for the year 2016. 

# SDG 3.1 Births attended by skilled health personnel (%))
SDG3.1_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_skilled_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_2") # Create a new column labed SDG with rows all having the label specified in "".
# With the above changes made to the data set, we are able to view our desired
# variables for oyr desired time period. 

# SDG 3.2 Number of neonatal deaths (Child mortality)
SDG3.2_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_neonatal_deaths.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>% # Only keep the rows that conatin "Both sexes" from the variable Dim1. 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_1") # Create a new column labed SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables for both sexes in 2016. 

# SDG 3.2 Number of under-five deaths (Child mortality)
SDG3.2_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_under_5_deaths.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>% # Only keep the rows that contain "Both sexes" from the variable Dim1. 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_2") # Create a new column labeled SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables for both sexes in 2016. 

# SDG 3.2 Number of infant deaths (Child mortality)
SDG3.2_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_infant_deaths.csv") %>% 
  filter(Period == 2016,
         Dim1 == "Both sexes") %>% # Only keep the rows that contain "Both sexes" from the variable Dim1. 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_3") # # Create a new column labeled SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables for both sexes in 2016. 

# SDG 3.3 New HIV infections (per 1000 uninfected population))
SDG3.3_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_new_HIV_infections.csv") %>% 
  filter(Period == 2015, #retains only the rows which contain information from 2015. 
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_1") # # Create a new column labed SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables for both sexes in 2015. 

# SDG 3.3 Incidence of tuberculosis (per 100 000 population per year))
SDG3.3_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_TB.csv") %>% 
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_2") # # Create a new column labed SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables in 2016. 

# SDG 3.3 Malaria incidence (per 1 000 population at risk))
SDG3.3_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_malaria.csv") %>% 
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_3") # # Create a new column labed SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables in 2016. 

# SDG 3.3 Hepatitis B surface antigen (HBsAg) prevalence among children under 5 years-prevalence-among-children-under-5-years)
SDG3.3_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_hepatitis_B.csv") %>% 
  filter(Period == 2015) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_4") # # Create a new column labed SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables in 2015. 

# SDG 3.3 Reported number of people requiring interventions against NTDs
SDG3.3_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_NCD_interventions.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_5") # # Create a new column labed SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables in 2016. 

# SDG 3.4 Adult mortality rate (probability of dying between 15 and 60 years per 1000 population))
SDG3.4_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_adult_death_prob.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_1") # # Create a new column labed SDG with rows all having the label specified in "".
# We are no able to have the data set contain only the information for the desired 
# variables in 2016, with both sexes being taken into consideration. 

# SDG 3.4 Number of deaths attributed to non-communicable diseases, by type of disease and sex
SDG3.4_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes", # Retain rows which refer to both sexes in Dim1.
         Dim2 == "Diabetes mellitus") %>% #Retain rows which refer to individuals with Diabetes  mellitus in Dim2. 
  mutate(Indicator = Dim2) %>% # Creates a column that named Indicator that contains Dim2 in all rows.
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_2") # Create a new column labeled SDG with rows all having the label specified in "".
# Data shows only includes data from desired variables and rows for the year 2016. 

SDG3.4_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes", # Retains rows which contain the information regarding both sexes in variable Dim1
         Dim2 == "Cardiovascular diseases") %>% # retains rows that contain information regarding Cardiovascular diseases in variables dim2.
  mutate(Indicator = Dim2) %>% # creates a new column labelled "Indicator" and all the rows are labelled as Dim2. 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_3") # Create a new column labelled SDG with rows all having the label specified in "".
# Data only shows desired variables and rows from the year 2016.  

SDG3.4_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Respiratory diseases") %>% # retains rows that contain information regarding Respiratory diseases in variables dim2.
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_4")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.4 Crude suicide rates (per 100 000 population) (SDG 3.4.2))
SDG3.4_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_suicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_5")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG3.4 Total NCD Deaths (in thousands)
SDG3.4_6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_data_total.csv") %>% 
filter(Period == 2016,
       Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_6")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.5 Alcohol, total per capita (15+) consumption (in litres of pure alcohol) (SDG Indicator 3.5.2)-alcohol-per-capita-(15-)-consumption)
SDG3.5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.5_alcohol_consumption.csv") %>% 
filter(Period == 2015,
       Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.5")
# Data set is made up of data from the year 2015, and is made up of desired variables and rows. 

# SDG 3.6 Estimated road traffic death rate (per 100 000 population))
SDG3.6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.6_traffic_deaths_prop.csv") %>% 
filter(Period == 2016,
       Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.6")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.7 Adolescent birth rate (per 1000 women aged 15-19 years))
SDG3.7 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.7_adolescent_births.csv")%>% 
filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.7")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.8 UHC Index of service coverage (SCI)
SDG3.8_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.8_UHC_data_availability.csv") %>% 
filter(Period == "2013-2017") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_1")
# Data set is made up of data from the years 2013-2017, and is made up of desired variables and rows. 

# SDG 3.8 Data availability for UHC index of essential service coverage (%))
SDG3.8_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.8_UHC_index_of_service_coverage.csv") %>% 
  filter(Period == 2017) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_2")
# Data set is made up of data from the year 2017, and is made up of desired variables and rows. 

# SDG 3.9 Poison control and unintentional poisoning
SDG3.9_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_unintentional_poisoning_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_1")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.9 Mortality rate attributed to exposure to unsafe WASH services (per 100 000 population) (SDG 3.9.2)-(sdg-3-9-2))
SDG3.9_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_WASH_mortalities.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_3")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 16.1 Estimates of rate of homicides (per 100 000 population)
SDG16.1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG16.1_homicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG16.1")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 
 
# SDG 3.a Prevalence of current tobacco use among persons aged 15 years and older (age-standardized rate)
SDG3.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.a_tobacco_control.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.a")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.b Total net official development assistance to medical research and basic health sectors per capita (US$), by recipient country
SDG3.b_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_dev_assistence_for_med_research.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_1")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.b Measles-containing-vaccine second-dose (MCV2) immunization coverage by the nationally recommended age (%)-immunization-coverage-by-the-nationally-recommended-age-(-))
SDG3.b_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_measles_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_2")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.b Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
SDG3.b_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_diphtheria_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_3")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.b Pneumococcal conjugate vaccines (PCV3) immunization coverage among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
SDG3.b_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_pneumococcal_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_4")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.b Girls aged 15 years old that received the recommended doses of HPV vaccine
SDG3.b_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_HPV_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_5")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.c SDG Target 3.c | Health workforce: Substantially increase health financing and the recruitment, development, training and retention of the health workforce in developing countries, especially in least developed countries and small island developing States
SDG3.c_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Medical doctors (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_1")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

SDG3.c_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Nursing and midwifery personnel (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_2")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

SDG3.c_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Dentists (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_3")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

SDG3.c_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Pharmacists  (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_4")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# SDG 3.d Average of 13 International Health Regulations core capacity scores, SPAR version
SDG3.d_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.d_health_risks.csv")  %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.d_1")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# Other Life expectancy at birth (years))
other_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015, # retains only rows from 2015. 
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at birth (years)") %>% # Rertains only rows which have "Life expectancy at birth (years)" as an indicator. 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_1")
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 

# Other Life expectancy at age 60 (years))
other_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at age 60 (years)") %>% # Retains row which have "Life expectancy at age 60 (years)" as an Indicator. 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_2") 
# Data set is made up of data from the year 2016, and is made up of desired variables and rows. 
# It shows the life excpeatncy that the current population aged 60 has. 

# rbind the data ------------

health <- do.call("rbind", lapply(ls(),get)) # do.call carries out a function call from a function or list arguements given to it. 
                                             # rbind is the method that will be used to conbine the rows into their similar columns. 
# all loaded and adjusted data frames will be combines and sorted based on their shared columns. 
# For th this to work, each dataframe must have the same number of columns. 
head(health)

# Create list of SDGs used
list <- unique(health[, c(5, 1)]) # the unique function extracts (removing duplicates)
# in the [] braces, it is specified that only the variables in column 5 and 1 must be retained. 
# With this method, a list of all the SDGs can be created. 
head(list)

# Pivot wider -------------

health_wide <- health %>%
  arrange(Location) %>% #arranges the data alphabetically based on location.
  select(-Indicator) %>% # the Indicator column is removed
  pivot_wider(names_from = SDG, values_from = FactValueNumeric) %>% 
  as_tibble() # data frame is made into a table format. 
# pivtor_wider increased the number of columns while decreasing the number of 
# rows, making the data wider. It separates the information from one column into
# their own respective columns in order to make available hidden numerical values. 
# names from SDG are extracted and made into columns, and their corresponding values for 
# FactValueNumeric are placed accordingly.
head(health_wide)
dim(health) # has more rows but less columns. 
dim(health_wide) #has more columns but less rows

# Add world population data
popl <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_population.csv") %>%
  filter(Year == 2016) %>% # retain only the population data for the year 2016. 
  rename(popl_size = `Population (in thousands) total`, #renames column `Population (in thousand) total` to popl_size.
         Location = Country) %>% # renames Country into Location to match health_wide data set. 
  select(Location, popl_size) %>% # retain only columns pol_size and Location
  mutate(popl_size = as.numeric(gsub("[[:space:]]", "", popl_size)) * 1000) # creates a new column named popl_size where population size is made into a numerical value. 

health_wide <- health_wide %>%
  left_join(popl) #joins population data per country with health_wide data. 
head(health_wide)
glimpse(health_wide)
# data sets are joined based on variable column they have in common (Location)

health_wide <- health_wide %>%
  mutate(SDG3.4_4 = SDG3.4_4 / popl_size * 100000,
         SDG3.4_3 = SDG3.4_3 / popl_size * 100000,
         SDG3.4_2 = SDG3.4_2 / popl_size * 100000,
         SDG3.4_6 = SDG3.4_6 / 100,
         SDG3.2_2 = SDG3.2_2 / popl_size * 100000,
         SDG3.2_3 = SDG3.2_3 / popl_size * 100000,
         SDG3.2_1 = SDG3.2_1 / popl_size * 100000)

# certain columns are standardised in order to have all variables expressed as a unit of population size. 

# Histograms of missing values, and correlations --------------

health_wide$na_count <- apply(health_wide[, 3:(ncol(health_wide) - 1)], 1, 
                              function(x) sum(is.na(x)))
# na_column is made by which sums up all the NA values within a specific row. 
hist(health_wide$na_count, breaks = 14, plot = TRUE) 
# creates a histogramn comparing the the frequency of the NA values at equal frequencies divided into 14 bins
# However, suing the bin = ... function, the hist arguement will still inist on using
# "pretty" values for boundries to best fit the histogram based on available values. 
view(health_wide$na_count)

# remove rows where there are more than 10 NAs
health_wide <- health_wide %>%
  filter(na_count <= 10) %>%
  select(-na_count)

# calculate pairwise correlations
corr <- round(cor(health_wide[, 3:(ncol(health_wide) - 1)]), 1)
view(corr)

# visualization of the correlation matrix
ggcorrplot(corr, type = 'upper', outline.col = "grey60", # type species that the upper values (above the linear line) must be plotted.
                                                         # aesthetics are set: the values are outlines in grey
           colors = c("#1679a1", "white", "#f8766d"), # the colours used for the gradient legend is set. 
           lab = TRUE) # labels must be shown. 

# Imoute remaining NA values ------
health_wide_complete <- imputePCA(health_wide[, 3:(ncol(health_wide) - 1)])$completeObs 
# Missing values are imputed into the dataset that will be used in the PCA.

# Scale and center the data and do the PCA ------
health_wide_complete_std <- decostand(health_wide_complete, method = "standardize") # the data is standardsied. 
health_pca <- rda(health_wide_complete_std) # pca is calculated 
health_pca # Information on intertia, eigenvalues for each ordinations can be seen. 
# Through the 
summary(health_pca) #provides more information such as species scores and site scores. 
# Importance component can also be viewed> It shows the proportion of variation 
# represented in each ordination. Ordinations have an increasingly lower eigenvalue 
# the more ordination are constructed. 

# Graphical displays ---------
par(mfrow = c(1, 2))
biplot(health_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2)) # a biplot is constructed by using scaling 1 (relationships between "sites" are shown). 
# The chosen ordination include PC1 and PC2
biplot(health_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2)) # a biplot is constructed by using scaling 2 (relationships between "species" are shown)
# The chosen ordinations are PC1 and PC2 which hold the bulk of the information of variation. 

# the following code adds more aesthetic detail to the biplots, allowing for easier interpretations. 
pl1 <- ordiplot(health_pca, type = "none", scaling = 1, main = "PCA WHO/SDG scaling 1") # main = title. 
points(pl1, "sites", pch = 21, cex = 1.0, col = "grey20", bg = "grey80") # the points showing  "sites" are given a set of aesthetics. 
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE) # the points representing "species" are given a set of aesthetics. 
text(pl1, "species", col = "blue4", cex = 0.9) # test regarding "species" are given a specific aesthetic
text(pl1, "sites", col = "red4", cex = 0.9)# test regarding "sites" are given a specfic aesthetic

pl2 <- ordiplot(health_pca, type = "none", scaling = 2, main = "PCA WHO/SDG scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9) 


# Ordination plot with the use of ggplot -------
site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                      Location = health_wide$Location) # site_scores are made into table data and
                                                       # columns parent location and Location is added. 
# Parent location is gotten from ParentLocation in the health_wide data set. This is specified using the $. The same applies to Location. 

site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7)))) #
# site_scores is combined with the first 7 PCs in site data from health_pca. 

species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7))) 
# the species scores data for the first 7 PCs from health_pca are assigned as a data frame called species_scores. 

species_scores$species <- rownames(species_scores) # species column in species_scores is renamed to species_scores.  
species_scores <- tibble(species_scores) # species_scores data frame is converted into a table format. 

ggplot(data = site_scores, aes(x = PC1, y = PC2)) + # the data from site_scores is used to great a plot with ggplot. The axis are set. 
  geom_point(aes(col = ParentLocation)) + # Points are differentiated by colour based on ParentLocation. 
  geom_segment(data = species_scores, # geom_segment draws a straight line based on x, y and xend, and yend points. 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), # the straight line is drawn from the center (origin) to the PCs site scores. 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), # an arrow is added to the lines to show direction. (assist in interpretation of data). 
               color = "lightseagreen", alpha = 1, size = 0.3) + # colour, size, and transparency of arrow and line is set. 
  geom_text(data = species_scores, # geom_text is used to insert and edit text that appears on the plot. 
            aes(x = PC1, y = PC2, label = species), # x and y positions are set as PC1 and PC 2 respectively, and the nature of the text (label) is set to "species" (i.e. SDGs). 
            color = "black") + # Colour of text is set. 
  xlab("PC1") + ylab("PC2") + # x labels and y labels are specified. 
  ggtitle("WHO SDGs, Scaling 2") # The title of the plot is specified in "". 
# With the use of ggplot, the plot's aesthetics can be edited be allow for easier 
# and more effective interpretations. 
# 2. ---------------
# Discuss and explain the patterns observed. How does South Africa fare in terms 
# of attaining SDGs? Contrast with some key countries of your choice to make your points. 
# Label the key countries that you refer to in your text by updating the code accordingly.

# The biplot scaling 1 shows the majority of variance explained by found in PC1 and PC2. 
# In this biplot, the relationships between the sites (i.e. the locations) can be seen. 
# Majority of the sites are plotted close together as a lot of them use the same set 
# of measurements to obtain SDGs. Sites further away from each are more different 
# in terms of what information is majorly used to produce an SDG for that country. 
# Different continents (locations) use different sets of information based what is available. 
# Differences in standards of living and GDP's could possible influence which measures
# influence SDG as well. Depending of the financial and socio-economic condition of a country,
# different "species" (sets of measures such as infancy mortality rate) would have 
# varying influences (higher score) in the variation caused between sites in terms 
# of SDG. A change from more impoverished area to more financially stable areas
# can be seen from left to right. 

# Biplot scaling 2, the correlation between measurements used to attain SDG can be
# seen. In this plot, we can see negative correlations between measurements on left 
# to measurements on the right had side of the plot. This is in support of the change
# from poorer location to wealthier ones. Depending on the wealth of the location, 
# the measurement that has most influence on their variation in SDG is different. 
# Poorer locations, such as Africa, have a SDG that is more influenced by malaria incidence,
# children under with Hepatitis B, road traffic deaths, poison control and unintentional poisoning, 
# new HIV infections, and Adult mortality rate (probably of dying between 15 to 60 years old). 
# These are factor that are most present in more vulnerable regions where public safety access
# to adequate health care facilities are lacking. These factors are negatively correlated with 
# regions in wealthier conditions. These regions have SDGs that are more influenced 
# by having more births attended by qualified healthcare workers, having a higher life
# expectancy, data availability for UHC index of essential service coverage (%), higher 
# domestic general government health expenditure (GGHE-D) as percentage of general 
# government expenditure (GGE) (%), and having more access to vaccinations preventing 
# infections that poorer countries are plagued by. The poorer countries have negative 
# correlations to these factors as they have less access to these things, thus they 
# affect the SDGs of those locations. This further supports the pattern from impoverished 
# to wealthier countries. 

# Key countries 
# For a more specific interpretation, we will be observing how specific countries' 
# are affected by the factors and measures most prevalent and most influential in the 
# attainment of SDG. 
view(site_scores)
site_scores <- site_scores %>% 
  filter(Location %in% c("South Africa", "Ethiopia", "Netherlands", "United States of America"))
# Filters the data so that the only site scores present are that of the stipulated locations. 

ggplot(data = site_scores, aes(x = PC1, y = PC2)) + # the data from site_scores is used to great a plot with ggplot. The axis are set. 
  geom_point(aes(col = Location)) + # Points are differentiated by colour based Location. 
  geom_segment(data = species_scores, # geom_segment draws a straight line based on x, y and xend, and yend points. 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), # the straight line is drawn from the center (origin) to the PCs site scores. 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), # an arrow is added to the lines to show direction. (assist in interpretation of data). 
               color = "lightseagreen", alpha = 1, size = 0.3) + # colour, size, and transparency of arrow and line is set. 
  geom_text(data = species_scores, # geom_text is used to insert and edit text that appears on the plot. 
            aes(x = PC1, y = PC2, label = species), # x and y positions are set as PC1 and PC 2 respectively, and the nature of the text (label) is set to "species" (i.e. SDGs). 
            color = "black") + # Colour of text is set. 
  xlab("PC1") + ylab("PC2") + # x labels and y labels are specified. 
  ggtitle("WHO SDGs (Country based), Scaling 2") 

# As seen in the above plots, two developing countries (South Africa and Ethiopia) are 
# being compared to two developed countries (USA and Netherlands). Ethiopia is strongly 
# influenced by the factors mentioned above which are positively correlated with sites 
# found in Africa. South Africa's SDG, is more strongly influenced and correlated to high 
# values in deaths by non-communicable diseases (Diabetes mellitus) and Crude suicide 
# rates (per 100 000 population). It is, however, strongly negatively correlated to girls aged 
# 15 years old that received the recommended doses of HPV vaccine. This is as apposed 
# to Ethiopia and many other African sites that are more positively correlated to it. 
# The Netherlands and the USA are positively correlated to all the factors that have 
# high values in wealthier countries (stipulated previously). South Africa has a 
# SDG which is most also influenced by all factors in the upper quadrants of the plot)
# These include factors such as new HIV infections, incidence of tuberculosis, 
# Poison control and unintentional poisoning, and high adult mortality rates. These
# are factors that also have positive correlations to most poorer countries. However, 
# it's SDG is also positively correlated to factors possessively correlated with the 
# SDG of wealthier countries such as Deaths by respiratory diseases, alcohol consumption rates, 
# cardiovascular diseases, and prevalent usage of tobacco. The prevalence of these factors 
# may be due to South Africa's financial and governmental situation. While we are a 
# developing country, many citizens have access to services such present in many 
# first world countries such as fast food services, recreational activities, etc. 
# But, we re still vulnerable to many health issues associated with underdeveloped countries. 
# As a result, South Africa also has many negative associations to both negative factors 
# positively affecting poorer at richer countries. These include factors such as 
# high malaria indices, high road traffic deaths, high adolescent birth rate, and 
# high rates of Mortality attributed to exposure to unsafe WASH services in poor countries.
# Additionally, it has negative correlation to high rates of UHC Index of service coverage (SCI), 
# pneumococcal conjugate vaccines (PCV3) immunization coverage among 1-year-olds (%), 
# high life expectancy, GGHE-D as percentage of GGE (%) associated with wealthier
# countries. This shows how, while South Africa has a prevent middle and upper class and
# developed areas, it still has a significant number undeveloped and impoverished area. 
# Thus, showing evidence to the gap in standard of living present in the country. 

# 3. ----------
# The data used in our observations was note not up to date as situations might have changed 
# over the course of the years. Some data sets used to calculate SDG were also not from the 
# same year due to lack of, what was then, up-to-date data. Additionally, factors such 
# as political (both past and present) situations are not taken into account. The construct
# of these data sets may be biased to developed countries as they have the resources 
# and infrastructure to collect sufficient data to accurate represent their SDG
# as apposed to poorer countries. 

# Conclusion ------
# As seen in the data sets, we are still far from attaining the ultimate goal. Many 
# countries are vulnerable to detrimental factors (both local and international)
# motivating uneven standards of living and inequality to access to resources. 












