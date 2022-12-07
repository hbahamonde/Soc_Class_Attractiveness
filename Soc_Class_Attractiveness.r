cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Soc_Class_Attractiveness/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import data
p_load(haven)
candidate.voter.d <- haven::read_dta("https://seafile.utu.fi/f/37476771864141508360/?dl=1")

# recode gender
candidate.voter.d$candidate_gender[candidate.voter.d$candidate_gender=="Mies"] <- "Male" 
candidate.voter.d$candidate_gender[candidate.voter.d$candidate_gender=="Nainen"] <- "Female" 
candidate.voter.d$candidate_gender = as.factor(candidate.voter.d$candidate_gender)

# deleting attributes
attributes(candidate.voter.d)["class"] <- NULL
candidate.voter.d = data.frame(candidate.voter.d)

# deleting unused attractiveness levels
## table(candidate.voter.d$candidate_attractiveness) # 1 less - 5 more
p_load(dplyr)
candidate.voter.d = candidate.voter.d %>% filter(candidate_attractiveness > 0)
candidate.voter.d = candidate.voter.d %>% filter(candidate_attractiveness < 8)

# excluding NA
p_load(tidyr)
candidate.voter.d = candidate.voter.d %>% drop_na(voter_isei)
candidate.voter.d = candidate.voter.d %>% drop_na(candidate_isei)
candidate.voter.d = candidate.voter.d %>% drop_na(candidate_attractiveness)

# Partitioning the data
candidate.voter.male.d = candidate.voter.d %>% filter(candidate_gender == "Male")
candidate.voter.female.d = candidate.voter.d %>% filter(candidate_gender == "Female")


# Descriptive



# OLS
p_load(texreg)
m1 <- lm(candidate_attractiveness ~ voter_isei * candidate_isei, data = candidate.voter.d)
m1.male <- lm(candidate_attractiveness ~ voter_isei * candidate_isei, data = candidate.voter.male.d)
m1.female <- lm(candidate_attractiveness ~ voter_isei * candidate_isei, data = candidate.voter.female.d)

screenreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
  list(m1, m1.male, m1.female) # list all the saved models here
)

# Plot
p_load(DAMisc)
DAintfun2(m1, varnames = c("voter_isei", "candidate_isei"),rug = TRUE, hist = TRUE)
DAintfun2(m1.male, varnames = c("voter_isei", "candidate_isei"),rug = TRUE, hist = TRUE)
DAintfun2(m1.female, varnames = c("voter_isei", "candidate_isei"),rug = TRUE, hist = TRUE)
