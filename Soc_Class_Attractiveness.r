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

# Merging: loading Voting data from Pol Psych paper
load(file = "/Users/hectorbahamonde/research/Physical/dat.Rdata")
dat = dat %>% select(-one_of(c('gender')))
candidate.voter.d = merge(candidate.voter.d, dat, by = "id")

# Formatting
candidate.voter.d$municipality = as.factor(candidate.voter.d$municipality)
candidate.voter.d$id = as.factor(candidate.voter.d$id)


# Partitioning the data
candidate.voter.male.d = candidate.voter.d %>% filter(candidate_gender == "Male")
candidate.voter.female.d = candidate.voter.d %>% filter(candidate_gender == "Female")


# Descriptive



# OLS (stage 1)
m1 <- lm(candidate_attractiveness ~ voter_isei * candidate_isei, data = candidate.voter.d)
m1.male <- lm(candidate_attractiveness ~ voter_isei * candidate_isei, data = candidate.voter.male.d)
m1.female <- lm(candidate_attractiveness ~ voter_isei * candidate_isei, data = candidate.voter.female.d)


p_load(texreg)
screenreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
  list(m1, m1.male, m1.female)#, # list all the saved models here
  #omit.coef = "id"
)

# Plot
p_load(DAMisc)
DAintfun2(m1, varnames = c("voter_isei", "candidate_isei"),rug = TRUE, hist = TRUE)
DAintfun2(m1.male, varnames = c("voter_isei", "candidate_isei"),rug = TRUE, hist = TRUE)
DAintfun2(m1.female, varnames = c("voter_isei", "candidate_isei"),rug = TRUE, hist = TRUE)

##############################
# OLS (stage 2)
m2 <- lm(turnout ~ candidate_attractiveness + voter_isei*candidate_isei + age + candidate_gender + city + party, data = candidate.voter.d) # 

options(scipen=999)
summary(m2)

p_load(texreg)
screenreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
  list(m2)#, # list all the saved models here
  #omit.coef = "id"
)

# Plot
p_load(DAMisc)
DAintfun2(m2, varnames = c("candidate_isei", "voter_isei"), 
          rug = TRUE, 
          hist = TRUE,
          ylab = c("Cond. Eff. of Candidate Soc. Class | Voter Soc. Class", "Cond. Eff. of Voter  Soc. Class | Candidate Soc. Class"),
          xlab = c("Voter Social Class", "Candidate Social Class"),
          nclass = c(20, 20),
          )

p_load(effects)
attractiveness.p = plot(predictorEffect("candidate_attractiveness", m2), asp = 1, xlab = "Candidate Attractiveness", ylab = "Predicted Votes", main = "")
gender.p = plot(predictorEffect("candidate_gender", m2), asp = 1, xlab = "Candidate Gender", ylab = "", main = "")
age.p = plot(predictorEffect("age", m2), asp = 1, xlab = "Candidate Age", ylab = "", main = "")

# combining plots
p_load(gridExtra)
grid.arrange(attractiveness.p, gender.p, age.p, ncol = 3)
