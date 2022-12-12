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

# recoded ideology
candidate.voter.d$ideology.recoded = candidate.voter.d$ideology
levels(candidate.voter.d$ideology.recoded)[levels(candidate.voter.d$ideology.recoded)=="Center-Left"] = "Center"
levels(candidate.voter.d$ideology.recoded)[levels(candidate.voter.d$ideology.recoded)=="Center-Right"] = "Center"

# Partitioning the data
candidate.voter.male.d = candidate.voter.d %>% filter(candidate_gender == "Male")
candidate.voter.female.d = candidate.voter.d %>% filter(candidate_gender == "Female")


# Descriptive
dev.off()
par(mfrow = c(1, 3))
plot(density(log(candidate.voter.d$turnout)), main = "Dependent Variable", xlab = "Votes Received (log)", ylab = "Density")
plot(candidate.voter.d$ideology.recoded, main = "Ideology of Party", xlab = "", ylab = "Frequency")
plot(density(candidate.voter.d$attractiveness), main = "Attractiveness Levels", xlab = "", ylab = "Density")



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
# OLS 2 (stage 2)
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

# Way out 1: candidates that are systematically evaluated higher on physical appearance receive more votes in the election.
# THIS DOES NOT WORK: candidates that receive more votes are associated with voters that score higher in social class (BUT THIS LINK IS JUST RANDOM ASSOCIATION BETWEEN THE VOTER AND THE SAMPLE OF CANDIDATES THEY WERE ASSIGNED TO EVALUATE).


##############################
# OLS 3 (stage 2)

# all ideology
m3 <- lm(turnout ~ candidate_attractiveness*ideology + age + candidate_gender + city-1, data = candidate.voter.d) # 
m3.male <- lm(turnout ~ candidate_attractiveness*ideology + age + city-1, data = candidate.voter.male.d) # 
m3.female <- lm(turnout ~ candidate_attractiveness*ideology + age + city-1, data = candidate.voter.female.d) # 

# recoded ideology
m3.2 <- lm(turnout ~ candidate_attractiveness*ideology.recoded + age + candidate_gender + city-1, data = candidate.voter.d) # 
m3.male.2 <- lm(turnout ~ candidate_attractiveness*ideology.recoded + age + city-1, data = candidate.voter.male.d) # 
m3.female.2 <- lm(turnout ~ candidate_attractiveness*ideology.recoded + age + city-1, data = candidate.voter.female.d) # 


# story
## Voters usually use a myriad of resources to choose their candidates. While we already know that voters use a mix of class heuristics and physical appearance cues, unfortunately it's not clear which type of information predicts votes best.
## While class cues contribute to successful electoral outcomes, we find that candidates that are ranked better according to their physical attractiveness perform best in elections.

options(scipen=999)
summary(m3)

p_load(texreg)
screenreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
  list(m3)#, # list all the saved models here
  #omit.coef = "id"
)

# Plot
# p_load(DAMisc)
# DAintfun2(m3, varnames = c("candidate_attractiveness", "ideology"), 
#          rug = TRUE, 
#          hist = TRUE,
#          #ylab = c("Cond. Eff. of Candidate Attractiveness", "Cond. Eff. of Voter  Soc. Class"),
#          #xlab = c("Candidate Social Class", "Candidate Attractiveness"),
#          nclass = c(20, 20)
#          )


## plots
p_load(sjPlot,sjmisc,ggplot2)
theme_set(theme_sjplot())

# All ideology
#p1 = plot_model(m3, type = "int", title = "Complete Data (controlling for gender)", axis.title = c("Candidate Attractiveness", "Votes", show.legend = F))
p2 = plot_model(m3.male, type = "int", title = "Male Data", axis.title = c("Candidate Attractiveness", "Votes", show.legend = F))
p3 = plot_model(m3.female, type = "int", title = "Female Data", axis.title = c("Candidate Attractiveness", "Votes", show.legend = T))
# combining plots
p_load(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)


# All ideology
p_load(sjPlot,sjmisc,ggplot2)
theme_set(theme_sjplot())
#p1.2 = plot_model(m3.2, type = "int", title = "Complete Data (controlling for gender)", axis.title = c("Candidate Attractiveness", "Votes", show.legend = F))
p2.2 = plot_model(m3.male.2, type = "int", title = "Male Data", axis.title = c("Candidate Attractiveness", "Votes", legend.title = "Political Party"))
p3.2 = plot_model(m3.female.2, type = "int", title = "Female Data", axis.title = c("Candidate Attractiveness", "Votes", legend.title = "Political Party"))
# combining plots
p_load(gridExtra)
grid.arrange(p2.2, p3.2, ncol = 2)








# p_load(effects)
# attractiveness.p3 = plot(predictorEffect("candidate_attractiveness", m3), asp = 1, xlab = "Candidate Attractiveness", ylab = "Predicted Votes", main = "")
# candidate.isei.p3 = plot(predictorEffect("candidate_isei", m3), asp = 1, xlab = "Candidate Social Class", ylab = "", main = "")
p1 = plot(predictorEffect("candidate_attractiveness", m3), asp = 1, xlab = "candidate_attractiveness", ylab = "", main = "")
p2 = plot(predictorEffect("candidate_isei", m3), asp = 1, xlab = "candidate_isei", ylab = "", main = "")
p3 = plot(predictorEffect("age", m3), asp = 1, xlab = "age", ylab = "", main = "")

# combining plots
p_load(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)





# todo
## 1. try attractiveness*ideology (or party)
## 2. split by gender.
## read: Beholding Inequality: Race, Gender, and Returns to Physical Attractiveness in the United States.

