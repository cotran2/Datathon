# Jane Smith 2016 project analysis
rm(list = ls())

# Read in data ----
wd <- "~/Downloads/"
rand_data <- read.csv(paste0(wd, "data/smith_rand.csv"), stringsAsFactors = F)
covar_data <- read.csv(paste0(wd, "data/smith_covariates.csv"), stringsAsFactors = F)
outcome_data <- read.csv(paste0(wd, "data/smith_outcomes.csv"), stringsAsFactors = F)

# Combine randomization, covariate, and survey outcome data
df <- merge(rand_data, covar_data, by = "ai_id")
df <- merge(df, outcome_data, by = "ai_id")

# Get rid of duplicated ai_ids
df <- df[!duplicated(df$ai_id),]

# Clean Data -----
# Take covariate data from covariate file
df$gender <- df$gender.x
df$race <- df$race.x
df$age <- df$age.x

# Cut down number of race categories
df$race_clean <- NA
df$race_clean[df$race == "caucasian"] <- "0White"
df$race_clean[df$race == "black"] <- "1Black"
df$race_clean[df$race == "hispanic"] <- "2Hispanic"
df$race_clean[df$race == "asian" |
                df$race == "middleEastern" |
                df$race == "unknown"] <- "3Other"

# Bin age
df$age_bin <- NA
df$age_bin[df$age >= 18 & df$age < 34] <- "18-34"
df$age_bin[df$age >= 35 & df$age < 44] <- "35-44"
df$age_bin[df$age >= 45 & df$age < 64] <- "45-64"
df$age_bin[df$age >= 65 ] <- "65+"

# Check for balance across assignment -----
# So I know we want to check that there is no statistically significant
# relationship between treatment and any of the covariates. I'm not sure
# how to do because there are 3 treatment categories.

# Estimate voter persuasion treatment effects -----
# Code outcome variable
df$democrat <- NA
df$democrat[df$outcome == "Democrat Jane Smith"] <- TRUE
df$democrat[df$outcome == "Republican Paul Jones"] <- FALSE

persuade_no_covar <- glm(democrat ~ canvass_treat, data = df, family = "binomial")
summary(persuade_no_covar)

# Control for race, age bins, gender, partisanship, and whether they turned
# out in 2016.
persuade_covar <- glm(democrat ~ canvass_treat + race_clean + age + gender +
                        partisanscore + turnout2016,
                      data = df, family = "binomial")
summary(persuade_covar)

# Looks like ProGreen was effective and ProEcon wasn't.

# Estimate voter mobilization treatment effects -----

turnout_no_covar <- glm(turnout2016 ~ canvass_treat, data = df,
                        family = "binomial")
summary(turnout_no_covar)

turnout_covar <- glm(turnout2016 ~ canvass_treat + race_clean + age + gender +
                       partisanscore,
                     data = df, family = "binomial")
summary(turnout_covar)

# Looks like both ProGreen and ProEcon increased turnout, but ProEcon by more.

# Graphs ------
# I didn't have time to create the graphs for the presentation. Could you do it?
# I know it needs to have the treatment effect in all groups and 90% error bars.
