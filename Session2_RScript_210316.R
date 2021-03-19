
################################################################
###               Longitudinal Data Analysis                 ###
###            Session 2 - Growth Curve Models               ###
###                      16/03/2021                          ###
################################################################


# Set working directory, read in data and load packages ----------

### Set working directory
setwd("") # Insert path name into the speech marks e.g. ("pathname")


### Install and load relevant packages
## Install any packages that are not already installed 
install.packages("haven")         # reading in Stata files
install.packages("Hmisc")         # summary functions
install.packages("lme4")          # regression functions
install.packages("lmtest")        # likelihood ratio tests
install.packages("multcomp")      # general linear hypotheses
install.packages("psych")         # descriptive statistics
library.packages("stargazer")     # nice outputs
install.packages("tidyverse")     # loading the tidyverse


### Load relevant packages
library(haven)
library(Hmisc)
library(lme4) 
library(lmtest)  
# library(multcomp) # do not load as it overwrites the 'select' function in tidyverse
library(psych)
library(summarytools)
library(stargazer)
library(tidyverse)


# Part 1 - Read in the data -----------------------------------------

### Read in ELSA data
ELSA_wide <- read_dta("GCMPractical.dta")


# Part 2 - Descriptive Statistics and Reshaping the Data ------------------

### Check column names
colnames(ELSA_wide)


### Check variable classes
lapply(ELSA_wide, class)


### Recode sex from "Female" and "Male" to 1 and 0 respectively
ELSA_wide$indsex <- ifelse(ELSA_wide$indsex == 1, 0, 1)


### Set 'indsex' to interger
ELSA_wide$indsex <- as.integer(ELSA_wide$indsex)


### Make sure that the recoding has been successful
head(ELSA_wide$indsex)


### Summarise wave specific values of CF_exec*', either using the 'describe' function
describe(ELSA_wide$CF_exec1)
describe(ELSA_wide$CF_exec2)
describe(ELSA_wide$CF_exec3)
describe(ELSA_wide$CF_exec4)
describe(ELSA_wide$CF_exec5)


### Or, like last session, create a correlation matrix of 'CF_exec*' variable 
cor <- ELSA_wide %>%
  select(CF_exec1,
         CF_exec2,
         CF_exec3,
         CF_exec4,
         CF_exec5) %>% 
  as.matrix() %>%
  rcorr(type = "pearson")

### Use '[X]' to access objects in lists
cor[1]    # correlation matrix
cor[2]    # number of observations


### Reorder the variables before reshaping
ELSA_wide_new <- ELSA_wide %>% 
  relocate(starts_with(c("indsex", "indager", "agegroup", "educ1", "CF_exec","QoLscore", "age")), .after = idauniq)


### Check the reordering was successful
colnames(ELSA_wide_new)


### Reshape the data from wide to long format
ELSA_long <- reshape(as.data.frame(ELSA_wide_new),                                  
                     idvar = "idauniq", 
                     direction = "long",
                     varying = list(c(6:10),c(11:15), c(16:20), c(21:25)),
                     timevar = "time",
                     v.names=c("CF_exec","QoLscore", "wave", "age"))


### Sort the data by idauniq and time using arrange
ELSA_long <- ELSA_long %>%
  arrange(ELSA_long, idauniq, time)


### Keep only the variables that we need in new dataframe using the 'select' command
ELSA_long <- ELSA_long %>% 
  select(idauniq, indsex, indager, agegroup, educ1, CF_exec, QoLscore, age, time) 


### Look at the first couple of rows, we have 56955 records
head(ELSA_long)


### Drop any missing values of 'CF_exec'
ELSA_long <- ELSA_long %>% filter(!is.na(CF_exec))


### Examine distribution of CF_exec scores
### Summarise data, we now have 37173 records
describe(ELSA_long$CF_exec)


### Create a new variable to summarise the number of waves that participants have CF scores
ELSA_long <- ELSA_long %>%
  group_by(idauniq) %>%
  mutate(nwaves = n()) # mutate creates a new variable ('nwaves') that summarises how many waves per individual


### Create an idauniq-level dataset (called IDdata) using same principle as tag in Stata 
## (NB: tag picks one record per cluster (i.e. individual) and is useful for when you are wanting to 
## summarise records in clustered data)
IDdata <- ELSA_long %>%
  group_by(idauniq) %>%
  arrange(time) %>%
  filter(row_number()==1)  # takes the data from the first row only

IDdata$nwaves<-as.factor(IDdata$nwaves)
freq(IDdata$nwaves)

## NOTE: this is slightly different to STATA solutions as it doesn't show those with missing data at ALL five waves



# Part 3 - Growth Curve Model ---------------------------------------------

### Set waves to run from 0 to 4 (instead of from 1 to 5)
### Minus 1 from current wave values (to make intercept refer to time 1; otherwise it will refer to time '0' which is not a datapoint)
ELSA_long$time = ELSA_long$time-1


### Check that this recoding has been successful
summary(ELSA_long$time)


### Fit a linear growth curve model ("linuncond") with time
linuncond <- lmer(CF_exec ~ 1                  # outcome: executive function; '1' indicates that you want a model with only an intercept
                  + time + (1 + time|idauniq), # 'time|idauniq' allows the gradient to change over time
                  data = ELSA_long,            # data frame
                  REML = FALSE)                # should the estimates be chosen to optimise the restricted maximum likelihood (REML) criterion? FALSE means that it optimises log-liklihood instead (when dataset is large enough that the two are equivalent and can use lmtest.

confint(linuncond) # this can take a while to run but produces the confidence intervals for the random and fixed parts of the model

# Note: the random parts of the model are: '.sig.*' and 'sigma'; and the fixed parts of the model are: '(Intercept)' and 'time'


### Look at model table
summary(linuncond)


### We can display this in a neater way using stargazer
stargazer(linuncond, ci = TRUE, type="text", title="Results", align=TRUE) # we include ci = TRUE to get the confidence intervals for the fixed part of the model


### Create new dataframe with individuals that have data over all 5 waves (i.e. complete data; n = 4144)
ELSA_long_complete <- ELSA_long[which(ELSA_long$nwaves=='5'),]


## Run the model again with individuals that have complete data
linuncond_complete <- lmer(CF_exec ~ 1                   # outcome: executive function; '1' indicates that you want a model with only an intercept
                           + time + (1 + time|idauniq),  # 'time|idauniq' allows the gradient to change over time 
                           data = ELSA_long_complete,    # data frame
                           REML = FALSE)                 # same as above


### Look at model table
summary(linuncond_complete) 


### Display results using stargazer
stargazer(linuncond_complete, type="text", title="Results", align=TRUE)

### Display results using stargazer
stargazer(linuncond, linuncond_complete, type="text", title="Results", align=TRUE)



# Part 4 - Quadratic Growth Curve Model -----------------------------------

### Add a wave^2 column ###
ELSA_long$time_2 <- ELSA_long$time^2


### Run quadratic model (with full (i.e. not complete cases) dataset) 
quaduncond <- lmer(CF_exec ~ 1                            # outcome: executive function
                   + time + time_2 + (1 + time |idauniq), # predictors: time, time^2
                   data = ELSA_long,                      # data frame
                   REML = FALSE)                          # same as above

## Ignore warning message


### Look at model table
summary(quaduncond)


### Display results using stargazer
stargazer(quaduncond, type="text", title="Results", align=TRUE)


stargazer(linuncond, quaduncond, type="text", title="Results", align=TRUE)


### Run a Likelihood Ratio (LR) test to compare the quadratic model ("quaduncond") with the linear model ("linuncond")
lrtest(quaduncond, linuncond) ## Larger model goes first




# Part 5 - Conditional Growth Curve Model ---------------------------------

### Look at mean age at the start of the study ("indager")
mean(ELSA_long$indager) 


### Look at the distribution of age at the start of the study
hist(ELSA_long$indager)


### Centre age at the start of the study around mean(indager) (i.e. 65)
ELSA_long$indager_centre <- (ELSA_long$indager - 65)


### Create new variable with interaction between time and age at the start of the study 
ELSA_long$timeage <- (ELSA_long$time*ELSA_long$indager_centre)


### Fit conditional model ("cond_timeage") with time*age interaction
cond_timeage <- lmer(CF_exec ~ time 
                     + indager_centre + timeage + (1 + time|idauniq), 
                     data = ELSA_long,
                     REML = FALSE)


### Look at model table
summary(cond_timeage)


### Display results using stargazer
stargazer(cond_timeage, type="text", title="Results", align=TRUE)


### Create new variable with interaction between time and sex - we will include this in the model
ELSA_long$timesex <- (ELSA_long$time*ELSA_long$indsex)


### Fit conditional model ("cond_timeage_timesex") with time*age and time*sex interactions
cond_timeage_timesex <- lmer(CF_exec ~ 1 + time 
                             + indager_centre + timeage + factor(indsex) + timesex + (1 + time|idauniq), 
                             data = ELSA_long, 
                             REML = FALSE)


### Look at model table
summary(cond_timeage_timesex)


### Display results using stargazer
stargazer(cond_timeage_timesex, type="text", title="Results", align=TRUE)


### Run a Likelihood Ratio (LR) test to compare the time*age ("cond_timeage") with the time*age + time*sex ("cond_timeage_timesex") models ### ###
lrtest(cond_timeage_timesex, cond_timeage) 


### Create new variable with interaction between time and education 
ELSA_long$timeedu <- (ELSA_long$time*ELSA_long$educ1)


### Fit conditional model ("cond_timeage_timesex_timeedu") with time*age, time*sex and time*educcation interactions
cond_timeage_timesex_timeedu <- lmer(CF_exec ~ time 
                                     + indager_centre + timeage + indsex + timesex + timeedu + educ1 + (1 + time|idauniq), 
                                     data = ELSA_long, 
                                     REML = FALSE)


### Look at model table
summary(cond_timeage_timesex_timeedu)


### Display results using stargazer
stargazer(cond_timeage_timesex_timeedu, type="text", title="Results", align=TRUE)




# Part 6 - Age as metric of time ------------------------------------------

### Centre age throughout the study around mean(indager)  (i.e. time-varying age centered to mean)
ELSA_long$age_centre <- (ELSA_long$indager - 67)


### Fit conditional model ("cond_age") with centred age as time
cond_age <- lmer(CF_exec ~ age_centre + (age_centre|idauniq),  
                 data = ELSA_long)

## Ignore warning messages


### Look at the model table
summary(cond_age)


### Display results using stargazer
stargazer(cond_age, type="text", title="Results", align=TRUE)


  
# Part 7 - Making Graphs --------------------------------------------------

### Fit simple growth model with time only ("time")
time <- lmer(CF_exec ~ time + (time|idauniq), 
                 data = ELSA_long, 
                 REML = FALSE)


### Look at the model table ###
summary(time)


### Display results using stargazer
stargazer(time, type="text", title="Results", align=TRUE)


### Create a function ('get_pred') which selects the variables from the 'time' model that you want to plot
get_pred <- function(string, t, mod){                   # set the input for the 'get_pred' function
  multcomp::glht(mod, string) %>%                       # put the input 'string' into second argument of 'glht' function 
    confint() %>%                                       # compute the confidence intervals
    pluck("confint") %>%                                # use pluck instead of the square brackets
    as_tibble() %>%                                     # make the  matrix into a tibble
    mutate(time = !!t) %>%                              # create a new time variable 't'
    select(time, est = Estimate, lci = lwr, uci = upr)  # select the variables
}


### Set the time sequence from 0 - 4 years 
time_seq <- seq(from = 0.001, to = 4, length = 40) 


### Apply the function to each element of the 'time_seq' values
map_dfr(time_seq,  
        ~ paste0("(Intercept) + ", .x, "*time = 0") %>%  # indicate which variables you want to select
          get_pred(.x, time)) %>%                        # use the 'get_pred' function you created above
  ggplot() +                                             # add these into ggplot
  aes(x = time, y = est, ymin = lci, ymax = uci) +       # set the x, y and confidence intervals
  geom_ribbon(color = NA, alpha = 0.2) +                 # make the ribbon transparent
  geom_line() +                                          # line graph
  labs(x = "Time", y = "Predicted")                      # give the x-axis and y-axis labels


### Run the model with education and education*time
time_educ <- lmer(CF_exec ~ time + educ1 + educ1*time + (1 + time|idauniq),
                  data = ELSA_long,
                  REML = FALSE)
summary(time_educ)


### Run the same command as above but separately for low and high education ('1' and '0' respectively)
## Low education ('1')
educ_low <- map_dfr(time_seq,
                    ~ paste0("(Intercept) + educ1 + ", .x, "*time + ", # indicate which variables you want to select
                             .x, "*`time:educ1` = 0") %>%   
                      get_pred(.x, time_educ))                         # use the 'get_pred' function you created above


## High education ('0')
educ_high <- map_dfr(time_seq,
                     ~ paste0("(Intercept) + ", .x, "*time = 0") %>%   # indicate which variables you want to select
                       get_pred(.x, time_educ))                        # use the 'get_pred' function you created above


### Create a matrix with these values and plot them in ggplot
bind_rows(Low = educ_low, High = educ_high, .id = "educ") %>% # name the dataframes; put new name in a column
  ggplot() +
  aes(x = time, y = est, ymin = lci, ymax = uci,              # set the x, y and confidence intervals
      color = educ, fill = educ) +                            # set colours to change with differing levels of education
  geom_ribbon(color = NA, alpha = 0.2) +                      # make ribbon transparent
  geom_line() +                                               # line graph
  labs(x = "Time", y = "Predicted")                           # give the x-axis and y-axis labels


##### FINISHED #####
