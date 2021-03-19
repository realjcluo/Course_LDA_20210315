
################################################################
###               Longitudinal Data Analysis                 ###
###               Session 3 - Binary Outcomes                ###
###                      17/03/2021                          ###
################################################################


# Set working directory and load packages ----------

### Set working directory
setwd("") # Insert path name into the speech marks e.g. ("pathname")


### Install and load relevant packages 
## Install any packages that are not already installed 
install.packages("dplyr")         # data manipulation (NB: may need compilation)
install.packages("gee")           # generalised estimation equations
install.packages("ggplot2")       # nice graphs
install.packages("haven")         # reading in Stata files
install.packages("Hmisc")         # summary functions
install.packages("lme4")          # regression functions
install.packages("lmtest")        # likelihood ratio tests
install.packages("psych")         # descriptive statistics
library.packages("stargazer")     # nice outputs
install.packages("tidyverse")     # loading the tidyverse

### Load relevant packages ###
library(dplyr) 
library(gee)    
library(ggplot2)
library(haven)
library(Hmisc)
library(lme4)
library(lmtest)
library(psych)
library(stargazer)
library(tidyverse)


# OPTIONAL - Data cleaning (WILL NOT BE COVERED IN PRACTICAL) -------------


### Create function ('remove_na') that recodes variables with missing values in the Stata dataset (coded as negative numbers) as (NA) when importing
remove_na <- function(var){   # give the function an input 'var'
  lbls <- attr(var, "labels") # get labels of 'var'
  lbls <- lbls[lbls >= 0] # keep any labels if it's greater than 0
  
  if (length(lbls)==0){ # if it is 0 then replace with NULL
    attr(var, "labels") <- NULL
    class(var) <- typeof(var) # change the class to the type of variable (created by 'attr' which stands for attributes)
  } else attr(var, "labels") <- lbls # if it is more than 0, then leave it as it is
  
  if (is.numeric(var)){ # only for numeric variables
    atrb <- attributes(var) # create list of atributes
    var <- ifelse(var < 0, NA, var) # if 'var' is less than 0 then replace with NA, if else, keep as var 
    attributes(var) <- atrb # replace attributes with the metadata for the variables you have created
  } 
  
  var
}


### Haven imports variables with value labels, we need to convert these from as_factor() to factor
### Clean the wave specific datasets
ELSA_wave1 <- read_dta("Wave_1_elsa.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()

ELSA_wave2 <- read_dta("Wave_2_elsa.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()

ELSA_wave3 <- read_dta("Wave_3_elsa.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()

ELSA_wave4 <- read_dta("Wave_4_elsa.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()

ELSA_wave5 <- read_dta("Wave_5_elsa.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()

ELSA_wave6 <- read_dta("Wave_6_elsa.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()

ELSA_wave7 <- read_dta("Wave_7_elsa.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()

ELSA_wave8 <- read_dta("Wave_8_elsa.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()



### Recode sex from "Female" and "Male" to 1 and 0 respectively
ELSA_wave1$dhsex <- ifelse(ELSA_wave1$dhsex == 1, 0, 1)
ELSA_wave2$dhsex <- ifelse(ELSA_wave2$dhsex == 1, 0, 1)
ELSA_wave3$dhsex <- ifelse(ELSA_wave3$dhsex == 1, 0, 1)
ELSA_wave4$dhsex <- ifelse(ELSA_wave4$dhsex == 1, 0, 1)
ELSA_wave5$dhsex <- ifelse(ELSA_wave5$dhsex == 1, 0, 1)
ELSA_wave6$dhsex <- ifelse(ELSA_wave6$dhsex == 1, 0, 1)
ELSA_wave7$dhsex <- ifelse(ELSA_wave7$dhsex == 1, 0, 1)
ELSA_wave8$dhsex <- ifelse(ELSA_wave8$dhsex == 1, 0, 1)


### Set 'dhsex' to interger
ELSA_wave1$dhsex <- as.integer(ELSA_wave1$dhsex)
ELSA_wave2$dhsex <- as.integer(ELSA_wave2$dhsex)
ELSA_wave3$dhsex <- as.integer(ELSA_wave3$dhsex)
ELSA_wave4$dhsex <- as.integer(ELSA_wave4$dhsex)
ELSA_wave5$dhsex <- as.integer(ELSA_wave5$dhsex)
ELSA_wave6$dhsex <- as.integer(ELSA_wave6$dhsex)
ELSA_wave7$dhsex <- as.integer(ELSA_wave7$dhsex)
ELSA_wave8$dhsex <- as.integer(ELSA_wave8$dhsex)


### Make sure that the recoding has been successful
head(ELSA_wave1$dhsex)


### Change 'wpdes' variable to factor
ELSA_wave1$wpdes <- as.integer(ELSA_wave1$wpdes)
ELSA_wave2$wpdes <- as.integer(ELSA_wave2$wpdes)
ELSA_wave3$wpdes <- as.integer(ELSA_wave3$wpdes)
ELSA_wave4$wpdes <- as.integer(ELSA_wave4$wpdes)
ELSA_wave5$wpdes <- as.integer(ELSA_wave5$wpdes)
ELSA_wave6$wpdes <- as.integer(ELSA_wave6$wpdes)
ELSA_wave7$wpdes <- as.integer(ELSA_wave7$wpdes)
ELSA_wave8$wpdes <- as.integer(ELSA_wave8$wpdes)


### Change ''edqual' variable to factors
ELSA_wave1$edqual <- as.integer(ELSA_wave1$edqual)
ELSA_wave2$edqual <- as.integer(ELSA_wave2$edqual)
ELSA_wave3$edqual <- as.integer(ELSA_wave3$edqual)
ELSA_wave4$edqual <- as.integer(ELSA_wave4$edqual)
ELSA_wave5$edqual <- as.integer(ELSA_wave5$edqual)
ELSA_wave6$edqual <- as.integer(ELSA_wave6$edqual)
ELSA_wave7$edqual <- as.integer(ELSA_wave7$edqual)
ELSA_wave8$edqual <- as.integer(ELSA_wave8$edqual)


### Order columns in ELSA_1 to make them consistent across dataframes
ELSA_wave1 <- ELSA_wave1[c("idauniq", "dhsex", "wpdes", "indager", "edqual", "depression", "depressed" )]


### Check this has been successful
colnames(ELSA_wave1)


### Order columns in ELSA_8 to make them consistent across dataframes
ELSA_wave8 <- ELSA_wave8[c("idauniq", "dhsex", "wpdes", "indager", "edqual", "depression", "depressed" )]


### Check this has been successful
colnames(ELSA_wave8)


### Add wave values to variable names (e.g. depression1 etc)
## Wave 1
colnames(ELSA_wave1)[which(names(ELSA_wave1) == "indager")] <- "indager0"
colnames(ELSA_wave1)[which(names(ELSA_wave1) == "depression")] <- "depression0"
colnames(ELSA_wave1)[which(names(ELSA_wave1) == "depressed")] <- "depressed0"
colnames(ELSA_wave1)[which(names(ELSA_wave1) == "dhsex")] <- "dhsex0"
colnames(ELSA_wave1)[which(names(ELSA_wave1) == "wpdes")] <- "wpdes0"
colnames(ELSA_wave1)[which(names(ELSA_wave1) == "edqual")] <- "edqual0"


### Check this has been successful
colnames(ELSA_wave1)


## Wave 2
colnames(ELSA_wave2)[which(names(ELSA_wave2) == "indager")] <- "indager1"
colnames(ELSA_wave2)[which(names(ELSA_wave2) == "depression")] <- "depression1"
colnames(ELSA_wave2)[which(names(ELSA_wave2) == "depressed")] <- "depressed1"
colnames(ELSA_wave2)[which(names(ELSA_wave2) == "dhsex")] <- "dhsex1"
colnames(ELSA_wave2)[which(names(ELSA_wave2) == "wpdes")] <- "wpdes1"
colnames(ELSA_wave2)[which(names(ELSA_wave2) == "edqual")] <- "edqual1"

## Wave 3
colnames(ELSA_wave3)[which(names(ELSA_wave3) == "indager")] <- "indager2"
colnames(ELSA_wave3)[which(names(ELSA_wave3) == "depression")] <- "depression2"
colnames(ELSA_wave3)[which(names(ELSA_wave3) == "depressed")] <- "depressed2"
colnames(ELSA_wave3)[which(names(ELSA_wave3) == "dhsex")] <- "dhsex2"
colnames(ELSA_wave3)[which(names(ELSA_wave3) == "wpdes")] <- "wpdes2"
colnames(ELSA_wave3)[which(names(ELSA_wave3) == "edqual")] <- "edqual2"

## Wave 4
colnames(ELSA_wave4)[which(names(ELSA_wave4) == "indager")] <- "indager3"
colnames(ELSA_wave4)[which(names(ELSA_wave4) == "depression")] <- "depression3"
colnames(ELSA_wave4)[which(names(ELSA_wave4) == "depressed")] <- "depressed3"
colnames(ELSA_wave4)[which(names(ELSA_wave4) == "dhsex")] <- "dhsex3"
colnames(ELSA_wave4)[which(names(ELSA_wave4) == "wpdes")] <- "wpdes3"
colnames(ELSA_wave4)[which(names(ELSA_wave4) == "edqual")] <- "edqual3"

## Wave 5
colnames(ELSA_wave5)[which(names(ELSA_wave5) == "indager")] <- "indager4"
colnames(ELSA_wave5)[which(names(ELSA_wave5) == "depression")] <- "depression4"
colnames(ELSA_wave5)[which(names(ELSA_wave5) == "depressed")] <- "depressed4"
colnames(ELSA_wave5)[which(names(ELSA_wave5) == "dhsex")] <- "dhsex4"
colnames(ELSA_wave5)[which(names(ELSA_wave5) == "wpdes")] <- "wpdes4"
colnames(ELSA_wave5)[which(names(ELSA_wave5) == "edqual")] <- "edqual4"

## Wave 6
colnames(ELSA_wave6)[which(names(ELSA_wave6) == "indager")] <- "indager5"
colnames(ELSA_wave6)[which(names(ELSA_wave6) == "depression")] <- "depression5"
colnames(ELSA_wave6)[which(names(ELSA_wave6) == "depressed")] <- "depressed5"
colnames(ELSA_wave6)[which(names(ELSA_wave6) == "dhsex")] <- "dhsex5"
colnames(ELSA_wave6)[which(names(ELSA_wave6) == "wpdes")] <- "wpdes5"
colnames(ELSA_wave6)[which(names(ELSA_wave6) == "edqual")] <- "edqual5"

## Wave 7
colnames(ELSA_wave7)[which(names(ELSA_wave7) == "indager")] <- "indager6"
colnames(ELSA_wave7)[which(names(ELSA_wave7) == "depression")] <- "depression6"
colnames(ELSA_wave7)[which(names(ELSA_wave7) == "depressed")] <- "depressed6"
colnames(ELSA_wave7)[which(names(ELSA_wave7) == "dhsex")] <- "dhsex6"
colnames(ELSA_wave7)[which(names(ELSA_wave7) == "wpdes")] <- "wpdes6"
colnames(ELSA_wave7)[which(names(ELSA_wave7) == "edqual")] <- "edqual6"

## Wave 8
colnames(ELSA_wave8)[which(names(ELSA_wave8) == "indager")] <- "indager7"
colnames(ELSA_wave8)[which(names(ELSA_wave8) == "depression")] <- "depression7"
colnames(ELSA_wave8)[which(names(ELSA_wave8) == "depressed")] <- "depressed7"
colnames(ELSA_wave8)[which(names(ELSA_wave8) == "dhsex")] <- "dhsex7"
colnames(ELSA_wave8)[which(names(ELSA_wave8) == "wpdes")] <- "wpdes7"
colnames(ELSA_wave8)[which(names(ELSA_wave8) == "edqual")] <- "edqual7"


### Merge the dataframes to create a wide format dataframe using the 'left_join' command
ELSA_wide <- full_join(ELSA_wave1, ELSA_wave2, by='idauniq') %>%
  full_join(.,ELSA_wave3, by='idauniq') %>% # pastes the output on the left hand side
  full_join(.,ELSA_wave4, by='idauniq') %>%
  full_join(.,ELSA_wave5, by='idauniq') %>%
  full_join(.,ELSA_wave6, by='idauniq') %>%
  full_join(.,ELSA_wave7, by='idauniq') %>%
  full_join(.,ELSA_wave8, by='idauniq')



### Save the cleaned wide dataframe
save(ELSA_wide, file = "ELSA_wide.Rdata")


### Clean the long dataset
ELSA_long <- read_dta("ELSA_long.dta") %>%
  mutate(across(matches(c("^indager", "^dhsex")), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()


### Only keep the variables we will use in the analyses
ELSA_long_new <- subset(ELSA_long, select = c("idauniq", "wave", "wpdes_w", "indager_w", "dhsex_w", "edqual_w", "depression_w", "depressed_w"))


### Rename the variables
colnames(ELSA_long_new)[which(names(ELSA_long_new) == "wpdes_w")] <- "wpdes"
colnames(ELSA_long_new)[which(names(ELSA_long_new) == "indager_w")] <- "indager"
colnames(ELSA_long_new)[which(names(ELSA_long_new) == "dhsex_w")] <- "dhsex"
colnames(ELSA_long_new)[which(names(ELSA_long_new) == "edqual_w")] <- "edqual"
colnames(ELSA_long_new)[which(names(ELSA_long_new) == "depression_w")] <- "depression"
colnames(ELSA_long_new)[which(names(ELSA_long_new) == "depressed_w")] <- "depressed"
colnames(ELSA_long_new)[which(names(ELSA_long_new) == "wave")] <- "time"


### Recode sex from "Female" and "Male" to 1 and 0 respectively
ELSA_long_new$dhsex <- ifelse(ELSA_long_new$dhsex == 1, 0, 1)


### Change 'wpdes' and 'edqual' variables to factors
ELSA_long_new$wpdes <- as.factor(ELSA_long_new$wpdes) 
ELSA_long_new$edqual <- as.factor(ELSA_long_new$edqual) 


### Change 'indager' variables to intergers
ELSA_long_new$indager <- as.integer(ELSA_long_new$indager) 


### Recode sex from "Female" and "Male" to 1 and 0 respectively
ELSA_long_new$depressed <- ifelse(ELSA_long_new$depressed == 1, 0, 1)


### Save the cleaned wide dataframe
save(ELSA_long_new, file = "ELSA_long_new.Rdata")


# Part 1 - Read in the data --------------------------------------------------------

### Read in the wide data if you have not run the optional code above
ELSA_wide <- load("ELSA_wide.Rdata")


### Read in the long data if you have not run the optional code above
ELSA_long_new <- load("ELSA_long_new.Rdata")



# Part 2 - Preparing and merging the data ------------------

### Create another dataframe for individuals that are present at every wave using the 'inner_join' command
ELSA_wide_complete <- inner_join(ELSA_wave1, ELSA_wave2, by='idauniq') %>%
  inner_join(.,ELSA_wave3, by='idauniq') %>% # pastes the output on the left hand side
  inner_join(.,ELSA_wave4, by='idauniq') %>%
  inner_join(.,ELSA_wave5, by='idauniq') %>%
  inner_join(.,ELSA_wave6, by='idauniq') %>%
  inner_join(.,ELSA_wave7, by='idauniq') %>%
  inner_join(.,ELSA_wave8, by='idauniq')


### Drop participants who did not have a valid depression scores at any wave
ELSA_wide_complete_dep <- drop_na(ELSA_wide_complete, matches("^depression."))


### Create a correlation matrix of 'depression*' variable 
cor <- ELSA_wide_complete_dep %>%
  select(depression0,
         depression1,
         depression2,
         depression3,
         depression4,
         depression5,
         depression6,
         depression7) %>% 
  as.matrix() %>%
  rcorr(type = "pearson")

### Use '[X]' to access objects in lists
cor[1]    # correlation matrix


### Drop any missing values of 'depression'  
ELSA_long_new <- ELSA_long_new %>% 
  filter(!is.na(depression)) %>%
  filter(!is.na(depressed))


### Set waves to run from 0 to 7 (instead of from 1 to 8)
### Minus 1 from current wave values (to make intercept refer to time 1; otherwise it will refer to time '0' which is not a datapoint)
ELSA_long_new$time = ELSA_long_new$time-1


### Check that this was successful
summary(ELSA_long_new$time)


### Create a new variable to summarise the number of waves that participants were present
ELSA_long_new <- ELSA_long_new %>%
  group_by(idauniq) %>%
  mutate(nwaves = n()) # mutate creates a new variable ('nwaves') that summarises how many waves per individual


### View the dataframe to make sure that this has been successful
View(ELSA_long_new)


### Create an idauniq-level dataset (called IDdata) using same principle as tag in Stata 
## (NB: tag picks one record per cluster (i.e. individual) and is useful for when you are wanting to 
## summarise records in clustered data)
IDdata <- ELSA_long_new %>%
  group_by(idauniq) %>%
  arrange(time) %>%
  filter(row_number()==1)  # takes the data from the first row only

IDdata$nwaves<-as.factor(IDdata$nwaves)
freq(IDdata$nwaves)



# Part 1.2 - Graphical Display of the Data ------------------

### Drop any missing values of 'wdpes'  
ELSA_long_new <- ELSA_long_new %>% filter(!is.na(wpdes))


### Bar plot showing number of respondents who are without depression at each wave ###
ELSA_bar <- ELSA_long_new %>%
  group_by(time,wpdes) %>%
  filter(!is.na(depressed),!is.na(wpdes)) %>%
  summarise(prop = sum(depressed)/n()) %>%
  ungroup() %>%
  group_by(time) %>%
  mutate(prop.of.dep = prop/sum(prop))

ggplot(data=ELSA_bar, mapping = aes(x = time, 
                                    y = prop.of.dep, 
                                    fill = wpdes, 
                                    color=wpdes,
                                    group = wpdes)) + 
  facet_wrap(~ wpdes) +
  geom_bar(position = 'dodge', stat = "identity") 



# Part 2 - Linear analysis ------------------

### Part 2.1 - Random effects

### Sort the data by idauniq and time using arrange
ELSA_long_new <- ELSA_long_new %>%
  arrange(ELSA_long_new, idauniq, time)


### Change 'depressed' to factor
ELSA_long_new$depressed <- as.factor(ELSA_long_new$depressed)


### Run a random effects regression for depression, including predictors for employment, education, age, sex and time
## NOTE: glmer can take a long time to run
random1 <- glmer(depressed ~ 1                                                       # outcome: depressed (0, 1)
                + factor(wpdes) + factor(edqual) + indager + factor(dhsex) + time    # predictors: employment, education, age, sex and time;
                + ( 1 | idauniq),                                                    # clustering by ID
               data=ELSA_long_new,                                                   # data frame
               family = binomial (link = "logit"),                                   # binomial (binary exposure)
               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)) # speeds up processing                                


### Look at model table
summary(random1) 


### We can display this in a neater way using stargazer
stargazer(random1, apply.coef = exp, type="text", title="Results", align=TRUE) # we include apply.coef = exp to exponentiate the coeficcients so that they are odds ratios 


### Calculate ICC ###
performance::icc(random1)

# Part 2.2 - Generalized Estimating Equations ------------------
### Run a GEE model with the correlation structure set to 'exchangeable'
gee_1 <- gee(depressed ~ 1                                                    # outcome: depressed (0, 1)
             + factor(wpdes) + factor(edqual) + indager + factor(dhsex) + time, # predictors: employment, education, age, sex and time;
             data = ELSA_long_new, 
             id = idauniq, 
             family = binomial (link = "logit"),
             na.action = na.omit,
             corstr = "exchangeable")

### We can display this in a neater way using stargazer
stargazer(gee_1, type="text", title="Results", align=TRUE) 


### Change the correlation structure to 'unstructured'
gee_2 <- gee(depressed ~ 1                                                    # outcome: depressed (0, 1)
             + factor(wpdes) + factor(edqual) + indager + factor(dhsex) + time, # predictors: employment, education, age, sex and time;
             data = ELSA_long_new, 
             id = idauniq, 
             family = binomial (link = "logit"),
             na.action = na.omit,
             corstr = "unstructured")


### We can display this in a neater way using stargazer
stargazer(gee_2, type="text", title="Results", align=TRUE) 


### Display results ###
display <- stargazer(random1, gee_1, gee_2, type="text", title="Results", align=TRUE)


# Part 3 - Random-coefficient logistic regression and growth model -------------------------------

  
### Fit a logistic regression with random effects for education
## NOTE: glmer can take a long time to run
random2 <- glmer(depressed ~ 1                                                         # outcome: depressed (0, 1)
                 + factor(wpdes) + factor(edqual) + indager + factor(dhsex) + time     # predictors: employment, education, age, sex and time;
                 + ( edqual | idauniq),                                                # growth curve for education
                 data=ELSA_long_new,                                                   # data frame
                 family = binomial (link = "logit"),                                   # binomial (binary exposure)
                 control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)) # speeds up processing


### Look at model table
summary(random2)


### We can display this in a neater way using stargazer
stargazer(random2, apply.coef = exp, type="text", title="Results", align=TRUE) # we include apply.coef = exp to exponentiate the coeficcients so that they are odds ratios 


### Fit a logistic regression with random effects for time
## NOTE: glmer can take a long time to run
random3 <- glmer(depressed ~ 1                                                            # outcome: depressed (0, 1)
                    + factor(wpdes) + factor(edqual) + indager + factor(dhsex) + time     # predictors: employment, education, age, sex and time;
                    + ( time | idauniq),                                                  # growth curve for education
                    data=ELSA_long_new,                                                   # data frame
                    family = binomial (link = "logit"),                                   # binomial (binary exposure)
                    control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)) # speeds up processing


### Look at model table
summary(random3)


### We can display this in a neater way using stargazer
stargazer(random3, apply.coef = exp, type="text", title="Results", align=TRUE) # we include apply.coef = exp to exponentiate the coeficcients so that they are odds ratios 


### Fit a logistic regression with random effects for time, with an interaction between education and time
## NOTE: glmer can take a long time to run
random3 <- glmer(depressed ~ 1                                                            # outcome: depressed (0, 1)
                 + factor(wpdes) + indager + factor(dhsex) + edqual:time               # predictors: employment, age, sex and education*time;
                 + ( time | idauniq),                                                  # growth curve for education
                 data=ELSA_long_new,                                                   # data frame
                 family = binomial (link = "logit"),                                   # binomial (binary exposure)
                 control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)) # speeds up processing


### Look at model table
summary(random3)


### We can display this in a neater way using stargazer
stargazer(random3, apply.coef = exp, type="text", title="Results", align=TRUE) # we include apply.coef = exp to exponentiate the coeficcients so that they are odds ratios 


# Optional - Linear analyses ------------------

### Run a linear regression with continuous regression
lin1 <- lmer(depression ~ 1                     # outcome: depression
             + factor(wpdes) + factor(edqual) 
             + indager + factor(dhsex) + time
             + (1 |idauniq)                    # cluster by id
           data = ELSA_long_new,               # data frame                                 
           REML = FALSE)                       # same as above 


### We can display this in a neater way using stargazer
stargazer(lin1, type="text", title="Results", align=TRUE)  

### Run a GEE model with the covariance structure set to 'indendence'
gee_3 <- gee(depression ~ 1                                                    # outcome: depression
             + factor(wpdes) + factor(edqual) 
             + indager + factor(dhsex) + time, # predictors: employment, education, age, sex and time;
             data = ELSA_long_new, 
             id = idauniq, 
             family = gaussian,
             na.action = na.omit,
             corstr = "independence")

### We can display this in a neater way using stargazer
stargazer(gee3, type="text", title="Results", align=TRUE)  


### Run a GEE model with the covariance structure set to 'exchangeable'
gee_4 <- gee(depression ~ 1                                                    # outcome: depression
             + factor(wpdes) + factor(edqual) 
             + indager + factor(dhsex) + time, # predictors: employment, education, age, sex and time;
             data = ELSA_long_new, 
             id = idauniq, 
             family = gaussian,
             na.action = na.omit,
             corstr = "exchangeable")


### We can display this in a neater way using stargazer
stargazer(gee4, type="text", title="Results", align=TRUE)  

##### FINISHED #####