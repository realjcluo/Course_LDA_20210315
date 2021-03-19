
################################################################
###               Longitudinal Data Analysis                 ###
###            Session 4 - Event History Analysis            ###
###                      18/03/2021                          ###
################################################################


# Set working directory, read in data and load packages ----------

### Set working directory
setwd("") # Insert path name into the speech marks e.g. ("pathname")


### Install and load relevant packages
## Install any packages that are not already installed 
install.packages("broom")        # converts objects into tidy tibbles (tidyverse)
install.packages("cmprsk")       # analysis of competing risks (NB: does not need compilation)
install.packages("haven")        # reading in Stata files
install.packages("labelled")     # manipulating labelled data
install.packages("lubridate")    # for dealing with dates
install.packages("summarytools") # summarising Stata files
install.packages("survival")     # running survival analysis
install.packages("survminer")    # drawing survival curves
install.packages("tidyverse")    # loading the tidyverse


### Load relevant packages
library(broom) 
library(cmprsk)
library(haven) 
library(labelled) 
library(lubridate)
library(summarytools) 
library(survival) 
library(survminer)
library(tidyverse)



# Part 1 - Read in the data -----------------------------------------

### Recode variables with missing values in the Stata dataset (coded as negative numbers) as (NA) when importing
remove_na <- function(var){
  lbls <- attr(var, "labels")
  lbls <- lbls[lbls >= 0]
  if (length(lbls)==0){
    attr(var, "labels") <- NULL
    class(var) <- typeof(var)
  } else attr(var, "labels") <- lbls
  if (is.numeric(var)){
    atrb <- attributes(var)
    var <- ifelse(var < 0, NA, var)
    attributes(var) <- atrb
  } 
  
  var
}


### Haven imports variables with value labels, we need to convert these from as_factor() to factor
elsa_raw <- read_dta("EHApractical.dta") %>%
  mutate(across(matches("^indager"), zap_labels)) %>%
  map_dfc(remove_na) %>%
  as_factor() %>%
  zap_formats()

look_for(elsa_raw)


# Part 2 - Prepare the data for survival analyses ------------------

### Clean the data ready for survival analyses
elsa <- elsa_raw %>%
  mutate(entry_y = iintdty1,
         entry_m = iintdtm1,
         age_w1 = indager1,
         pain = factor(pain, 1:2, c("No", "Yes")),
         fall_w1 = case_when(hefla1 == "Yes" ~ 1,
                             hefla1 == "No" ~ 0),
         entry_date = make_date(entry_y, entry_m, 15),
         last_date = ifelse(!is.na(dody) & !is.na(dodm),
                            paste(dody, dodm, 15, sep = "-"),
                            "2011-12-15") %>% 
           as.Date(),
         survtime = (last_date - entry_date)/365.25, # Create variable survtime (time (in days) between first interview and death/end of follow-up)
         survtime = as.numeric(survtime)) %>%
  filter(survtime > 0)


### Save the cleaned dataframe
save(elsa, file = "elsa.Rdata")



# Part 3 - Descriptive Statistics -----------------------------------------

### Use the functions from 'summarytools' to get descriptive statistics on survival
## Use 'descr()' for continuous variables
descr(elsa$survtime)

## Use 'freq()' for categorical variables
freq(elsa$dead)


### Calculate the incidence rate (i.e. total deaths over total time exposed)
sum(elsa$survtime)

sum(elsa$dead)/sum(elsa$survtime)


### Plot a Kaplan-Meier estimate of the survival function by report of falls at wave 1
ggsurvplot(
  fit = survfit(Surv(survtime, dead) ~ fall_w1, data = elsa), 
  xlab = "Days", 
  ylab = "Overall survival probability")



# Part 4 - Cox Model for Single Event -------------------------------------

### Fit a Cox proportional hazards model to the data with sex, age and fall at wave 1 as covariates
coxph(Surv(survtime, dead) 
      ~ indsex + age_w1 + fall_w1, 
      data = elsa, method = "breslow") %>%

summary()


### Add an interaction term between sex and falls '(indsex:fall_w1)' to see if the association between falls and mortality differs by sex
coxph(Surv(survtime, dead) ~ indsex + age_w1 + fall_w1 + indsex:fall_w1,
      data = elsa, method = "breslow") %>%
  summary()



# Part 5 - Checking Proportional Hazards Assumption -----------------------

### Check the proportional hazards assumption 
fit <- survfit(coxph(Surv(survtime, dead) ~ strata(fall_w1) + indsex + age_w1, 
                     data = elsa, method = "breslow"))

ggsurvplot(fit, elsa, fun = "cloglog")


### We can also carry out a formal test of the proportional hazards assumption
coxph(Surv(survtime, dead) ~ indsex + age_w1 + fall_w1 + tt(fall_w1),
      data = elsa, method = "breslow",
      tt = function(x, t, ...) x * log(t)) %>%
  summary()


### Run the Cox model without stratification using the funtion 'coz.zph()' to formally test PH assumption for each independent variable
coxph(Surv(survtime, dead) ~ indsex + age_w1 + fall_w1, 
             data = elsa, method = "breslow") %>%
  cox.zph()


summary(fit)
cox.zph(fit, transform = "km")


### Conduct separate analyses for males and females using the 'filter()' function
## Males
coxph(Surv(survtime, dead) ~ age_w1 + fall_w1, 
      data = elsa %>%
        filter(indsex == "Male"), 
      method = "breslow") %>%
  summary()


### Females
coxph(Surv(survtime, dead) ~ age_w1 + fall_w1, 
      data = elsa %>%
        filter(indsex == "Female"), 
      method = "breslow") %>%
  summary()


### Alternatively we can include sex as a 'strata' variable which allows different baseline hazard functions for men and women (BUT assumes proportional hazards for the other covariates)
coxph(Surv(survtime, dead) ~ age_w1 + fall_w1 + strata(indsex), 
      data = elsa, method = "breslow") %>%
  summary()


### We can also expand on this by including an interaction term between sex and fall at wave 1
coxph(Surv(survtime, dead) ~ age_w1 + fall_w1 + strata(indsex) + fall_w1:indsex, 
      data = elsa, method = "breslow") %>%
  summary()



# OPTIONAL - Plotting Survival Function -----------------------------------

### To plot the estimated survival function we can place a 'coxph()' regression object into the 'ggsurvplot()' function
fit <- coxph(Surv(survtime, dead) ~ indsex + age_w1 + fall_w1, 
             data = elsa, method = "breslow")

## NB: remember that the default is the mean of all covariates)

ggsurvplot(survfit(fit), 
           data = elsa,
           color = "#2E9FDF",
           ggtheme = theme_minimal())


### To compare the plot for different groups, we need to predict survival probabilities for each group using 'tidy()' function from the 'broom' package
## Create new dataframe that contains covariate information for combinations of sex, falls and age at wave 1 (age 65)
new_df <- expand_grid(fall_w1 = unique(elsa$fall_w1), 
                      indsex = unique(elsa$indsex), 
                      age_w1 = 65) %>%
  drop_na() # drop any missing values

new_df


### To get survival probabilities for women with no falls at wave 1 we can run
survfit(fit, newdata=slice(new_df,1)) %>%
  tidy() %>%
  slice(1:5) # for the first five rows


### We can create a function (as we will use it multiple times) and add a variable 'tag' to clarify who the predicted values represent
get_survs <- function(index, tag){
  survfit(fit, newdata = slice(new_df, index)) %>% 
    tidy() %>%
    mutate(tag = tag)
}


### Use the data for females by fall at wave 1 and plot this using 'ggplot()'
p <- bind_rows(get_survs(1, "Female, No Fall"),
               get_survs(3, "Female, Fall")) %>%
  ggplot() +
  aes(x = time, y = estimate, 
      ymin = conf.low, ymax = conf.high,
      color = tag, fill = tag) +
  geom_ribbon(color = NA, alpha = 0.2) +
  geom_line()

p


### We can make this plot EVEN nicer by adding further elements
p +
  labs(x = "Time", y = "Survival Probability",
       color = NULL, fill = NULL) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = c(.85, .85))



# Part 6 - Data Structure for Recurrent Events --------------------------

### Select a subsample of the ELSA dataset who have data on falls at wave1 and reshape the data into long format
elsa_long <- elsa %>%
  filter(age_w1 == 65,
         !is.na(fall_w1)) %>% # keep subsample
  select(idauniq, entry_date, dead, survtime, # keep relevant rows
         indsex, pain,
         matches("^(iintdt|hefla|indager)")) %>%
  pivot_longer(matches("^(iintdt|hefla|indager)"), # reshape to long
               names_to = c(".value", "wave"),
               names_pattern = "(.*)(.)") %>%
  mutate(wave = as.numeric(wave), # add new fall variables
         int_date = make_date(iintdty, iintdtm, 15),
         fall = ifelse(hefla == "Yes", 1, 0),
         fall_time = as.numeric(int_date - entry_date)/365.25) %>%
  select(-matches("iint")) %>%
  drop_na(fall, fall_time) %>%
  filter(fall_time > 0) %>%
  arrange(idauniq, wave) %>%
  group_by(idauniq) %>% # group to compute variables by iduniq
  filter(fall != 0 | row_number() == n()) %>%
  mutate(start = ifelse(row_number() == 1, 0, lag(fall_time)), # time of previous wave
         num_fall = sum(fall)) %>%
  ungroup()


### Save this new long format dataframe
save(elsa_long, file = "elsa_long.Rdata")


### The distribution of falls is
elsa_long %>%
  select(idauniq, num_fall) %>%
  distinct() %>% # keep one record per idauniq
  count(num_fall)



# Part 7 - Cox Model for Recurrent Events -------------------------------

### Fit a Cox model ignoring clustering of falls within individuals
coxph(Surv(fall_time, fall) ~ indsex + pain, 
      data = elsa_long, method = "breslow") %>%
  summary()


### Add 'cluster()' to indicate that observations are clustered at the 'idauniq' level
coxph(Surv(start, fall_time, fall) ~ indsex + pain + cluster(idauniq),
      data = elsa_long, method = "breslow") %>%
  summary()


### Add a random effect for fraility() term to take into account fall clustering
coxph(Surv(start, fall_time, fall) ~ indsex + pain + frailty(idauniq),
      data = elsa_long, method = "breslow") %>%
  summary()



# Questions ---------------------------------------------------------------

elsa_long %>%
  select(idauniq) %>%
  distinct() %>%
  count()

elsa_long %>%
  count(fall)



# Part 8 - Falls History ------------------------------------------------


### Generate an idicator variable 'prevfall = 1' if individual has had a previous fall and '0' if not reported a previous fall
elsa_long <- elsa_long %>%
  group_by(idauniq) %>%
  mutate(prev_fall = ifelse(row_number() == 1, 0, lag(fall))) %>%
  ungroup()


### Add this variable to a Cox model
fit1 <- coxph(Surv(start, fall_time, fall) ~ indsex + pain + prev_fall + frailty(idauniq),
              data = elsa_long, method = "breslow")
summary(fit1)


### Include an interaction between pain and falls history
fit2 <- coxph(Surv(start, fall_time, fall) ~ indsex + pain*prev_fall + frailty(idauniq),
              data = elsa_long, method = "breslow")
summary(fit2)



# OPTIONAL - Competing Risks Model ----------------------------------------

### Create two new numeric variables for pain and sex and a new time and event variable to reflect the different event types (0 = censored, 1 = fall, 2 = died)
elsa_short <- elsa_long %>%
  filter(prev_fall != 1) %>%
  mutate(fall_comp = ifelse(dead == 1 & fall == 0, survtime, fall_time),
         fall_type = ifelse(dead == 1 & fall == 0, 2, fall),
         female = ifelse(indsex == "Female", 1, 0),
         in_pain = ifelse(pain == "Yes", 1, 0))


### Fit a standard Cox model for first fall (ignoring death as a competing risk)
coxph(Surv(fall_time, fall) ~ indsex + pain,
      data = elsa_short, method = "breslow") %>%
  summary()


### Fit a competing risks model using the 'crr()' function from the 'cmprsk' package
crr(ftime = elsa_short$fall_comp,
    fstatus = elsa_short$fall_type,
    cov1 = elsa_short %>%
      select(female, in_pain)) %>%
  summary()



##### FINISHED #####
