
################################################################
###               Longitudinal Data Analysis                 ###
###     Session 1 - Mixed Models for Continuous Outcomes     ###
###                      15/03/2021                          ###
################################################################



# Set working directory, read in data and load packages ----------

### Set working directory
setwd("") # Insert path name into the speech marks e.g. ("pathname")


### Install and load relevant packages
install.packages("dplyr")         # data manipulation (NB: may need compilation)
install.packages("effects")       # effects from a fitted model
install.packages("haven")         # reading in Stata files
install.packages("Hmisc")         # summary functions
install.packages("lme4")          # regression functions
install.packages("lmtest")        # likelihood ratio tests
install.packages("psych")         # descriptive statistics
install.packages("rlang")         # tidyverse features
#install.packages("summarytools")  # summarising Stata files
install.packages("tidyr")         # tidying data
install.packages("tidyverse")     # loading the tidyverse


### Try to install these packages
install.packages("sjstats") # data/variable transformations
install.packages("Hmisc")    # summary functions


## If you have trouble installing them this way then use the commands below
devtools::install_github("easystats/effectsize")     # required for 'sjstats' package
devtools::install_github("strengejacke/sjstats")     # data/variable transformations
devtools::install_github("ge11232002/latticeExtra")  # required for 'Hmisc' package
devtools::install_github("harrelfe/Hmisc")           # summary functions


### Load relevant packages ###
library(dplyr) 
library(effects) 
library(effectsize)
library(haven)
library(Hmisc)
library(latticeExtra)
library(lme4)
library(lmtest)
library(psych)
library(rlang)
library(sjstats)
#library(summarytools)
library(tidyr)
library(tidyverse)




# Part 1 - Read in the data ---------------------------------------------------

### Read in ELSA data
ELSA_wave1 <- read_dta("Wave_1_elsa.dta")
ELSA_wave2 <- read_dta("Wave_2_elsa.dta")
ELSA_wave3 <- read_dta("Wave_3_elsa.dta")
ELSA_wave4 <- read_dta("Wave_4_elsa.dta")
ELSA_wave5 <- read_dta("Wave_5_elsa.dta")
ELSA_wave6 <- read_dta("Wave_6_elsa.dta")
ELSA_wave7 <- read_dta("Wave_7_elsa.dta")



### Look at one of the datasets in more detail
## Look at the first 10 rows ##
head(ELSA_wave1)


## Check column names 
colnames(ELSA_wave1)


## Check class of variables ##
lapply(ELSA_wave1, class)


# Part 2 - Preparing and merging the data ------------------

### Drop participants who did not have a valid memory score (cflisen0) at baseline
ELSA_wave1_new <- ELSA_wave1 %>% filter(!is.na(cflisen0)) 


### Recode sex from "Female" and "Male" to 1 and 0 respectively
ELSA_wave1_new$dhsex <- ifelse(ELSA_wave1_new$dhsex == 1, 0, 1)


### Set 'dhsex' to interger
ELSA_wave1_new$dhsex <- as.integer(ELSA_wave1_new$dhsex)


### Make sure that the recoding has been successful
head(ELSA_wave1_new$dhsex)


### At this stage we should have a 'wide' dataset with N = 11,035 rows
### Check the length of the new dataframe
length(ELSA_wave1_new$idauniq)


### Keep only the variables that we need in new dataframes using the 'select' command
ELSA_wave1_new <- ELSA_wave1_new %>% 
  select(idauniq,cflisen0,QoLscore0,dhsex) 

ELSA_wave2_new <- ELSA_wave2 %>% 
  select(idauniq,cflisen1,QoLscore1) 

ELSA_wave3_new <- ELSA_wave3 %>% 
  select(idauniq,cflisen2,QoLscore2) 

ELSA_wave4_new <- ELSA_wave4 %>% 
  select(idauniq,cflisen3,QoLscore3)

ELSA_wave5_new <- ELSA_wave5 %>% 
  select(idauniq,cflisen4,QoLscore4)

ELSA_wave6_new <- ELSA_wave6 %>% 
  select(idauniq,cflisen5,QoLscore5)

ELSA_wave7_new <- ELSA_wave7 %>% 
  select(idauniq,cflisen6,QoLscore6)


### Merge the dataframes to create a wide format dataframe using the 'left_join' command
ELSA_wide <- left_join(ELSA_wave1_new, ELSA_wave2_new, by='idauniq') %>%
  left_join(.,ELSA_wave3_new , by='idauniq') %>% # pastes the output on the left hand side
  left_join(.,ELSA_wave4_new , by='idauniq') %>%
  left_join(.,ELSA_wave5_new , by='idauniq') %>%
  left_join(.,ELSA_wave6_new , by='idauniq') %>%
  left_join(.,ELSA_wave7_new , by='idauniq')


### Look at the new dataframe
View(ELSA_wide)



# Part 3 - Descriptive Statistics and Reshaping the Data ------------------

### Summarise wave specific values of 'QoLscore*' variable
describe(ELSA_wide$QoLscore0)
describe(ELSA_wide$QoLscore1)
describe(ELSA_wide$QoLscore2)
describe(ELSA_wide$QoLscore3)
describe(ELSA_wide$QoLscore4)
describe(ELSA_wide$QoLscore5)
describe(ELSA_wide$QoLscore6)


### Create a correlation matrix of 'QoLscore*' variable 
cor <- ELSA_wide %>%
  select(QoLscore0,
         QoLscore1,
         QoLscore2,
         QoLscore3,
         QoLscore4,
         QoLscore5,
         QoLscore6) %>% 
  as.matrix() %>%
  rcorr(type = "pearson")

### Use '[X]' to access objects in lists
cor[1]    # correlation matrix
cor[2]    # number of observations


### Reorder the variables before reshaping
ELSA_wide_new <- ELSA_wide %>% 
  relocate(starts_with(c("cflisen","QoLscore")), .after = idauniq)


### View the new wide dataframe to make sure that the reordering was successful
View(ELSA_wide_new)


### Reshape the data from wide to long format
ELSA_long <- reshape(as.data.frame(ELSA_wide_new),                                  
                     idvar = "idauniq", 
                     direction = "long",
                     varying = list(c(2:8),c(9:15)),
                     timevar = "time",
                     v.names=c("cflisen","QoLscore"))


### Check the number of records in this long dataset
ELSA_long %>% summarise(r=n())


### View the new long dataframe
View(ELSA_long)


### Drop any missing values of 'QoLscore' and 'cflisen'
ELSA_long <- ELSA_long %>% filter(!is.na(QoLscore))
ELSA_long <- ELSA_long %>% filter(!is.na(cflisen))


### Check the number of records
ELSA_long %>% summarise(n=n()) 


### Sort the data by idauniq and time using arrange
ELSA_long <- ELSA_long %>%
  arrange(ELSA_long, idauniq, time)


### Look at the first couple of rows
head(ELSA_long)


### Summarise memory scores
describe(ELSA_long$cflisen)


### Examine distribution of QoL scores
describe(ELSA_long$QoLscore)


### Create a new variable to summarise the number of waves that participants were present
ELSA_long <- ELSA_long %>%
  group_by(idauniq) %>%
  mutate(nwaves = n()) # mutate creates a new variable ('nwaves') that summarises how many waves per individual


### View the dataframe to make sure that this has been successful
View(ELSA_long)


### Create an idauniq-level dataset (called IDdata) using same principle as tag in Stata 
## (NB: tag picks one record per cluster (i.e. individual) and is useful for when you are wanting to 
## summarise records in clustered data)
IDdata <- ELSA_long %>%
  group_by(idauniq) %>%
  arrange(time) %>%
  filter(row_number()==1)  # takes the data from the first row only

IDdata$nwaves<-as.factor(IDdata$nwaves)
freq(IDdata$nwaves)

# Part 4 - Random Effects Model - Varying Intercepts --------

### Fit null model ("randint") with random effects
randint <- lmer(QoLscore ~ 1      # outcome: QoL; '1' indicates that you want a model with only an intercept
                + (1|idauniq),    # grouping variable: idauniq; '1|' adds that there is also a random effect for the intercept (so it varies across individuals)
                data = ELSA_long, # data frame
                REML = FALSE)     # should the estimates be chosen to optimise the restricted maximum likelihood (REML) criterion? FALSE means that it optimises log-liklihood instead (when dataset is large enough that the two are equivalent and can use lmtest.


### Look at model table 
summary(randint)


### Now add the intercept and residual values (taken from "random effects" section in randint summary table above) into ICC equation 
ICC <- (55.70)/(55.70 + 23.36)
ICC


### Add fixed effects for 'cflisen' and 'sex' into the null random effects model
M1 <- lmer(QoLscore ~ 1                     # outcome: QoL score
           + (1|idauniq) + cflisen + dhsex, # grouping variable: idauniq; fixed effects: memory and sex 
           data = ELSA_long,                # data frame
           REML = FALSE)                    # same as above


### Look at model table
summary(M1)


### Plot the pop.average trajectory (model predicted average for QoL based on observed predictors and estimated fixed effects)
## Create new column with predicted values
ELSA_long$yhat <- predict(M1, re.form = NA)  


### Take a look at these values 
summary(ELSA_long$yhat)


### Create reduced dataframe that only contains 'idauniq', 'dhsex', 'cflisen' and 'yhat'
fitted.data <- subset(ELSA_long, select = c("idauniq", "dhsex", "cflisen", "yhat"))


### Take a look at the first couple of rows of this new dataframe
head(fitted.data)


### Plot the fitted data
with(fitted.data,
     {plot(cflisen, yhat,
           main = "Random intercept model: main effects",
           xlab="cflisen",ylab="QoL");
       lines(cflisen[dhsex==0], yhat[dhsex==0],type = "l", col="blue")
       lines(cflisen[dhsex==1], yhat[dhsex==1],type = "l", col="red")
       legend("topleft", c("Male","Female"), lty = c(1,1), col = c("blue","red"))})




# Part 4 - Random Effects Model - Varying Intercepts and Slopes --------

### The commands that follow show you how to: 
     # - fit the model; 
     # - store the estimates; 
     # - save the predicted values of the random intercepts and slopes; 
     # - obtain histograms; 
     # - and run a LR test comparing the models with (M2) and without random slopes (M1).


### Add a random slope to the model and allow estimation of the correlation between randomly varying intercepts and slopes
M2 <- lmer(QoLscore ~ 1            # outcome: QoL
           + (1 + cflisen|idauniq) # added random variability in the slope for memory ("cflisen") (i.e. lets individuals' cognitive function vary)
           + cflisen + dhsex,      # fixed effects: cflisen and dhsex
           data = ELSA_long,       # data frame                                 
           REML = FALSE)           # same as above                                    

# NOTE: Ignore warning messages


### Look at model table
summary(M2)


### Run a Likelihood ratio test to compare the models 
lrtest(M2, M1) 


### Create a new variables for the predicted values and random effects
ELSA_long$person.specific <- fitted(M2)     


### Fitted values: person-specific
ELSA_long$pop.average <- predict(M2, re.form = NA) 


### Look at these new variables
head(ELSA_long)


### Create new dataframe containing random effect
random.effects <- as.data.frame(ranef(M2)$idauniq)    


### Look at the names of the variables in this new dataframe
names(random.effects)


### Add idauniq column
random.effects <- cbind(idauniq = rownames(random.effects), random.effects)


### Check that this was successful
names(random.effects)
head(random.effects)


### Summarise the intercept ("(Intercept)") column
summary(random.effects$"(Intercept)")    


### Summarise the slope ("cflisen") column
summary(random.effects$"cflisen")  


### Plot a histogram of the random effects
## Plot density of the intercept
hist(random.effects$"(Intercept)", 
     main="Histogram for random intercepts", 
     xlab="Predicted random intercepts", 
     border="blue", 
     col="gold", 
     xlim=c(-30,30), 
     las=1, breaks=30, prob = TRUE)
lines(density(random.effects$"(Intercept)"))


### Plot density of the slope
hist(random.effects$"cflisen", 
     main="Histogram for random slope", 
     xlab="Predicted random slopes", 
     border="blue", 
     col="gold", 
     xlim=c(-1,1), 
     las=1, breaks=30, prob = TRUE)
lines(density(random.effects$"cflisen"))



# OPTIONAL - Plotting the Person-Specific Trajectories ----------------------

### To keep things manageable, we will focus on just nine participants. 

### Create a new dataframe with a subset of nine participants
ELSA_9IDs <-subset(ELSA_long,idauniq==104827|idauniq==104858|idauniq==107495|idauniq==108668|idauniq==111069|idauniq==113025|idauniq==118249|idauniq==118796|idauniq==119758)
head(ELSA_9IDs)


### Plot trajectories for these nine participants using the predicted values we obtained earlier
myplot <- ggplot(data=ELSA_9IDs,aes(cflisen)) + 
  geom_line(aes(y=pop.average,colour="pop.average")) + 
  geom_line(aes(y=person.specific,colour="person.specific trajectory")) + 
  facet_wrap(~idauniq) +
  scale_colour_manual(values=c("red","blue"))

myplot


### List the residuals for intercept and slope
mydata <- subset(random.effects,idauniq==104827|idauniq==104858|idauniq==107495|idauniq==108668|
                   idauniq==111069|idauniq==113025|idauniq==118249|idauniq==118796|idauniq==119758)

mydata




##### FINISHED #####
