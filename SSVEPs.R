library(ggplot2)
library(readxl)
library(stringr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(BayesFactor)
library(ggpubr)
library(rstatix)
library(reshape2)
library(emmeans)
library(lsr)

#-------------------data preparation and preprocessing-------------------------#

#importing data
excel_sheets("C:/Users/blu12/Downloads/Flinders work/Project 1 IB/For Brendan/For Brendan/actual data/SubtractiveOutput_PIDbyHz.xlsx")

#four sheets to the data - "Bin1" "Bin2" "Bin3" "Bin4" - where each refers to different trial, so we'll extract each as separate dataframes
#data.frame(colnames(training)) #gives us the index number of all columns (in case this is needed)
training_1 <- read_excel("C:/Users/blu12/Downloads/Flinders work/Project 1 IB/For Brendan/For Brendan/actual data/SubtractiveOutput_PIDbyHz.xlsx", sheet = 1)
critical_trial_1 <- read_excel("C:/Users/blu12/Downloads/Flinders work/Project 1 IB/For Brendan/For Brendan/actual data/SubtractiveOutput_PIDbyHz.xlsx", sheet = 2)
DA_trial_1 <- read_excel("C:/Users/blu12/Downloads/Flinders work/Project 1 IB/For Brendan/For Brendan/actual data/SubtractiveOutput_PIDbyHz.xlsx", sheet = 3)
FA_trial_1 <- read_excel("C:/Users/blu12/Downloads/Flinders work/Project 1 IB/For Brendan/For Brendan/actual data/SubtractiveOutput_PIDbyHz.xlsx",  sheet = 4)


"
Plan: identify columns correspondent with critical frequencies; extract only those to a new dataframe

Critical frequencies:
15hz = 15.0083752093802
17hz = 17.1524288107203
20hz = 20.0111669458403
24hz = 24.0134003350084
"

#R is having issue with matching decimals to the excel spreadsheets (presumably something to do with rounding differences).
#That's OK we can just use starts_with and take enough decimal places to identify the correct column >>>
hz_15 <- c('15.008375')
hz_17 <- c('17.152428')
hz_20 <- c('20.011166')
hz_24 <- c('24.013400')

vars_we_need <- c("Hz", hz_15, hz_17, hz_20, hz_24)
new_names <- c("ID", "15hz", "17hz", "20hz", "24hz")

#create new df from columns we need in df1....
training_2 <- training_1 %>% select(starts_with(c(vars_we_need)))
critical_trial_2 <- critical_trial_1 %>% select(starts_with(c(vars_we_need)))
DA_trial_2 <- DA_trial_1 %>% select(starts_with(c(vars_we_need)))
FA_trial_2 <- FA_trial_1 %>% select(starts_with(c(vars_we_need)))

#...then rename
names(training_2)[-1] <- paste("train", new_names[-1], sep = "_")
names(critical_trial_2)[-1] <- paste("CT", new_names[-1], sep = "_")
names(DA_trial_2)[-1] <- paste("DA", new_names[-1], sep = "_")
names(FA_trial_2)[-1] <- paste("FA", new_names[-1], sep = "_")
#this renaming proc adds to the variable which trial it belongs to, as this will be needed for later when we merge the dfs together

#now to merge by ID variable...
list_dfs <- list(training_2, critical_trial_2, DA_trial_2, FA_trial_2)
all_trial_data <- list_dfs %>% reduce(full_join, by = "Hz") #the ID variable is "Hz" from the data spreadsheet. We will fix this later.
all_trial_data <- all_trial_data[-c(1),]

all_trial_data$Hz <- as.numeric(all_trial_data$Hz) #before merging with other data, we need to coerce ID variable from string to numeric
names(all_trial_data)[1] <- "subject" #to ensure ID variables have same name

#next: merge with other data (age, gender, handedness, counterbalance condition, IB)
testing_data <- read_excel("C:/Users/blu12/Downloads/Flinders work/Project 1 IB/data/SSVEP_data.xlsx", na = "NA")
testing_data_2 <- testing_data[,1:8] #new dataset with only the variables we need

list_dfs_2 <- list(testing_data_2, all_trial_data)
SSVEP_dataset <- list_dfs_2 %>% reduce(full_join, by = "subject") #hoorah!


#-------counterbalancing
# [1 1 1] = 24 hz target, 15 hz distractor, 17.14hz CS
# [2 1 1] = 15 hz target, 24 hz distractor, 20 hz CS

SSVEP_dataset$cb <- case_when(
  SSVEP_dataset$counter_balance == "(1, 1, 1)" ~ "1",
  TRUE ~ "2")

"
plan:
- if subject is 1, take only their 24, 15, and 17 ; and label them as 24 = target, 15 = distractor, 17 = CS
- if subject is 2, take only their 15, 24, and 20 ; and label them as 15 = target, 24 = distractor, 20 = CS

1. divide dataset into two datasets (1's, 2's)
2. run a mutate
3. rejoin datasets
"

#1.
SSVEP_dataset_1 <- SSVEP_dataset[SSVEP_dataset$cb == 1,]
SSVEP_dataset_2 <- SSVEP_dataset[SSVEP_dataset$cb == 2,]

#2.
SSVEP_dataset_1_rev <- 
  SSVEP_dataset_1 %>%
  mutate(train_target = train_24hz,
         train_distractor = train_15hz,
         CT_target = CT_24hz,
         CT_distractor = CT_15hz,
         CT_CS = CT_17hz,
         DA_target = DA_24hz,
         DA_distractor = DA_15hz,
         DA_CS = DA_17hz,
         FA_target = FA_24hz,
         FA_distractor = FA_15hz,
         FA_CS =FA_17hz)

SSVEP_dataset_2_rev <- 
  SSVEP_dataset_2 %>%
  mutate(train_target = train_15hz,
         train_distractor = train_24hz,
         CT_target = CT_15hz,
         CT_distractor = CT_24hz,
         CT_CS = CT_20hz,
         DA_target = DA_15hz,
         DA_distractor = DA_24hz,
         DA_CS = DA_20hz,
         FA_target = FA_15hz,
         FA_distractor = FA_24hz,
         FA_CS = FA_20hz)

#3.
SSVEP_data_merged <- rbind(SSVEP_dataset_1_rev, SSVEP_dataset_2_rev)

vars_to_keep <- c("subject", "gender", "age", "handedness",  "cb", "CT_notice", "DA_notice", "FA_notice",
                  "train_target", "train_distractor", "CT_target", "CT_distractor", "CT_CS", "DA_target",
                  "DA_distractor", "DA_CS", "FA_target", "FA_distractor", "FA_CS")

SSVEP_data_merged <- SSVEP_data_merged[,vars_to_keep]

SSVEP_data_merged[,9:19] <- lapply(SSVEP_data_merged[,9:19], as.numeric)
str(SSVEP_data_merged)


SSVEP_dataset_2 <- SSVEP_data_merged #this is just done to retain consistency between scripts
#done!


#composite CT_notice + DA_notice variable for analysis of trials 1-4
SSVEP_dataset_2$comp_notice <- case_when(
  SSVEP_dataset_2$CT_notice == "Y" | SSVEP_dataset_2$DA_notice == "Y" ~ "Y",
  TRUE ~ "N")

#factorising
factor_variables <- c('gender', 'handedness', 'CT_notice', 'DA_notice', 'FA_notice', 'comp_notice', 'cb')
SSVEP_dataset_2[,factor_variables] <- lapply(SSVEP_dataset_2[,factor_variables], factor)
str(SSVEP_dataset_2)

#remove FA IB participants
SSVEP_dataset_3 <- SSVEP_dataset_2[!SSVEP_dataset_2$FA_notice == "N",]

#for ANOVA, we need the data from wide format to long format
SSVEP_dataset_long <- reshape2::melt(SSVEP_dataset_3,
                          id.vars = c("subject", "CT_notice", "DA_notice", "FA_notice", "gender", "handedness", "comp_notice", "cb"),
                          measure.vars = c("train_target", "train_distractor",
                                           "CT_CS", "CT_target", "CT_distractor", 
                                           "DA_CS", "DA_target", "DA_distractor",
                                           "FA_CS", "FA_target", "FA_distractor"),
                          variable.name = "stimulus", 
                          value.name = "SSVEP")

#trial and stimulus should be separate variables
SSVEP_dataset_long$trial <- case_when(
  SSVEP_dataset_long$stimulus == "train_target" | SSVEP_dataset_long$stimulus == "train_distractor" ~ "train",
  SSVEP_dataset_long$stimulus == "CT_CS" | SSVEP_dataset_long$stimulus == "CT_target" | SSVEP_dataset_long$stimulus == "CT_distractor" ~ "CT",
  SSVEP_dataset_long$stimulus == "DA_CS" | SSVEP_dataset_long$stimulus == "DA_target" | SSVEP_dataset_long$stimulus == "DA_distractor" ~ "DA",
  SSVEP_dataset_long$stimulus == "FA_CS" | SSVEP_dataset_long$stimulus == "FA_target" | SSVEP_dataset_long$stimulus == "FA_distractor" ~ "FA",
  TRUE            ~ "nope")

renaming <- c("target", "distractor", "CS", "target", "distractor", "CS", "target", "distractor", "CS", "target", "distractor")
levels(SSVEP_dataset_long$stimulus) <- renaming
SSVEP_dataset_long$trial <- as.factor(SSVEP_dataset_long$trial)







#----------------------------summary stats-------------------------------------#

IB_rates_CT <- prop.table(table(SSVEP_dataset_3$CT_notice)) * 100
IB_rates_DA <- prop.table(table(SSVEP_dataset_3$DA_notice)) * 100
IB_rates_comp <- prop.table(table(SSVEP_dataset_3$comp_notice)) * 100

SSVEP_summary <- SSVEP_dataset_3 %>%
  group_by(comp_notice) %>% #(un)hash this for difference between groups / overall
  summarise(
    mean_CT_target = mean(CT_target, na.rm = TRUE),
    SD_CT_target = sd(CT_target, na.rm =TRUE),
    mean_CT_distractor = mean(CT_distractor, na.rm = TRUE),
    SD_CT_distractor = sd(CT_distractor, na.rm =TRUE),
    mean_CT_CS = mean(CT_CS, na.rm = TRUE),
    SD_CT_CS = sd(CT_CS, na.rm =TRUE),
    
    mean_DA_target = mean(DA_target, na.rm = TRUE),
    SD_DA_target = sd(DA_target, na.rm =TRUE),
    mean_DA_distractor = mean(DA_distractor, na.rm = TRUE),
    SD_DA_distractor = sd(DA_distractor, na.rm =TRUE),
    mean_DA_CS = mean(DA_CS, na.rm = TRUE),
    SD_DA_CS = sd(DA_CS, na.rm =TRUE), 
    
    mean_train_target = mean(train_target, na.rm = TRUE),
    SD_train_target = sd(train_target, na.rm =TRUE),
    mean_train_distractor = mean(train_distractor, na.rm = TRUE),
    SD_train_distractor = sd(train_distractor, na.rm =TRUE)
  )
View(SSVEP_summary)






#------------------------------Analyses----------------------------------------#

#---------full attention trial one sample t tests

t.test(SSVEP_dataset_3$FA_CS, mu = 0)
cohensD(SSVEP_dataset_3$FA_CS, mu = 0)
ssvep.t.facs <- ttestBF(x = SSVEP_dataset_3$FA_CS,mu = 0)
extractBF(ssvep.t.facs)$bf
ttest.tstat(t = 3.1356, n1 = 41, simple = TRUE)

t.test(SSVEP_dataset_3$FA_target, mu = 0)
cohensD(SSVEP_dataset_3$FA_target,mu = 0)
ssvep.t.fat <- ttestBF(x = SSVEP_dataset_3$FA_target,mu = 0)
extractBF(ssvep.t.fat)$bf
ttest.tstat(t = 3.4252, n1 = 41, simple = TRUE)

t.test(SSVEP_dataset_3$FA_distractor, mu = 0)
cohensD(SSVEP_dataset_3$FA_distractor,mu = 0)
ssvep.t.fad <- ttestBF(x = SSVEP_dataset_3$FA_distractor,mu = 0)
extractBF(ssvep.t.fad)$bf
ttest.tstat(t = 4.1572, n1 = 41, simple = TRUE)


#----------full attention trial mixed ANOVA

fa.aov <- anova_test(data = SSVEP_dataset_long[SSVEP_dataset_long$trial == "FA",],
                     dv = SSVEP,
                     wid = subject,
                     within = stimulus,
                     between = comp_notice,
                     effect.size = 'pes')
fa.aov

ssvep.anova.fa <- anovaBF(SSVEP ~ comp_notice * stimulus,
                          data = SSVEP_dataset_long[SSVEP_dataset_long$trial == "FA",])
extractBF(ssvep.anova.fa)$bf
plot(ssvep.anova.fa)

#----------one sample t tests
#to ensure each SSVEP is significantly different from zero

t.test(SSVEP_dataset_3$CT_CS, mu = 0)
cohensD(SSVEP_dataset_3$CT_CS,mu = 0)
ssvep.t.ctcs <- ttestBF(x = SSVEP_dataset_3$CT_CS,mu = 0)
extractBF(ssvep.t.ctcs)$bf

t.test(SSVEP_dataset_3$CT_target, mu = 0)
cohensD(SSVEP_dataset_3$CT_target,mu = 0)
ssvep.t.ctt <- ttestBF(x = SSVEP_dataset_3$CT_target,mu = 0)
extractBF(ssvep.t.ctt)$bf

t.test(SSVEP_dataset_3$CT_distractor, mu = 0)
cohensD(SSVEP_dataset_3$CT_distractor,mu = 0)
ssvep.ctd <- ttestBF(x = SSVEP_dataset_3$CT_distractor,mu = 0)
extractBF(ssvep.ctd)$bf


t.test(SSVEP_dataset_3$DA_CS, mu = 0)
cohensD(SSVEP_dataset_3$DA_CS,mu = 0)
ssvep.t.dacs <- ttestBF(x = SSVEP_dataset_3$DA_CS,mu = 0)
extractBF(ssvep.t.dacs)$bf


t.test(SSVEP_dataset_3$DA_target, mu = 0)
cohensD(SSVEP_dataset_3$DA_target,mu = 0)
ssvep.t.dat <- ttestBF(x = SSVEP_dataset_3$DA_target,mu = 0)
extractBF(ssvep.t.dat)$bf


t.test(SSVEP_dataset_3$DA_distractor, mu = 0)
cohensD(SSVEP_dataset_3$DA_distractor,mu = 0)
ssvep.t.dad <- ttestBF(x = SSVEP_dataset_3$DA_distractor,mu = 0)
extractBF(ssvep.t.dad)$bf



#---------mixed ANOVA (trial * noticing status) for different stimuli 

SSVEP_dataset_long_2 <- SSVEP_dataset_long[!SSVEP_dataset_long$trial == "train" & !SSVEP_dataset_long$trial == "FA",]
SSVEP_long_CS <- SSVEP_dataset_long_2[!SSVEP_dataset_long_2$stimulus == "target" & !SSVEP_dataset_long_2$stimulus == "distractor",]
SSVEP_long_target <- SSVEP_dataset_long_2[!SSVEP_dataset_long_2$stimulus == "CS" & !SSVEP_dataset_long_2$stimulus == "distractor",]
SSVEP_long_distractor <- SSVEP_dataset_long_2[!SSVEP_dataset_long_2$stimulus == "target" & !SSVEP_dataset_long_2$stimulus == "CS",]


ssvep.anova.cs <- anovaBF(SSVEP ~ comp_notice * trial,
                         data = SSVEP_long_CS) #3 here
extractBF(ssvep.anova.t)$bf


target_across.aov <- anova_test(data = SSVEP_long_target,
                                dv = SSVEP,
                                wid = subject,
                                within = trial,
                                between = comp_notice,
                                effect.size = 'pes')
target_across.aov #no effect


ssvep.anova.t <- anovaBF(SSVEP ~ comp_notice * trial,
                         data = SSVEP_long_target) #3 here
extractBF(ssvep.anova.t)$bf


distractor_across.aov <- anova_test(data = SSVEP_long_distractor,
                                    dv = SSVEP,
                                    wid = subject,
                                    within = trial,
                                    between = comp_notice,
                                    effect.size = 'pes')
distractor_across.aov #significant interaction

ssvep.anova.d <- anovaBF(SSVEP ~ comp_notice * trial,
                         data = SSVEP_long_distractor)
extractBF(ssvep.anova.d)$bf
plot(ssvep.anova.d)


# >>> follow-up t tests

distractor_across.pwc <- SSVEP_long_distractor %>% group_by(comp_notice) %>%
  pairwise_t_test(SSVEP ~ trial,
                  paired = TRUE,
                  pooled.sd = FALSE)
distractor_across.pwc #no within-group differences across trials


CT_distr <- t.test(formula = CT_distractor ~ CT_notice,
       data = SSVEP_dataset_3,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95) #significant effect

cohens_d(formula = CT_distractor ~ CT_notice,
         data = SSVEP_dataset_3,
         var.equal = FALSE,
         hedges.correction = TRUE)

ssvep.t.ctd <- ttestBF(formula = CT_distractor ~ CT_notice,
                       data = SSVEP_dataset_3,
                       rscale = "medium",
                       posterior = FALSE)
extractBF(ssvep.t.ctd)$bf

x = count(SSVEP_dataset_3[SSVEP_dataset_3$CT_notice == "Y",]) %>% pull(n)
y = count(SSVEP_dataset_3[SSVEP_dataset_3$CT_notice == "N",]) %>% pull(n)

ttest.tstat(t = CT_distr$statistic,
            n1 = x,
            n2 = y,
            simple = TRUE)
