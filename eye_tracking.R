library(readxl)
library(dplyr)
library(reshape2)
library(rstatix)
library(lsr)
library(naniar)
library(BayesFactor)

#-------------------data preparation and preprocessing-------------------------#

eye_tracking_wide <- read_excel("Documents/eye_tracking_wide.xlsx", na = "NA")

#factorising
str(eye_tracking_wide)
factor_variables <- c('gender', 'handedness', 'CT_notice', 'DA_notice', 'FA_notice', 'comp_notice', 'cb')
eye_tracking_wide[,factor_variables] <- lapply(eye_tracking_wide[,factor_variables], factor)
str(eye_tracking_wide)


#------------------for repeated measures, need the data from wide to long format
ET_long <- reshape2::melt(eye_tracking_wide,
                               id.vars = c("subject", "CT_notice", "DA_notice", "FA_notice", "comp_notice", "gender", "handedness"),
                               measure.vars = c("log_train_target_fd", "log_train_distractor_fd",
                                                "log_CT_CS_fd", "log_CT_target_fd", "log_CT_distractor_fd", 
                                                "log_DA_CS_fd", "log_DA_target_fd", "log_DA_distractor_fd",
                                                "log_FA_CS_fd", "log_FA_target_fd", "log_FA_distractor_fd"),
                               variable.name = "stimulus", 
                               value.name = "log_fixation_distance")

#trial and stimulus to separate variables
ET_long$trial <- case_when(
  ET_long$stimulus == "log_train_target_fd" | ET_long$stimulus == "log_train_distractor_fd" ~ "train",
  ET_long$stimulus == "log_CT_CS_fd" | ET_long$stimulus == "log_CT_target_fd" | ET_long$stimulus == "log_CT_distractor_fd" ~ "CT",
  ET_long$stimulus == "log_DA_CS_fd" | ET_long$stimulus == "log_DA_target_fd" | ET_long$stimulus == "log_DA_distractor_fd" ~ "DA",
  ET_long$stimulus == "log_FA_CS_fd" | ET_long$stimulus == "log_FA_target_fd" | ET_long$stimulus == "log_FA_distractor_fd" ~ "FA",
  TRUE            ~ "nope")

renaming <- c("target", "distractor",
              "CS", "target", "distractor",
              "CS", "target", "distractor",
              "CS", "target", "distractor")
levels(ET_long$stimulus) <- renaming
ET_long$trial <- as.factor(ET_long$trial)



#-----------------------same thing for fixation count
ET_long_FC <- reshape2::melt(eye_tracking_wide,
                                  id.vars = c("subject", "CT_notice", "DA_notice", "FA_notice", "comp_notice", "gender", "handedness"),
                                  measure.vars = c("train_target_fc", "train_distractor_fc",
                                                   "CT_CS_fc", "CT_target_fc", "CT_distractor_fc", 
                                                   "DA_CS_fc", "DA_target_fc", "DA_distractor_fc",
                                                   "FA_CS_fc", "FA_target_fc", "FA_distractor_fc"),
                                  variable.name = "stimulus", 
                                  value.name = "fixation_count")

#trial and stimulus should be separate variables
ET_long_FC$trial <- case_when(
  ET_long_FC$stimulus == "train_target_fc" | ET_long_FC$stimulus == "train_distractor_fc" ~ "train",
  ET_long_FC$stimulus == "CT_CS_fc" | ET_long_FC$stimulus == "CT_target_fc" | ET_long_FC$stimulus == "CT_distractor_fc" ~ "CT",
  ET_long_FC$stimulus == "DA_CS_fc" | ET_long_FC$stimulus == "DA_target_fc" | ET_long_FC$stimulus == "DA_distractor_fc" ~ "DA",
  ET_long_FC$stimulus == "FA_CS_fc" | ET_long_FC$stimulus == "FA_target_fc" | ET_long_FC$stimulus == "FA_distractor_fc" ~ "FA",
  TRUE            ~ "nope")

renaming <- c("target", "distractor", "CS", "target", "distractor", "CS", "target", "distractor", "CS", "target", "distractor")
levels(ET_long_FC$stimulus) <- renaming
ET_long_FC$trial <- as.factor(ET_long_FC$trial)




#------------one sample t-tests for CS fixation duration against zero----------#

#critical trial
t.ctcsfd <- t.test(eye_tracking_wide$CT_CS_fd, mu = 0)
cohensD(eye_tracking_wide$CT_CS_fd, mu = 0)

#DA trial
t.dacsfd <- t.test(eye_tracking_wide$DA_CS_fd, mu = 0)
cohensD(eye_tracking_wide$DA_CS_fd, mu = 0)


#-------------------------fixation duration ANOVAs-----------------------------#

#--------2 (noticing) x 2 (trial: CT, DA) x 3 (stimulus: target, distractor, CS)

#dataset with only target, distractor, CS
ET_long_tdcs <- ET_long[ET_long$stimulus == "target" | ET_long$stimulus == "distractor" | ET_long$stimulus == "CS",]

#dataset with only CT and DA
ET_long_tdcs_ctda <- ET_long_tdcs[ET_long_tdcs$trial == "CT" | ET_long_tdcs$trial == "DA",]
ET_long_tdcs_ctda <- droplevels(ET_long_tdcs_ctda)
str(ET_long_tdcs_ctda)

across.aov <- anova_test(data = ET_long_tdcs_ctda,
                         dv = log_fixation_distance,
                         wid = subject,
                         within = c(trial, stimulus),
                         between = comp_notice,
                         effect.size = 'pes')
across.aov

#---follow up t tests (simple effects)

#distractor critical trial
log.ct.dist.t <- t.test(formula = log_CT_distractor_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_CT_distractor_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#target critical trial
t.test(formula = log_CT_target_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_CT_target_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#CS critical trial
t.test(formula = log_CT_CS_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_CT_CS_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)


#distractor DA trial
t.test(formula = log_DA_distractor_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_DA_distractor_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#target DA trial
t.test(formula = log_DA_target_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_DA_target_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#CS DA trial
t.test(formula = log_DA_CS_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_DA_CS_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)


#---follow up t tests (noticer status per trial, rather than aggregate noticer variable)

#distractor critical trial
t.test(formula = log_CT_distractor_fd ~ CT_notice,
                        data = eye_tracking_wide,
                        var.equal = FALSE,
                        alternative = "two.sided",
                        conf.level = .95)
cohens_d(formula = log_CT_distractor_fd ~ CT_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#target critical trial
t.test(formula = log_CT_target_fd ~ CT_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_CT_target_fd ~ CT_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#CS critical trial
t.test(formula = log_CT_CS_fd ~ CT_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_CT_CS_fd ~ CT_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)


#distractor DA trial
t.test(formula = log_DA_distractor_fd ~ DA_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_DA_distractor_fd ~ DA_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#target DA trial
t.test(formula = log_DA_target_fd ~ DA_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_DA_target_fd ~ DA_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#CS DA trial
t.test(formula = log_DA_CS_fd ~ DA_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_DA_CS_fd ~ DA_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)






#--------2 (noticing) x 3 (stimulus: target, distractor, CS) for training trials

#dataset with only target, distractor
ET_long_td <- ET_long[ET_long$stimulus == "target" | ET_long$stimulus == "distractor",]

#dataset with only training
ET_long_td_train <- ET_long_td[ET_long_td$trial == "train",]
ET_long_td_train <- droplevels(ET_long_td_train)
str(ET_long_td_train)

train_across.aov <- anova_test(data = ET_long_td_train,
                         dv = log_fixation_distance,
                         wid = subject,
                         within = stimulus,
                         between = comp_notice,
                         effect.size = 'pes')
train_across.aov

training.target.distractor.pwc <- ET_long_td_train %>%
  pairwise_t_test(log_fixation_distance ~ stimulus,
                  paired = TRUE)


#--------------2 (noticing) x 3 (stimulus: target, distractor, CS) for FA trials

#dataset with only target, distractor, CS
ET_long_tdcs <- ET_long[ET_long$stimulus == "target" | ET_long$stimulus == "distractor" | ET_long$stimulus == "CS",]

#dataset with only training
ET_long_tdcs_FA <- ET_long_tdcs[ET_long_tdcs$trial == "FA",]
ET_long_tdcs_FA <- droplevels(ET_long_tdcs_FA)
str(ET_long_tdcs_FA)

#ANOVA
FA_across.aov <- anova_test(data = ET_long_tdcs_FA,
                               dv = log_fixation_distance,
                               wid = subject,
                               within = stimulus,
                               between = comp_notice,
                               effect.size = 'pes')
FA_across.aov

#---follow up t tests

#distractor FA trial
t.test(formula = log_FA_distractor_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_FA_distractor_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#target FA trial
t.test(formula = log_FA_target_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_FA_target_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#CS FA trial
t.test(formula = log_FA_CS_fd ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = log_FA_CS_fd ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)



#---------------------------fixation count ANOVAs------------------------------#

#--------2 (noticing) x 2 (trial: CT, DA) x 3 (stimulus: target, distractor, CS)

#dataset with only target, distractor, CS
ET_long_FC_tdcs <- ET_long_FC[ET_long_FC$stimulus == "target" | ET_long_FC$stimulus == "distractor" | ET_long_FC$stimulus == "CS",]

#dataset with only CT and DA
ET_long_FC_tdcs_ctda <- ET_long_FC_tdcs[ET_long_FC_tdcs$trial == "CT" | ET_long_FC_tdcs$trial == "DA",]
ET_long_FC_tdcs_ctda <- droplevels(ET_long_FC_tdcs_ctda)
str(ET_long_FC_tdcs_ctda)

CT_DA_fc_across.aov <- anova_test(data = ET_long_FC_tdcs_ctda,
                         dv = fixation_count,
                         wid = subject,
                         within = c(trial, stimulus),
                         between = comp_notice,
                         effect.size = 'pes')
CT_DA_fc_across.aov



#--------------------------supplementary chi square----------------------------#

#categorical fixate versus no fixate variable for CS critical trial
eye_tracking_wide$CT_CS_fixate_cat <- case_when(
  eye_tracking_wide$CT_CS_fc == 0 ~ "N",
  TRUE ~ "Y")

#categorical fixate versus no fixate variable for CS DA trial
eye_tracking_wide$DA_CS_fixate_cat <- case_when(
  eye_tracking_wide$DA_CS_fc == 0 ~ "N",
  TRUE ~ "Y")

fixation_CS_CT <- table(eye_tracking_wide$CT_notice, eye_tracking_wide$CT_CS_fixate_cat)
Xsq_CT <- chisq.test(fixation_CS_CT, correct = FALSE)

fixation_CS_DA <- table(eye_tracking_wide$DA_notice, eye_tracking_wide$DA_CS_fixate_cat)
Xsq_DA <- chisq.test(fixation_CS_DA, correct = FALSE)

#for bayesian contingency table need to specify the sampling type.
#will use poisson because nothing (sample size, row or column totals) is fixed.

contingencyTableBF(x = fixation_CS_CT,
                   sampleType = "poisson",
                   posterior = FALSE)

contingencyTableBF(x = fixation_CS_DA,
                   sampleType = "poisson",
                   posterior = FALSE)

#Quick custom function for effect size
Cramers_V <- function(chi, n, df) sqrt((chi)/(n * df))
df <- min(dim(fixation_CS_CT)) - 1
Cramers_V(chi = Xsq_CT$statistic, n = sum(fixation_CS_CT), df = Xsq_CT$parameter) #critical trial
Cramers_V(chi = Xsq_DA$statistic, n = sum(fixation_CS_DA), df = Xsq_DA$parameter) #DA trial


#-------------------------t tests for gaze onset CS----------------------------#
eye_tracking_wide$CT_onset_fix1_fixed <- eye_tracking_wide$CT_onset_time_fix1
eye_tracking_wide$DA_onset_fix1_fixed <- eye_tracking_wide$DA_onset_time_fix1

#removing 0 scores
eye_tracking_wide <- eye_tracking_wide %>% replace_with_na(replace = list(CT_onset_fix1_fixed = 0)) %>% replace_with_na(replace = list(DA_onset_fix1_fixed = 0))

#CT trial
et.t.cs.o.ct <- t.test(formula = CT_onset_fix1_fixed ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = CT_onset_fix1_fixed ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)

#DA trial
et.t.cs.o.da <- t.test(formula = DA_onset_fix1_fixed ~ comp_notice,
       data = eye_tracking_wide,
       var.equal = FALSE,
       alternative = "two.sided",
       conf.level = .95)
cohens_d(formula = DA_onset_fix1_fixed ~ comp_notice,
         data = eye_tracking_wide,
         var.equal = FALSE,
         hedges.correction = TRUE)


