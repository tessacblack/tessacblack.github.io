###### TCB Final Project Spring 2024 ######
# Tessa: Need to look into using the broom.mixed package to fucking clean up your fucking models for this project :) 
library(pacman)
pacman::p_load("tidyverse", "janitor", "patchwork", "skimr", "readxl", "lme4", 
               "lmerTest", "glmmADMB", "arm", "emmeans", "easystats", "car", "performance")


# Data Preparation --------------------------------------------------------

# * Load data, clean names, view structure ####
hammies <- read_csv("final_project/IU106_Social Behavior First and Last Five Minutes_Formatted_6-6-23.csv")
hammies <- janitor::clean_names(hammies)
names(hammies)
str(hammies)

# * Clean data --------------------------------------------------------------
# Remove paw because we didn't score it, remove avoid because idek why it's there,
# remove id bc it's a repeat col of juvenile_id, remove aggression_score bc 
# idk what it is, remove receive_aggression duration bc there was no duration 
columns_removed <- c("id", "aggression_number_assigned", "comp1", "comp2", "comp3", 
                     "aggression_score_freq", "aggression_score_duration")
hammies <- dplyr::select(hammies, -columns_removed)
names(hammies)

tidy_hammies <- hammies %>% 
  mutate(trial_timing = as.factor(first_or_last_five_minutes)) %>% 
  mutate(treatment = as.factor(maternal_treatment)) %>% 
  mutate(juvenile_id = as.factor(juvenile_id)) %>% 
  dplyr::select(-first_or_last_five_minutes)

# Fix spelling of all "occurence" columns
tidy_hammies <- tidy_hammies %>% 
  mutate(attack_number_of_occurrences = attack_number_of_occurences) %>%
  mutate(chase_number_of_occurrences = chase_number_of_occurences) %>%
  mutate(groom_number_of_occurrences = groom_number_of_occurences) %>%
  mutate(investigation_number_of_occurrences = investigation_number_of_occurences) %>%
  mutate(jump_number_of_occurrences = jump_number_of_occurences) %>% 
  dplyr::select(-contains("occurences"))

str(tidy_hammies)

# Originally, we subset the data here to do PCA scores, but because PCAs did not correlate, we decided to leave it out of our publication. I'm commenting these lines out here but leaving this note as a reminder of what we did. 
#behavior_last <- tidy_hammies %>% 
 # filter(trial_timing == "Last",)
#behavior_first <- tidy_hammies %>% 
 # filter(trial_timing == "First",)

################### Modeling juvenile behavior ###################

# Number of Attacks -------------------------------------------------------
# Running model using Poisson distribution
# keeping the interaction between treatment and sex


# * Model A.1 -------------------------------------------------------------

modelA.1 <- tidy_hammies %>% 
  glmer(attack_number_of_occurrences ~ sex*treatment+trial_timing+scale(si_cort)
        +weight_at_trial+(1|intruder_id)+(1|juvenile_id),family=poisson,
        glmerControl(optimizer="bobyqa"),data=.)
summary(modelA.1)

#pairwise comparisons with corrected p-value
lsmeans(modelA.1, pairwise~treatment, type = "response") 
lsmeans(modelA.1, pairwise~treatment*sex, type = "response") 

# note to self for final project: include some kind of visual here for these tables?

## Create a single variable for the interaction effect, then apply it to the original model structure (note covariates are allowed, see Jess' code line 98)
# Before then using the 'lsmeans' function to assess Tukey contrasts

tidy_hammies <- tidy_hammies %>% 
  mutate(sextreatment = interaction(sex,treatment,drop = T))

postmodelA.1 <- tidy_hammies %>% 
  glmer(attack_number_of_occurrences~sextreatment + trial_timing + scale(si_cort) + weight_at_trial + (1|intruder_id)+(1|juvenile_id),family=poisson,
        glmerControl(optimizer="bobyqa"),data=.)
results_postmodelA.1 <- summary(postmodelA.1)$coefficients
#write.table(results_postmodelA.1,sep='\t',"OUTPUT_results_postmodelA.1_Summary_11-29-23.text") <- this is from jess' code

lsmeans(postmodelA.1,pairwise~sextreatment)
lsmeans(modelA.1, pairwise~treatment*sex)



# * * Checking model assumptions ------------------------------------------
qqnorm(residuals(modelA.1))
qqline(residuals(modelA.1))
shapiro.test(residuals(modelA.1))


# * * Check overdispersion ------------------------------------------------
overdisp_fun <- function(modelA.1) {
  rdf <- df.residual(modelA.1)
  rp <- residuals(modelA.1,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun
overdisp_fun(modelA.1)

sum(residuals(modelA.1,type="pearson")^2)/(nrow(tidy_hammies)-length(fixef(modelA.1)-1))

vif(modelA.1)


# Note to self for project: may want to consider printing out results here to show the results of the model, see lines 
# 149-157 in Jess' code


# * Graphing number of attacks --------------------------------------------
attack_number_graph <- tidy_hammies %>% 
  ggplot(aes(x=interaction(trial_timing, sex),
             y = attack_number_of_occurrences))+
  geom_boxplot(aes(fill=treatment))+
  labs(x= "Trial timing", 
       y = "Number of attacks", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(attack_number_graph)




# Attack Duration: Model 2 ------------------------------------------------
str(tidy_hammies)
modelA.2 <- lmer(sqrt(attack_total_duration)~ sex+treatment+trial_timing+
                   scale(si_cort)+weight_at_trial+(1|intruder_id)+(1|juvenile_id),
                 data = tidy_hammies)

# relevel treatment to antibiotic only
tidy_hammies <- tidy_hammies %>% 
  mutate(treatment = relevel(treatment, ref = "Antibiotic Only"))
tidy_hammies$treatment

# model output
summary(modelA.2)

lsmeans(modelA.2, pairwise~treatment, type = "response")



# * * Checking model assumptions ------------------------------------------
qqnorm(residuals(modelA.2))
qqline(residuals(modelA.2))
shapiro.test(residuals(modelA.2))

#Printing out results
results_modelA.2<-summary(modelA.2)$coefficients
results_modelA.2
#write.table(results_modelA.2,sep='\t',"OUTPUT_ModelA.2_Attack Duration_Summary_11-30-23.text")



# * Graphing attack duration ----------------------------------------------
attack_duration_graph <- tidy_hammies %>% 
  ggplot(aes(x=interaction(trial_timing, sex),
             y = attack_total_duration))+
  geom_boxplot(aes(fill=treatment))+
  labs(x= "Trial timing", 
       y = "Attack Duration", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(attack_number_graph)



# Investigation Occurrence: Model 3 ---------------------------------------

modelB=glmer(investigation_number_of_occurrences ~sex*treatment*trial_timing+
               scale(si_cort)+weight_at_trial+
               (1|intruder_id)+(1|juvenile_id), 
             family=poisson, glmerControl(optimizer="bobyqa"),
             data=tidy_hammies)

#relevel treatment to control
tidy_hammies <- tidy_hammies %>% 
  mutate(treatment = relevel(treatment, ref = "Control"))
tidy_hammies$treatment

# model output 
summary(modelB)

lsmeans(modelB, pairwise~treatment, type = "response")

#Below, we create a single variable representing an interaction effect
#And then apply it to the original model structure (note covariates are allowed)
#before then using the 'lsmeans' function to assess Tukey contrasts

tidy_hammies <- tidy_hammies %>% 
  mutate(sextreatmenttime = interaction(sex,treatment,trial_timing, drop = T))

modelB.2=glmer(investigation_number_of_occurrences ~sextreatmenttime+
               scale(si_cort)+weight_at_trial+(1|intruder_id)+(1|juvenile_id), 
             family=poisson, glmerControl(optimizer="bobyqa"),
             data=tidy_hammies)

lsmeans(modelB.2,pairwise~sextreatmenttime)

# * Checking model assumptions --------------------------------------------
qqnorm(residuals(modelB))
qqline(residuals(modelB))
shapiro.test(residuals(modelB))

# * Check overdispersion --------------------------------------------------
overdisp_fun <- function(modelB) {
  rdf <- df.residual(modelB)
  rp <- residuals(modelB,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun
overdisp_fun(modelB)

sum(residuals(modelB.1,type="pearson")^2)/(nrow(tidy_hammies)-length(fixef(modelB.1)-1))

vif(modelB)


#Printing out results
results_modelB<-summary(modelB)$coefficients
results_modelB
#write.table(results_modelB,sep='\t',"OUTPUT_ModelB_Investigation Occurrence_Summary_6-6-23.text") 



# * Graph Investigation Occurrence ----------------------------------------

investigation_occurrence_graph <- tidy_hammies %>% 
  ggplot(aes(x=interaction(trial_timing, sex),
             y = investigation_number_of_occurrences))+
  geom_boxplot(aes(fill=treatment))+
  labs(x= "Trial timing", 
       y = "Investigation occurrences", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(investigation_occurrence_graph)



# Investigation Duration: Model 4 -----------------------------------------
# no significant three-way interaction
modelB.3 <- lmer(investigation_total_duration~ sex + trial_timing+scale(si_cort)
                 +weight_at_trial+(1|intruder_id)+(1|juvenile_id), 
                 data = tidy_hammies)
summary(modelB.3)

# relevel treatment to Antibiotic only
tidy_hammies <- tidy_hammies %>% 
  mutate(treatment = relevel(treatment, ref = "Antibiotic Only"))
tidy_hammies$treatment

lsmeans(modelB.3, pairwise~treatment, type = "response")


# * Checking model assumptions --------------------------------------------
qqnorm(residuals(modelB.3))
qqline(residuals(modelB.3))
shapiro.test(residuals(modelB.3))

#Printing out results
results_modelB.3<-summary(modelB.3)$coefficients
results_modelB.3
#write.table(results_modelB.3,sep='\t',"OUTPUT_ModelB.3_Investigation Duration_Summary_11-30-23.text") 



# * Graph: Interaction investigation duration -----------------------------
investigation_duration_interaction_graph <- tidy_hammies %>% 
  ggplot(aes(x=interaction(trial_timing, sex),
             y = investigation_total_duration))+
  geom_boxplot(aes(fill=treatment))+
  labs(x= "Trial timing", 
       y = "Investigation duration", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(investigation_duration_interaction_graph)



# * Graph: Sex only investigation duration --------------------------------


investigation_duration_sex_graph <- tidy_hammies %>% 
  ggplot(aes(x=sex, y = investigation_total_duration))+
  geom_boxplot(aes(fill = treatment))+
  labs(x= "Offspring Sex", 
       y = "Investigation duration", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(investigation_duration_sex_graph)




# Escape Occurrence: Model 5 ----------------------------------------------
modelC <-  glmer(jump_number_of_occurrences ~ sex*treatment*trial_timing+
                 scale(si_cort)+weight_at_trial+(1|intruder_id)+(1|juvenile_id),
               family = poisson, glmerControl(optimizer="bobyqa"), 
               data = tidy_hammies)

# relevel treatment to antibiotic only
tidy_hammies <- tidy_hammies %>% 
  mutate(treatment = relevel(treatment, ref = "Antibiotic Only"))
tidy_hammies$treatment

summary(modelC) # output of model


# * Check overdispersion --------------------------------------------------


overdisp_fun <- function(modelC) {
  rdf <- df.residual(modelC)
  rp <- residuals(modelC,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun
overdisp_fun(modelC)

lsmeans(modelC,pairwise~treatment, type="response")


#Below, we create a single variable representing an interaction effect
#And then apply it to the original model structure (note covariates are allowed)
#before then using the 'lsmeans' function to assess Tukey contrasts
tidy_hammies <- tidy_hammies %>% 
  mutate(modcsextreatmenttime = interaction(sex, treatment, trial_timing, drop = T))

modelc.2 <- glmer(jump_number_of_occurrences~modcsextreatmenttime+scale(si_cort)
                  +weight_at_trial+(1|intruder_id)+(1|juvenile_id),family=poisson,
                  glmerControl(optimizer="bobyqa"), 
                  data=tidy_hammies)

lsmeans(modelc.2,pairwise~modcsextreatmenttime)



# * Checking model assumptions --------------------------------------------
qqnorm(residuals(modelC))
qqline(residuals(modelC))
shapiro.test(residuals(modelC))

#Printing out results
results_modelC<-summary(modelC)$coefficients
results_modelC
#write.table(results_modelC,sep='\t',"OUTPUT_ModelC_Jump Occurrence_Summary_11-30-23.text") 


# * Graph: Escape occurrence ----------------------------------------------


escape_occurrence_graph <- tidy_hammies %>% 
  ggplot(aes(x=interaction(trial_timing, sex), y = jump_number_of_occurrences))+
  geom_boxplot(aes(fill = treatment))+
  labs(x= "Trial timing", 
       y = "Number of jumps", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(escape_occurrence_graph)



# Escape Duration: Model 6 ------------------------------------------------
# no three-way interaction

modelC.3 <- lmer(jump_total_duration_s ~sex+treatment+trial_timing+scale(si_cort)+
                   weight_at_trial+(1|intruder_id)+(1|juvenile_id), 
                 data = tidy_hammies)
# relevel treatment to control
tidy_hammies <- tidy_hammies %>% 
  mutate(treatment = relevel(treatment, ref = "Control"))
tidy_hammies$treatment

summary(modelC.3)

lsmeans(modelC.3, pairwise~treatment, type = response)

# Checking model assumptions ----------------------------------------------
qqnorm(residuals(modelC.3))
qqline(residuals(modelC.3))
shapiro.test(residuals(modelC.3))



#Printing out results
results_modelC.3<-summary(modelC.3)$coefficients
results_modelC.3
#write.table(results_modelC.3,sep='\t',"OUTPUT_ModelC.3_Jump Duration_Summary_1-19-24.text") 


# * Graph: Interaction jump duration --------------------------------------
# interaction of trial timing and sex
escape_duration_interaction_graph <- tidy_hammies %>% 
  ggplot(aes(x=interaction(trial_timing, sex), 
             y = jump_total_duration_s))+
  geom_boxplot(aes(fill = treatment))+
  labs(x= "Trial timing", 
       y = "Duration of jump behaviors", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(escape_duration_interaction_graph)


# * Graph: Trial timing only jump duration --------------------------------
escape_duration_timing_graph <- tidy_hammies %>% 
  ggplot(aes(x=trial_timing, 
             y = jump_total_duration_s))+
  geom_boxplot(aes(fill = sex))+
  labs(x= "Trial timing", 
       y = "Duration of jump behaviors", 
       fill = "Sex")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(escape_duration_timing_graph)



# Grooming Occurrence: Model 7 --------------------------------------------
# no significant 3-way interaction
modelD <- glmer(groom_number_of_occurrences~sex+treatment+trial_timing+
                  scale(si_cort)+weight_at_trial+(1|intruder_id)+(1|juvenile_id),
                family=poisson, glmerControl(optimizer="bobyqa"), 
                data = tidy_hammies)


# * Check overdispersion --------------------------------------------------


overdisp_fun <- function(modelD) {
  rdf <- df.residual(modelD)
  rp <- residuals(modelD,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun
overdisp_fun(modelD)


#relevel treatment to control
tidy_hammies <- tidy_hammies %>% 
  mutate(treatment = relevel(treatment, ref = "Control"))
tidy_hammies$treatment

#output of model
summary(modelD)

lsmeans(modelD,pairwise~treatment, type="response")


# * Checking model assumptions --------------------------------------------
qqnorm(residuals(modelD))
qqline(residuals(modelD))
shapiro.test(residuals(modelD))
hist(tidy_hammies$groom_number_of_occurrences)

#Printing out results
results_modelD<-summary(modelD)$coefficients
results_modelD
#write.table(results_modelD,sep='\t',"OUTPUT_ModelD_Grooming Occurrence_Summary_1-19-24.text") 



# * Graph: Interaction groom occurrence -----------------------------------
groom_occurrence_interaction_graph <- tidy_hammies %>% 
  ggplot(aes(x=interaction(trial_timing, sex), 
             y = groom_number_of_occurrences))+
  geom_boxplot(aes(fill = treatment))+
  labs(x= "Trial timing", 
       y = "Number of Grooming Bouts", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(groom_occurrence_interaction_graph)


# * Graph: Trial timing groom occurrence ----------------------------------
groom_occurrence_sex_graph <- tidy_hammies %>% 
  ggplot(aes(x=trial_timing, 
             y = groom_number_of_occurrences))+
  geom_boxplot(aes(fill = sex))+
  labs(x= "Trial timing", 
       y = "Number of Grooming Bouts", 
       fill = "Sex")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(groom_occurrence_sex_graph)


# Grooming Duration: Model 8 ----------------------------------------------
# there is an interaction between sex and treatment, no 3-way interaction

modelD.2 <- lmer(groom_total_duration_s~sex*treatment+trial_timing+
                   scale(si_cort)+weight_at_trial+(1|intruder_id)+(1|juvenile_id),
                 data = tidy_hammies)

# relevel treatment to control
tidy_hammies <- tidy_hammies %>% 
  mutate(treatment = relevel(treatment, ref = "Control"))
tidy_hammies$treatment

#output of model
summary(modelD.2)

lsmeans(modelD.2,pairwise~treatment, type="response")
lsmeans(modelD.2,pairwise~treatment*sex, type="response")

tidy_hammies <- tidy_hammies %>% 
  mutate(modD_sextreatment = interaction(sex, treatment, drop = T))

postmodelD.2=glmer(groom_total_duration_s ~ modD_sextreatment+trial_timing+
                   scale(si_cort)+weight_at_trial+(1|intruder_id)+(1|juvenile_id),
                   family=poisson,glmerControl(optimizer="bobyqa"), 
                   data=tidy_hammies)

lsmeans(postmodelD.2,pairwise~modD_sextreatment)


# * Checking model assumptions ----------------------------------------------
qqnorm(residuals(modelD.2))
qqline(residuals(modelD.2))
shapiro.test(residuals(modelD.2))



#Printing out results
results_modelD.2<-summary(modelD.2)$coefficients
results_modelD.2
#write.table(results_modelD.2,sep='\t',"OUTPUT_ModelD.2_Grooming Duration_Summary_1-19-24.text") 


#### * Graphing groom duration ####
groom_duration_sex_graph <- tidy_hammies %>% 
  ggplot(aes(x=sex, 
             y = groom_total_duration_s))+
  geom_boxplot(aes(fill = treatment))+
  labs(x= "Offspring Sex", 
       y = "Duration of grooming", 
       fill = "Maternal treatment")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(groom_duration_sex_graph)


groom_duration_timing_graph <- tidy_hammies %>% 
  ggplot(aes(x=trial_timing, 
             y = groom_total_duration_s))+
  geom_boxplot(aes(fill = trial_timing))+
  labs(x= "Trial Timing", 
       y = "Duration of grooming")+
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
print(groom_duration_timing_graph)