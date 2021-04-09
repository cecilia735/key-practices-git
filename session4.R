# Created on 9th of April 2021
# This is code for behavioral anlyses for the project "Electrophysiological Signatures of Conceptual and Lexical Retrieval from Semantic Memory"
# In this script we generate a quantile plot and compute t-tests
# Code developed by Cecilia Husta

# Data description: Data belongs to two experiments, verbal and nonverbal. 
# Both experiments included trials that contrained either constraining or nonconstraining context that indicated to different digrees what the target picture was going to be.
# The target pictures had to be named.
# Expectations: RTs will be shorter folowing constraining context (in both expeirments)


# packages
library(tidyverse)
library(purrr)

# input files
input_file_VerbalRTs <- "Verbal_NewRT.txt" 
input_file_VerbalCloze <- "Cloze_ID_Verbal.txt" 
input_file_VerbalID <- "_ID_Verbal.txt"           # 18 files (for each subject) with item IDs for verbal task (e.g. P_13_ID_Verbal.txt)
input_file_NonverbalRTs <- "Nonverbal_NewRT.txt" 
input_file_NonverbalCloze <- "Cloze_ID.txt" 
input_file_NonverbalID <- "_ID.txt"                  # 18 files (for each subject) with item IDs for nonverbal task (e.g. P_13_ID.txt)


#### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### ####

# Load Verbal Data

# Dataset includes 18 subjects (subj 1 & 9 are excluded because of EEG artifacts), 120 trials
# constrained or unconstrained condition, RTs in ms, accuracy (0 = incorrect, 1 = correct,2 = synonym of a correct answer)
verbal <-
  read_tsv(input_file_VerbalRTs, F) 
  colnames(verbal) = c("sbj", "trial", "condition", "rt", "accuracy")
  verbal <- verbal %>% 
  mutate(task = "verbal", cond = if_else(condition == 11, "constrained", "unconstrained"))
verbal

# Load cloze probability information based on trial number in the verbal dataset

# Load file with cloze probabilities and item Ids
ppNumbers <- c(2:8, 10:20)
ID <- data.frame()
Cloze_verbal <- data.frame()
Cloze_ID <- read_tsv(input_file_VerbalCloze, F)
colnames(Cloze_ID) = c("ID", "Cloze")

# Load all item Ids per participant
for ( i in ppNumbers) {
  file <- gsub(" ", "", paste("P", i, input_file_VerbalID))
  ID_participant <- read_tsv(file, F)
  colnames(ID_participant) = c("trial_num", "ID")
  ID <- rbind(ID, ID_participant)
}

# Merge the item Ids to the verbal data
Verbal_cloze <- cbind(verbal,ID[,2])

# Add Cloze probabilities based on the item IDs
trials <- c(1:2160)
for (i in trials) {
  x <- which(Cloze_ID[,1] == Verbal_cloze[i,8], arr.ind=TRUE)
  Verbal_cloze[i,9] <- Cloze_ID[x[,1],2]
}

indexa <- match(Verbal_cloze[with(Verbal_cloze, sbj == 11 & trial == 118), ])

# For participant 11 two trials are marked incorrectly 
# Switch the constraining and unconstraining trials
for (i in trials){
  if (Verbal_cloze[i,1] == 11 & Verbal_cloze[i,2] == 118) {
    Verbal_cloze[i,3] <- 19
    Verbal_cloze[i,7] <- 'unconstrained'
  }
  if (Verbal_cloze[i,1] == 11 & Verbal_cloze[i,2] == 119) {
    Verbal_cloze[i,3] <- 11
    Verbal_cloze[i,7] <- 'constrained'
  }
}

#### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### ####

# Load Nonverbal Data

# Dataset includes the same 18 subjects as verbal experiment (subj 1 & 9 are excluded because of EEG artifacts), 120 trials
# constrained or unconstrained condition, RTs in ms, accuracy (0 = incorrect, 1 = correct,2 = synonym of a correct answer)
nverbal <-
  read_tsv(input_file_NonverbalRTs, F)
  colnames(nverbal) = c("sbj", "trial", "condition", "rt", "accuracy")
  nverbal <- nverbal %>% 
  mutate(task = "nonverbal", cond = if_else(condition == 3, "constrained", "unconstrained"))
nverbal

# Load cloze probability information based on trial number in the nonverbal dataset

# Load file with cloze probabilities and item Ids
ppNumbers <- c(2:8, 10:20)
ID <- data.frame()
Cloze_verbal <- data.frame()
Cloze_ID <- read_tsv(input_file_NonverbalCloze, F)
colnames(Cloze_ID) = c("ID", "Cloze")

# Load all item Ids per participant
for ( i in ppNumbers) {
  file <- gsub(" ", "", paste("P", i, input_file_NonverbalID))
  ID_participant <- read_tsv(file, F)
  colnames(ID_participant) = c("trial_num", "ID")
  ID <- rbind(ID, ID_participant)
}

# Merge the ID to the nonverbal data
Nonverbal_cloze <- cbind(nverbal,ID[,2])

# Add Cloze probabilities based on the item IDs
trials <- c(1:2160)
for (i in trials) {
  x <- which(Cloze_ID[,1] == Nonverbal_cloze[i,8], arr.ind=TRUE)
  Nonverbal_cloze[i,9] <- Cloze_ID[x[,1],2]
}

#### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### ####

# Merge the Verbal and Nonverbal Data

Verb_Nonverb_Dat = bind_rows(verbal, nverbal)
table(Verb_Nonverb_Dat$cond)

#### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### ####

# Summarise of the data

Verb_Nonverb_Dat %>%
  filter(accuracy == 1 | accuracy == 2) %>% # choose trials with correct answer or with a sysnonym of a correct answer
  group_by(sbj, cond, task) %>%
  summarise(rt = mean(rt)) %>% 
  group_by(cond, task) %>%
  summarise(sd = sd(rt), rt = mean(rt))

#### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### ####

# Generate quantile plot

# Separate data into 5 bins
nbins = 5
dat_binned <- Verb_Nonverb_Dat %>%
  filter(accuracy == 1 | accuracy == 2) %>% 
  group_by(sbj, cond, task) %>% 
  mutate(bin = cut_number(rt,nbins, labels = as.character(1:nbins/5)))

# Calculate the mean per bin,subject, and condition
sbj_bin_mean <- dat_binned %>% group_by(sbj,task,cond,bin) %>% summarise(rt = mean(rt))

# Calculate the mean per condition and group over subject
bin_mean <- sbj_bin_mean %>%  group_by(task,cond,bin)  %>% summarise(rt = mean(rt)) 

bin_mean %>% 
  ggplot(aes(x = rt, y = bin)) + 
  geom_line(aes(group = interaction(cond, task),
                color = task,
                linetype = cond),
            size = 1.25) + 
  scale_linetype_manual(values=c("solid", "dotdash"))+
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  ylab("\nCumulative distribution\n") +
  xlab("\nResponse Time (ms)") +
  labs(color = "Condition", linetype = "Experiment") +
  scale_color_brewer(type = "qual",palette="Set2") 


#### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### ####

# Compute t tests for verbal and nonverbal experiment

dat_mean <-   
  Verb_Nonverb_Dat %>%
  filter(accuracy == 1 | accuracy == 2) %>% 
  group_by(sbj, cond, task) %>%
  summarise(mean = mean(rt))
dat_mean

with(dat_mean, t.test(mean[cond=="constrained" & task=="verbal"], mean[cond=="unconstrained" & task=="verbal"], paired=T))

with(dat_mean, t.test(mean[cond=="constrained" & task=="nonverbal"], mean[cond=="unconstrained" & task=="nonverbal"], paired=T))
