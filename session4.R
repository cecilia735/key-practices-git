library(tidyverse)
library(purrr)
library(lme4)
library(nlme)
library(lmerTest)

#### ####

# Load Verbal Data
verbal <-
  read_tsv("Verbal_NewRT.txt", F) 
colnames(verbal) = c("sbj", "trial", "condition", "rt", "accuracy")
# subject number, trial number, condition, reaction time, and the answer 
verbal <- verbal %>% 
  mutate(task = "verbal", 
         cond = if_else(condition == 11, "constrained", "unconstrained"))
verbal

ppNumbers <- c(2:8, 10:20)
ID <- data.frame()
Cloze_verbal <- data.frame()
Cloze_ID <- read_tsv("Cloze_ID_Verbal.txt", F)
colnames(Cloze_ID) = c("ID", "Cloze")

# Load all Item Ids
for ( i in ppNumbers) {
  file <- gsub(" ", "", paste("P", i, "_ID_Verbal.txt"))
  ID_participant <- read_tsv(file, F)
  colnames(ID_participant) = c("trial_num", "ID")
  ID <- rbind(ID, ID_participant)
}

Verbal_cloze <- cbind(verbal,ID[,2]) # Merge the Item ID to the verbal data

# Add Cloze probabilities based on the item IDs
trials <- c(1:2160)
for (i in trials) {
  x <- which(Cloze_ID[,1] == Verbal_cloze[i,8], arr.ind=TRUE)
  Verbal_cloze[i,9] <- Cloze_ID[x[,1],2]
}


print(Verbal_cloze[1,5])

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

# Load Nonverbal Data
nverbal <-
  read_tsv("Nonverbal_NewRT.txt", F)
colnames(nverbal) = c("sbj", "trial", "condition", "rt", "accuracy")
nverbal <- nverbal %>% 
  mutate(task = "nonverbal", 
         cond = if_else(condition == 3, "constrained", "unconstrained"))

# Load all Item Ids
ppNumbers <- c(2:8, 10:20)
ID <- data.frame()
Cloze_verbal <- data.frame()
Cloze_ID <- read_tsv("Cloze_ID.txt", F)
colnames(Cloze_ID) = c("ID", "Cloze")

for ( i in ppNumbers) {
  file <- gsub(" ", "", paste("P", i, "_ID.txt"))
  ID_participant <- read_tsv(file, F)
  colnames(ID_participant) = c("trial_num", "ID")
  ID <- rbind(ID, ID_participant)
}

Nonverbal_cloze <- cbind(nverbal,ID[,2]) # Merge the ID to the nonverbal data

# Add Cloze probabilities based on the item IDs
trials <- c(1:2160)
for (i in trials) {
  x <- which(Cloze_ID[,1] == Nonverbal_cloze[i,8], arr.ind=TRUE)
  Nonverbal_cloze[i,9] <- Cloze_ID[x[,1],2]
}

#### ####
dat = bind_rows(verbal, nverbal)
table(dat$cond)

#---- means ---- 
dat %>%
  filter(accuracy == 1 | accuracy == 2) %>% 
  group_by(sbj, cond, task) %>%
  summarise(rt = mean(rt)) %>% 
  group_by(cond, task) %>%
  summarise(sd = sd(rt), rt = mean(rt))

#---- quantiles ----
nbins = 5
dat_binned <- dat %>%
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


#---- t test ---- 

dat_mean <-   
  dat %>%
  filter(accuracy == 1 | accuracy == 2) %>% 
  group_by(sbj, cond, task) %>%
  summarise(mean = mean(rt))
dat_mean

with(dat_mean, t.test(mean[cond=="constrained" & task=="verbal"], mean[cond=="unconstrained" & task=="verbal"], paired=T))

with(dat_mean, t.test(mean[cond=="constrained" & task=="nonverbal"], mean[cond=="unconstrained" & task=="nonverbal"], paired=T))

with(dat_mean, t.test(mean[cond=="constrained" & task=="verbal"], mean[cond=="constrained" & task=="nonverbal"], paired=T))

with(dat_mean, t.test(mean[cond=="unconstrained" & task=="verbal"], mean[cond=="unconstrained" & task=="nonverbal"], paired=T))

#--------- Mixed Effects Model ----------------------------

# Verbal task
Verbal_cloze_correct <- filter(Verbal_cloze, accuracy == 1 | accuracy == 2)
Verbal_cloze_correct_cons <- filter(Verbal_cloze_correct, condition == 11)

# Option 1
mixed.lmer <- lmer(rt ~ Cloze + (1|sbj), data = Verbal_cloze_correct_cons)
summary(mixed.lmer)

(mm_plot <- ggplot(Verbal_cloze_correct_cons, aes(x = Cloze, y = rt)) +
    aes(color=condition)+
    facet_wrap(~sbj, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(Verbal_cloze_correct_cons, pred = predict(mixed.lmer)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
    panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Nonverbal task

Nonverbal_cloze_correct <- filter(Nonverbal_cloze, accuracy == 1 | accuracy == 2)
Nonverbal_cloze_correct_cons <- filter(Nonverbal_cloze_correct, condition == 3)

mixed.lmer2 <- lmer(rt ~ Cloze + (1|sbj), data = Nonverbal_cloze_correct_cons)
summary(mixed.lmer2)

(mm_plot2 <- ggplot(Nonverbal_cloze_correct_cons, aes(x = Cloze, y = rt)) +
    aes(color=condition)+
    facet_wrap(~sbj, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(Nonverbal_cloze_correct_cons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)
