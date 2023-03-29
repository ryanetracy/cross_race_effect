############################################
# cross-race effect - study 2
# expertise vs motivation
# motivation measured by desired friendship
############################################

#packages
pckgs <- c("lme4", "lmerTest", "effectsize", "interactions", "performance", "see", "Rmisc", "correlation", "ggcorrplot", "DHARMa", "lavaan", "ggraph", "ggpubr", "tidyverse")

#check installation and load
for (i in 1:length(pckgs)) {
  if(!pckgs[[i]] %in% installed.packages()) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}


#data
s2Data <- read.csv("data/study 2 data.csv")

#clean responses
#filter participants with NA values in the consent
s2Data <- filter(s2Data, consent == 1) #289 total participants

#check duplicated IPs and IDs
s2Data$dupID <- duplicated(s2Data$ResponseId)
s2Data$dupIP <- duplicated(s2Data$IPAddress)

s2Data <- filter(s2Data, dupID == FALSE & dupIP == FALSE)


#demographics
mean(s2Data$age, na.rm = T)
sd(s2Data$age, na.rm = T)

#race
race <- plyr::count(s2Data$race) %>%
  mutate(prop = freq/(sum(freq)) * 100) %>%
  mutate(x = case_when(
    x == 1 ~ "white",
    x == 2 ~ "black",
    x == 6 ~ "bi/multiracial",
    TRUE ~ "what?"
  ))
race

#remove two participants for not being black/white
s2Data <- filter(s2Data, race == 1 | race == 2 | race == 6)

#recode race
s2Data$race <- case_when(
  s2Data$race == 1 ~ "white",
  s2Data$race == 2 ~ "black",
  s2Data$race == 6 ~ "black"
)

#gender
gender <- plyr::count(s2Data$sex) %>%
  mutate(prop = freq/(sum(freq)) * 100) %>%
  mutate(x = case_when(
    x == 1 ~ "male",
    x == 2 ~ "female",
    x == 3 ~ "nonbinary",
    TRUE ~ "other"
  ))
gender


#check bot check responses
bots <- plyr::count(s2Data$botCheck) #nothing out of the ordinary

#check experiment probe
expProbe <- plyr::count(s2Data$manipCheck) 
#look for keywords in manipulation check
s2Data$expProbe <- str_detect(s2Data$manipCheck, "other race effect|cross race effect") #none found

#rename responseID and block columns
names(s2Data)[c(9,311)] <- c("subj", "block")


# get alphas for experience and motivation
white_exp <- s2Data[,c(270,271,274,275,278,281,282)]
black_exp <- s2Data[,c(269,272,273,276,277,279,280,283)]

psych::alpha(white_exp)
psych::alpha(black_exp)

white_mot <- s2Data[,294:302]
black_mot <- s2Data[,285:293]

psych::alpha(white_mot)
psych::alpha(black_mot)

#calculate experience and motivation items
#experience
#black exp
s2Data$blackExp <- rowMeans(s2Data[,c(269,272,273,276,277,279,280,283)], na.rm = T)
#white exp
s2Data$whiteExp <- rowMeans(s2Data[,c(270,271,274,275,278,281,282)], na.rm = T)

#motivation
#black motivation
s2Data$blackMotiv <- rowMeans(s2Data[,285:293], na.rm = T)
#white motivation
s2Data$whiteMotiv <- rowMeans(s2Data[,294:302], na.rm = T)

#filter by participant race
blackPs2 <- filter(s2Data, race == "black")
whitePs2 <- filter(s2Data, race == "white")

#test for differences by race
#black participants
#experience
t.test(blackPs2$blackExp, blackPs2$whiteExp, paired = T); effsize::cohen.d(blackPs2$blackExp, blackPs2$whiteExp, paired = T)
#motivation
t.test(blackPs2$blackMotiv, blackPs2$whiteMotiv, paired = T); effsize::cohen.d(blackPs2$blackMotiv, blackPs2$whiteMotiv, paired = T)
#descriptives
#mean
blackPs2 %>%
  select(blackExp, whiteExp, blackMotiv, whiteMotiv) %>%
  apply(2, mean)
#sd
blackPs2 %>%
  select(blackExp, whiteExp, blackMotiv, whiteMotiv) %>%
  apply(2, sd)


ggplot(blackPs2) +
  geom_histogram(aes(blackMotiv), color = 'black', fill = 'red') +
  geom_histogram(aes(whiteMotiv), color = 'black', fill = 'blue') +
  labs(x = 'black = red \n white = blue',
       title = 'motivation') +
  theme_bw()

ggplot(blackPs2) +
  geom_histogram(aes(blackExp), color = 'black', fill = 'red') +
  geom_histogram(aes(whiteExp), color = 'black', fill = 'blue') +
  labs(x = 'black = red \n white = blue',
       title = 'motivation') +
  theme_bw()


#white participants
#experience
t.test(whitePs2$whiteExp, whitePs2$blackExp, paired = T); effsize::cohen.d(whitePs2$whiteExp, whitePs2$blackExp, paired = T)
#motivation
t.test(whitePs2$whiteMotiv, whitePs2$blackMotiv, paired = T); effsize::cohen.d(whitePs2$whiteMotiv, whitePs2$blackMotiv, paired = T)
#descriptives
#mean
whitePs2 %>%
  select(blackExp, whiteExp, blackMotiv, whiteMotiv) %>%
  apply(2, mean)
#sd
whitePs2 %>%
  select(blackExp, whiteExp, blackMotiv, whiteMotiv) %>%
  apply(2, sd)


ggplot(whitePs2) +
  geom_histogram(aes(blackMotiv), color = 'black', fill = 'red') +
  geom_histogram(aes(whiteMotiv), color = 'black', fill = 'blue') +
  labs(x = 'black = red \n white = blue',
       title = 'motivation') +
  theme_bw()

ggplot(whitePs2) +
  geom_histogram(aes(blackExp), color = 'black', fill = 'red') +
  geom_histogram(aes(whiteExp), color = 'black', fill = 'blue') +
  labs(x = 'black = red \n white = blue',
       title = 'experience') +
  theme_bw()


#correlate within race
s2Data %>% 
  group_by(race) %>%
  select(race, blackExp, whiteExp, blackMotiv, whiteMotiv) %>%
  correlation::correlation()


#select needed columns
s2 <- s2Data %>%
  select(subj, block, X1_recog:X48_recog, blackExp, whiteExp, blackMotiv, whiteMotiv, race, sex)

#split the data by blocks
b1 <- filter(s2, block == "encoding-block1")
b2 <- filter(s2, block == "encoding-block2")

#rename blocks
b1Names <- paste0(
  rep("stim"),
  rep(1:48, each = 1),
  rep(c("white", "black"), each = 24),
  rep(c("old", "new"), each = 1)
)

b2Names <- paste0(
  rep("stim"),
  rep(1:48, each = 1),
  rep(c("white", "black"), each = 24),
  rep(c("new", "old"), each = 1)
)

names(b1)[3:50] <- b1Names
names(b2)[3:50] <- b2Names

#reshape to long form
b1L <- b1 %>%
  gather(key = "stimID", value = "seenBefore",
         stim1whiteold:stim48blacknew) %>%
  separate(col = "stimID", into = c("stimID", "type"), sep = -3) %>%
  separate(col = "stimID", into = c("stimID", "stimRace"), sep = -5) %>%
  mutate(raceC = case_when(
    race == "white" ~ 1,
    race == "black" ~ -1
  )) %>%
  mutate(raceCat = case_when(
    race == "white" & stimRace == "white" ~ "same",
    race == "white" & stimRace == "black" ~ "other",
    race == "black" & stimRace == "black" ~ "same",
    race == "black" & stimRace == "white" ~ "other"
  )) %>%
  mutate(raceCatC = case_when(
    raceCat == "same" ~ 1,
    raceCat == "other" ~ -1
  ))

b2L <- b2 %>%
  gather(key = "stimID", value = "seenBefore",
         stim1whitenew:stim48blackold) %>%
  separate(col = "stimID", into = c("stimID", "type"), sep = -3) %>%
  separate(col = "stimID", into = c("stimID", "stimRace"), sep = -5) %>%
  mutate(raceC = case_when(
    race == "white" ~ 1,
    race == "black" ~ -1
  )) %>%
  mutate(raceCat = case_when(
    race == "white" & stimRace == "white" ~ "same",
    race == "white" & stimRace == "black" ~ "other",
    race == "black" & stimRace == "black" ~ "same",
    race == "black" & stimRace == "white" ~ "other"
  )) %>%
  mutate(raceCatC = case_when(
    raceCat == "same" ~ 1,
    raceCat == "other" ~ -1
  )) 


#merge
s2Long <- rbind(b1L, b2L)

#create accuracy column
s2Long$accuracy <- with(s2Long,
                        case_when(
                          seenBefore == 1 & type == "old" ~ 1,
                          seenBefore == 2 & type == "old" ~ 0,
                          seenBefore == 1 & type == "new" ~ 0,
                          seenBefore == 2 & type == "new" ~ 1
                        ))

#separate black and white participants to comptue same/other experience motivation items
#black
blackPs <- filter(s2Long, race == "black")

#own-race experience bias
blackPs$ownRaceExp <- blackPs$whiteExp - blackPs$blackExp

#own-race motivation bias
blackPs$ownRaceMotiv <- blackPs$whiteMotiv - blackPs$blackMotiv

#white
whitePs <- filter(s2Long, race == "white")

#own-race experience bias
whitePs$ownRaceExp <- whitePs$blackExp - whitePs$whiteExp

#own-race motivation bias
whitePs$ownRaceMotiv <- whitePs$blackMotiv - whitePs$whiteMotiv


#merge
s2Final <- rbind(blackPs, whitePs)

#scale these (for glmm analyses)
s2Final$orExpStd <- scale(s2Final$ownRaceExp)
s2Final$orMotivStd <- scale(s2Final$ownRaceMotiv)

#plot the distributions
#experience
ggplot(s2Final, aes(ownRaceExp, fill = race)) +
  geom_histogram(color = "black", alpha = .7, bins = 20) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_classic() +
  xlab("own race expertise")

#motivation
ggplot(s2Final, aes(ownRaceMotiv, fill = race)) +
  geom_histogram(color = "black", alpha = .7, bins = 20) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_classic() +
  xlab("own race motivation")


## analyses
#confirm ORE
m1ORB <- glmer(accuracy ~ raceCatC + (1|subj) + (1|stimID), family = binomial, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
               data = s2Final)
summary(m1ORB)

#test interaction with race
m1 <- glmer(accuracy ~ raceCatC * raceC + (1|subj) + (1|stimID), family = binomial, 
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
            data = s2Final)
summary(m1)
exp(fixef(m1))
sem1 <- sqrt(diag(vcov(m1)))
exp(cim1 <- cbind(est = fixef(m1), ll = fixef(m1) - (1.96*sem1), ul = fixef(m1) + (1.96*sem1)))

#check fit
check_model(m1)

m1Resid <- simulateResiduals(m1, n = 500)

testUniformity(m1Resid)
testDispersion(m1Resid)
testZeroInflation(m1Resid)

#additional glmm analyses
#enter experience and motivation as additive terms
m2 <- glmer(accuracy ~ raceCatC * raceC + orExpStd + orMotivStd + (1|subj) + (1|stimID), family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
            data = s2Final)
summary(m2)
exp(fixef(m2))
sem2 <- sqrt(diag(vcov(m2)))
exp(cim2 <- cbind(est = fixef(m2), ll = fixef(m2) - (1.96*sem2), ul = fixef(m2) + (1.96*sem2)))

#test predicted interactions between experience/motivation
m3 <- glmer(accuracy ~ raceCatC * raceC + orExpStd + orMotivStd + orExpStd:orMotivStd + (1|subj) + (1|stimID), family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
            data = s2Final)
summary(m3)
exp(fixef(m3))
sem3 <- sqrt(diag(vcov(m3)))
exp(cim3 <- cbind(est = fixef(m3), ll = fixef(m3) - (1.96*sem3), ul = fixef(m3) + (1.96*sem3)))

#add full interaction terms (test whether same/other race recognition interacts with experience/motivation)
m4 <- glmer(accuracy ~ raceCatC * raceC + orExpStd + orMotivStd + raceCatC:orExpStd + raceCatC:orMotivStd + orExpStd:orMotivStd + raceCatC:orMotivStd:orExpStd + (1|subj) + (1|stimID), family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
            data = s2Final)
summary(m4)
exp(fixef(m4))
sem4 <- sqrt(diag(vcov(m4)))
exp(cim4 <- cbind(est = fixef(m4), ll = fixef(m4) - (1.96*sem4), ul = fixef(m4) + (1.96*sem4)))

#explore interaction between experience and motivation (without racecat it's hard to attribute this to ORE)
sim_slopes(m4, pred = "orMotivStd", modx = "orExpStd")
interact_plot(m4, pred = "orMotivStd", modx = "orExpStd",
              interval = T,
              int.type = "confidence",
              legend.main = "Experience") +
  theme_classic() +
  xlab("Motivation") +
  ylab("Recognition Accuracy") +
  theme(legend.position = "top")

#get average accuracy and plot
#group-level
s2Summary <- summarySE(data = s2Final,
                       measurevar = "accuracy",
                       groupvars = c("raceCat"),
                       na.rm = T)
s2Summary

summarySE(data = s2Final,
          measurevar = "accuracy",
          groupvars = c("race"),
          na.rm = T)

#participant averages (for points)
s2SumPart <- summarySE(data = s2Final,
                       measurevar = "accuracy",
                       groupvars = c("subj", "raceCat"),
                       na.rm = T)

#plot
(p1 <- ggplot(s2Summary, aes(raceCat, accuracy)) +
    geom_bar(stat = "identity", color = "black", alpha = .8, fill = "#0073cf") +
    geom_errorbar(aes(ymin = accuracy - ci, ymax = accuracy + ci),
                  width = .3, alpha = .7) +
    geom_point(data = s2SumPart, aes(raceCat, accuracy),
               alpha = .25, position = position_jitter(.15, .15), color = "#ffb612") +
    scale_y_continuous(expand = c(0, 0)) +
    expand_limits(y = 1) +
    theme_classic(base_size = 15) +
    xlab("") +
    ylab("Recognition Accuracy") +
    scale_x_discrete(labels = c("Other-race\ntargets", "Same-race\ntargets")) +
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12))
)

# ggsave("study 2 accuracy data.jpg", device = "jpeg", units = "cm")


## calculate d' for discriminability analyses
s2Med <- s2Final %>%
  select(subj, sex, race, raceCat, type, seenBefore, accuracy, ownRaceExp, ownRaceMotiv)

s2Med$hits <- with(s2Med, case_when(
  seenBefore == 1 & type == "old" ~ 1,
  TRUE ~ 0
))

s2Med$FAs <- with(s2Med, case_when(
  seenBefore == 1 & type == "new" ~ 1,
  TRUE ~ 0
))

#collapse into rates
s2Group <- s2Med %>%
  group_by(subj, race, raceCat) %>%
  dplyr::summarize(
    mHits = mean(hits),
    mFAs = mean(FAs),
    mExp = mean(ownRaceExp),
    mMot = mean(ownRaceMotiv)
  ) %>% 
  mutate(mHits = case_when(
    mHits == 1 ~ .9999,
    mHits == 0 ~ .0001,
    TRUE ~ mHits
  )) %>%
  mutate(mFAs = case_when(
    mFAs == 1 ~ .9999,
    mFAs == 0 ~ .0001,
    TRUE ~ mFAs
  )) %>%
  mutate(zHits = qnorm(mHits)) %>%
  mutate(zFAs = qnorm(mFAs)) %>%
  mutate(dprime = zHits - zFAs) %>%
  select(subj, race, raceCat, mExp, mMot, dprime) %>%
  pivot_wider(names_from = "raceCat", values_from = "dprime") %>%
  mutate(oreD = same - other) %>%
  as.data.frame
s2Group <- s2Group %>%
  mutate(mExp = scale(mExp)) %>%
  mutate(mMot = scale(mMot)) %>%
  mutate(isWhite = case_when(
    race == "white" ~ 1,
    race == "black" ~ 0,
    TRUE ~ 0
  )) %>%
  as.data.frame

outliers <- s2Group %>%
  select(race, subj, oreD) %>%
  group_by(race) %>%
  rstatix::identify_outliers(oreD) %>%
  filter(is.extreme == TRUE)

s2Group <- s2Group %>% filter(!(subj %in% outliers$subj))

#look at correlations
corMat <- s2Group[,c(3,4,7)]
cors <- psych::corr.test(corMat)
cors

ggcorrplot(cors$r,
           type = "lower",
           lab = T,
           p.mat = cors$p) +
  scale_x_discrete(labels = c("Other Race \nMotivation", "Other Race \nEffect")) +
  scale_y_discrete(labels = c("Other Race \nExperience", "Other Race \nMotivation"))


#look at grouped effects
s2Group %>%
  select(race, mExp, mMot, oreD) %>%
  group_by(race) %>%
  correlation()


#model effects (additive)
lm1 <- lm(oreD ~ isWhite + mExp + mMot, data = s2Group)
summary(lm1)

#interactive
lm2 <- lm(oreD ~ isWhite + mExp + mMot + mExp:mMot, data = s2Group)
summary(lm2)

#full
lm3 <- lm(oreD ~ isWhite * mExp * mMot, data = s2Group)
summary(lm3) 
standardize_parameters(lm3)

#test model fit
anova(lm1, lm2, lm3)


#plot the race effects in a bar graph 
s2Race <- summarySE(s2Group,
                    measurevar = "oreD",
                    groupvars = "race")
s2Race

s2RaceP <- summarySE(s2Group,
                     measurevar = "oreD",
                     groupvars = c("subj", "race"))


(p2 <- ggplot(s2Race, aes(race, oreD)) +
    geom_bar(stat = "identity", color = "black", alpha = .8, fill = "#0073cf") +
    geom_errorbar(aes(ymin = oreD - ci, ymax = oreD + ci),
                  alpha = .7, width = .3) +
    geom_point(data = s2RaceP, aes(race, oreD),
               alpha = .25, position = position_jitter(.15, .15), color = "#ffb612") +
    theme_classic() +
    xlab("") +
    scale_x_discrete(labels = c("Black \nParticipants", "White \nParticipants")) +
    ylab("Other Race Effect (d')"))

#plot the effects (marginal effect of experience, effect of white)
(p2.1 <- ggplot(s2Group, aes(mExp, oreD)) +
  geom_point(alpha = .5, color = "#0073cf") +
  geom_smooth(method = "lm", color = "#ffb612") +
  theme_classic(base_size = 15) +
  xlab("Same Race Experience (Standardized)") +
  ylab("Other Race Effect (d')"))

(p2.2 <- ggplot(s2Group, aes(isWhite, oreD)) +
    geom_point(alpha = .5, position = position_jitter(.05, .05), color = "#0073cf") +
    geom_smooth(method = "lm", color = "#ffb612") +
    theme_classic(base_size = 15) +
    xlab("") +
    scale_x_continuous(n.breaks = 2,
                       labels = c("Black \nParticipants", "White \nParticipants")) +
    ylab("Other Race Effect (d')"))

ggarrange(p1, p2, nrow = 1, ncol = 2, labels = c("A","B"))

# ggsave("study 2 plots.jpg", device = "jpeg", units = "cm")


#spsp plots

jtools::effect_plot(lm3,
                    pred = 'mMot',
                    plot.points = T,
                    interval = T,
                    data = s2Group,
                    int.type = 'confidence',
                    x.label = 'Desired Interaction (Standardized)',
                    y.label = "Cross Race Effect (d\')",
                    colors = '#ffb612',
                    point.color = '#003831') +
  theme_classic()

# ggsave('study 2 - mMot (SPSP).jpg', units = 'cm', device = 'jpeg')


jtools::effect_plot(lm3,
                    pred = 'mExp',
                    plot.points = T,
                    interval = T,
                    data = s2Group,
                    int.type = 'confidence',
                    x.label = 'Experience (Standardized)',
                    y.label = "Cross Race Effect (d\')",
                    colors = '#ffb612',
                    point.color = '#003831') +
  theme_classic()

# ggsave('study 2 - mExp (SPSP).jpg', units = 'cm', device = 'jpeg')


interact_plot(lm3,
              pred = 'mExp',
              modx = 'mMot',
              data = s2Group,
              plot.points = T,
              interval = F,
              int.type = 'confidence',
              point.alpha = .25,
              colors = 'Greens',
              x.label = 'Cross Race Experience',
              y.label = "Cross Race Effect (d')",
              legend.main = 'Desired \nInteraction') +
  theme_classic() +
  theme(legend.position = 'top')

# ggsave('study 2 - mExp x mMot (SPSP).jpg', units = 'cm', device = 'jpeg')


#test the effect on only black participants (white ps had no motivation differences for same/other races)
blackPsLM <- filter(s2Group, race == "black")

lm1B <- lm(oreD ~ mExp * mMot, data = blackPsLM)
summary(lm1B)



# now add in gender analyses (exploratory)
#collapse into rates
s2GroupGender <- s2Med %>%
  group_by(subj, sex, race, raceCat) %>%
  dplyr::summarize(
    mHits = mean(hits),
    mFAs = mean(FAs),
    mExp = mean(ownRaceExp),
    mMot = mean(ownRaceMotiv)
  ) %>% 
  mutate(mHits = case_when(
    mHits == 1 ~ .9999,
    mHits == 0 ~ .0001,
    TRUE ~ mHits
  )) %>%
  mutate(mFAs = case_when(
    mFAs == 1 ~ .9999,
    mFAs == 0 ~ .0001,
    TRUE ~ mFAs
  )) %>%
  mutate(zHits = qnorm(mHits)) %>%
  mutate(zFAs = qnorm(mFAs)) %>%
  mutate(dprime = zHits - zFAs) %>%
  select(subj, sex, race, raceCat, mExp, mMot, dprime) %>%
  pivot_wider(names_from = "raceCat", values_from = "dprime") %>%
  mutate(oreD = same - other) %>%
  as.data.frame
s2GroupGender <- s2GroupGender %>%
  mutate(mExp = scale(mExp)) %>%
  mutate(mMot = scale(mMot)) %>%
  mutate(isWhite = case_when(
    race == "white" ~ 1,
    race == "black" ~ 0,
    TRUE ~ 0
  )) %>%
  mutate(sameSex = case_when(
    sex == 1 ~ 1,
    sex != 1 ~ 0
  )) %>%
  as.data.frame

outliers <- s2GroupGender %>%
  select(race, sex, subj, oreD) %>%
  group_by(race, sex) %>%
  rstatix::identify_outliers(oreD) %>%
  filter(is.extreme == TRUE)

s2GroupGender <- s2GroupGender %>% filter(!(subj %in% outliers$subj))

# include gender to control for things
#full
lm3_sex <- lm(oreD ~ isWhite * mExp * mMot + sameSex, data = s2GroupGender)
summary(lm3_sex) 
standardize_parameters(lm3_sex)




#no major effects.. explore mediations
set.seed(147)

#expertise -> friendship motivation -> recognition accuracy
med1 <- '
        #a path (experience -> motivation)
        mMot ~ a*mExp
        
        #b path (motivation -> oreD)
        oreD ~ b*mMot
        
        #cprime path (direct effect; experience -> oreD)
        oreD ~ cprime*mExp
        
        #indirect and total effects
        ab := a*b
        total := cprime + ab
        '
med1Sem <- sem(med1, data = s2Group, se = "bootstrap", bootstrap = 5000)
summary(med1Sem, standardized = T)
parameterEstimates(med1Sem, boot.ci.type = "bca.simple", standardize = T)

#flip it (motivation -> experience -> ORE)
med2 <- '
        #a path (motivation -> experience)
        mExp ~ a*mMot
        
        #b path (experience -> oreD)
        oreD ~ b*mExp
        
        #cprime path (direct effect; motivation -> oreD)
        oreD ~ cprime*mMot
        
        #indirect and total effects
        ab := a*b
        total := cprime + ab
        '
med2Sem <- sem(med2, data = s2Group, se = "bootstrap", bootstrap = 5000)
summary(med2Sem, standardized = T)
parameterEstimates(med2Sem, boot.ci.type = "bca.simple", standardize = T)
