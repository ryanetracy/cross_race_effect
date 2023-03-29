##################################################
# cross-race effect - study 1
# expertise vs motivation
# motivation measured by anticipated interaction 
# & recognition accuracy importance
##################################################

#packages
pckgs <- c("lme4", "lmerTest", "effectsize", "interactions", "performance", 
           "see", "Rmisc", "correlation", "ggcorrplot", "DHARMa", "lavaan", 
           "car", "ggraph", "ggpubr", "tidyverse")

#check installation and load
for (i in 1:length(pckgs)) {
  # if(!pckgs[[i]] %in% installed.packages()) {
  #   install.packages(pckgs[[i]])
  # }
  lapply(pckgs[[i]], library, character.only = T)
}


#data
s1Data <- read.csv("data/study 1 data.csv")

#clean responses
#remove consent != 1
s1Data <- filter(s1Data, consent == 1)

#check duplicated IP and response IDs
s1Data$dupID <- duplicated(s1Data$ResponseId)
s1Data$dupIP <- duplicated(s1Data$IPAddress)

s1Data <- filter(s1Data, dupID == FALSE & dupIP == FALSE)


#demographics
#age
mean(s1Data$age, na.rm = T)
sd(s1Data$age, na.rm = T)

#race
race <- plyr::count(s1Data$race) %>%
  mutate(prop = freq/(sum(freq)) * 100) %>%
  mutate(x = case_when(
    x == 1 ~ "white",
    x == 2 ~ "black",
    x == 6 ~ "biracial"
  ))
race 

#recode race in the data set
s1Data$race <- case_when(
  s1Data$race == 1 ~ "white",
  s1Data$race == 2 ~ "black",
  s1Data$race == 6 ~ "black" #include biracial individual with black participants
)

#gender
gender <- plyr::count(s1Data$sex) %>%
  mutate(prop = freq/sum(freq) * 100) %>%
  mutate(x = case_when(
    x == 1 ~ "male",
    x == 2 ~ "female",
    x == 3 ~ "nonbinary"
  ))
gender


#check bot check responses
bots <- plyr::count(s1Data$botCheck) #nothing out of the ordinary

#check experiment probe
expProbe <- plyr::count(s1Data$manipCheck) 
#look for keywords in manipulation check
s1Data$expProbe <- str_detect(s1Data$manipCheck, "other race effect|cross race effect") #none found

#rename responseID column (because laziness)
names(s1Data)[c(9, 296)] <- c("subj", "block")

# compute alpha for experience measures
white_exp <- s1Data[,c(270,271,274,275,278,281,282)]
black_exp <- s1Data[,c(269,272,273,276,277,279,280,283)]

psych::alpha(white_exp)
psych::alpha(black_exp)

#calculate experience and motivation columns
#experience
#white experience
s1Data$whiteExp <- rowMeans(s1Data[,c(270,271,274,275,278,281,282)], na.rm = T)
#black experience
s1Data$blackExp <- rowMeans(s1Data[,c(269,272,273,276,277,279,280,283)], na.rm = T)

#filter by race
blackPs1 <- filter(s1Data, race == "black")
whitePs1 <- filter(s1Data, race == "white")

#test for differences in experience/motivation
#black
#experience
t.test(blackPs1$blackExp, blackPs1$whiteExp, paired = T); effsize::cohen.d(blackPs1$blackExp, blackPs1$whiteExp, paired = T)
#motivation - recognition
t.test(blackPs1$blkRecog_1, blackPs1$whtRecog_1, paired = T); effsize::cohen.d(blackPs1$blkRecog_1, blackPs1$whtRecog_1, paired = T)
#motivation - interaction
t.test(blackPs1$blkInter_1, blackPs1$whtInter_1, paired = T); effsize::cohen.d(blackPs1$blkInter_1, blackPs1$whtInter_1, paired = T)
#descriptives
#means
blackPs1 %>%
  select(blackExp, whiteExp, blkRecog_1, whtRecog_1, blkInter_1, whtInter_1) %>%
  apply(2, mean)
#sds
blackPs1 %>%
  select(blackExp, whiteExp, blkRecog_1, whtRecog_1, blkInter_1, whtInter_1) %>%
  apply(2, sd)

#white
#experience
t.test(whitePs1$whiteExp, whitePs1$blackExp, paired = T); effsize::cohen.d(whitePs1$whiteExp, whitePs1$blackExp, paired = T)
#motivation - recognition
t.test(whitePs1$whtRecog_1, whitePs1$blkRecog_1, paired = T); effsize::cohen.d(whitePs1$whtRecog_1, whitePs1$blkRecog_1, paired = T) #interesting effect...
#motivation - interaction
t.test(whitePs1$whtInter_1, whitePs1$blkInter_1, paired = T); effsize::cohen.d(whitePs1$whtInter_1, whitePs1$blkInter_1, paired = T)
#descriptives
#means
whitePs1 %>%
  select(blackExp, whiteExp, blkRecog_1, whtRecog_1, blkInter_1, whtInter_1) %>%
  apply(2, mean)
#sds
whitePs1 %>%
  select(blackExp, whiteExp, blkRecog_1, whtRecog_1, blkInter_1, whtInter_1) %>%
  apply(2, sd)

#correlate within race
s1Data %>% 
  group_by(race) %>%
  select(race, blackExp, whiteExp, blkRecog_1, whtRecog_1, blkInter_1, whtInter_1) %>%
  correlation::correlation()


#select appropriate columns
s1 <- s1Data %>%
  select(subj, block, X1_recog:X48_recog, whiteExp, blackExp, blkRecog_1:whtInter_1, race, sex)


#split the data by blocks
b1 <- filter(s1, block == "encoding-block1")
b2 <- filter(s1, block == "encoding-block2")

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
    race == "black" ~ -1,
    race == "biracial" ~ 0
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
    race == "black" ~ -1,
    race == "biracial" ~ 0
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
s1Long <- rbind(b1L, b2L)

#create accuracy column
s1Long$accuracy <- with(s1Long,
                        case_when(
                          seenBefore == 1 & type == "old" ~ 1,
                          seenBefore == 2 & type == "old" ~ 0,
                          seenBefore == 1 & type == "new" ~ 0,
                          seenBefore == 2 & type == "new" ~ 1
                        ))

#separate black and white participants to compute experience/motivation biases (always toward own race)
#black participants
blackPs <- filter(s1Long, race == "black")

#experience bias
blackPs$ownRaceExp <- blackPs$whiteExp - blackPs$blackExp

#motivation bias
#importance 
blackPs$ownRaceRecog <- blackPs$whtRecog_1 - blackPs$blkRecog_1

#interaction
blackPs$ownRaceInter <- blackPs$whtInter_1 - blackPs$blkInter_1

#white participants
whitePs <- filter(s1Long, race == "white")

#experience bias
whitePs$ownRaceExp <- whitePs$blackExp - whitePs$whiteExp

#motivation bias
#importance
whitePs$ownRaceRecog <- whitePs$blkRecog_1 - whitePs$whtRecog_1

#interaction
whitePs$ownRaceInter <- whitePs$blkInter_1 - whitePs$whtInter_1


#merge these
s1Final <- rbind(blackPs, whitePs)
#scale the variables
s1Final$orExpStd <- scale(s1Final$ownRaceExp)
s1Final$orIntStd <- scale(s1Final$ownRaceInter)
s1Final$orRecStd <- scale(s1Final$ownRaceRecog)

#visualize the distributions of the individual difference measures of experience and motivation (both)
#experience
ggplot(s1Final, aes(ownRaceExp, fill = race)) +
  geom_histogram(color = "black", alpha = .7, bins = 20) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_classic() +
  xlab("own race expertise")

#motivation
#importance
ggplot(s1Final, aes(ownRaceRecog, fill = race)) +
  geom_histogram(color = "black", alpha = .7, bins = 7) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_classic() +
  xlab("own race recognition importance")

#anticipation
ggplot(s1Final, aes(ownRaceInter, fill = race)) +
  geom_histogram(color = "black", alpha = .7, bins = 15) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_classic() +
  xlab("own race anticipated interaction")


## analyses

#confirm the presence of an own race bias
m1ORB <- glmer(accuracy ~ raceCatC + (1|subj) + (1|stimID), family = binomial, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
               data = s1Final)
summary(m1ORB) #confirmed


#test interaction with race 
m1 <- glmer(accuracy ~ raceCatC * raceC + (1|subj) + (1|stimID), family = binomial, 
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
            data = s1Final)
summary(m1)
exp(fixef(m1))
sem1 <- sqrt(diag(vcov(m1)))
exp(cim1 <- cbind(est = fixef(m1), ll = fixef(m1) - (1.96*sem1), ul = fixef(m1) + (1.96*sem1)))

#check model fit
check_model(m1)

m1Resid <- simulateResiduals(m1, n = 500)

testUniformity(m1Resid)
testDispersion(m1Resid)
testZeroInflation(m1Resid)


#additional glmm analyses
# #enter experience and motivation as additive terms
# m2 <- glmer(accuracy ~ raceCatC * raceC + orExpStd + orRecStd + orIntStd + (1|subj) + (1|stimID), family = binomial,
#             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
#             data = s1Final)
# summary(m2)
# exp(fixef(m2))
# sem2 <- sqrt(diag(vcov(m2)))
# exp(cim2 <- cbind(est = fixef(m2), ll = fixef(m2) - (1.96*sem2), ul = fixef(m2) + (1.96*sem2)))

#test predicted interactions between experience/motivation
# m3 <- glmer(accuracy ~ raceCatC * raceC + orExpStd + orRecStd + orIntStd + orExpStd:orRecStd + orExpStd:orIntStd + (1|subj) + (1|stimID), family = binomial,
#             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
#             data = s1Final)
# summary(m3)
# exp(fixef(m3))
# sem3 <- sqrt(diag(vcov(m3)))
# exp(cim3 <- cbind(est = fixef(m3), ll = fixef(m3) - (1.96*sem3), ul = fixef(m3) + (1.96*sem3)))

#add full interaction terms (test whether same/other race recognition interacts with experience/motivation)
m4 <- glmer(accuracy ~ raceCatC * raceC + orExpStd + orRecStd + orIntStd + raceCatC:orExpStd + raceCatC:orRecStd + raceCatC:orIntStd + orExpStd:orRecStd + orExpStd:orIntStd + raceCatC:orRecStd:orExpStd + raceCatC:orIntStd:orExpStd + (1|subj) + (1|stimID), family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)),
            data = s1Final)
summary(m4)
exp(fixef(m4))
sem4 <- sqrt(diag(vcov(m4)))
exp(cim4 <- cbind(est = fixef(m4), ll = fixef(m4) - (1.96*sem4), ul = fixef(m4) + (1.96*sem4)))

#look at moderate interaction between race category and anticipated interaction
sim_slopes(m4, pred = "orIntStd", modx = "raceCatC")
interact_plot(m4, pred = "orIntStd", modx = "raceCatC",
              interval = T,
              int.type = "confidence",
              legend.main = "Stimulus \nCategory",
              modx.labels = c("Other Race", "Same Race")) +
  theme_classic() +
  xlab("Anticipated Interaction") +
  ylab("Recognition Accuracy") +
  theme(legend.position = "top")


#generate average accuracy and plot
#group-level
s1Summary <- summarySE(data = s1Final,
                       measurevar = "accuracy",
                       groupvars = c("raceCat"),
                       na.rm = T)
s1Summary

#participant averages (for points)
s1SumPart <- summarySE(data = s1Final,
                       measurevar = "accuracy",
                       groupvars = c("subj", "raceCat"),
                       na.rm = T)

# just look at race
summarySE(data = s1Final,
          measurevar = "accuracy",
          groupvars = c("race"),
          na.rm = T)


#plot
(p1 <- ggplot(s1Summary, aes(raceCat, accuracy)) +
  geom_bar(stat = "identity", color = "black", alpha = .8, fill = "#0073cf") +
  geom_errorbar(aes(ymin = accuracy - ci, ymax = accuracy + ci),
                width = .3, alpha = .7) +
  geom_point(data = s1SumPart, aes(raceCat, accuracy),
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
# ggsave("study 1 accuracy data.jpg", device = "jpeg", units = "cm")


## calculate d' for discriminability analyses
s1Med <- s1Final %>%
  select(subj, race, sex, raceCat, type, seenBefore, accuracy, ownRaceExp, ownRaceInter, ownRaceRecog)

s1Med$hits <- with(s1Med, case_when(
  seenBefore == 1 & type == "old" ~ 1,
  TRUE ~ 0
))

s1Med$FAs <- with(s1Med, case_when(
  seenBefore == 1 & type == "new" ~ 1,
  TRUE ~ 0
))

#collapse into rates
s1Group <- s1Med %>%
  group_by(subj, race, raceCat) %>%
  dplyr::summarize(
    mHits = mean(hits),
    mFAs = mean(FAs),
    mExp = mean(ownRaceExp),
    mRec = mean(ownRaceRecog),
    mInt = mean(ownRaceInter)
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
  select(subj, race, raceCat, mExp, mRec, mInt, dprime) %>%
  pivot_wider(names_from = "raceCat", values_from = "dprime") %>%
  mutate(oreD = same - other) %>%
  as.data.frame
s1Group <- s1Group %>%
  mutate(mExp = scale(mExp)) %>%
  mutate(mRec = scale(mRec)) %>%
  mutate(mInt = scale(mInt)) %>%
  mutate(isWhite = case_when(
    race == "white" ~ 1,
    race == "black" ~ 0,
    TRUE ~ 0
  )) %>%
  as.data.frame

outliers <- s1Group %>%
  select(race, subj, oreD) %>%
  group_by(race) %>%
  rstatix::identify_outliers(oreD) %>%
  filter(is.extreme == TRUE)
outliers

s1Group <- s1Group %>% filter(!(subj %in% outliers$subj))


#look at correlations
corMat <- s1Group[,c(3:5,8)]
cors <- psych::corr.test(corMat)
cors

ggcorrplot(cors$r,
           type = "lower",
           lab = T,
           p.mat = cors$p) +
  scale_x_discrete(labels = c("Recognition \nImportance", "Anticipated \nInteraction", "Other Race \nEffect")) +
  scale_y_discrete(labels = c("Same Race \nExperience", "Recognition \nImportance", "Anticipated \nInteraction"))


#look at grouped effects
s1Group %>%
  select(race, mExp, mRec, mInt, oreD) %>%
  group_by(race) %>%
  correlation()

#model effects (additive)
lm1 <- lm(oreD ~ isWhite + mExp + mRec + mInt, data = s1Group)
summary(lm1)

#interactive
lm2 <- lm(oreD ~ isWhite + mExp + mRec + mInt + mExp:mRec + mExp:mInt, data = s1Group)
summary(lm2)

#full
lm3 <- lm(oreD ~ isWhite + mExp + mRec + mInt + mExp:mRec + mExp:mInt + isWhite:mExp:mRec + isWhite:mExp:mInt, data = s1Group)
summary(lm3)
standardize_parameters(lm3)

plot(lm3)

#test model fit
anova(lm1, lm2, lm3)


#plot the effects of mInt on oreD
(p2 <- ggplot(s1Group, aes(mExp, oreD)) +
  geom_point(alpha = .5, color = "#0073cf") +
  geom_smooth(method = "lm", color = "#ffb612") +
  theme_classic(base_size = 15) +
  xlab("Anticipated Interaction") +
  ylab("Other Race Effect (d')") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
)

# ggsave("ore - dprime plot.jpg", device = "jpeg", units = "cm")

ggarrange(p1, p2, nrow = 1, ncol = 2, labels = c("A", "B"))
# ggsave("study 1 plots.jpg", device = "jpeg", units = "cm")

# get summary stats of d'
summarySE(s1Group,
          measurevar = 'oreD',
          groupvars = 'race')

t.test(oreD ~ race, data = s1Group, var.equal = T)
effsize::cohen.d(oreD ~ race, data = s1Group)


## spsp regression plots
jtools::effect_plot(lm3,
                    pred = 'mInt',
                    plot.points = T,
                    interval = T,
                    data = s1Group,
                    int.type = 'confidence',
                    x.label = 'Anticipated Interaction',
                    y.label = "Cross Race Effect (d\')",
                    colors = '#ffb612',
                    point.color = '#003831') +
  theme_classic()

# ggsave('study 1 - mInt (SPSP).jpg', units = 'cm', device = 'jpeg')


jtools::effect_plot(lm3,
                    pred = 'mExp',
                    plot.points = T,
                    interval = T,
                    data = s1Group,
                    int.type = 'confidence',
                    x.label = 'Experience (Standardized)',
                    y.label = "Cross Race Effect (d\')",
                    colors = '#ffb612',
                    point.color = '#003831') +
  theme_classic()

# ggsave('study 1 - mExp (SPSP).jpg', units = 'cm', device = 'jpeg')


interact_plot(lm3,
              pred = 'mExp',
              modx = 'mInt',
              data = s1Group,
              plot.points = T,
              interval = F,
              int.type = 'confidence',
              point.alpha = .25,
              colors = 'Greens',
              x.label = 'Cross Race Experience',
              y.label = "Cross Race Effect (d')",
              legend.main = 'Anticipated \nInteraction') +
  theme_classic() +
  theme(legend.position = 'top')

# ggsave('study 1 - mExp x mInt (SPSP).jpg', units = 'cm', device = 'jpeg')


#explore separate effects for black/white participants
blackPsLm <- filter(s1Group, race == "black")
whitePsLm <- filter(s1Group, race == "white")

#model black participants
lmB1 <- lm(oreD ~ mExp*mRec*mInt, data = blackPsLm)
summary(lmB1)
standardize_parameters(lmB1)

#model white participants
lmW1 <- lm(oreD ~ mExp*mRec*mInt, data = whitePsLm)
summary(lmW1)
standardize_parameters(lmW1)



# now add in gender analyses (exploratory)
#collapse into rates
s1GroupGender <- s1Med %>%
  group_by(subj, race, raceCat, sex) %>%
  dplyr::summarize(
    mHits = mean(hits),
    mFAs = mean(FAs),
    mExp = mean(ownRaceExp),
    mRec = mean(ownRaceRecog),
    mInt = mean(ownRaceInter)
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
  select(subj, sex, race, raceCat, mExp, mRec, mInt, dprime) %>%
  pivot_wider(names_from = "raceCat", values_from = "dprime") %>%
  mutate(oreD = same - other) %>%
  as.data.frame
s1GroupGender <- s1GroupGender %>%
  mutate(mExp = scale(mExp)) %>%
  mutate(mRec = scale(mRec)) %>%
  mutate(mInt = scale(mInt)) %>%
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

outliers <- s1GroupGender %>%
  select(race, sameSex, subj, oreD) %>%
  group_by(race, sameSex) %>%
  rstatix::identify_outliers(oreD) %>%
  filter(is.extreme == TRUE)
outliers

s1GroupGender <- s1GroupGender %>% filter(!(subj %in% outliers$subj))


# include gender to control for things
lm3_sex <- lm(oreD ~ isWhite + mExp + mRec + mInt + mExp:mRec + mExp:mInt + isWhite:mExp:mRec + isWhite:mExp:mInt + sameSex, data = s1GroupGender)
summary(lm3_sex)
standardize_parameters(lm3_sex)


#now run mediation models
set.seed(147)

#expertise -> anticipated interaction -> recognition accuracy
med1 <- '
        #a path (experience -> interaction)
        mInt ~ a*mExp
        
        #b path (interaction -> oreD)
        oreD ~ b*mInt
        
        #cprime path (direct effect; experience -> oreD)
        oreD ~ cprime*mExp
        
        #indirect and total effects
        ab := a*b
        total := cprime + ab
        '
med1Sem <- sem(med1, data = s1Group, se = "bootstrap", bootstrap = 5000)
summary(med1Sem, standardized = T)
parameterEstimates(med1Sem, boot.ci.type = "bca.simple", standardize = T)

#expertise -> recognition importance -> recognition accuracy
med2 <- '
        #a path (experience -> interaction)
        mRec ~ a*mExp
        
        #b path (interaction -> oreD)
        oreD ~ b*mRec
        
        #cprime path (direct effect; experience -> oreD)
        oreD ~ cprime*mExp
        
        #indirect and total effects
        ab := a*b
        total := cprime + ab
        '
med2Sem <- sem(med2, data = s1Group, se = "bootstrap", bootstrap = 5000)
summary(med2Sem, standardized = T)
parameterEstimates(med2Sem, boot.ci.type = "bca.simple", standardize = T)

#try a parallel mediation model
med3 <- '
        #a1 path (experience -> interaction)
        mInt ~ a1*mExp
        
        #a2 path (experience -> recognition)
        mRec ~ a2*mExp
        
        #b1 path (interaction -> oreD)
        oreD ~ b1*mInt
        
        #b2 path (recognition -> oreD)
        oreD ~ b2*mRec
        
        #cprime path (direct effect experience -> oreD)
        oreD ~ cp*mExp
        
        #indirect and total effects
        ab1 := a1*b1
        ab2 := a2*b2
        total1 := cp + ab1
        total2 := cp + ab2
        '
med3Sem <- sem(med3, data = s1Group, se = "bootstrap", bootstrap = 5000)
summary(med3Sem, standardize = T)
parameterEstimates(med3Sem, boot.ci.type = "bca.simple", standardize = T)


#flip model 1 over to test whether experience mediates the interaction -> ore link
med4 <- '
        #a path (interaction -> experience)
        mExp ~ a*mInt
        
        #b path (experience -> oreD)
        oreD ~ b*mExp
        
        #cprime path (direct effect; interaction -> oreD)
        oreD ~ cprime*mInt
        
        #indirect and total effects
        ab := a*b
        total := cprime + ab
        '
med4Sem <- sem(med4, data = s1Group, se = "bootstrap", bootstrap = 5000)
summary(med4Sem, standardize = T)
parameterEstimates(med4Sem, boot.ci.type = "bca.simple", standardize = T)


