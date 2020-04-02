library(tidyverse)

location_of_csv = paste0("/Users/zacharyfrere", "/desktop", "/BIOS706", "/midterm", "/frmgham2.csv")
dat <- read.csv(location_of_csv)

#Subset Data so that we only have records that are either in Record 1 or 2
dat_subset <- dat %>% filter(dat$PERIOD <= 2 )

#We want to study the effect of glucose on the incident of MI

#First, we will try to find two groups those who develop and those who do not develop MI
# need to remove those with MI in Period 1 from our dataset

dat_noMI_p1 <- dat_subset %>% group_by(RANDID) %>% filter( PERIOD == 1 && PREVMI == 0  )

#Check the filter above
dat_MI_p1 <- dat_subset %>% group_by(RANDID) %>% filter(PERIOD == 1 && PREVMI == 1)

id_mi_p1 <- unique(dat_MI_p1$RANDID)
id_nomi_p1 <- unique(dat_noMI_p1$RANDID)
intersect(id_mi_p1, id_nomi_p1)
# Since there are no common terms between the two vectors this indicates we have removed all
# observation with RANDIDs who already have MI in Period 1

dat_filter_p2 <- dat_noMI_p1 %>% filter(PERIOD == 2 )
#Remove NA
dat_filter_p2 <- dat_filter_p2 %>% filter(!is.na(GLUCOSE))
#Now, let's test for normality of our variable of interest, Glucose
qqnorm(dat_filter_p2$GLUCOSE)
qqline(dat_filter_p2$GLUCOSE)
hist(dat_filter_p2$GLUCOSE)


#Now, look at normality of the individual groups, those who develop MI and those who do not
dat_filter_p2_devMI <- dat_filter_p2 %>% filter(PREVMI == 1)
dat_filter_p2_nodevMI <- dat_filter_p2 %>% filter(PREVMI == 0)

#Look at distribution for dev MI, Glucose level
qqnorm((dat_filter_p2_devMI$GLUCOSE))
qqline((dat_filter_p2_devMI$GLUCOSE))
hist((dat_filter_p2_devMI$GLUCOSE))
shapiro.test((dat_filter_p2_devMI$GLUCOSE))

#Look at distribution for those who did not develop MI
qqnorm((dat_filter_p2_nodevMI$GLUCOSE))
qqline((dat_filter_p2_nodevMI$GLUCOSE))
hist((dat_filter_p2_nodevMI$GLUCOSE))
shapiro.test((dat_filter_p2_nodevMI$GLUCOSE))
#The results above show that our two groups do not appear to be normally distributed

#We can try a non-paramteric test
wilcox.test((dat_filter_p2_devMI$GLUCOSE), (dat_filter_p2_nodevMI$GLUCOSE))

#Here we can try logistic regression
model <- glm(PREVMI~GLUCOSE+AGE+PREVHYP, data = dat_filter_p2, family = "binomial")
summary(model)

