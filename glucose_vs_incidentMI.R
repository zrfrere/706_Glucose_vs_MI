library(tidyverse)

location_of_csv = paste0(getwd(), "/Desktop/BIOS706/midterm/frmgham2.csv")
dat <- read.csv(location_of_csv)

#exclude period 3 data
#exclude those who had PREVMI at period 1
data_aim3 = dat %>% filter(PERIOD != 3) %>% filter(!(PERIOD == 1 & PREVMI == 1))
data_aim3
#create dataset for Period 1 and Glucose
data_aim31 = data_aim3 %>% filter(PERIOD ==1) %>% select(RANDID, GLUCOSE)
data_aim31

#create dataset for Period 2 and MI
data_aim32 = data_aim3 %>% filter(PERIOD == 2) %>% select(RANDID, PREVMI)
data_aim32

#merge datasets to include one dataset with glucose(Period 1) and MI(Period 2)
data_aim33 = left_join(data_aim31, data_aim32, by='RANDID')
data_aim33

#exclude Na's
data_aim34 = na.omit(data_aim33)
data_aim34

data_aim34 %>% group_by(PREVMI) %>% summarize(n=n())



#Now, look at normality of the individual groups, those who develop MI and those who do not
dat_filter_p2_devMI <- data_aim34 %>% filter(PREVMI == 1)
dat_filter_p2_nodevMI <- data_aim34 %>% filter(PREVMI == 0)

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

data_p1 = data_aim3 %>% filter(PERIOD ==1) %>% select(-c(PREVMI))
data_p2 = data_aim3 %>% filter(PERIOD == 2) %>% select(RANDID, PREVMI)
data_p1_leftj_p2 = left_join(data_p1, data_p2, by='RANDID') %>% filter(!(is.na(GLUCOSE))) %>% filter(!(is.na(PREVMI))) 
data_p1_leftj_p2


model <- glm(PREVMI~GLUCOSE+AGE+PREVHYP, data = data_p1_leftj_p2, family = "binomial")
summary(model)
