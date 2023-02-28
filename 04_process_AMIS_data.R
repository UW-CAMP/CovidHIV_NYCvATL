# Decide on which to break out by age

# Partner nums: MWUT or neg bin?
# Time since last test: what test?

### Intro
library(tidyverse)  # Temp to make stand-alone during writing
library(magrittr)
library(coin)

#################################################################
### Load in data
amis19 <- read.csv(paste("originals/COVID-HIV-MSM-CAMP2019 NO FORMAT.csv", sep=""))
amis20 <- read.csv(paste("originals/COVID-HIV-MSM-CAMP2020 NO FORMAT.csv", sep=""))

amis20.match <- read.csv(paste("originals/13OCT2021_COVID-HIV-MSM-CAMP2020 NO FORMAT.csv", sep=""))  # addition of match info
amis19.dates <- read.csv(paste("originals/12OCT2021_COVID-HIV-MSM-CAMP2019 NO FORMAT.csv", sep=""))  # corrected dates

# Numb of reps
amis20_reps <- table(amis20.match$geo_def_city[!is.na(amis20.match$lastid2019)])
amis20_reps

### Split out data by cityyear
amis <- list()
amis$ATL19 <- amis19 %>% filter(geo_def_city=="ATL")
amis$ATL20 <- amis20 %>% filter(geo_def_city=="ATL")
amis$NYC19 <- amis19 %>% filter(geo_def_city=="NYC")
amis$NYC20 <- amis20 %>% filter(geo_def_city=="NYC")

#################################################################
### Exploratory data analysis on basic demographic variables

### Pop size
lapply(amis, nrow)

### Respondent IDs
sum(amis$ATL20$respondent_id %in% amis$ATL19$respondent_id)
sum(amis$NYC20$respondent_id %in% amis$NYC19$respondent_id)

### Dates
table(as.Date(amis19.dates$IDATE, format="%m/%d/%Y"))
table(as.Date(amis20$IDATE, format="%m/%d/%Y"))

### Age
sapply(1:4, function(x) prop.table(table(amis[[x]]$X_age5yr)))

### Race   1 NH Black, 2 Hispanic, 3 NH White, 4 Other, multiple, unknown

sapply(1:4, function(x) table(amis[[x]]$newrace)) # col = cityyear, row = race
prop.table(sapply(1:4, function(x) table(amis[[x]]$newrace)), 2) # col = cityyear, row = race

chisq.test(sapply(c(1,3), function(x) table(amis[[x]]$newrace)))
chisq.test(sapply(1:2, function(x) table(amis[[x]]$newrace)))
chisq.test(sapply(3:4, function(x) table(amis[[x]]$newrace)))

### HIV status  1= pos, 2 = neg, 3 = U=unk
sapply(1:4, function(x) table(amis[[x]]$hiv_status)) # col = cityyear, row = race
prop.table(sapply(1:4, function(x) table(amis[[x]]$hiv_status)), 2) # col = cityyear, row = race

chisq.test(sapply(c(1,3), function(x) table(amis[[x]]$hiv_status%in%1))) # col = cityyear, row = race
chisq.test(sapply(1:2, function(x) table(amis[[x]]$hiv_status%in%1))) # col = cityyear, row = race
chisq.test(sapply(3:4, function(x) table(amis[[x]]$hiv_status%in%1))) # col = cityyear, row = race


#################################################################
# Create age groups
for (i in 1:4) {
  amis[[i]]$age6 <- NA
  amis[[i]] %<>% mutate(age6 = ifelse(X_age5yr%in% 1: 2, 1,age6))  # 15-24
  amis[[i]] %<>% mutate(age6 = ifelse(X_age5yr%in% 3: 4, 2,age6))  # 25-34
  amis[[i]] %<>% mutate(age6 = ifelse(X_age5yr%in% 5: 6, 3,age6))  # 35-44
  amis[[i]] %<>% mutate(age6 = ifelse(X_age5yr%in% 7: 8, 4,age6))  # 45-54
  amis[[i]] %<>% mutate(age6 = ifelse(X_age5yr%in% 9:10, 5,age6))  # 55-64
  amis[[i]] %<>% mutate(age6 = ifelse(X_age5yr%in%11:14, 6,age6))  # 65+
}

for (i in 1:4) {
  amis[[i]]$age3 <- NA
  amis[[i]] %<>% mutate(age3 = ifelse(X_age5yr%in% 1: 2, 1,age3))  # 15-24
  amis[[i]] %<>% mutate(age3 = ifelse(X_age5yr%in% 3: 8, 2,age3))  # 25-54
  amis[[i]] %<>% mutate(age3 = ifelse(X_age5yr%in% 9: 14, 3,age3))  # 55+
}

sapply(1:4, function(x) table(amis[[x]]$age3))
sapply(1:4, function(x) prop.table(table(amis[[x]]$age3)))

chisq.test(sapply(c(1,3), function(x) table(amis[[x]]$age3)))
chisq.test(sapply(1:2, function(x) table(amis[[x]]$age3)))
chisq.test(sapply(3:4, function(x) table(amis[[x]]$age3)))

#################################################################
# Number of AI partners in last 12 months
AI_cutoff <- 5

for (i in 1:4) {
  amis[[i]]$numAI <- NA
  amis[[i]]$numAI[amis[[i]]$MSMP12M_ANAL==0] <- 0   # did not have A
  amis[[i]]$numAI[is.na(amis[[i]]$numAI)] <- amis[[i]]$M_MP12MANUM[is.na(amis[[i]]$numAI)]  # had A and O, with >1P
  amis[[i]]$numAI[is.na(amis[[i]]$numAI) & amis[[i]]$M_MP12OANUM==1] <- 1              # had A and O, with 1P
  amis[[i]]$numAI[is.na(amis[[i]]$numAI)] <- amis[[i]]$M_MP12ANUM[is.na(amis[[i]]$numAI)]   # had A and not O
  # remaining NAs are skips or perhaps answered MSMP12_ with anal and with none/DK/PNTO, if that's possible
  # curious that there are a bit more in 2019 than 2020, but cannot investigate further with the data available to me
}

sapply(1:4, function(x) quantile(amis[[x]]$numAI, prob = c(0.25, 0.50, 0.75, 0.9), na.rm = T))

AI_degdist4 <- round(rbind(sapply(1:4, function(x) sum(amis[[x]]$numAI==0, na.rm = T)),
                        sapply(1:4, function(x) sum(amis[[x]]$numAI==1, na.rm = T)),
                        sapply(1:4, function(x) sum(amis[[x]]$numAI%in%2:AI_cutoff, na.rm = T)),
                        sapply(1:4, function(x) sum(amis[[x]]$numAI>=(AI_cutoff+1), na.rm = T))
                        ),3)
AI_degdist4_prop <- prop.table(AI_degdist4, 2)

AI_degdist4
AI_degdist4_prop

wilcox.test(amis[[1]]$numAI, amis[[3]]$numAI)
wilcox.test(amis[[1]]$numAI, amis[[2]]$numAI)
wilcox.test(amis[[3]]$numAI, amis[[4]]$numAI)


#################################################################
### Partner types

# Among those having just one partner 1=M, 2=C, 7=PNTA, 9=DK

prop.table(table(amis[[1]]$M_M1SX)[1:2])
prop.table(table(amis[[2]]$M_M1SX)[1:2])
prop.table(table(amis[[3]]$M_M1SX)[1:2])
prop.table(table(amis[[4]]$M_M1SX)[1:2])

chisq.test(cbind(table(amis[[1]]$M_M1SX)[1:2], table(amis[[3]]$M_M1SX)[1:2]))
chisq.test(cbind(table(amis[[1]]$M_M1SX)[1:2], table(amis[[2]]$M_M1SX)[1:2]))
chisq.test(cbind(table(amis[[3]]$M_M1SX)[1:2], table(amis[[4]]$M_M1SX)[1:2]))

#################################################################
# Number of CLAI partners in last 12 months
CLAI_cutoff <- 5

for (i in 1:4) {
  amis[[i]]$numCLAI <- NA
  amis[[i]]$numCLAI[amis[[i]]$numAI==0] <- 0   # did not have AI
  amis[[i]]$numCLAI[is.na(amis[[i]]$numCLAI) &  amis[[i]]$M_M1UAS==0] <- 0  # Had AI but not CLAI
  amis[[i]]$numCLAI[is.na(amis[[i]]$numCLAI)] <- amis[[i]]$M_M1UASNUM1[is.na(amis[[i]]$numCLAI)]  # Had CLAI (and had anal with > 1 P and no oral)

  amis[[i]]$numCLAI[is.na(amis[[i]]$numCLAI)] <- amis[[i]]$M_M1UASNUM2[is.na(amis[[i]]$numCLAI)]  # Had CLAI (and had anal or oral with > 1 P)
  amis[[i]]$numCLAI[is.na(amis[[i]]$numCLAI) & amis[[i]]$M_MP12MANUM==1 & amis[[i]]$M_M1UAS==1] <- 1
  amis[[i]]$numCLAI[is.na(amis[[i]]$numCLAI) & amis[[i]]$M_MP12OANUM==1 & amis[[i]]$M_M1UAS==1] <- 1
  # remaining NAs are skips or perhaps answered MSMP12_ with anal and with none/DK/PNTO, if that's possible
}

#sapply(1:4, function(x) quantile(amis[[x]]$numCLAI, prob = c(0.25, 0.50, 0.75, 0.9), na.rm = T))

CLAI_degdist4 <- round(rbind(sapply(1:4, function(x) sum(amis[[x]]$numCLAI==0, na.rm = T)),
                        sapply(1:4, function(x) sum(amis[[x]]$numCLAI==1, na.rm = T)),
                        sapply(1:4, function(x) sum(amis[[x]]$numCLAI%in%2:CLAI_cutoff, na.rm = T)),
                        sapply(1:4, function(x) sum(amis[[x]]$numCLAI>=(CLAI_cutoff+1), na.rm = T))
),3)

CLAI_degdist4_prop <- prop.table(CLAI_degdist4, 2)

CLAI_degdist4
CLAI_degdist4_prop


wilcox.test(amis[[1]]$numCLAI, amis[[3]]$numCLAI)
wilcox.test(amis[[1]]$numCLAI, amis[[2]]$numCLAI)
wilcox.test(amis[[3]]$numCLAI, amis[[4]]$numCLAI)


#################################################################
### Any discordant CLAI

any_dclai <- sapply(1:4, function(x) table(amis[[x]]$uas_discord12))

chisq.test(any_dclai[,c(1,3)])
chisq.test(any_dclai[,c(1,2)])
chisq.test(any_dclai[,c(3,4)])

xtabs(~amis[[1]]$uas_discord12 + amis[[1]]$PREP_CURRENT)

sapply(1:4, function(x) sum((amis[[x]]$PREP_CURRENT%in%0 | amis[[x]]$CURRAMED%in%0) & amis[[x]]$uas_discord12%in%1))


#################################################################
# Time SINCE LAST TEST
#sapply(1:4, function(x) quantile(amis[[x]]$X_mo_lastHIVtest, prob = c(0.25, 0.50, 0.75), na.rm = T))
sapply(1:4, function(x) quantile(amis[[x]]$X_mo_lastHIVtest[
  amis[[x]]$hiv_status%in%c(2)], prob = c(0.25, 0.50, 0.75), na.rm = T))

testover12 <- sapply(1:4, function(x) table(amis[[x]]$X_mo_lastHIVtest>12))
testover12_prop <- prop.table(testover12, 2)

wilcox.test(amis[[1]]$X_mo_lastHIVtest, amis[[3]]$X_mo_lastHIVtest)
wilcox.test(amis[[1]]$X_mo_lastHIVtest, amis[[2]]$X_mo_lastHIVtest)
wilcox.test(amis[[3]]$X_mo_lastHIVtest, amis[[4]]$X_mo_lastHIVtest)

chisq.test(testover12[,c(1,3)])
chisq.test(testover12[,c(1,2)])
chisq.test(testover12[,c(3,4)])


#################################################################
# COVID 0 = not changed, 1 = decreased, 2 = increased


# ATL Sep 2020
cv_atl <- rbind(tabulate(1+amis[[2]]$CVSEX_NUM, 3),
            tabulate(1+amis[[2]]$CVSEX_CONDOMUSE, 3),
            tabulate(1+amis[[2]]$CVHLTH_HIVTEST, 3)
)
cv_atl
round(rbind(prop.table(tabulate(1+amis[[2]]$CVSEX_NUM, 3)),
            prop.table(tabulate(1+amis[[2]]$CVSEX_CONDOMUSE, 3)),
            prop.table(tabulate(1+amis[[2]]$CVHLTH_HIVTEST, 3))
), 3)

# NYC Sep 2020
cv_nyc <- rbind(tabulate(1+amis[[4]]$CVSEX_NUM, 3),
            tabulate(1+amis[[4]]$CVSEX_CONDOMUSE, 3),
            tabulate(1+amis[[4]]$CVHLTH_HIVTEST, 3)
)
cv_nyc
round(rbind(prop.table(tabulate(1+amis[[4]]$CVSEX_NUM, 3)),
            prop.table(tabulate(1+amis[[4]]$CVSEX_CONDOMUSE, 3)),
            prop.table(tabulate(1+amis[[4]]$CVHLTH_HIVTEST, 3))
), 3)

fisher.test(cbind(cv_atl[,1],cv_nyc[,1]))
fisher.test(cbind(cv_atl[,2],cv_nyc[,2]))
fisher.test(cbind(cv_atl[,3],cv_nyc[,3]))



#############################################
## Num AI partners by age

sapply(1:4, function(x) quantile(amis[[x]]$numAI[amis[[x]]$age3==1], prob = c(0.25, 0.50, 0.75, 0.9), na.rm = T))
sapply(1:4, function(x) quantile(amis[[x]]$numAI[amis[[x]]$age3==2], prob = c(0.25, 0.50, 0.75, 0.9), na.rm = T))
sapply(1:4, function(x) quantile(amis[[x]]$numAI[amis[[x]]$age3==3], prob = c(0.25, 0.50, 0.75, 0.9), na.rm = T))

round(wilcox.test(amis[[1]]$numAI[amis[[1]]$age3==1], amis[[2]]$numAI[amis[[2]]$age3==1])$p.value,3)
round(wilcox.test(amis[[1]]$numAI[amis[[1]]$age3==2], amis[[2]]$numAI[amis[[2]]$age3==2])$p.value,3)
round(wilcox.test(amis[[1]]$numAI[amis[[1]]$age3==3], amis[[2]]$numAI[amis[[2]]$age3==3])$p.value,3)

round(wilcox.test(amis[[3]]$numAI[amis[[3]]$age3==1], amis[[4]]$numAI[amis[[4]]$age3==1])$p.value,3)
round(wilcox.test(amis[[3]]$numAI[amis[[3]]$age3==2], amis[[4]]$numAI[amis[[4]]$age3==2])$p.value,3)
round(suppressWarnings(
  wilcox.test(amis[[3]]$numAI[amis[[3]]$age3==3], amis[[4]]$numAI[amis[[4]]$age3==3])
)$p.value,3)

AI_degdist4_atl_age <- matrix(NA, 4, 8)
for(layer in 1:2) {
 for(age in 1:3) {
   AI_degdist4_atl_age[,(3*(age-1))+layer] <-
   c(sum(amis[[layer]]$numAI[amis[[layer]]$age3==age]==0, na.rm = T),
     sum(amis[[layer]]$numAI[amis[[layer]]$age3==age]==1, na.rm = T),
     sum(amis[[layer]]$numAI[amis[[layer]]$age3==age]%in%2:AI_cutoff, na.rm = T),
     sum(amis[[layer]]$numAI[amis[[layer]]$age3==age]>=AI_cutoff+1, na.rm = T))
  }
}
AI_degdist4_atl_age_prop <- round(prop.table(AI_degdist4_atl_age, 2), 3)
AI_degdist4_nyc_age <- matrix(NA, 4, 8)
for(layer in 3:4) {
  for(age in 1:3) {
    AI_degdist4_nyc_age[,(3*(age-1))+layer-2] <-
      c(sum(amis[[layer]]$numAI[amis[[layer]]$age3==age]==0, na.rm = T),
        sum(amis[[layer]]$numAI[amis[[layer]]$age3==age]==1, na.rm = T),
        sum(amis[[layer]]$numAI[amis[[layer]]$age3==age]%in%2:AI_cutoff, na.rm = T),
        sum(amis[[layer]]$numAI[amis[[layer]]$age3==age]>=AI_cutoff+1, na.rm = T))
  }
}
AI_degdist4_nyc_age_prop <- round(prop.table(AI_degdist4_nyc_age, 2), 3)

mycex <- 0.8

tiff("fig_AI_degdist_by_age.tif", height = 4*1200, width = 6*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
AI_degdist4_age_prop_bp <- barplot(cbind(AI_degdist4_nyc_age_prop, matrix(NA,4,3),
              AI_degdist4_atl_age_prop, matrix(NA,4,7)), col=gray(5:2/6), cex.axis=mycex)
axis(1, at=c(mean(AI_degdist4_age_prop_bp[1:2]), mean(AI_degdist4_age_prop_bp[4:5]),
             mean(AI_degdist4_age_prop_bp[7:8]), mean(AI_degdist4_age_prop_bp[12:13]),
             mean(AI_degdist4_age_prop_bp[15:16]), mean(AI_degdist4_age_prop_bp[18:19])),
     labels=c("15-24yo", "25-54yo", "55+yo", "15-24yo", "25-54yo", "55+yo"), cex.axis=mycex, tick=F,
     mgp=c(3,0,0))
axis(1, at=c(mean(AI_degdist4_age_prop_bp[4:5]), mean(AI_degdist4_age_prop_bp[15:16])),
     labels=c("NYC", "Metro Atlanta"), cex.axis=mycex, tick=F, mgp=c(3,2,0))
axis(1, at=c(mean(AI_degdist4_age_prop_bp[1:2]), mean(AI_degdist4_age_prop_bp[4:5]),
             mean(AI_degdist4_age_prop_bp[7:8]), mean(AI_degdist4_age_prop_bp[12:13]),
             mean(AI_degdist4_age_prop_bp[15:16]), mean(AI_degdist4_age_prop_bp[18:19])),
     labels=c("p=0.340", "p=0.830", "p=0.019", "p=0.434", "p=0.040", "p=0.048"), cex.axis=mycex-0.1, tick=F,
     mgp=c(3,1,0))
axis(3, at=AI_degdist4_age_prop_bp[c(1:2,4:5,7:8,12:13,15:16,18:19)],
     labels=rep(c("'19","'20"),6), cex.axis=mycex-0.2, tick=F,
     mgp=c(3,0,0))
legend(x=mean(AI_degdist4_age_prop_bp[20:21]), y=1, fill=gray(2:5/6), cex=mycex,
       legend = c("6+ AI partners","2-5 AI partners","1 AI partners","0 AI partners"))
dev.off()

###################################################

sapply(1:4, function(x) quantile(amis[[x]]$numCLAI[amis[[x]]$age3==1], prob = c(0.25, 0.50, 0.75, 0.9), na.rm = T))
sapply(1:4, function(x) quantile(amis[[x]]$numCLAI[amis[[x]]$age3==2], prob = c(0.25, 0.50, 0.75, 0.9), na.rm = T))
sapply(1:4, function(x) quantile(amis[[x]]$numCLAI[amis[[x]]$age3==3], prob = c(0.25, 0.50, 0.75, 0.9), na.rm = T))

round(wilcox.test(amis[[1]]$numCLAI[amis[[1]]$age3==1], amis[[2]]$numCLAI[amis[[2]]$age3==1])$p.value,3)
round(wilcox.test(amis[[1]]$numCLAI[amis[[1]]$age3==2], amis[[2]]$numCLAI[amis[[2]]$age3==2])$p.value,3)
round(wilcox.test(amis[[1]]$numCLAI[amis[[1]]$age3==3], amis[[2]]$numCLAI[amis[[2]]$age3==3])$p.value,3)

round(wilcox.test(amis[[3]]$numCLAI[amis[[3]]$age3==1], amis[[4]]$numCLAI[amis[[4]]$age3==1])$p.value,3)
round(wilcox.test(amis[[3]]$numCLAI[amis[[3]]$age3==2], amis[[4]]$numCLAI[amis[[4]]$age3==2])$p.value,3)
round(suppressWarnings(
  wilcox.test(amis[[3]]$numCLAI[amis[[3]]$age3==3], amis[[4]]$numCLAI[amis[[4]]$age3==3])
)$p.value,3)

CLAI_degdist4_atl_age <- matrix(NA, 4, 8)
for(layer in 1:2) {
  for(age in 1:3) {
    CLAI_degdist4_atl_age[,(3*(age-1))+layer] <-
      c(sum(amis[[layer]]$numCLAI[amis[[layer]]$age3==age]==0, na.rm = T),
        sum(amis[[layer]]$numCLAI[amis[[layer]]$age3==age]==1, na.rm = T),
        sum(amis[[layer]]$numCLAI[amis[[layer]]$age3==age]%in%2:CLAI_cutoff, na.rm = T),
        sum(amis[[layer]]$numCLAI[amis[[layer]]$age3==age]>=CLAI_cutoff+1, na.rm = T))
  }
}
CLAI_degdist4_atl_age_prop <- round(prop.table(CLAI_degdist4_atl_age, 2), 3)
CLAI_degdist4_nyc_age <- matrix(NA, 4, 8)
for(layer in 3:4) {
  for(age in 1:3) {
    CLAI_degdist4_nyc_age[,(3*(age-1))+layer-2] <-
      c(sum(amis[[layer]]$numCLAI[amis[[layer]]$age3==age]==0, na.rm = T),
        sum(amis[[layer]]$numCLAI[amis[[layer]]$age3==age]==1, na.rm = T),
        sum(amis[[layer]]$numCLAI[amis[[layer]]$age3==age]%in%2:CLAI_cutoff, na.rm = T),
        sum(amis[[layer]]$numCLAI[amis[[layer]]$age3==age]>=(CLAI_cutoff+1), na.rm = T))
  }
}
CLAI_degdist4_nyc_age_prop <- round(prop.table(CLAI_degdist4_nyc_age, 2), 3)
barplot(cbind(CLAI_degdist4_atl_age_prop, matrix(NA,4,3),
              CLAI_degdist4_nyc_age_prop), col=gray(5:2/6))


tiff("fig_CLAI_degdist_by_age.tif", height = 4*1200, width = 6*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
CLAI_degdist4_age_prop_bp <- barplot(cbind(CLAI_degdist4_nyc_age_prop, matrix(NA,4,3),
                                             CLAI_degdist4_atl_age_prop, matrix(NA,4,6)), col=gray(5:2/6),
                                             cex.axis=mycex)
axis(1, at=c(mean(CLAI_degdist4_age_prop_bp[1:2]), mean(CLAI_degdist4_age_prop_bp[4:5]),
             mean(CLAI_degdist4_age_prop_bp[7:8]), mean(CLAI_degdist4_age_prop_bp[12:13]),
             mean(CLAI_degdist4_age_prop_bp[15:16]), mean(CLAI_degdist4_age_prop_bp[18:19])),
     labels=c("15-24yo", "25-54yo", "55+yo", "15-24yo", "25-54yo", "55+yo"), cex.axis=mycex, tick=F,
     mgp=c(3,0,0))
axis(1, at=c(mean(CLAI_degdist4_age_prop_bp[4:5]), mean(CLAI_degdist4_age_prop_bp[15:16])),
     labels=c("NYC", "Metro Atlanta"), cex.axis=mycex, tick=F, mgp=c(3,2,0))
axis(3, at=CLAI_degdist4_age_prop_bp[c(1:2,4:5,7:8,12:13,15:16,18:19)],
     labels=rep(c("'19","'20"),6), cex.axis=mycex-0.2, tick=F, mgp=c(3,0,0))
axis(1, at=c(mean(CLAI_degdist4_age_prop_bp[1:2]), mean(CLAI_degdist4_age_prop_bp[4:5]),
             mean(CLAI_degdist4_age_prop_bp[7:8]), mean(CLAI_degdist4_age_prop_bp[12:13]),
             mean(CLAI_degdist4_age_prop_bp[15:16]), mean(CLAI_degdist4_age_prop_bp[18:19])),
     labels=c("p=0.382", "p=0.595", "p=0.047", "p=0.493", "p=0.145", "p=0.032"), cex.axis=mycex-0.1, tick=F,
     mgp=c(3,1,0))
legend(x=mean(CLAI_degdist4_age_prop_bp[20:21]), y=1, fill=gray(2:5/6), cex=mycex,
       legend = c("6+ CLAI partners","2-5 CLAI partners","1 CLAI partners","0 CLAI partners"))
dev.off()


#################################################################
# Time since last test, by age

#sapply(1:4, function(x) quantile(amis[[x]]$X_mo_lastHIVtest, prob = c(0.25, 0.50, 0.75), na.rm = T))
sapply(1:4, function(x) c(
  quantile(amis[[x]]$X_mo_lastHIVtest[amis[[x]]$hiv_status%in%c(2) & amis[[x]]$age3%in%1], prob=0.5, na.rm = T),
  quantile(amis[[x]]$X_mo_lastHIVtest[amis[[x]]$hiv_status%in%c(2) & amis[[x]]$age3%in%2], prob=0.5, na.rm = T),
  quantile(amis[[x]]$X_mo_lastHIVtest[amis[[x]]$hiv_status%in%c(2) & amis[[x]]$age3%in%3], prob=0.5, na.rm = T)
))

testover12 <- sapply(1:4, function(x) table(amis[[x]]$X_mo_lastHIVtest>12))
testover12_prop <- prop.table(testover12, 2)


# ATL-19 vs NYC-19
wilcox.test(amis[[1]]$X_mo_lastHIVtest[amis[[1]]$hiv_status%in%c(2) & amis[[1]]$age3%in%1],
            amis[[3]]$X_mo_lastHIVtest[amis[[3]]$hiv_status%in%c(2) & amis[[3]]$age3%in%1])
wilcox.test(amis[[1]]$X_mo_lastHIVtest[amis[[1]]$hiv_status%in%c(2) & amis[[1]]$age3%in%2],
            amis[[3]]$X_mo_lastHIVtest[amis[[3]]$hiv_status%in%c(2) & amis[[3]]$age3%in%2])
suppressWarnings(wilcox.test(amis[[1]]$X_mo_lastHIVtest[amis[[1]]$hiv_status%in%c(2) & amis[[1]]$age3%in%3],
            amis[[3]]$X_mo_lastHIVtest[amis[[3]]$hiv_status%in%c(2) & amis[[3]]$age3%in%3]))

# ATL-19 vs ATL-20
wilcox.test(amis[[1]]$X_mo_lastHIVtest[amis[[1]]$hiv_status%in%c(2) & amis[[1]]$age3%in%1],
            amis[[2]]$X_mo_lastHIVtest[amis[[2]]$hiv_status%in%c(2) & amis[[2]]$age3%in%1])
wilcox.test(amis[[1]]$X_mo_lastHIVtest[amis[[1]]$hiv_status%in%c(2) & amis[[1]]$age3%in%2],
            amis[[2]]$X_mo_lastHIVtest[amis[[2]]$hiv_status%in%c(2) & amis[[2]]$age3%in%2])
suppressWarnings(wilcox.test(amis[[1]]$X_mo_lastHIVtest[amis[[1]]$hiv_status%in%c(2) & amis[[1]]$age3%in%3],
            amis[[2]]$X_mo_lastHIVtest[amis[[2]]$hiv_status%in%c(2) & amis[[2]]$age3%in%3]))

# NYC-19 vs NYC-20
wilcox.test(amis[[3]]$X_mo_lastHIVtest[amis[[3]]$hiv_status%in%c(2) & amis[[3]]$age3%in%1],
            amis[[4]]$X_mo_lastHIVtest[amis[[4]]$hiv_status%in%c(2) & amis[[4]]$age3%in%1])
wilcox.test(amis[[3]]$X_mo_lastHIVtest[amis[[3]]$hiv_status%in%c(2) & amis[[3]]$age3%in%2],
            amis[[4]]$X_mo_lastHIVtest[amis[[4]]$hiv_status%in%c(2) & amis[[4]]$age3%in%2])
suppressWarnings(wilcox.test(amis[[3]]$X_mo_lastHIVtest[amis[[3]]$hiv_status%in%c(2) & amis[[3]]$age3%in%3],
            amis[[4]]$X_mo_lastHIVtest[amis[[4]]$hiv_status%in%c(2) & amis[[4]]$age3%in%3]))









##############################################33
## Unused

#################################################################
# ART

sapply(1:4, function(x) sum(amis[[x]]$CURRAMED==1, na.rm = TRUE) /
         sum(amis[[x]]$CURRAMED%in%0:1, na.rm = TRUE))              # % currently on ART
sapply(1:4, function(x) sum(amis[[x]]$CURRAMED %in% c(0,1)))        # n

table(amis[[1]]$WHNOMEDS)
table(amis[[2]]$WHNOMEDS)
table(amis[[3]]$WHNOMEDS)
table(amis[[4]]$WHNOMEDS)

art_table <- sapply(1:4, function(x) table(amis[[x]]$CURRAMED))        # n
fisher.test(art_table[,c(1,3)])$p.value
fisher.test(art_table[,c(1,2)])$p.value
fisher.test(art_table[,c(3,4)])$p.value

#barplot(degdist4_prop)
#degdist10 <- round(rbind(sapply(1:4, function(x)
#                        c(sapply(0:9, function(y) mean(amis[[x]]$numAI==y, na.rm = T)),
#                         mean(amis[[x]]$numAI>9, na.rm = T))
#                         )),3)
#matplot(0:10, degdist10, type='l', lty=c(1,2,1,2), col=c('black','black','red','red'))
#atl_numcl_test <- lbl_test(as.table(degdist4[,1:2]))
#atl_numcl_test
#nyc_numcl_test <- lbl_test(as.table(degdist4[,3:4]))
#nyc_numcl_test

# Testing whether a neg-bin test is more powerful

temp_nyc <- data.frame(Y=c(amis[[3]]$numAI, amis[[4]]$numAI),
                       X = c(rep('Y1', nrow(amis[[3]])), rep('Y2', nrow(amis[[4]]))))

library(MASS)
temp_nyc_nb <- glm.nb(Y~X, data=temp_nyc)
summary(temp_nyc_nb)

##### partner types

#table(amis[[1]]$M_M1SX)
#table(amis[[2]]$M_M1SX)
#prop.table(table(amis[[1]]$M_M1SX))
#prop.table(table(amis[[2]]$M_M1SX))

#table(amis[[3]]$M_M1SX)
#table(amis[[4]]$M_M1SX)
#prop.table(table(amis[[3]]$M_M1SX))
#prop.table(table(amis[[4]]$M_M1SX))


# Among those having >1 partner 1=M, 2=C, 3=MC, 7=PNTA, 9=DK
table(amis[[1]]$M_MTYP)
table(amis[[2]]$M_MTYP)
prop.table(table(amis[[1]]$M_MTYP))
prop.table(table(amis[[2]]$M_MTYP))

table(amis[[3]]$M_MTYP)
table(amis[[4]]$M_MTYP)
prop.table(table(amis[[3]]$M_MTYP))
prop.table(table(amis[[4]]$M_MTYP))

#CLAI_degdist10 <- round(rbind(sapply(1:4, function(x)
#  c(sapply(0:9, function(y) mean(amis[[x]]$numCLAI==y, na.rm = T)),
#    mean(amis[[x]]$numCLAI>9, na.rm = T))
#)),3)
#matplot(0:10, CLAI_degdist10, type='l', lty=c(1,2,1,2), col=c('black','black','red','red'))

#################################################################
# PrEP

for (i in 1:4) {
  amis[[i]]$prephist <- NA
  amis[[i]] %<>% mutate(prephist = ifelse(used_prep == 0, 0, prephist))
  amis[[i]] %<>% mutate(prephist = ifelse(used_prep == 1 & PREP_CURRENT==0, 1, prephist))
  amis[[i]] %<>% mutate(prephist = ifelse(used_prep == 1 & PREP_CURRENT==1, 2, prephist))
  amis[[i]] %<>% mutate(prephist = ifelse(hiv_status==1, NA, prephist))
}

prephist_table <- sapply(1:4, function(x) table(amis[[x]]$prephist))
prop.table(prephist_table, 2)

colSums(prephist_table)

prep_current <- rbind(colSums(prephist_table[1:2,]), prephist_table[3,])
chisq.test(prep_current[,c(1,3)])
chisq.test(prep_current[,c(1,2)])
chisq.test(prep_current[,c(3,4)])

chisq.test(prephist_table[2:3,c(1,3)])
chisq.test(prephist_table[2:3,c(1,2)])
chisq.test(prephist_table[2:3,c(3,4)])

sapply(1:4, function(x) sum(amis[[x]]$PREPSTOP_REASON1 %in%0:1))

sapply(1:4, function(x) rbind(sum(amis[[x]]$prephist==1 & amis[[x]]$PREPSTOP_REASON1==1, na.rm=T),
                              sum(amis[[x]]$prephist==1 & amis[[x]]$PREPSTOP_REASON2==1, na.rm=T),
                              sum(amis[[x]]$prephist==1 & amis[[x]]$PREPSTOP_REASON5==1, na.rm=T),
                              sum(amis[[x]]$prephist==1 & amis[[x]]$PREPSTOP_REASON6==1, na.rm=T),
                              sum(amis[[x]]$prephist==1 & amis[[x]]$PREPSTOP_REASONOTH==1, na.rm=T)
))

sapply(1:4, function(x) rbind(sum(amis[[x]]$prephist==1 & (amis[[x]]$PREPSTOP_REASON1==1 | amis[[x]]$PREPSTOP_REASON2==1), na.rm=T),
                              sum(amis[[x]]$prephist==1 & (amis[[x]]$PREPSTOP_REASON5==1 | amis[[x]]$PREPSTOP_REASON6==1), na.rm=T),
                              sum(amis[[x]]$prephist==1 & (amis[[x]]$PREPSTOP_REASONOTH==1), na.rm=T)
))


amis[[1]]$PREPSTOP_REASONOTHSPEC[amis[[1]]$PREPSTOP_REASONOTHSPEC!=""]
amis[[2]]$PREPSTOP_REASONOTHSPEC[amis[[2]]$PREPSTOP_REASONOTHSPEC!=""]
amis[[3]]$PREPSTOP_REASONOTHSPEC[amis[[3]]$PREPSTOP_REASONOTHSPEC!=""]
amis[[4]]$PREPSTOP_REASONOTHSPEC[amis[[4]]$PREPSTOP_REASONOTHSPEC!=""]

if(F) {
  # ATL Apr 2019
  round(rbind(prop.table(tabulate(1+amis[[1]]$CVSEX_NUMA, 3)),
              prop.table(tabulate(1+amis[[1]]$CVSEX_CONDOMUSEA, 3)),
              prop.table(tabulate(1+amis[[1]]$CVHLTH_HIVTESTA, 3)),
              prop.table(tabulate(1+amis[[1]]$CVPREP_PRESA, 3)),
              prop.table(tabulate(1+amis[[1]]$CVPREP_FILLA, 3)),
              prop.table(tabulate(1+amis[[1]]$CVART_PRESA, 3)),
              prop.table(tabulate(1+amis[[1]]$CVART_FILLA, 3))), 3)
  # NYC Apr 2019
  round(rbind(prop.table(tabulate(1+amis[[3]]$CVSEX_NUMA, 3)),
              prop.table(tabulate(1+amis[[3]]$CVSEX_CONDOMUSEA, 3)),
              prop.table(tabulate(1+amis[[3]]$CVHLTH_HIVTESTA, 3)),
              prop.table(tabulate(1+amis[[3]]$CVPREP_PRESA, 3)),
              prop.table(tabulate(1+amis[[3]]$CVPREP_FILLA, 3)),
              prop.table(tabulate(1+amis[[3]]$CVART_PRESA, 3)),
              prop.table(tabulate(1+amis[[3]]$CVART_FILLA, 3))), 3)
  # ATL Jul 2019
  round(rbind(prop.table(tabulate(1+amis[[1]]$CVSEX_NUMJ, 3)),
              prop.table(tabulate(1+amis[[1]]$CVSEX_CONDOMUSEJ, 3)),
              prop.table(tabulate(1+amis[[1]]$CVHLTH_HIVTESTJ, 3)),
              prop.table(tabulate(1+amis[[1]]$CVPREP_PRESJ, 3)),
              prop.table(tabulate(1+amis[[1]]$CVPREP_FILLJ, 3)),
              prop.table(tabulate(1+amis[[1]]$CVART_PRESJ, 3)),
              prop.table(tabulate(1+amis[[1]]$CVART_FILLJ, 3))), 3)
  # NYC Jul 2019
  round(rbind(prop.table(tabulate(1+amis[[3]]$CVSEX_NUMJ, 3)),
              prop.table(tabulate(1+amis[[3]]$CVSEX_CONDOMUSEJ, 3)),
              prop.table(tabulate(1+amis[[3]]$CVHLTH_HIVTESTJ, 3)),
              prop.table(tabulate(1+amis[[3]]$CVPREP_PRESJ, 3)),
              prop.table(tabulate(1+amis[[3]]$CVPREP_FILLJ, 3)),
              prop.table(tabulate(1+amis[[3]]$CVART_PRESJ, 3)),
              prop.table(tabulate(1+amis[[3]]$CVART_FILLJ, 3))), 3)
}

round(rbind(tabulate(1+amis[[4]]$CVSEX_NUM, 3),
            tabulate(1+amis[[4]]$CVSEX_CONDOMUSE, 3),
            tabulate(1+amis[[4]]$CVHLTH_HIVTEST, 3),
            tabulate(1+amis[[4]]$CVPREP_PRES, 3),
            tabulate(1+amis[[4]]$CVPREP_FILL, 3),
            tabulate(1+amis[[4]]$CVART_PRES, 3),
            tabulate(1+amis[[4]]$CVART_FILL, 3)
), 3)

(round(cbind(prop.table(tabulate(1+amis[[2]]$CVSEX_NUM, 3)),
             prop.table(tabulate(1+amis[[4]]$CVSEX_NUM, 3)),
             rep(NA,3),
             prop.table(tabulate(1+amis[[2]]$CVSEX_CONDOMUSE, 3)),
             prop.table(tabulate(1+amis[[4]]$CVSEX_CONDOMUSE, 3)),
             rep(NA,3),
             prop.table(tabulate(1+amis[[2]]$CVHLTH_HIVTEST, 3)),
             prop.table(tabulate(1+amis[[4]]$CVHLTH_HIVTEST, 3))
), 3))

barplot(round(cbind(
  prop.table(tabulate(1+amis[[2]]$CVPREP_PRES, 3)),
  prop.table(tabulate(1+amis[[4]]$CVPREP_PRES, 3)),
  rep(NA,3),
  prop.table(tabulate(1+amis[[2]]$CVPREP_FILL, 3)),
  prop.table(tabulate(1+amis[[4]]$CVPREP_FILL, 3)),
  rep(NA,3),
  prop.table(tabulate(1+amis[[2]]$CVART_PRES, 3)),
  prop.table(tabulate(1+amis[[4]]$CVART_PRES, 3)),
  rep(NA,3),
  prop.table(tabulate(1+amis[[2]]$CVART_FILL, 3)),
  prop.table(tabulate(1+amis[[4]]$CVART_FILL, 3))
), 3))

#################################################################
# P number and types together

# Note: if a R has oral and/or anal with >1 P, they are asked the question about
#   multiple partner types. But we are interested in AI partners only, so if they
#   have only 1 AI partner, the type is indeterminate (although reasonably likly to be main)

#table(amis[[i]]$numAI)
#table(amis[[1]]$M_M1SX)
#table(amis[[1]]$M_MTYP)
#xtabs(~numAI+M_M1SX, amis[[i]])
#xtabs(~numAI+M_MTYP, amis[[i]])

p_num_typ <- matrix(NA,4,7)

for(i in 1:4) {
  p_num_typ[i,1:7] <-
    cbind(sum(amis[[i]]$numAI==0 , na.rm=T),                                                  # No AI partners
          sum(amis[[i]]$numAI==1 & amis[[i]]$M_M1SX==1 & is.na(amis[[i]]$M_MTYP), na.rm=T) +
            sum(amis[[i]]$numAI==1 & is.na(amis[[i]]$M_M1SX) & amis[[i]]$M_MTYP==1, na.rm=T), # Only one AI partner, main
          sum(amis[[i]]$numAI==1 & amis[[i]]$M_M1SX==2 & is.na(amis[[i]]$M_MTYP), na.rm=T) +
            sum(amis[[i]]$numAI==1 & is.na(amis[[i]]$M_M1SX) & amis[[i]]$M_MTYP==2, na.rm=T), # Only one AI partner, cas
          sum(amis[[i]]$numAI==1 & is.na(amis[[i]]$M_M1SX) & amis[[i]]$M_MTYP==3, na.rm=T),   # Only one AI partner, indeterminate
          sum(amis[[i]]$numAI>1 & is.na(amis[[i]]$M_M1SX) & amis[[i]]$M_MTYP==1, na.rm=T),    # >1 AI partner, all main
          sum(amis[[i]]$numAI>1 & is.na(amis[[i]]$M_M1SX) & amis[[i]]$M_MTYP==2, na.rm=T),    # >1 AI partner, all cas
          sum(amis[[i]]$numAI>1 & is.na(amis[[i]]$M_M1SX) & amis[[i]]$M_MTYP==3, na.rm=T)     # >1 AI partner, mix
    )
}

#p_num_typ[,8] <- sapply(1:4, function(x) nrow(amis[[x]])) - rowSums(p_num_typ[,1:7])

p_num_typ_prop <- prop.table(p_num_typ, 1)

p_num_typ_prop <- rbind(p_num_typ_prop[1:2,], rep(NA,7), p_num_typ_prop[3:4,])
barplot(t(p_num_typ_prop))



# Num AI by age

#par(mfrow=c(1,2))
#plot(table(amis[[3]]$numAI[amis[[3]]$age3==3]))
#plot(table(amis[[4]]$numAI[amis[[4]]$age3==3]))

#par(mfrow=c(2,2))
#plot(prop.table((table(amis[[1]]$numAI))), xlim=c(0,20),ylim=c(0,0.3))
#plot(prop.table((table(amis[[3]]$numAI))), xlim=c(0,20),ylim=c(0,0.3))
#plot(prop.table((table(amis[[2]]$numAI))), xlim=c(0,20),ylim=c(0,0.3))
#plot(prop.table((table(amis[[4]]$numAI))), xlim=c(0,20),ylim=c(0,0.3))

temp_atl_age1 <- data.frame(Y=c(amis[[3]]$numAI[amis[[3]]$age3==1], amis[[4]]$numAI[amis[[4]]$age3==1]),
                            X = c(rep('Y1', sum(amis[[3]]$age3==1)), rep('Y2', sum(amis[[4]]$age3==1))))
temp_atl_age1_nb <- glm.nb(Y~X, data=temp_atl_age1)
summary(temp_atl_age1_nb)

temp_atl_age2 <- data.frame(Y=c(amis[[3]]$numAI[amis[[3]]$age3==2], amis[[4]]$numAI[amis[[4]]$age3==2]),
                            X = c(rep('Y1', sum(amis[[3]]$age3==2)), rep('Y2', sum(amis[[4]]$age3==2))))
temp_atl_age2_nb <- glm.nb(Y~X, data=temp_atl_age2)
summary(temp_atl_age2_nb)

temp_atl_age3 <- data.frame(Y=c(amis[[3]]$numAI[amis[[3]]$age3==3], amis[[4]]$numAI[amis[[4]]$age3==3]),
                            X = c(rep('Y1', sum(amis[[3]]$age3==3)), rep('Y2', sum(amis[[4]]$age3==3))))
temp_atl_age3_nb <- glm.nb(Y~X, data=temp_atl_age3)
summary(temp_atl_age3_nb)


temp_nyc_age1 <- data.frame(Y=c(amis[[3]]$numAI[amis[[3]]$age3==1], amis[[4]]$numAI[amis[[4]]$age3==1]),
                            X = c(rep('Y1', sum(amis[[3]]$age3==1)), rep('Y2', sum(amis[[4]]$age3==1))))
temp_nyc_age1_nb <- glm.nb(Y~X, data=temp_nyc_age1)
summary(temp_nyc_age1_nb)

temp_nyc_age2 <- data.frame(Y=c(amis[[3]]$numAI[amis[[3]]$age3==2], amis[[4]]$numAI[amis[[4]]$age3==2]),
                            X = c(rep('Y1', sum(amis[[3]]$age3==2)), rep('Y2', sum(amis[[4]]$age3==2))))
temp_nyc_age2_nb <- glm.nb(Y~X, data=temp_nyc_age2)
summary(temp_nyc_age2_nb)

temp_nyc_age3 <- data.frame(Y=c(amis[[3]]$numAI[amis[[3]]$age3==3], amis[[4]]$numAI[amis[[4]]$age3==3]),
                            X = c(rep('Y1', sum(amis[[3]]$age3==3)), rep('Y2', sum(amis[[4]]$age3==3))))
temp_nyc_age3_nb <- glm.nb(Y~X, data=temp_nyc_age3)
summary(temp_nyc_age3_nb)

rbind(quantile(amis[[1]]$numAI[amis[[1]]$age3==1], na.rm=T),
      quantile(amis[[2]]$numAI[amis[[2]]$age3==1], na.rm=T))

rbind(quantile(amis[[1]]$numAI[amis[[1]]$age3==2], na.rm=T),
      quantile(amis[[2]]$numAI[amis[[2]]$age3==2], na.rm=T))

rbind(quantile(amis[[1]]$numAI[amis[[1]]$age3==3], na.rm=T),
      quantile(amis[[2]]$numAI[amis[[2]]$age3==3], na.rm=T))

rbind(quantile(amis[[3]]$numAI[amis[[3]]$age3==1], na.rm=T),
      quantile(amis[[4]]$numAI[amis[[4]]$age3==1], na.rm=T))

rbind(quantile(amis[[3]]$numAI[amis[[3]]$age3==2], na.rm=T),
      quantile(amis[[4]]$numAI[amis[[4]]$age3==2], na.rm=T))

rbind(quantile(amis[[3]]$numAI[amis[[3]]$age3==3], na.rm=T),
      quantile(amis[[4]]$numAI[amis[[4]]$age3==3], na.rm=T))


###########################################################################
### Request by reviewers to add predictors of behavior change in AMIS

tableNA <- function(x) table(x, useNA='always')

amis[[2]]$CVSEX_NUM_BIN <- (amis[[2]]$CVSEX_NUM==1)
amis[[4]]$CVSEX_NUM_BIN <- (amis[[4]]$CVSEX_NUM==1)
tableNA(amis[[2]]$CVSEX_NUM_BIN)
tableNA(amis[[4]]$CVSEX_NUM_BIN)

amis[[2]]$newrace_cat <- factor(amis[[2]]$newrace, levels = c('3', '1', '2', '4'))
amis[[4]]$newrace_cat <- factor(amis[[4]]$newrace, levels = c('3', '1', '2', '4'))
tableNA(amis[[2]]$newrace_cat) # 1B, 2H, 3W, 4O
tableNA(amis[[4]]$newrace_cat)

amis[[2]]$age3_cat <- factor(amis[[2]]$age3, levels = c("2", "1", "3"))
amis[[4]]$age3_cat <- factor(amis[[4]]$age3, levels = c("2", "1", "3"))
tableNA(amis[[2]]$age3_cat)  # 1=15-24, 2=25-54, 3=55+
tableNA(amis[[4]]$age3_cat)

amis[[2]]$income2 <- amis[[2]]$income >=3
amis[[4]]$income2 <- amis[[4]]$income >=3
amis[[2]]$income2_cat <- factor(amis[[2]]$income2)
amis[[4]]$income2_cat <- factor(amis[[4]]$income2)
tableNA(amis[[2]]$income2_cat) #1 $0-19,999, 2 $20,000-39,999, 3 $40,000-74,999, 4 $75,000+
tableNA(amis[[4]]$income2_cat) #1 $0-19,999, 2 $20,000-39,999, 3 $40,000-74,999, 4 $75,000+

amis[[2]]$X_educat2 <- 0+(amis[[2]]$X_educat>2)  # 0 HS or less, 2 = some higher ed
amis[[4]]$X_educat2 <- 0+(amis[[4]]$X_educat>2)
tableNA(amis[[2]]$X_educat2)
tableNA(amis[[4]]$X_educat2)

amis[[2]]$hiv_pos <- 0+(amis[[2]]$hiv_status==1)
amis[[4]]$hiv_pos <- 0+(amis[[4]]$hiv_status==1)
tableNA(amis[[2]]$hiv_pos)
tableNA(amis[[4]]$hiv_pos)

amis[[2]]$living3 <- amis[[2]]$LIVE_ALONE
amis[[4]]$living3 <- amis[[4]]$LIVE_ALONE
amis[[2]]$living3[amis[[2]]$living3==7] <- NA
amis[[4]]$living3[amis[[4]]$living3==7] <- NA
amis[[2]]$living3[amis[[2]]$LIVE_PARTNER%in%1] <- 2
amis[[4]]$living3[amis[[4]]$LIVE_PARTNER%in%1] <- 2
amis[[2]]$living3 <- 2-amis[[2]]$living3 # 0 = living with P, 1 = living alone, 2 = living with other
amis[[4]]$living3 <- 2-amis[[4]]$living3
amis[[2]]$living3 <- factor(amis[[2]]$living3)
amis[[4]]$living3 <- factor(amis[[4]]$living3)
tableNA(amis[[2]]$living3)
tableNA(amis[[4]]$living3)

amis[[2]]$living2 <- 0+(amis[[2]]$living3==0)
amis[[4]]$living2 <- 0+(amis[[4]]$living3==0)

#bivar_nyc <- summary(glm(CVSEX_NUM_BIN~age3_cat, data=amis[[4]], family=binomial(link='logit')))$coefficients[-1,c(1,4)]
#bivar_nyc <- rbind(bivar_nyc, summary(glm(CVSEX_NUM_BIN~newrace_cat, data=amis[[4]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_nyc <- rbind(bivar_nyc, summary(glm(CVSEX_NUM_BIN~hiv_pos, data=amis[[4]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_nyc <- rbind(bivar_nyc, summary(glm(CVSEX_NUM_BIN~prep_eligible, data=amis[[4]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_nyc <- rbind(bivar_nyc, summary(glm(CVSEX_NUM_BIN~used_prep, data=amis[[4]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_nyc <- rbind(bivar_nyc, summary(glm(CVSEX_NUM_BIN~living2, data=amis[[4]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_nyc <- rbind(bivar_nyc, summary(glm(CVSEX_NUM_BIN~X_educat2, data=amis[[4]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_nyc <- rbind(bivar_nyc, summary(glm(CVSEX_NUM_BIN~income2_cat, data=amis[[4]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_nyc[,1] <- exp(bivar_nyc[,1])
#bivar_nyc

redux_nyc <- glm(CVSEX_NUM_BIN ~ age3_cat + newrace_cat + hiv_pos + prep_eligible +
                   used_prep + living2 + X_educat2 + income2_cat,
                 data=amis[[4]], family=binomial(link='logit'))
multivar_nyc <- summary(redux_nyc)$coef[-1,c(1,4)]
multivar_nyc[,1] <- exp(multivar_nyc[,1])
multivar_nyc

#bivar_atl <- summary(glm(CVSEX_NUM_BIN~age3_cat, data=amis[[2]], family=binomial(link='logit')))$coefficients[-1,c(1,4)]
#bivar_atl <- rbind(bivar_atl, summary(glm(CVSEX_NUM_BIN~newrace_cat, data=amis[[2]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_atl <- rbind(bivar_atl, summary(glm(CVSEX_NUM_BIN~hiv_pos, data=amis[[2]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_atl <- rbind(bivar_atl, summary(glm(CVSEX_NUM_BIN~prep_eligible, data=amis[[2]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_atl <- rbind(bivar_atl, summary(glm(CVSEX_NUM_BIN~used_prep, data=amis[[2]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_atl <- rbind(bivar_atl, summary(glm(CVSEX_NUM_BIN~living2, data=amis[[2]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_atl <- rbind(bivar_atl, summary(glm(CVSEX_NUM_BIN~X_educat2, data=amis[[2]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_atl <- rbind(bivar_atl, summary(glm(CVSEX_NUM_BIN~income2_cat, data=amis[[2]], family=binomial(link='logit')))$coefficients[-1,c(1,4)])
#bivar_atl[,1] <- exp(bivar_atl[,1])
#bivar_atl

redux_atl <- glm(CVSEX_NUM_BIN ~ age3_cat + newrace_cat + hiv_pos + prep_eligible +
                   used_prep + living2 + X_educat2 + income2_cat,
                 data=amis[[2]], family=binomial(link='logit'))
multivar_atl <- summary(redux_atl)$coef[-1,c(1,4)]
multivar_atl[,1] <- exp(multivar_atl[,1])
multivar_atl

#cbind(bivar_nyc, multivar_nyc, bivar_atl, multivar_atl)
cbind(multivar_nyc, multivar_atl)

