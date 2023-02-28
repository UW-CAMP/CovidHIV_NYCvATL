
##############################################
## Time series analyses on PrEP and ART
## Tables and figs in Suppl, some basic numbers in text

dates7 <- as.Date(c(covidepi_atl$dt_start[seq(1,78,13)],covidepi_atl$dt_start[78]+7))

## PrEP in ATL

prep_atl_all_abs %<>% mutate(week = 1:78)
prep_atl_all_abs %<>% mutate(quarter = c(rep("P",26), rep("Q1",13), rep("Q2",13), rep("Q3",13), rep("Q4",13)))

prep_atl_all_prev_reg <- lm(n_a_prev ~ week + quarter +week*quarter, data=prep_atl_all_abs)
prep_atl_all_prev_reg_sum <-  summary(prep_atl_all_prev_reg)

prep_atl_all_init_reg <- lm(n_c_ep1 ~ week + quarter +week*quarter, data=prep_atl_all_abs)
prep_atl_all_init_reg_sum <-  summary(prep_atl_all_init_reg)

prep_atl_all_term_reg <- lm(n_d_end ~ week + quarter +week*quarter, data=prep_atl_all_abs)
prep_atl_all_term_reg_sum <-  summary(prep_atl_all_term_reg)

cbind(round(prep_atl_all_prev_reg_sum$coef[,1],1),round(prep_atl_all_prev_reg_sum$coef[,4],3),
      round(prep_atl_all_init_reg_sum$coef[,1],1),round(prep_atl_all_init_reg_sum$coef[,4],3),
      round(prep_atl_all_term_reg_sum$coef[,1],1),round(prep_atl_all_term_reg_sum$coef[,4],3))

round(c(prep_atl_all_prev_reg_sum$r.squared,
        prep_atl_all_init_reg_sum$r.squared,
        prep_atl_all_term_reg_sum$r.squared),3)


## PrEP in NYC

prep_nyc_all_abs %<>% mutate(week = 1:78)
prep_nyc_all_abs %<>% mutate(quarter = c(rep("P",26), rep("Q1",13), rep("Q2",13), rep("Q3",13), rep("Q4",13)))

prep_nyc_all_prev_reg <- lm(n_a_prev ~ week + quarter +week*quarter, data=prep_nyc_all_abs)
prep_nyc_all_prev_reg_sum <-  summary(prep_nyc_all_prev_reg)

prep_nyc_all_init_reg <- lm(n_c_ep1 ~ week + quarter +week*quarter, data=prep_nyc_all_abs)
prep_nyc_all_init_reg_sum <-  summary(prep_nyc_all_init_reg)

prep_nyc_all_term_reg <- lm(n_d_end ~ week + quarter +week*quarter, data=prep_nyc_all_abs)
prep_nyc_all_term_reg_sum <-  summary(prep_nyc_all_term_reg)

cbind(round(prep_nyc_all_prev_reg_sum$coef[,1],1),round(prep_nyc_all_prev_reg_sum$coef[,4],3),
      round(prep_nyc_all_init_reg_sum$coef[,1],1),round(prep_nyc_all_init_reg_sum$coef[,4],3),
      round(prep_nyc_all_term_reg_sum$coef[,1],1),round(prep_nyc_all_term_reg_sum$coef[,4],3))

round(c(prep_nyc_all_prev_reg_sum$r.squared,
        prep_nyc_all_init_reg_sum$r.squared,
        prep_nyc_all_term_reg_sum$r.squared),3)


## ART in ATL

art_atl_all_abs %<>% mutate(week = 1:78)
art_atl_all_abs %<>% mutate(quarter = c(rep("P",26), rep("Q1",13), rep("Q2",13), rep("Q3",13), rep("Q4",13)))

art_atl_all_prev_reg <- lm(n_a_prev ~ week + quarter +week*quarter, data=art_atl_all_abs)
art_atl_all_prev_reg_sum <-  summary(art_atl_all_prev_reg)

art_atl_all_init_reg <- lm(n_c_ep1 ~ week + quarter +week*quarter, data=art_atl_all_abs)
art_atl_all_init_reg_sum <-  summary(art_atl_all_init_reg)

art_atl_all_term_reg <- lm(n_d_end ~ week + quarter +week*quarter, data=art_atl_all_abs)
art_atl_all_term_reg_sum <-  summary(art_atl_all_term_reg)

cbind(round(art_atl_all_prev_reg_sum$coef[,1],1),round(art_atl_all_prev_reg_sum$coef[,4],3),
      round(art_atl_all_init_reg_sum$coef[,1],1),round(art_atl_all_init_reg_sum$coef[,4],3),
      round(art_atl_all_term_reg_sum$coef[,1],1),round(art_atl_all_term_reg_sum$coef[,4],3))

round(c(art_atl_all_prev_reg_sum$r.squared,
        art_atl_all_init_reg_sum$r.squared,
        art_atl_all_term_reg_sum$r.squared),3)


## ART in NYC

art_nyc_all_abs %<>% mutate(week = 1:78)
art_nyc_all_abs %<>% mutate(quarter = c(rep("P",26), rep("Q1",13), rep("Q2",13), rep("Q3",13), rep("Q4",13)))

art_nyc_all_prev_reg <- lm(n_a_prev ~ week + quarter +week*quarter, data=art_nyc_all_abs)
art_nyc_all_prev_reg_sum <-  summary(art_nyc_all_prev_reg)

art_nyc_all_init_reg <- lm(n_c_ep1 ~ week + quarter +week*quarter, data=art_nyc_all_abs)
art_nyc_all_init_reg_sum <-  summary(art_nyc_all_init_reg)

art_nyc_all_term_reg <- lm(n_d_end ~ week + quarter +week*quarter, data=art_nyc_all_abs)
art_nyc_all_term_reg_sum <-  summary(art_nyc_all_term_reg)

cbind(round(art_nyc_all_prev_reg_sum$coef[,1],1),round(art_nyc_all_prev_reg_sum$coef[,4],3),
      round(art_nyc_all_init_reg_sum$coef[,1],1),round(art_nyc_all_init_reg_sum$coef[,4],3),
      round(art_nyc_all_term_reg_sum$coef[,1],1),round(art_nyc_all_term_reg_sum$coef[,4],3))

round(c(art_nyc_all_prev_reg_sum$r.squared,
        art_nyc_all_init_reg_sum$r.squared,
        art_nyc_all_term_reg_sum$r.squared),3)


## testing in ATL

testing_byweek %<>% mutate(week = 1:78)
testing_byweek %<>% mutate(quarter = c(rep("P",26), rep("Q1",13), rep("Q2",13),
                                       rep("Q3",13), rep("Q4",13)))
testing_byweek %<>% mutate(week2 = (1:78)^2)
testing_byweek %<>% mutate(week2q1 = week2)
testing_byweek %<>% mutate(week2q1 = ifelse(quarter=="Q1", week2q1, 0))

#testing_byweek_atl_all <- lm(Overall_ATLM_total ~ week + quarter +week*quarter +
#                               week2 + week2*quarter, data=testing_byweek)
#testing_byweek_atl_all_sum <-  summary(testing_byweek_atl_all)

#testing_byweek_atl_all <- lm(Overall_ATLM_total ~ week + quarter +week*quarter +
#                               week2*I(quarter=="Q1") - week2, data=testing_byweek)
#testing_byweek_atl_all_sum <-  summary(testing_byweek_atl_all)

testing_byweek_atl_all <- lm(Overall_ATLM_total ~ week + quarter +week*quarter +
                               week2q1, data=testing_byweek)
testing_byweek_atl_all_sum <-  summary(testing_byweek_atl_all)

cbind(round(testing_byweek_atl_all_sum$coef[,1],1),
      round(testing_byweek_atl_all_sum$coef[,4],3))

round(testing_byweek_atl_all_sum$r.squared,3)


## testing in NYC

#testing_byweek_nyc_all <- lm(Overall_NYCM_total ~ week + quarter +week*quarter +
#                               week2 + week2*quarter, data=testing_byweek)
#testing_byweek_nyc_all_sum <-  summary(testing_byweek_nyc_all)

#testing_byweek_nyc_all <- lm(Overall_NYCM_total ~ week + quarter +week*quarter +
#                               week2*I(quarter=="Q1") - week2, data=testing_byweek)
#testing_byweek_nyc_all_sum <-  summary(testing_byweek_nyc_all)

testing_byweek_nyc_all <- lm(Overall_NYCM_total ~ week + quarter +week*quarter +
                               week2q1, data=testing_byweek)
testing_byweek_nyc_all_sum <-  summary(testing_byweek_nyc_all)

cbind(round(testing_byweek_nyc_all_sum$coef[,1],1),
      round(testing_byweek_nyc_all_sum$coef[,4],3))

round(testing_byweek_nyc_all_sum$r.squared,3)


# Positivity

pos_ATL_lm <- lm(positivity_ATL~testing_byweek$week)
pos_NYC_lm <- lm(positivity_NYC~testing_byweek$week)
summary(pos_ATL_lm)
summary(pos_NYC_lm)

pos_ATL_lm$coefficients[2] * 52

##### Figures used in paper

tiff("fig_prep_time_series.tif", height = 4.5*1200, width = 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))

plot(as.Date(covidepi_nyc$dt_start), prep_nyc_all_abs$n_a_prev, type='l', col='red',
     xlab='time', ylab='# men with active PrEP prescription',xaxt='n')
points(as.Date(covidepi_nyc$dt_start), predict(prep_nyc_all_prev_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7[-2], lty=3, col='lightgray')
legend(as.Date(covidepi_us$dt_start)[1]+45,15750, c("observed","predicted"), col='red', lty=c(1,-1), pch=c(-1,1))
text(c(mean(dates7[c(1,3)])+50, mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     18000, c('pre-COVID', 'Q1','Q2','Q3','Q4'))
text(dates7[1]+10, 18000, c('A. NYC'), adj=0, cex=1.5)

plot(as.Date(covidepi_atl$dt_start), prep_atl_all_abs$n_a_prev, type='l', col='red',
     xlab='time', ylab='# men with active PrEP prescription',xaxt='n',
     ylim=c(4800,5400))
points(as.Date(covidepi_atl$dt_start), predict(prep_atl_all_prev_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7[-2], lty=3, col='lightgray')
legend(as.Date(covidepi_us$dt_start)[1]+45,4975, c("observed","predicted"), col='red', lty=c(1,-1), pch=c(-1,1))
text(c(mean(dates7[c(1,3)])+50, mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     5400, c('pre-COVID', 'Q1','Q2','Q3','Q4'))
text(dates7[1]+10, 5390, c('B. Atlanta'), adj=0, cex=1.5)

dev.off()

tiff("fig_art_time_series.tif", height = 4.5*1200, width = 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))

plot(as.Date(covidepi_nyc$dt_start), art_nyc_all_abs$n_a_prev, type='l', col='red',
     xlab='time', ylab='# men with active art prescription',xaxt='n',
     ylim=c(38500,40200))
points(as.Date(covidepi_nyc$dt_start), predict(art_nyc_all_prev_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7[-2], lty=3, col='lightgray')
legend(as.Date(covidepi_us$dt_start)[1]+45,39800, c("observed","predicted"), col='red', lty=c(1,-1), pch=c(-1,1))
text(c(mean(dates7[c(1,3)])+50, mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     40175, c('pre-COVID', 'Q1','Q2','Q3','Q4'))
text(dates7[1]+10, 40175, c('A. NYC'), adj=0, cex=1.5)

plot(as.Date(covidepi_atl$dt_start), art_atl_all_abs$n_a_prev, type='l', col='red',
     xlab='time', ylab='# men with active art prescription',xaxt='n',
     ylim=c(18500,19800))
points(as.Date(covidepi_atl$dt_start), predict(art_atl_all_prev_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7[-2], lty=3, col='lightgray')
legend(as.Date(covidepi_us$dt_start)[1]+45,19500, c("observed","predicted"), col='red', lty=c(1,-1), pch=c(-1,1))
text(c(mean(dates7[c(1,3)])+50, mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     19775, c('pre-COVID', 'Q1','Q2','Q3','Q4'))
text(dates7[1]+10, 19775, c('B. Atlanta'), adj=0, cex=1.5)

dev.off()

tiff("fig_testing_time_series.tif", height = 4.5*1200, width = 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))

plot(as.Date(covidepi_nyc$dt_start), testing_byweek$Overall_NYCM_total, type='l',
     col='red', xlab='time', ylab='# valid HIV tests for males',xaxt='n', ylim=c(0,7e3))
points(as.Date(covidepi_nyc$dt_start), predict(testing_byweek_nyc_all), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7[-2], lty=3, col='lightgray')
legend(as.Date(covidepi_us$dt_start)[1]+45,2000, c("observed","predicted"), col='red', lty=c(1,-1), pch=c(-1,1))
text(c(mean(dates7[c(1,3)])+50, mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     6900, c('pre-COVID', 'Q1','Q2','Q3','Q4'))
text(dates7[1]+10, 6900, c('A. NYC'), adj=0, cex=1.5)

plot(as.Date(covidepi_atl$dt_start), testing_byweek$Overall_ATLM_total, type='l', col='red',
     xlab='time', ylab='# valid HIV tests for males',xaxt='n',
     ylim=c(0,3e3))
points(as.Date(covidepi_atl$dt_start), predict(testing_byweek_atl_all), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7[-2], lty=3, col='lightgray')
legend(as.Date(covidepi_us$dt_start)[1]+45,750, c("observed","predicted"),
       col='red', lty=c(1,-1), pch=c(-1,1))
text(c(mean(dates7[c(1,3)])+50, mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     2900, c('pre-COVID', 'Q1','Q2','Q3','Q4'))
text(dates7[1]+10, 2900, c('B. Atlanta'), adj=0, cex=1.5)

dev.off()





















# Figure placed in residuals for now

tiff("fig_prep_time_series_atl.tif", height = 4.5*1200, width = 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))
plot(as.Date(covidepi_atl$dt_start), prep_atl_all_abs$n_c_ep1, type='l', col='red',
     xlab='time', ylab='# men initiating PrEP spell',xaxt='n')
points(as.Date(covidepi_atl$dt_start), predict(prep_atl_all_init_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7, lty=3, col='lightgray')
plot(as.Date(covidepi_atl$dt_start), prep_atl_all_abs$n_d_end, type='l', col='red',
     xlab='time', ylab='# men ending PrEP spell',xaxt='n')
points(as.Date(covidepi_atl$dt_start), predict(prep_atl_all_term_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7, lty=3, col='lightgray')
dev.off()

tiff("fig_prep_time_series_nyc.tif", height = 4.5*1200, width = 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))
plot(as.Date(covidepi_nyc$dt_start), prep_nyc_all_abs$n_c_ep1, type='l', col='red',
     xlab='time', ylab='# men initiating PrEP spell',xaxt='n')
points(as.Date(covidepi_nyc$dt_start), predict(prep_nyc_all_init_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7, lty=3, col='lightgray')
plot(as.Date(covidepi_nyc$dt_start), prep_nyc_all_abs$n_d_end, type='l', col='red',
     xlab='time', ylab='# men ending PrEP spell',xaxt='n')
points(as.Date(covidepi_nyc$dt_start), predict(prep_nyc_all_term_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7, lty=3, col='lightgray')
dev.off()

tiff("fig_art_time_series_atl.tif", height = 4.5*1200, width = 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))
plot(as.Date(covidepi_atl$dt_start), art_atl_all_abs$n_c_ep1, type='l', col='red',
     xlab='time', ylab='# men initiating art spell',xaxt='n')
points(as.Date(covidepi_atl$dt_start), predict(art_atl_all_init_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7, lty=3, col='lightgray')
plot(as.Date(covidepi_atl$dt_start), art_atl_all_abs$n_d_end, type='l', col='red',
     xlab='time', ylab='# men ending art spell',xaxt='n')
points(as.Date(covidepi_atl$dt_start), predict(art_atl_all_term_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7, lty=3, col='lightgray')
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     40000, c('pre-COVID', 'Q1','Q2','Q3','Q4'))
dev.off()

tiff("fig_art_time_series_nyc.tif", height = 4.5*1200, width = 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))
plot(as.Date(covidepi_nyc$dt_start), art_nyc_all_abs$n_c_ep1, type='l', col='red',
     xlab='time', ylab='# men initiating art spell',xaxt='n')
points(as.Date(covidepi_nyc$dt_start), predict(art_nyc_all_init_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7, lty=3, col='lightgray')
plot(as.Date(covidepi_nyc$dt_start), art_nyc_all_abs$n_d_end, type='l', col='red',
     xlab='time', ylab='# men ending art spell',xaxt='n')
points(as.Date(covidepi_nyc$dt_start), predict(art_nyc_all_term_reg), col='red')
axis(1, labels = dates7, at=dates7)
abline(v=dates7, lty=3, col='lightgray')
dev.off()





