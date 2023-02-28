
dpi <- 72

dates6 <- as.Date(covidepi_nyc$dt_start[c(1,14,27,40,53,66)])
dates7 <- c(dates6, as.Date(covidepi_nyc$dt_start[66]+13*7))

# Epidemic curve plot

tiff("covid_timeseries.tif", height = 6*1200*1.4, width = 4.5*1200*1.4,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))
plot(as.Date(covidepi_nyc$dt_start), covidepi_nyc$caserate, type='p', pch=1,
     xlab='time', ylab='COVID case rate per 100k', ylim=c(0,600),xaxt='n',
     cex=1.4, cex.lab=1.4, cex.axis=1.4)
points(as.Date(covidepi_us$dt_start), covidepi_us$caserate, col='darkgray', pch=4, cex=1.4)
points(as.Date(covidepi_atl$dt_start), covidepi_atl$caserate, pch=16, cex=1.4)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
legend(as.Date(covidepi_us$dt_start)[1]+20,300,
       c("New York City", "Metro Atlanta", "Nationwide"),
       pch=c(1,16,4), col=c('black', 'black', 'darkgray'), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
        mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
        600, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)

lines(c(as.Date("2020-03-22"), as.Date("2020-06-27")), c(540, 540))
text(as.Date("2020-05-09"), 555, "NY stay-at-home", cex=1)
lines(c(as.Date("2020-04-08"), as.Date("2020-05-01")), c(500, 500))
text(as.Date("2020-04-19"), 515, "GA stay-at-home", cex=1)

dev.off()

# All-age PrEP Plots
tiff("fig_prep_over_time.tif", height = 6*1200*1.4, width = 4.5*1200*1.4,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))

plot(as.Date(prep$nyc_all$dt_start), prep_nyc_all_relpre[,1],
     ylim=c(0, 2), type='p', pch=1, cex=1.4, # las=2, lab = c(100,10,7)
     xlab='', ylab = 'relative no. on PrEP',xaxt='n', cex.axis=1.4, cex.lab=1.4)
points(as.Date(prep$atl_all$dt_start), prep_atl_all_relpre[,1], pch=16, cex=1.4)
abline(h=(seq(0, 2.0, 0.1)), lty=3, col='gray95')
abline(h=1, lty=2)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
legend(dates7[1]+45,0.725, c("New York City", "Metro Atlanta"),
       pch=c(1,16), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     2, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)
text(dates7[1], 1.95, "A)", cex=2.5)
lines(c(as.Date("2020-03-22"), as.Date("2020-06-27")), c(1.800, 1.800), lwd=1.5)
text(as.Date("2020-05-09"), 1.850, "NY stay-at-home", cex=1.2)
lines(c(as.Date("2020-04-08"), as.Date("2020-05-01")), c(1.667, 1.667), lwd=1.5)
text(as.Date("2020-04-19"), 1.717, "GA stay-at-home", cex=1.2)

plot(as.Date(prep$nyc_all$dt_start), prep_nyc_all_relpre[,3],
     ylim=c(0,2), type='p', pch=1, cex=1.4, # las=2, lab = c(100,10,7)
     xlab='', ylab = 'relative no. initiating PrEP spell', xaxt='n', cex.axis=1.4, cex.lab=1.4)
points(as.Date(prep$atl_all$dt_start), prep_atl_all_relpre[,3], pch=16, cex=1.4)
abline(h=(seq(0, 2.0, 0.1)), lty=3, col='gray95')
abline(h=1, lty=2)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
legend(dates7[1]+45,0.45, c("New York City", "Metro Atlanta"),
       pch=c(1,16), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     2, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)
text(dates7[1], 1.95, "B)", cex=2.5)

mar=c(3, 5, 1, 1)
plot(as.Date(prep$nyc_all$dt_start), prep_nyc_all_relpre[,4],
     ylim=c(0,2), type='p', pch=1, cex=1.4, # las=2, lab = c(100,10,7)
     xlab='time', ylab = 'relative no. ending PrEP spell', xaxt='n', cex.axis=1.4, cex.lab=1.4)
points(as.Date(prep$atl_all$dt_start), prep_atl_all_relpre[,4], pch=16, cex=1.4)
abline(h=(seq(0, 2.0, 0.1)), lty=3, col='gray95')
abline(h=1, lty=2)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
legend(dates7[1]+45,0.45, c("New York City", "Metro Atlanta"),
       pch=c(1,16), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     2, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)
text(dates7[1], 1.95, "C)", cex=2.5)

dev.off()

# Age PrEP Plots
tiff("fig_prep_by_age.tif", height = 6*1200, width = 4.5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")

par(mfrow=c(3,1), mar=c(2, 5, 1, 1))   # 3 rows preserves formatting from other plots; otherwise everything changes font size no matter what I do!

plot(as.Date(prep$nyc_all$dt_start), rep(0,length(as.Date(prep$nyc_all$dt_start))),
     type='b', col='white', xlab='Time', ylab='relative no. on PrEP', ylim=c(0, 1.4), cex=0.7, xaxt='n')

matplot(as.Date(prep$nyc_all$dt_start), prep_nyc_age_relpre[,1,], type='b', col=c(1:num_aggps),
        cex=0.7, add=T)
abline(h=1)
legend(x=as.Date("2019-09-29"), y=0.8, text.col = 1:num_aggps, cex=0.7, ncol=2,
       legend=c('1 = 13-24', '2 = 25-34', '3 = 35-44', '4 = 45-54', '5 = 55-64', '6 = 65+'))
abline(v=dates7, lty=3, col='lightgray')
axis(1, labels = dates7, at=dates7)
text(dates7[1]+10, 1.375, "A) NYC", cex=1.5)

matplot(as.Date(prep$atl_all$dt_start), prep_atl_age_relpre[,1,], type='b', col=c(1:num_aggps),
        xlab='Time', ylab='relative no. on PrEP', ylim=c(0, 1.4), cex=0.7)
abline(h=1)
legend(x=as.Date("2019-09-29"), y=0.8, text.col = 1:num_aggps, cex=0.7, ncol=2,
       legend=c('1 = 13-24', '2 = 25-34', '3 = 35-44', '4 = 45-54', '5 = 55-64', '6 = 65+'))
abline(v=dates7, lty=3, col='lightgray')
axis(1, labels = dates7, at=dates7)
text(dates7[1]+18,1.375, "B) Atlanta", cex=1.5)

dev.off()

# All-age ART Plots
tiff("fig_art_over_time.tif", height = 6*1200*1.4, width = 4.5*1200*1.4,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))

plot(as.Date(art$nyc_all$dt_start), art_nyc_all_relpre[,1],
     ylim=c(0,2), type='p', pch=1, cex=1.4, # las=2, lab = c(100,10,7)
     xlab='', ylab = 'relative no. on ART',xaxt='n', cex.axis=1.4, cex.lab=1.4)
points(as.Date(art$atl_all$dt_start), art_atl_all_relpre[,1], pch=16, cex=1.4)
abline(h=(seq(0, 2.0, 0.1)), lty=3, col='gray95')
abline(h=1, lty=2)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
legend(dates7[1]+45,0.725, c("New York City", "Metro Atlanta"),
       pch=c(1,16), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     2, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)
text(dates7[1], 1.95, "A)", cex=2.5)
lines(c(as.Date("2020-03-22"), as.Date("2020-06-27")), c(1.800, 1.800), lwd=1.5)
text(as.Date("2020-05-09"), 1.850, "NY stay-at-home", cex=1.2)
lines(c(as.Date("2020-04-08"), as.Date("2020-05-01")), c(1.667, 1.667), lwd=1.5)
text(as.Date("2020-04-19"), 1.717, "GA stay-at-home", cex=1.2)

plot(as.Date(art$nyc_all$dt_start), art_nyc_all_relpre[,3],
     ylim=c(0,2), type='p', pch=1, cex=1.4, # las=2, lab = c(100,10,7)
     xlab='', ylab = 'relative no. initiating ART spell', xaxt='n', cex.axis=1.4, cex.lab=1.4)
points(as.Date(art$atl_all$dt_start), art_atl_all_relpre[,3], pch=16, cex=1.4)
abline(h=(seq(0, 2.0, 0.1)), lty=3, col='gray95')
abline(h=1, lty=2)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
legend(dates7[1]+45,0.45, c("New York City", "Metro Atlanta"),
       pch=c(1,16), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     2, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)
text(dates7[1], 1.95, "B)", cex=2.5)

mar=c(3, 5, 1, 1)
plot(as.Date(art$nyc_all$dt_start), art_nyc_all_relpre[,4],
     ylim=c(0,2), type='p', pch=1, cex=1.4, # las=2, lab = c(100,10,7)
     xlab='time', ylab = 'relative no. ending ART spell', xaxt='n', cex.axis=1.4, cex.lab=1.4)
points(as.Date(art$atl_all$dt_start), art_atl_all_relpre[,4], pch=16, cex=1.4)
abline(h=(seq(0, 2.0, 0.1)), lty=3, col='gray95')
abline(h=1, lty=2)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
legend(dates7[1]+45,0.45, c("New York City", "Metro Atlanta"),
       pch=c(1,16), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     2, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)
text(dates7[1], 1.95, "C)", cex=2.5)

dev.off()


# Age ART Plots
tiff("fig_art_by_age.tif", height = 6*1200, width = 4.5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")

par(mfrow=c(3,1), mar=c(2, 5, 1, 1))   # 3 rows preserves formatting from other plots; otherwise everything changes font size no matter what I do!

plot(as.Date(art$nyc_all$dt_start), rep(0,length(as.Date(art$nyc_all$dt_start))),
     type='b', col='white', xlab='Time', ylab='relative no. on ART', ylim=c(0, 1.4), cex=0.7, xaxt='n')

matplot(as.Date(art$nyc_all$dt_start), art_nyc_age_relpre[,1,], type='b', col=c(1:num_aggps),
        cex=0.7, add=T)
abline(h=1)
legend(x=as.Date("2019-09-29"), y=0.8, text.col = 1:num_aggps, cex=0.7, ncol=2,
       legend=c('1 = 13-24', '2 = 25-34', '3 = 35-44', '4 = 45-54', '5 = 55-64', '6 = 65+'))
abline(v=dates7, lty=3, col='lightgray')
axis(1, labels = dates7, at=dates7)
text(dates7[1]+10,1.375, "A) NYC", cex=1.5)

matplot(as.Date(art$atl_all$dt_start), art_atl_age_relpre[,1,], type='b', col=c(1:num_aggps),
        xlab='Time', ylab='relative no. on ART', ylim=c(0, 1.4), cex=0.7)
abline(h=1)
legend(x=as.Date("2019-09-29"), y=0.8, text.col = 1:num_aggps, cex=0.7, ncol=2,
       legend=c('1 = 13-24', '2 = 25-34', '3 = 35-44', '4 = 45-54', '5 = 55-64', '6 = 65+'))
abline(v=dates7, lty=3, col='lightgray')
axis(1, labels = dates7, at=dates7)
text(dates7[1]+18,1.375, "B) Atlanta", cex=1.5)

dev.off()

#### All age Testing

tiff("fig_testing_over_time.tif", height = 6*1200*1.4, width = 4.5*1200*1.4,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
par(mfrow=c(3,1), mar=c(2, 5, 1, 1))

plot(as.Date(testing_byweek$dt_start),testing_relpre$Overall_NYCM_total,
     ylim=c(0,2), type='p', pch=1, cex=1.4, # las=2, lab = c(100,10,7)
     xlab='', ylab = 'relative no. of HIV tests',xaxt='n', cex.axis=1.4, cex.lab=1.4)
points(as.Date(testing_byweek$dt_start),testing_relpre$Overall_ATLM_total,
       pch=16, cex=1.4)
abline(h=(seq(0, 2.0, 0.1)), lty=3, col='gray95', cex.axis=1.4)
abline(h=1, lty=2)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
legend(dates7[1]+45,0.4, c("New York City", "Metro Atlanta"),
       pch=c(1,16), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     2, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)
text(dates7[1], 1.95, "A)", cex=2.5)
lines(c(as.Date("2020-03-22"), as.Date("2020-06-27")), c(1.800, 1.800), lwd=1.5)
text(as.Date("2020-05-09"), 1.850, "NY stay-at-home", cex=1.2)
lines(c(as.Date("2020-04-08"), as.Date("2020-05-01")), c(1.667, 1.667), lwd=1.5)
text(as.Date("2020-04-19"), 1.717, "GA stay-at-home", cex=1.2)

plot(as.Date(testing_byweek$dt_start), positivity_NYC,
     ylim=c(0,0.05), type='p', pch=1, cex=1.4,
     xlab='', ylab = 'positivity',xaxt='n', cex.axis=1.4, cex.lab=1.4)
points(as.Date(testing_byweek$dt_start), positivity_ATL,
       pch=16, cex=1.4)
abline(h=(seq(0, 0.05, 0.01)), lty=3, col='gray95', cex.axis=1.4)
axis(1, labels = dates7, at=dates7, cex.axis=1.4)
abline(v=dates7[-2], lty=3, col='darkgray')
#legend(dates7[1]+45,0.045, c("New York City", "Metro Atlanta"),
#       pch=c(1,16), cex=1.4)
text(c(mean(dates7[c(1,3)]), mean(dates7[c(3:4)]), mean(dates7[c(4:5)]),
       mean(dates7[c(5:6)]), mean(dates7[c(6:7)])),
     0.05, c('pre-COVID', 'Q1','Q2','Q3','Q4'), cex=1.4)
text(dates7[1], 0.04875, "B)", cex=2.5)

dev.off()


# Age Testing Plots
tiff("fig_testing_by_age.tif", height = 6*1200, width = 4.5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")

par(mfrow=c(3,1), mar=c(2, 5, 1, 1))   # 3 rows preserves formatting from other plots; otherwise everything changes font size no matter what I do!

matplot(as.Date(testing_byweek$dt_start),
        cbind(testing_relpre$Overall_NYCM1324_total,
              testing_relpre$Overall_NYCM2534_total,
              testing_relpre$Overall_NYCM3544_total,
              testing_relpre$Overall_NYCM4554_total,
              testing_relpre$Overall_NYCM5564_total,
              testing_relpre$Overall_NYCM65_total),
        type='b', col=c(1:num_aggps),
        cex=0.8, xlab='Time', ylab='relative no. of tests', ylim=c(0, 1.4), xaxt='n')
abline(h=1)
abline(v=dates7, lty=3, col='lightgray')
legend(x=as.Date("2019-08-30"), y=0.3, text.col = 1:num_aggps, cex=1, ncol=2,
       legend=c('1 = ages 13-24', '2 = ages 25-34', '3 = ages 35-44',
                '4 = ages 45-54', '5 = ages 55-64', '6 = ages 65+'))
axis(1, labels = dates7, at=dates7)
text(dates7[1]+10,1.375, "A) NYC", cex=1.5)

matplot(as.Date(testing_byweek$dt_start),
        cbind(testing_relpre$Overall_ATLM1324_total,
          testing_relpre$Overall_ATLM2534_total,
          testing_relpre$Overall_ATLM3544_total,
          testing_relpre$Overall_ATLM4554_total,
          testing_relpre$Overall_ATLM5564_total,
          testing_relpre$Overall_ATLM65_total),
        type='b', col=c(1:num_aggps),
        cex=0.8, xlab='Time', ylab='relative no. of tests', ylim=c(0, 1.4), xaxt='n')
abline(h=1)
abline(v=dates7, lty=3, col='lightgray')
legend(x=as.Date("2019-08-30"), y=0.3, text.col = 1:num_aggps, cex=1, ncol=2,
       legend=c('1 = ages 13-24', '2 = ages 25-34', '3 = ages 35-44',
                '4 = ages 45-54', '5 = ages 55-64', '6 = ages 65+'))
axis(1, labels = dates7, at=dates7)
text(dates7[1]+18,1.375, "B) Atlanta", cex=1.5)


dev.off()

