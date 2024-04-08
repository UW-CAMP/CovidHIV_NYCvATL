
q1 <- 27:39
q2 <- 40:52
q3 <- 53:65
q4 <- 66:78

# all ages

#   100*round(rbind(
#   colMeans(prep_nyc_all_relpre[q1,3:4]),
#   colMeans(prep_nyc_all_relpre[q2,3:4]),
#   colMeans(prep_nyc_all_relpre[q3,3:4]),
#   colMeans(prep_nyc_all_relpre[q4,3:4])
# ), 3)
#
# 100*round(rbind(
#   colMeans(prep_atl_all_relpre[q1,3:4]),
#   colMeans(prep_atl_all_relpre[q2,3:4]),
#   colMeans(prep_atl_all_relpre[q3,3:4]),
#   colMeans(prep_atl_all_relpre[q4,3:4])
# ), 3)


# By age

nyc_y <- 100*round(rbind(
            colMeans(prep_nyc_age_relpre[q1,3:4,1]),
            colMeans(prep_nyc_age_relpre[q2,3:4,1]),
            colMeans(prep_nyc_age_relpre[q3,3:4,1]),
            colMeans(prep_nyc_age_relpre[q4,3:4,1])
          ), 3)

atl_y <- 100*round(rbind(
            colMeans(prep_atl_age_relpre[q1,3:4,1]),
            colMeans(prep_atl_age_relpre[q2,3:4,1]),
            colMeans(prep_atl_age_relpre[q3,3:4,1]),
            colMeans(prep_atl_age_relpre[q4,3:4,1])
          ), 3)

nyc_agewts_init <- prop.table(prep_nyc_age_premeans[-1,3])
nyc_agewts_stop <- prop.table(prep_nyc_age_premeans[-1,4])
atl_agewts_init <- prop.table(prep_atl_age_premeans[-1,3])
atl_agewts_stop <- prop.table(prep_atl_age_premeans[-1,4])

nyc_o <- cbind(100*round(rbind(
            sum(colMeans(prep_nyc_age_relpre[q1,3,2:6])*nyc_agewts_init),
            sum(colMeans(prep_nyc_age_relpre[q2,3,2:6])*nyc_agewts_init),
            sum(colMeans(prep_nyc_age_relpre[q3,3,2:6])*nyc_agewts_init),
            sum(colMeans(prep_nyc_age_relpre[q4,3,2:6])*nyc_agewts_init)
          ), 3),
            100*round(rbind(
            sum(colMeans(prep_nyc_age_relpre[q1,4,2:6])*nyc_agewts_stop),
            sum(colMeans(prep_nyc_age_relpre[q2,4,2:6])*nyc_agewts_stop),
            sum(colMeans(prep_nyc_age_relpre[q3,4,2:6])*nyc_agewts_stop),
            sum(colMeans(prep_nyc_age_relpre[q4,4,2:6])*nyc_agewts_stop)
          ), 3)
        )

atl_o <- cbind(100*round(rbind(
            sum(colMeans(prep_atl_age_relpre[q1,3,2:6])*atl_agewts_init),
            sum(colMeans(prep_atl_age_relpre[q2,3,2:6])*atl_agewts_init),
            sum(colMeans(prep_atl_age_relpre[q3,3,2:6])*atl_agewts_init),
            sum(colMeans(prep_atl_age_relpre[q4,3,2:6])*atl_agewts_init)
          ), 3),
          100*round(rbind(
            sum(colMeans(prep_atl_age_relpre[q1,4,2:6])*atl_agewts_stop),
            sum(colMeans(prep_atl_age_relpre[q2,4,2:6])*atl_agewts_stop),
            sum(colMeans(prep_atl_age_relpre[q3,4,2:6])*atl_agewts_stop),
            sum(colMeans(prep_atl_age_relpre[q4,4,2:6])*atl_agewts_stop)
          ), 3)
        )

cbind(nyc_y, atl_y, nyc_o, atl_o)
barplot(cbind(nyc_y, atl_y, nyc_o, atl_o), beside = T)


# NYC all - approx as unweighted age group mean

plot(rowMeans(prep_nyc_age_relpre[,1,]), ylim=c(0,2)); abline(h=1, v=26) # prev  # AS EXPECTED
plot(rowMeans(prep_nyc_age_relpre[,3,]), ylim=c(0,2)); abline(h=1, v=26) # start # AS EXPECTED
plot(rowMeans(prep_nyc_age_relpre[,4,]), ylim=c(0,2)); abline(h=1, v=26) # stop  # AS EXPECTED

# NYC young
plot(prep_nyc_age_relpre[,1,1], ylim=c(0,2)); abline(h=1, v=26) # prev  # AS EXPECTED
plot(prep_nyc_age_relpre[,3,1], ylim=c(0,2)); abline(h=1, v=26) # start # AS EXPECTED
plot(prep_nyc_age_relpre[,4,1], ylim=c(0,2)); abline(h=1, v=26) # stop  # WEIRD DECLINE IN STOPPING AFTER Q1

# ATL all - approx as unweighted age group mean

plot(rowMeans(prep_atl_age_relpre[,1,]), ylim=c(0,2)); abline(h=1, v=26) # prev  # AS EXPECTED
plot(rowMeans(prep_atl_age_relpre[,3,]), ylim=c(0,2)); abline(h=1, v=26) # start # AS EXPECTED
plot(rowMeans(prep_atl_age_relpre[,4,]), ylim=c(0,2)); abline(h=1, v=26) # stop  # AS EXPECTED - but with interesting decline at end

# atl young
plot(prep_atl_age_relpre[,1,1], ylim=c(0,2)); abline(h=1, v=26) # prev  # AS EXPECTED
plot(prep_atl_age_relpre[,3,1], ylim=c(0,2)); abline(h=1, v=26) # start # AS EXPECTED
plot(prep_atl_age_relpre[,4,1], ylim=c(0,2)); abline(h=1, v=26) # stop  # AS EXPECTED


# Do the IQVIA numbers have internal consistency?
# Not exact, bu pretty darn close:
plot(as.vector(prep_nyc_all_abs[-1,1])[[1]])
points(prep_nyc_all_abs[-78,1] + prep_nyc_all_abs[-78,3] - prep_nyc_all_abs[-78,4])

plot(as.vector(prep_atl_all_abs[-1,1])[[1]])
points(prep_atl_all_abs[-78,1] + prep_atl_all_abs[-78,3] - prep_atl_all_abs[-78,4])

# So will need to check on the levels of PrEP prevalence after simulations

