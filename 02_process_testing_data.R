### Obtaining, cleaning and analyzing the clinical data for the COVIDHIV_NYC_ATL project

# Data basics

  num.weeks.toavg <- 4    # How many weeks just pre-COVID should we average to get the baseline for comparison
  weeks.tot <- 83         # How many weeks are there in the files total, and how do they break down re pre-COVID, COVID, and then post-analysis extras?
  weeks.precov <- 26
  weeks.cov <- 52
  weeks.extra <- weeks.tot - weeks.precov - weeks.cov
  weeks.good <- weeks.precov + weeks.cov

# Get testing data
  #test.all <- suppressWarnings(read_xlsx("originals/HIV_Screening_tests.xlsx")) %>%
  #  dplyr::select(Order_Date | starts_with("Overall"))
  test.all <- suppressWarnings(read_xlsx("originals/HIV_Screening_tests11Nov2021final.xlsx")) %>%
    dplyr::select(Collection_Date | starts_with("Overall")) %>%
    dplyr::select(-Overall_Collection_Date)

# Notes on testing data
  # Data from two sources (lab and lab_B) come first, and then sums of the two;
  #   we use (and only keep) the latter
  # ***MY = unknown age

# Combine weeks

  testing_cutoff_row <- which(test.all$Collection_Date==as.Date("2019-08-31"))
  testing_week_enddates <- seq(testing_cutoff_row+7, 7*weeks.good+testing_cutoff_row, 7)  #

  testing_byweek <- test.all[testing_week_enddates-6,-1]+
                    test.all[testing_week_enddates-5,-1]+
                    test.all[testing_week_enddates-4,-1]+
                    test.all[testing_week_enddates-3,-1]+
                    test.all[testing_week_enddates-2,-1]+
                    test.all[testing_week_enddates-1,-1]+
                    test.all[testing_week_enddates  ,-1]
  testing_byweek$dt_start <- as.Date(test.all$Collection_Date[testing_week_enddates])-6
  testing_byweek$dt_end <- as.Date(test.all$Collection_Date[testing_week_enddates])

# Standardize capitalization

  testing_byweek %<>% rename(
    Overall_national_totneg=Overall_National_totneg,
    Overall_NYCM_total=Overall_NYCm_Total,
    Overall_ATLM_neg=Overall_ATLM_Neg,
    Overall_NYCM3544_total=Overall_NYCM3544_Total,
    Overall_NYCM4554_pos=Overall_NYCM4554_Pos,
    Overall_NYCM5564_neg=Overall_NYCM5564_Neg,
    Overall_ATLM3544_total=Overall_ATLM3544_Total,
    Overall_ATLM4554_pos=Overall_ATLM4554_Pos,
    Overall_ATLM5564_neg=Overall_ATLM5564_Neg
  )

#### Define new age groups

  testing_byweek %<>% mutate(Overall_NYCM2554_total = Overall_NYCM2534_total+
                               Overall_NYCM3544_total + Overall_NYCM4554_total)
  testing_byweek %<>% mutate(Overall_NYCM2554_pos = Overall_NYCM2534_pos+
                               Overall_NYCM3544_pos + Overall_NYCM4554_pos)
  testing_byweek %<>% mutate(Overall_NYCM2554_neg = Overall_NYCM2534_neg+
                               Overall_NYCM3544_neg + Overall_NYCM4554_neg)

  testing_byweek %<>% mutate(Overall_ATLM2554_total = Overall_ATLM2534_total+
                               Overall_ATLM3544_total + Overall_ATLM4554_total)
  testing_byweek %<>% mutate(Overall_ATLM2554_pos = Overall_ATLM2534_pos+
                               Overall_ATLM3544_pos + Overall_ATLM4554_pos)
  testing_byweek %<>% mutate(Overall_ATLM2554_neg = Overall_ATLM2534_neg+
                               Overall_ATLM3544_neg + Overall_ATLM4554_neg)

  testing_byweek %<>% mutate(Overall_NYCM55p_total = Overall_NYCM5564_total+
                               Overall_NYCM65_total)
  testing_byweek %<>% mutate(Overall_NYCM55p_pos = Overall_NYCM5564_pos+
                               Overall_NYCM65_pos)
  testing_byweek %<>% mutate(Overall_NYCM55p_neg = Overall_NYCM5564_neg+
                               Overall_NYCM65_neg)

  testing_byweek %<>% mutate(Overall_ATLM55p_total = Overall_ATLM5564_total+
                               Overall_ATLM65_total)
  testing_byweek %<>% mutate(Overall_ATLM55p_pos = Overall_ATLM5564_pos+
                               Overall_ATLM65_pos)
  testing_byweek %<>% mutate(Overall_ATLM55p_neg = Overall_ATLM5564_neg+
                               Overall_ATLM65_neg)

#### Calculating relative testing prevalence

data.cols <- as.vector(which(sapply(testing_byweek, class)=="numeric"))
precovwks <- (weeks.precov - num.weeks.toavg + 1):weeks.precov
testing_premeans <- colMeans(testing_byweek[precovwks,data.cols])
testing_relpre  <- sweep(testing_byweek[,data.cols], 2, testing_premeans, "/")

#### Positivity

positivity_ATL <- testing_byweek$Overall_ATLM_pos/testing_byweek$Overall_ATLM_total
positivity_NYC <- testing_byweek$Overall_NYCM_pos/testing_byweek$Overall_NYCM_total

## Numbers in paper

reduxbyage_NYC <- c(
  mean(testing_relpre$Overall_NYCM1324_total[31:36]),
  mean(testing_relpre$Overall_NYCM2534_total[31:36]),
  mean(testing_relpre$Overall_NYCM3544_total[31:36]),
  mean(testing_relpre$Overall_NYCM4554_total[31:36]),
  mean(testing_relpre$Overall_NYCM5564_total[31:36]),
  mean(testing_relpre$Overall_NYCM65_total[31:36])
)
reduxbyage_NYC

reduxbyage_ATL <- c(
  mean(testing_relpre$Overall_ATLM1324_total[31:36]),
  mean(testing_relpre$Overall_ATLM2534_total[31:36]),
  mean(testing_relpre$Overall_ATLM3544_total[31:36]),
  mean(testing_relpre$Overall_ATLM4554_total[31:36]),
  mean(testing_relpre$Overall_ATLM5564_total[31:36]),
  mean(testing_relpre$Overall_ATLM65_total[31:36])
)
reduxbyage_ATL
