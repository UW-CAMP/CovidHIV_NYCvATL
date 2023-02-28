# Cases by day

# NYC: file obtained from https://github.com/nychealth/coronavirus-data/tree/master/trends
# cases start with 2020-02-29 and are daily
nyc_data_offset <- 1 # Data begin one day before our period of interest
nyc_week_enddates <- seq(7+nyc_data_offset, 7*weeks.good+nyc_data_offset, 7)  #

nyc_cases <- read.csv("originals/NYC_cases-by-day.csv", header=TRUE)
nyc_cases$CASE_COUNT_7DAY_CUM <- NA
for(i in 1:length(nyc_cases$CASE_COUNT_7DAY_CUM)) {
  nyc_cases$CASE_COUNT_7DAY_CUM[i] <- sum(nyc_cases$CASE_COUNT[max(1,i-6):i])
}
covidepi_nyc <- data.frame(week=prep$nyc_all$week, dt_start = prep$nyc_all$dt_start, dt_end=prep$nyc_all$dt_end)
covidepi_nyc$cases[1:weeks.precov] <- 0
covidepi_nyc$cases[(1+weeks.precov):weeks.good] <- (nyc_cases$CASE_COUNT_7DAY_CUM[nyc_week_enddates])[1:weeks.cov]
pop2020_nyc <- 8253213  # https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.html
covidepi_nyc$caserate <- 1e5*covidepi_nyc$cases/pop2020_nyc

# Georgia: file obtained from https://dph.georgia.gov/covid-19-daily-status-report
# cases start with 2020-02-29 and are daily
ga_cases <- read.csv("originals/epicurve_rpt_date.csv", header=TRUE)

atl_counties <- c('Barrow', 'Bartow', 'Butts', 'Carroll', 'Cherokee', 'Clayton',
                  'Cobb', 'Coweta', 'Dawson', 'DeKalb', 'Douglas', 'Fayette',
                  'Forsyth', 'Fulton', 'Gwinnett', 'Haralson', 'Heard', 'Henry',
                  'Jasper', 'Lamar', 'Meriwether', 'Morgan', 'Newton',
                  'Paulding', 'Pickens', 'Pike', 'Rockdale', 'Spalding', 'Walton')

n_atl_counties <- length(atl_counties)
atl_cases <- filter(ga_cases, county==atl_counties[1])
for (i in 2:n_atl_counties) {
  atl_cases[,-(1:3)] <- atl_cases[,-(1:3)] + filter(ga_cases, county==atl_counties[i])[,-(1:3)]
}
atl_cases %<>% dplyr::select(-(1:2))

atl_cases$CASE_COUNT_7DAY_CUM <- NA
for(i in 1:nrow(atl_cases)) {
  atl_cases$CASE_COUNT_7DAY_CUM[i] <- sum(atl_cases$cases[max(1,i-6):i])
}

atl_data_offset <- 29 # Data begin 29 days before our period of interest
atl_week_enddates <- seq(7+atl_data_offset, 7*weeks.good+atl_data_offset, 7)
covidepi_atl <- data.frame(week=prep$atl_all$week, dt_start = prep$atl_all$dt_start, dt_end=prep$atl_all$dt_end)
covidepi_atl$cases[1:weeks.precov] <- 0
covidepi_atl$cases[(1+weeks.precov):weeks.good] <- (atl_cases$CASE_COUNT_7DAY_CUM[atl_week_enddates])[1:weeks.cov]
pop2020_atl <- 6087762 #https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.html

covidepi_atl$caserate <- 1e5*covidepi_atl$cases/pop2020_atl

# US: file obtained from https://covid.cdc.gov/covid-data-tracker/#trends_dailytrendscases
# Note: US cases already include 7-day moving average so code is simpler

us_cases <- read.csv("originals/data_table_for_daily_case_trends__united_states.csv")
covidepi_us <- covidepi_nyc[,1:3]
us_cases <- us_cases[nrow(us_cases):1,]

us_data_offset <- 39 # Data begin 29 days before our period of interest
us_week_enddates <- seq(7+us_data_offset, 7*weeks.good+us_data_offset, 7)
covidepi_us$cases[1:weeks.precov] <- 0
covidepi_us$cases[(1+weeks.precov):weeks.good] <- us_cases$X7.Day.Moving.Avg[us_week_enddates[1:weeks.cov]]*7

pop2020_us <- 329484123 #https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.html
covidepi_us$caserate <- 1e5*covidepi_us$cases/pop2020_us


