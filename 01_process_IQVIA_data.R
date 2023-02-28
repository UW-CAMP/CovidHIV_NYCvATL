### Obtaining, cleaning and analyzing the clinical data for the COVIDHIV_NYC_ATL project

# Data basics

num.weeks.toavg <- 4    # How many weeks just pre-COVID should we average to get the baseline for comparison
data.cols <- 3:6        # In which columsn are the key numerical data?
num_aggps <- 6          # How many age groups are there?

weeks.tot <- 83         # How many weeks are there in the files total, and how do they break down re pre-COVID, COVID, and then post-analysis extras?
weeks.precov <- 26
weeks.cov <- 52
weeks.extra <- weeks.tot - weeks.precov - weeks.cov
weeks.good <- weeks.precov + weeks.cov

# Get IQVIA data

  # 83 weeks x 2 geographies = 166 rows
  iqvia.prep.all <- suppressWarnings(read_csv("originals/prep_all_2021q1.csv")) %>% dplyr::select(-1)
  iqvia.art.all <- suppressWarnings(read_csv("originals/arv_all_2021q1.csv")) %>% dplyr::select(-1)

  # 83 weeks x 2 geographies x 6 age groups = 996 rows
  iqvia.prep.age <- suppressWarnings(read_csv("originals/prep_age_2021q1.csv")) %>% dplyr::select(-1)
  iqvia.art.age <- suppressWarnings(read_csv("originals/arv_age_2021q1.csv")) %>% dplyr::select(-1)

# Notes on IQVIA data
  # ARV pages = ART specifically. From Weiming Zhu email of 2021.07.02:
  #   ARV data here are specified for antiretroviral treatment for HIV. PrEP is specific for estimated PrEP user.
  #   We combined all available diagnosis for history of HIV Dx or HBV Dx, and calculated cumulative length of supply (we discussed last time), type and number of concurrent medications, ect. and  developed a comprehensive framework to process all prescriptions into PrEP, PEP, ART for HIV, anti-HBV treatment, and unknown. The PrEP algorithm has been published, and the basic logic is for those Truvada or Descovy monousers (no other ARV), with no HIV Dx, and more than 28 days of supply;
  #   https://pubmed.ncbi.nlm.nih.gov/27986691/
  #   https://www.cdc.gov/mmwr/volumes/67/wr/mm6741a3.htm

  # n_a_prev	Number of persons with active supply of arv at the start of the week.;
  # n_b_rx1	Number of persons filling their first ever prescription for arv during the week
  # n_c_ep1	Number of persons beginning a new supply episode of arv during the week (whether first episode ever or not);
  # n_d_end	Number of persons ending a supply episode of arv during the week;

  # if 15 <= age1 <= 24 then aggp = 1;
  # if 25 <= age1 <= 34 then aggp = 2;
  # if 35 <= age1 <= 44 then aggp = 3;
  # if 45 <= age1 <= 54 then aggp = 4;
  # if 55 <= age1 <= 64 then aggp = 5;
  # if age1 >= 65 then aggp = 6;
  # agpp = 99 is unknown age (from Weiming Zhu's email of 2021.07.02)

  # Nesting of values (see Weiming Zhu email 2021.07.02)
  #   SMG: I want to make very sure that I understand the way that the columns n_a_prev,  n_b_rx1, n_c_ep1 and n_d_end nest.  Are each of the following statements true or false?
  #        -> If someone appears in n_b_rx1 in a given week, then they also appear in n_a_prev      Yes
  #        -> If someone appears in n_c_ep1 in a given week, then they also appear in n_a_prev     Yes
  #        -> If someone appears in n_d_end in a given week, then they also appear in n_a_prev     Yes
  #        -> If someone appears in n_b_rx1 in a given week, then they also appear in n_c_ep1       Yes
  #   WZ: Yes to all.
  #       I used the starting date and inferred end date of one continued Rx episode to find these cases. Let min_dt be the start of the Rx, and max_dt be the inferred end of the Rx.
  #       For each week, let dt_start be the Sunday, and dt_end be the Saturday
  #       For prevalent users at the start of the week,  select all patients  with  min_dt <=  dt_start <= max_dt
  #       For ever 1st Rx, restrict data to the 1st Rx episode of each patient, and for each week, select  patients  with  min_dt <=  dt_start <= max_dt
  #       For starting new Rx episode,  for each week,  select patients with   dt_start <= min_dt <= dt_end
  #       For ending Rx episode, similarly, select patients with dt_start <= max_dt <= dt_end

#########################
#### Exploratory analysis

  #### All-age PrEP

    # Create all-age PrEP object
    prep <- list()
    prep$nyc_all <- iqvia.prep.all %>% filter(msa=="NYC")
    prep$atl_all <- iqvia.prep.all %>% filter(msa=="ATL")

    # Calculate pre-COVID prep means

    precovwks <- (weeks.precov - num.weeks.toavg + 1):weeks.precov
    prep_nyc_all_premeans <- colMeans(prep$nyc_all[precovwks,data.cols])
    prep_nyc_all_relpre  <- sweep(prep$nyc_all[,data.cols], 2, prep_nyc_all_premeans, "/")
    prep_nyc_all_abs <- prep$nyc_all[,data.cols]

    prep_atl_all_premeans <- colMeans(prep$atl_all[precovwks,data.cols])
    prep_atl_all_relpre  <- sweep(prep$atl_all[,data.cols], 2, prep_atl_all_premeans, "/")
    prep_atl_all_abs <- prep$atl_all[,data.cols]

  #### All-age ART

    # Create all-age ART object
    art <- list()
    art$nyc_all <- iqvia.art.all %>% filter(msa=="NYC")
    art$atl_all <- iqvia.art.all %>% filter(msa=="ATL")

    # Calculate pre-COVID ART means
    # Weeks 1-26 (i.e 2019-09-01 through 2020-02-29)
    art_nyc_all_premeans <- colMeans(art$nyc_all[precovwks,data.cols])
    art_nyc_all_relpre  <- sweep(art$nyc_all[,data.cols], 2, art_nyc_all_premeans, "/")
    art_nyc_all_abs <- art$nyc_all[,data.cols]

    art_atl_all_premeans <- colMeans(art$atl_all[precovwks,data.cols])
    art_atl_all_relpre  <- sweep(art$atl_all[,data.cols], 2, art_atl_all_premeans, "/")
    art_atl_all_abs <- art$atl_all[,data.cols]

  #### PrEP by age cats

    # Create age-specific PrEP
    prep$nyc_age <- iqvia.prep.age %>% filter(msa=="NYC")
    prep$atl_age <- iqvia.prep.age %>% filter(msa=="ATL")

    # Calculate pre-COVID prep means
    prep_nyc_age_premeans <- matrix(NA, num_aggps, 4)
    prep_nyc_age_relpre <- array(NA, dim=c(weeks.tot, 4, num_aggps))
    for (i in 1:num_aggps) {
      prep_nyc_age_premeans[i,] <-
        prep$nyc_age %>% filter(aggp==i) %>% slice(precovwks) %>% dplyr::select(n_a_prev:n_d_end) %>% colMeans
      prep_nyc_age_relpre[,,i] <-
        as.matrix(prep$nyc_age %>% filter(aggp==i) %>% dplyr::select(n_a_prev:n_d_end) %>%
          sweep(2, prep_nyc_age_premeans[i,], "/"))
    }

    prep_atl_age_premeans <- matrix(NA, num_aggps, 4)
    prep_atl_age_relpre <- array(NA, dim=c(weeks.tot, 4, num_aggps))
    for (i in 1:num_aggps) {
      prep_atl_age_premeans[i,] <-
        prep$atl_age %>% filter(aggp==i) %>% slice(precovwks) %>% dplyr::select(n_a_prev:n_d_end) %>% colMeans
      prep_atl_age_relpre[,,i] <-
        as.matrix(prep$atl_age %>% filter(aggp==i) %>% dplyr::select(n_a_prev:n_d_end) %>%
                    sweep(2, prep_atl_age_premeans[i,], "/"))
    }

  #### ART by age cats

    # Create age-specific art
    art$nyc_age <- iqvia.art.age %>% filter(msa=="NYC")
    art$atl_age <- iqvia.art.age %>% filter(msa=="ATL")

    # Calculate pre-COVID art means
    art_nyc_age_premeans <- matrix(NA, num_aggps, 4)
    art_nyc_age_relpre <- array(NA, dim=c(weeks.tot, 4, num_aggps))
    for (i in 1:num_aggps) {
      art_nyc_age_premeans[i,] <-
        art$nyc_age %>% filter(aggp==i) %>% slice(precovwks) %>% dplyr::select(n_a_prev:n_d_end) %>% colMeans
      art_nyc_age_relpre[,,i] <-
        as.matrix(art$nyc_age %>% filter(aggp==i) %>% dplyr::select(n_a_prev:n_d_end) %>%
                    sweep(2, art_nyc_age_premeans[i,], "/"))
    }

    art_atl_age_premeans <- matrix(NA, num_aggps, 4)
    art_atl_age_relpre <- array(NA, dim=c(weeks.tot, 4, num_aggps))
    for (i in 1:num_aggps) {
      art_atl_age_premeans[i,] <-
        art$atl_age %>% filter(aggp==i) %>% slice(precovwks) %>% dplyr::select(n_a_prev:n_d_end) %>% colMeans
      art_atl_age_relpre[,,i] <-
        as.matrix(art$atl_age %>% filter(aggp==i) %>% dplyr::select(n_a_prev:n_d_end) %>%
                    sweep(2, art_atl_age_premeans[i,], "/"))
    }

prep[[1]] %<>% slice_head(n=weeks.good)
prep[[2]] %<>% slice_head(n=weeks.good)
prep_atl_all_relpre %<>% slice_head(n=weeks.good)
prep_nyc_all_relpre %<>% slice_head(n=weeks.good)
prep_atl_age_relpre <- prep_atl_age_relpre[1:weeks.good,,]
prep_nyc_age_relpre <- prep_nyc_age_relpre[1:weeks.good,,]
prep_atl_all_abs %<>% slice_head(n=weeks.good)
prep_nyc_all_abs %<>% slice_head(n=weeks.good)

art[[1]] %<>% slice_head(n=weeks.good)
art[[2]] %<>% slice_head(n=weeks.good)
art_atl_all_relpre %<>% slice_head(n=weeks.good)
art_nyc_all_relpre %<>% slice_head(n=weeks.good)
art_atl_age_relpre <- art_atl_age_relpre[1:weeks.good,,]
art_nyc_age_relpre <- art_nyc_age_relpre[1:weeks.good,,]
art_atl_all_abs %<>% slice_head(n=weeks.good)
art_nyc_all_abs %<>% slice_head(n=weeks.good)


## Numbers in paper

mean(prep_nyc_all_relpre[40:78,1])
max(prep_nyc_all_relpre[40:78,1])
min(prep_nyc_all_relpre[40:78,1])
mean(prep_atl_all_relpre[40:78,1])
max(prep_atl_all_relpre[40:78,1])
min(prep_atl_all_relpre[40:78,1])

mean(art_nyc_all_relpre[40:78,1])
max(art_nyc_all_relpre[40:78,1])
min(art_nyc_all_relpre[40:78,1])
mean(art_atl_all_relpre[40:78,1])
max(art_atl_all_relpre[40:78,1])
min(art_atl_all_relpre[40:78,1])
