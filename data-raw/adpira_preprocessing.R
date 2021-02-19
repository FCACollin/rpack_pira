#==============================================================================#
#
# title:    RRMS/PIRA - Preparation of ADPIRA
# encoding: utf-8"
# author:   Francois C A COLLIN, Ph.D. - fca.collin@gmail.com
# date:     [FC 210219 08:20]
# Note:     vim-user friendly script / UTF8
#
#==============================================================================#

# ---
# **NOTE**
# Must be run from package root.
# The raw data are not included in the package, if you need it, contact me.
# The code is study specific.
# ---

input_file <- file.path("inst", "extdata", "data_MSclerosis_181210.csv")
library(parallel)

## @knitr dataRRMS ----
#[FC 200816 12:51]#  {{{

## nolint start ----

# [FC 190830 11:58]: the addition of MS_Centre has not changed the row number of 
# dim(RRMS$presc_raw)
# [1] 15368    55

RRMS <- list();

#[FC/190129/15:24]# RRMS$raw_df {{{ 

RRMS$raw_df <- read.table(
  file = input_file, skip = 3, header = TRUE, sep = ';',
  stringsAsFactors = FALSE, na.strings = ""
  )[-1];
RRMS$raw_df_initrecordnumber <- nrow(RRMS$raw_df);
RRMS$raw_df <- unique(RRMS$raw_df);

#RRMS$raw_df$MS_centre 
RRMS$raw_df <- within(
  RRMS$raw_df,
  {
    MS_centre <- gsub(
      pattern = "ODDZIAŁ WOJEWÓDZKI NARODOWEGO FUNDUSZU ZDROWIA",
      replacement = "", x = MS_centre
      );
    sex <- ifelse(test = sex == "mężczyzna", yes = "male", no = "female")
    date_obs <- as.Date(date_obs);

    # The initial treatment line information is not complete,
    # instead I use drug to determined the treatment line
    treatment_line <- as.factor(ifelse(
        test = drug %in% c(
          "alemtuzumab", "fumaran dimetylu", "interferon beta",
          "octan glatirameru", "peginterferon beta-1a", "teryflunomid"
          ),
        yes = "I", no = ifelse(
          test = drug %in% c("fingolimod", "natalizumab"),
          yes = "II",
          no = "error"
        )
        ));

    next_drug_old <- next_drug;
    next_drug <- dplyr::case_when(
      next_drug == "alemtuzumab" ~ "ALZ", 
      next_drug == "fumaran dimetylu" ~ "DMF", 
      next_drug == "interferon beta" ~ "INF", 
      next_drug == "octan glatirameru" ~ "GLA", 
      next_drug == "peginterferon beta-1a" ~ "PEG", 
      next_drug == "teryflunomid" ~ "TER", 
      next_drug == "fingolimod" ~ "FTY", 
      next_drug == "natalizumab" ~ "NAT", 
      TRUE      ~ "error"
      );
    # patient_age: the patient age is not accurate (age at what time) and 
    # different method have been employed to complete that information as the
    # vast majority of the patients are associated to a unique age, while
    # a substantial number of patient is associated with 2 ages and 19
    # patients are associated with 3 ages.

    prog_id <- paste(patient_id, start_date, treatment_line);

    n_relapse <- ifelse(
      test = n_relapse %in% c('1', '2', '>2') | is.na(n_relapse),
      yes  = n_relapse,
      no   = 0
      );

    n_new_T2[is.na(n_new_T2)] <- 0;
    new_change_gd[is.na(new_change_gd)] <- 0;
    new_mri <- n_new_T2 + new_change_gd;

    EDSS_delta <- EDSS_end - EDSS_init

  }
  );

RRMS$raw_df$MS_centre <- factor(
  gsub(" ", "", as.character(RRMS$raw_df$MS_centre))
  );
unique(as.character(RRMS$raw_df$MS_centre))
# summary(RRMS[c("sex", "drug", "treatment_line", "NFZ", "MS_centre")], 20);
# names(RRMS)

RRMS$raw_df <- RRMS$raw_df[with(RRMS$raw_df, order(patient_id, date_obs)),];

# each time `drug switch` is/is not NA then  `start_date_next_drug` is/ is also
# not NA.
# with(RRMS$raw_df, is.na(drug_switch) == is.na(start_date_next_drug)) %>% all

###}}} -- vim fold

gc();gc(reset=TRUE)

#[FC/190129/19:10]# dta_presc {{{

dta_presc <- subset(
  RRMS$raw_df,
  prog_id %in% list(
    # [1] all prog_id
    unique(prog_id),

    # [2] Random samples
    sample(unique(prog_id), 2),

    # [3] Basic: switch
    c("5083 2015-05-14 I", "2095 2012-09-20 I"),

    # [4] Relapses data
    c(
      #       "2942 2009-11-25 I"
      # , "4879 2009-04-17 I", "7125 2012-10-09 I",
      # "1723 2012-09-24 I", "151 2014-12-16 I", "5784 2015-12-08 I",
      "2758 2015-01-12 I"
      ),

    # [5] Frequent observations
    c("1185 2011-09-26 I"),

    # [6] Odd: long time intervale between presc start and obs_start
    c("1200 2014-02-14 I", "1581 2014-03-01 I"),

    # [7] Erreur about switches:
    c("6294 2015-12-31 I"),

    # [8] Erreur about EDSS:
    c("6742 2015-08-13 I", "1712 2013-12-04 I", "1035 2014-04-11 I"),

    # [9]
    c(
      "3556 2014-07-10 I", "156 2017-01-10 I", "156 2012-09-26 I",
      "10934 2016-06-07 I", "6023 2016-01-11 I"
      ),

    # [10]
    c(
      "1431 2014-01-15 I"
      ),

    # [11]
    c(
      #"9394 2010-10-21 I",
      # "4653 2015-10-28 I"
      "10036 2017-04-25 I"
    )
    )[[1]]
  );

###}}} -- vim fold

#[FC/190129/19:09]# RRMS$presc {{{

#[FC/190129/19:10]# by: RRMS$presc_raw ~ prog_id{{{

RRMS$presc_raw <- by(data = dta_presc, INDICES = list(dta_presc$prog_id), function(x) x);
PrescSynth <- function(x, verbose = FALSE, verbose_obs = FALSE){
  #[FC/190225/13:21]# {{{

  #[FC/190130/08:39]# Print x {{{

  if(verbose) cat(
    '\n\n', rep('_', 80), '\n\n',
    '\tprogram: ', unique(x$prog_id), '\n',
    rep('~', 50), '\n',
    sep = ''
    );

  if(verbose) print(x[c(
      "patient_id", "treatment_line",  "start_date", "termination_date",
      "MS_centre",
      # "prog_id",
      "date_obs"
      # , "patient_birthyear" 
      , "next_drug", "end_date_current_drug", "start_date_next_drug",
      "drug_switch", "date_relapse", "EDSS", "EDSS_init", "EDSS_end"
      )]);

  ###}}} -- vim fold
  #[FC/190130/08:40]# Basic y: list of prescription basically defined {{{

  y <- with(
    x,
    data.frame(
      prog_id      = prog_id,
      patient_id   = patient_id,
      MS_centre    = MS_centre,
      prog_start   = start_date,
      treatment_line = treatment_line,
      prog_end     = termination_date,
      prescription = next_drug,
      presc_start  = c(start_date[1], start_date_next_drug[-1]),
      presc_end    = end_date_current_drug,
      EDSS_start   = EDSS, 
      stringsAsFactors = FALSE
    )
    );

  y <- subset(y, !is.na(presc_start));
  y <- within(
    y,
    {
      presc_end <- c(presc_end[-1], rev(prog_end)[1]);
      eval_start <- as.character(as.Date(y$presc_start) + round(365.25/12*6));
      # eval_start <- as.character(as.Date(y$presc_start));
    }
    );

  ###}}} -- vim fold
  #[FC/190130/08:40]# y: obs_n {{{

  y$obs_start      <- NA;
  y$obs_end        <- NA;
  y$obs_number     <- NA;
  y$obs_baseline   <- min(as.character(x$date_obs));

  for(i in 1:nrow(y)){

    if(verbose_obs) print(y$prescription[i]);

    # Vector of obs date for a given program
    z <- as.character(x$date_obs)

    # To select the obs_date, the idea is to build a vector of
    # potential date, and take the first value which is not null
    # Scalar: the obs date when the drug start was decided 
    date_lo <- c(
      z[x$start_date_next_drug == y$presc_start[i]],
      z[as.character(min(x$start_date, as.character(x$date_obs[1]))) == y$presc_start[i]]
      );
    #     print(z[x$start_date_next_drug == y$presc_start[i]])
    #     print(z[as.character(x$start_date) == y$presc_start[i]])
    #     print(date_lo)
    date_lo <- date_lo[!is.na(date_lo)][1];
    date_up <- c(
      # The date obs when the drug switch was decided
      z[x$end_date_current_drug == y$presc_end[i]], max(z)
      );
    date_up <- date_up[!is.na(date_up)][1];
    z <- z[z >= date_lo & z <= date_up]

    y$obs_number[i] <- length(z);
    y$obs_start[i]  <- min(z);
    y$obs_end[i]    <- max(z);

  }

  y$obs_days   <- as.numeric(as.Date(y$obs_end) - as.Date(y$obs_start));
  y$obs_years  <- round(y$obs_days/365.25, 2);
  y$obs_months <- round(y$obs_days/(365.25/12), 2);

  ###}}} -- vim fold
  # [FC 190219 12:15] I should add a chunck here
  # Look at the y to see if a prescription, which is not the last, was
  # associated with 0 observation time. It would help detecting some errors.
  # e.g. patient 1413.
  #[FC/190130/10:02]# y: prescription duration and `switch_from` {{{

  y$presc_years   <- round(as.numeric(
      as.Date(ifelse(
          test = !is.na(y$presc_end),
          yes  = y$presc_end,
          no   = y$obs_end
          )) - as.Date(y$presc_start)
      )/365.25, 2); 

  y$presc_id <- paste(y$prog_id, y$prescription, y$presc_start);
  y$switch_from <- c("1st in prog.", rev(rev(y$prescription)[-1]));
  y$switch_from_id <- c("1st in prog.", rev(rev(y$presc_id)[-1]));

  ###}}} -- vim fold
  #[FC/190131/11:38]# y: EDSS {{{

  y$EDSS_end <- NA;
  for(i in 1:nrow(y)){

    a <- unique(x$EDSS[
      as.character(x$date_obs) == y$obs_end[i]
      ]); 

    if(length(a) == 1){ y$EDSS_end[i] <- a; }

    # cat('~~ Length a: ', length(a), '\n\n~~~~~');
    # if(length(a) > 1) cat("Pmb - Prog id:", y$prog_id[i], "\n\n");


  }

  ###}}} -- vim fold
  #[FC/190201/12:51]# y: relapse {{{

  y$rlp_n <- NA;   # number of relapses
  y$rlp_1st <- NA; # 1st date of relapse
  y$rlp_ft <- NA;  # relapse-free time
  y$rlp_arr <- NA;  # annual relapse rate

  y$rlp_d1_n <- NA;   # number of relapses
  y$rlp_d1_1st <- NA; # 1st date of relapse
  y$rlp_d1_ft <- NA;  # relapse-free time
  y$rlp_d1_arr <- NA;  # annual relapse rate

  for(i in 1:nrow(y)){
    #[FC/190204/09:53]# Relapse from obs start to obs end {{{

    #[FC/190226/13:21]# z: relapse along with EDSS {{{

    z <- subset(x, 
      x$date_obs >=  y$obs_start[i] &
        x$date_obs <= y$obs_end[i]
      )[c("date_obs", "date_relapse", "EDSS", "EDSS_init", "EDSS_end")];
    z <- unique(z);
    z <- z[order(z$date_obs, z$date_relapse),]
    z <- within(
      data = z,
      expr = {
        EDSS_current  <- ifelse(is.na(EDSS_end), EDSS, EDSS_end);
        EDSS_previous <- c(NA, rev(rev(EDSS_current)[-1]));
        EDSS_relapse <- ifelse(
          !is.na(EDSS_end) & EDSS_end > EDSS_init,
          EDSS_end, EDSS_init
          );
        EDSS_delta  <- ifelse(
          test = is.na(EDSS_end),
          yes  = EDSS,
          no   = EDSS_relapse
          );
        EDSS_delta <- EDSS_delta - EDSS_previous;
      }
      );
    z <- subset(z, date_relapse > y$eval_start[i]);

    ###}}} -- vim fold
    #[FC/190226/13:21]# y$rlp_1st[i] {{{

    y$rlp_1st[i] <- ifelse(
      test = all(is.na(z$date_relapse)),
      yes = NA,
      no =  min(z$date_relapse[!is.na(z$date_relapse)])
      );
    y$rlp_n[i] <- length(unique(z$date_relapse[!is.na(z$date_relapse)]));

    ###}}} -- vim fold
    #[FC/190226/13:21]# y$rlp_d1_1st[i] {{{

    y$rlp_d1_1st[i] <- ifelse(
      test = any(!is.na(z$date_relapse) & z$EDSS_delta >= 1),
      yes  = min(z$date_relapse[!is.na(z$date_relapse) & z$EDSS_delta >= 1]),
      no   = NA
      );
    y$rlp_d1_n[i] <- length(
      unique(z$date_relapse[!is.na(z$date_relapse) & z$EDSS_delta >= 1])
      );

    ###}}} -- vim fold
    if(FALSE) cat('\n data focus:\n');
    if(FALSE) print(z)

    ###}}} -- vim fold
  }

  y$rlp_ft  <- as.numeric(as.Date(y$rlp_1st) - as.Date(y$presc_start)); 
  y$rlp_arr <- y$rlp_n / y$presc_years; 
  y$rlp_d1_ft  <- as.numeric(as.Date(y$rlp_d1_1st) - as.Date(y$presc_start)); 
  y$rlp_d1_arr <- y$rlp_d1_n / y$presc_years; 

  ###}}} -- vim fold
  #[FC/190315/10:20]# first EDSS worsening{{{

  y$dEDSS_1st <- NA; # 1st date of EDSS worsened  > 1
  y$dEDSS_ft <- NA;  # EDSS worsening free time

  for(i in 1:nrow(y)){
    #[FC/190204/09:53]# Relapse from obs start to obs end {{{

    #[FC/190226/13:21]# z: relapse along with EDSS {{{

    z <- subset(x, 
      x$date_obs >=  y$obs_start[i] &
        x$date_obs <= y$obs_end[i]
      )[c("date_obs", "date_relapse", "EDSS", "EDSS_init", "EDSS_end")];
    z <- unique(z);
    z <- z[order(z$date_obs, z$date_relapse),]
    z <- within(
      data = z,
      expr = {
        EDSS_current  <- ifelse(is.na(EDSS_end), EDSS, EDSS_end);
        EDSS_previous <- c(NA, rev(rev(EDSS_current)[-1]));
        EDSS_relapse <- ifelse(
          !is.na(EDSS_end) & EDSS_end > EDSS_init,
          EDSS_end, EDSS_init
          );
        EDSS_delta  <- ifelse(
          test = is.na(EDSS_end),
          yes  = EDSS,
          no   = EDSS_relapse
          );
        EDSS_delta_baseline <- EDSS_delta - EDSS[1];
        EDSS_delta <- EDSS_delta - EDSS_previous;
      }
      );

    if(FALSE) cat('\n data focus:\n');
    if(FALSE) print(z)
    z <- subset(z, date_obs >= y$eval_start[i]);
    z <- subset(z, EDSS_delta_baseline >= 1);

    y$dEDSS_1st[i] <- ifelse(nrow(z) > 0, as.character(min(z$date_obs)), NA);


    ###}}} -- vim fold

    ###}}} -- vim fold
  }

  y$dEDSS_ft  <- as.numeric(as.Date(y$dEDSS_1st) - as.Date(y$presc_start)); 

  ###}}} -- vim fold
  #[FC 190226 13:34]# y: MRI {{{

  y$mri_t2_n <- NA;   # number of newT2
  y$mri_gd_n <- NA;   # number of newT2
  y$mri_t2gd_n <- NA; 

  y$mri_t2_1st <- NA;   # number of newT2
  y$mri_gd_1st <- NA;   # number of newT2
  y$mri_t2gd_1st <- NA; 

  for(i in 1:nrow(y)){

    #[FC/190226/13:21]# z: MRI aggregate {{{

    z <- subset(x, 
      x$date_obs   >  y$obs_start[i] &
        x$date_obs >  y$eval_start[i] &
        x$date_obs <= y$obs_end[i] &
        !is.na(x$date_mri)
      )[c("date_obs", "date_mri", "n_new_T2", "new_change_gd")];

    if(nrow(z)> 0){

      z <- aggregate(
        cbind(n_new_T2, new_change_gd) ~ date_mri,
        data = z, max, na.rm = TRUE
        );

      ###}}} -- vim fold
      #[FC/190226/14:03]# mri summarising {{{

      y$mri_t2_n[i] <- sum(z$n_new_T2);
      y$mri_gd_n[i] <- sum(z$new_change_gd);
      y$mri_t2gd_n[i] <- sum(c(z$n_new_T2, z$new_change_gd));

      y$mri_t2_1st[i] <- ifelse(
        test = any(z$n_new_T2 > 0),
        yes  = min(z$date_mri[z$n_new_T2 > 0]),
        no   = NA
        );
      y$mri_gd_1st[i] <- ifelse(
        test = any(z$new_change_gd > 0),
        yes  = min(z$date_mri[z$new_change_gd > 0]),
        no   = NA
        );
      y$mri_t2gd_1st[i] <- ifelse(
        test = any(!is.na(c(y$mri_t2_1st[i], y$mri_gd_1st[i]))),
        yes  = min(c(y$mri_t2_1st[i], y$mri_gd_1st[i]), na.rm = TRUE),
        no   = NA
        );

      ###}}} -- vim fold
      if(verbose) cat('\n data focus:\n');
      if(verbose) print(z)
      if(verbose) print(nrow(z))

    }

    y$mri_t2_ft   <- as.numeric( as.Date(y$mri_t2_1st) - as.Date(y$presc_start)); 
    y$mri_gd_ft   <- as.numeric( as.Date(y$mri_gd_1st) - as.Date(y$presc_start)); 
    y$mri_t2gd_ft <- as.numeric( as.Date(y$mri_t2gd_1st) - as.Date(y$presc_start)); 

  }

  ###}}} -- vim fold
  #[FC/190315/10:38]# Echo prescription synthesis (verbose condition) {{{

  if(FALSE) cat('\n\n==> Prescription synthesis:\n');
  if(FALSE) print(y[c(
      "prog_id",
      # "patient_id", "prog_start", "treatment_line", "prog_end",
      "prescription", "presc_start", "eval_start",
      "obs_start", "presc_end", "obs_end", "obs_number", "obs_days",
      "EDSS_start", "EDSS_end", "dEDSS_1st", "dEDSS_ft"
      # "obs_years", "obs_6months",
      # "presc_years", "presc_6months",
      # "switch_from",#,
      # "mri_t2_n", "mri_gd_n", "mri_t2gd_n",
      # "mri_t2_1st", "mri_gd_1st", "mri_t2gd_1st"
      )]);

  ###}}} -- vim fold
  #[FC/190130/08:41]# return {{{

  return(y);

  ###}}} -- vim fold

  ###}}} -- vim fold
};

numCores <- detectCores()
#   results <- lapply(RRMS$presc_raw, FUN = PrescSynth, verbose = FALSE);
# system.time({ results <- lapply(RRMS$presc_raw, FUN = PrescSynth) })
#   results <- mclapply(RRMS$presc_raw, FUN = PrescSynth, mc.cores = 1,
#     verbose = TRUE);
# 
results <- mclapply(RRMS$presc_raw, FUN = PrescSynth, mc.cores = numCores);
RRMS$presc_raw <- do.call(rbind, results);
RRMS$presc_raw <- RRMS$presc_raw[
  with(RRMS$presc_raw, order(patient_id, presc_start)),
  ];
rownames(RRMS$presc_raw) <- 1:nrow(RRMS$presc_raw);

###}}} -- vim fold
#[FC/190130/11:26]# by: RRMS$presc_raw ~ patient_id {{{

# 1/ list per patient
RRMS$presc_raw <- by(
  data = RRMS$presc_raw, INDICES = list(RRMS$presc_raw$patient_id),
  function(x) x
  );

# 2/ function to apply to every patient
PrescPatient <- function(x, verbose = FALSE){
  #[FC/190225/13:28]# {{{
  if(verbose) cat("\n===>", unique(x$patient_id), "===\n");
  if(verbose) print(
    subset(RRMS$raw_df, patient_id == unique(x$patient_id))[c(
      "prog_id", "start_date", "date_obs", "next_drug", "medication_type",
      "drug_switch"
      )]
    );

  I_df <- subset(x, as.character(treatment_line) == "I")
  I_df <- I_df[order(I_df$presc_start),]
  x$naive_I_presc <- I_df$prescription[1];
  x$naive_I_date <- I_df$presc_start[1];

  II_df <- subset(x, as.character(treatment_line) == "II")
  II_df <- II_df[order(II_df$presc_start),]
  x$naive_II_presc <- II_df$prescription[1];
  x$naive_II_date <- II_df$presc_start[1];

  presc_list <- c(NA, x$prescription);

  x$naive <- ifelse(
    test = x$treatment_line == "I",
    yes  =
      x$presc_start == x$naive_I_date & x$prescription == x$naive_I_presc,
    no   =
      x$presc_start == x$naive_II_date & x$prescription == x$naive_II_presc
    );

  x$switch_from <- ifelse(x$naive, "naive", x$switch_from);

  x$birthyear <- min(subset(
      RRMS$raw_df, patient_id == unique(x$patient_id)
      )$patient_birthyear)     
  x$sex <- unique(subset(
      RRMS$raw_df, patient_id == unique(x$patient_id)
      )$sex);     
  x$symptom_date <- min(subset(
      RRMS$raw_df, patient_id == unique(x$patient_id)
      )$first_symptom_date)     
  x$diagnosis_date <- min(subset(
      RRMS$raw_df, patient_id == unique(x$patient_id)
      )$diagnosis_date)     
  x$first_presc_date <- min(x$presc_start);

  x$sympt_diag_days <- as.numeric(
    as.Date(x$diagnosis_date) - as.Date(x$symptom_date)
    );
  x$diag_firstpresc_days <- as.numeric(
    as.Date(x$first_presc_date) - as.Date(x$diagnosis_date)
    );

  x$presc_start_age <- as.numeric(
    format(as.Date(x$presc_start), '%Y')) - x$birthyear;


  if(verbose) cat("\n~~~Result:\n");
  if(verbose) print(x[c("prog_id", "switch_from", "prescription")]);

  return(x);

  ###}}} -- vim fold
}

# 3/ application
results <- mclapply(RRMS$presc_raw, FUN = PrescPatient, mc.cores = numCores);

# 4/ Tidy ~ Misc
RRMS$presc_raw <- do.call(rbind, results);
RRMS$presc_raw <- RRMS$presc_raw[
  with(RRMS$presc_raw, order(patient_id, presc_start)),
  ];
rownames(RRMS$presc_raw) <- 1:nrow(RRMS$presc_raw);

###}}} -- vim fold

###}}} -- vim fold

gc();gc(reset=TRUE)

#[FC/190212/14:04]# RRMS$Obs_12m {{{
# 1/ list by prescription

RRMS$Obs_12m <- by(
  data = RRMS$presc_raw,
  INDICES = list(RRMS$presc_raw$presc_id),
  FUN = function(x) x
  );

# 2/ Function to apply to each prescription
PrescObs12m <- function(x, verbose = FALSE){
  #[FC/190213/10:49]# {{{

  # [FC 190213 10:18] expected: 1 row per x, however, some discrepancy were
  # not ruled out, this step makes sure that only 1 row pass the next steps
  # otherwise the prescription is not valid
  #[FC/190213/08:42]# subset of prescription data [y] {{{

  y <- x[c("prog_id", "prescription", "patient_id", "prog_start", "switch_from",
    "treatment_line", "presc_start", "presc_end", "obs_start", "obs_end",
    "presc_id", "switch_from_id")];
  if(verbose) cat(
    '\n\n', rep('_', 80), '\n\n',
    '\tprogram: ', unique(y$prog_id), ' | ', unique(y$prescription), ' | ',
    unique(y$presc_start), '\n', rep('~', 50), '\n', sep = ''
    );
  if(verbose) print(y);
  y <- subset(
    y,
    (
      ( as.numeric(as.Date(presc_end) - as.Date(presc_start)) > 14 ) |
        is.na(presc_end)
      ) & !is.na(obs_start)
    );
  if(verbose) print(y);

  ###}}} -- vim fold
  if(nrow(y) > 0){

    # [FC 190213 10:20] given a presc start, clinical events are associated
    # to that medication only after 6 months. Concretely, I have defined
    # eval_start and eval_end as limits to include an observation in
    # the clinical evaluation of that prescription
    #[FC/190213/10:22]# within y: eval_start/eval_end {{{

    # determination if evaluation start date - 6months after prescription start
    y$eval_start <- as.character(as.Date(y$presc_start) + round(365.25/12 * 6));

    # determination if evaluation end date - 6months after the end start
    y$eval_end <- ifelse(
      test = is.na(y$presc_end),
      yes  = y$obs_end,
      no   = as.character(as.Date(y$presc_end) + round(365.25/12 * 6))
      );

    ###}}} -- vim fold

    # [FC 190213 10:22] for each program we need to get the full set of
    # of obs record to estimate correctly the n-1 EDSS evaluation.
    # EDSS is part of the clinical evaluation, especially any positive
    # evolution is sign for disease worsening
    #[FC/190213/10:26]# obs_raw at the prescription level (for EDSS) {{{
    obs_raw <- subset(dta_presc, prog_id == y$prog_id)[c(
      "prog_id", "date_obs", "date_mri", "n_new_T2", "new_change_gd",
      "EDSS", "date_relapse", "EDSS_init", "EDSS_end")];
    obs_raw <- obs_raw[with(obs_raw, order(date_obs, date_relapse)),];
    obs_raw <- within(
      data = obs_raw,
      expr = {
        EDSS_current  <- ifelse(is.na(EDSS_end), EDSS, EDSS_end);
        EDSS_previous <- c(NA, rev(rev(EDSS_current)[-1]));
        EDSS_relapse <- ifelse(
          !is.na(EDSS_end) & EDSS_end > EDSS_init,
          EDSS_end, EDSS_init
          );

        EDSS_delta  <- ifelse(
          test = is.na(EDSS_end),
          yes  = EDSS,
          no   = EDSS_relapse
          );
        # EDSS_delta <- EDSS_current - EDSS_previous;
        EDSS_delta <- EDSS_delta - EDSS_previous;
      }
      );

    ###}}} -- vim fold

    if(verbose) cat('\n ==> Prescription\n');
    if(verbose) print(y);

    if(verbose) cat('\n ==> ObsData avail for program\n');
    if(verbose) print(obs_raw);

    # [FC 190213 10:28] After EDSS is available at each date, we refine the
    # obs_raw to the range of data corresponding to the prescription, i.e.
    # included between the obs_start (the first baseline) to the eval_stop
    # date
    #[FC/190213/10:30]# {{{

    obs_raw <- subset(
      obs_raw,
      date_obs >= y$obs_start[1] & date_obs <= y$eval_end[1]
      );

    ###}}} -- vim fold

    if(verbose) cat('\n ==> ObsData avail for presc\n');
    if(verbose) print(obs_raw);

    # [FC 190213 10:31] clinical evaluation is run at each observation date
    # available.
    if(nrow(obs_raw) > 0){
      #[FC/190213/10:46]# {{{

      # [FC 190213 10:34] I have prepared obs_clin: each line corresponds
      # to a obs date (no duplicated line) and estimation if prescription
      # years at the time of the observation
      #[FC/190213/10:33]# obs_clin prepation {{{

      obs_clin  <- merge(
        y, data.frame(
          obs_date = unique(obs_raw$date_obs),
          eval_start = y$eval_start,
          eval_end   = y$eval_end,
          EDSS_current = NA,
          obs_12m  = NA,
          dT2_12m  = NA,
          dGd_12m  = NA,
          n_rlps_12m = NA,
          n_rlps_d1_12m = NA
        )
        );


      if(verbose) cat('\n ==> ObsClin Prepa\n');
      if(verbose) print(obs_clin);

      obs_clin <- within(
        data = obs_clin,
        expr = {
          presc_years <- round(
            as.numeric(as.Date(obs_date) - as.Date(presc_start))/365.25, 2
            );
        }
        );

      ###}}} -- vim fold

      for(i in 1:nrow(obs_clin)){
        #[FC/190213/10:36]# clinical evaluation at each obs date {{{

        # [FC 190213 10:37] evaluation is based on 12months span, I detect
        # the 12month low boundary date (may not exists then not evaluated
        # [FC/190213/10:38]# [z] {{{

        z <- as.numeric(obs_clin$obs_date[i] - obs_raw$date_obs);
        names(z) <- obs_raw$date_obs;
        z <- z[z > (0.75 * 365.25) & z < (1.25 * 365.25)];

        ###}}} -- vim fold
        if(length(z) > 0){
          #[FC/190213/10:40]# obs_clin$obs_12m[i] {{{

          z <- z-365.25;
          obs_clin$obs_12m[i] <- names(which.min(z))

          ###}}} -- vim fold
          # [FC 190213 10:41] Once the lo boundary as been detected, I select
          # within the row obs date, the included obs record
          # obs record should be before or equal to the observation date,
          # + we go back in time to 12m obs date identified (z) (excluded)
          # + and we make sure the obs_date is valid for evaluation
          # (eval_start/eval_end)
          #[FC/190213/10:43]# [a] {{{

          a <- subset(
            obs_raw,
            date_obs <= obs_clin$obs_date[i] &
              date_obs > obs_clin$obs_12m[i] &
              date_obs >= obs_clin$eval_start[i] &
              date_obs <= obs_clin$eval_end[i]
            );

          ###}}} -- vim fold
          # [FC 190213 10:43] Then I have every thing to evaluate the change
          # over a 12 month period of T2/Gd+/EDSS/relapses
          #              #[FC/190213/10:44]# Clin Eval {{{
          # 
          #               obs_clin$dT2_12m[i] <- sum(a$n_new_T2);
          #               obs_clin$dGd_12m[i] <- sum(a$new_change_gd);
          #               obs_clin$n_rlps_12m[i] <- nrow(
          #                 subset(a, !is.na(date_relapse) &
          #                   date_relapse >= obs_clin$eval_start[i])
          #                 );
          #               obs_clin$n_rlps_d1_12m[i] <- nrow(
          #                 subset(a, !is.na(date_relapse) & EDSS_delta >= 1 &
          #                   date_relapse >= obs_clin$eval_start[i])
          #                 )
          # 
          #               if(verbose) cat(
          #                 '\n ==> for obs',
          #                 as.character(obs_clin$obs_date[i]), ' since ',
          #                 as.character(obs_clin$obs_12m[i]), '(presc start ',
          #                 as.character(obs_clin$presc_start[i]), 'eval val.',
          #                 as.character(obs_clin$eval_start[i]), ')\n');
          #               if(verbose) print(a);
          # 
          #               ###}}} -- vim fold
          #[FC/190213/10:44]# Clin Eval {{{

          a$date_mri[is.na(a$date_mri)] <- "1900-01-01";
          t2gd <- aggregate(
            cbind(n_new_T2, new_change_gd) ~ date_obs + date_mri,
            data = a, max, na.rm = TRUE
            );
          obs_clin$dT2_12m[i] <- sum(t2gd$n_new_T2);
          obs_clin$dGd_12m[i] <- sum(t2gd$new_change_gd);
          obs_clin$n_rlps_12m[i] <- nrow(
            subset(a, !is.na(date_relapse) &
              date_relapse >= obs_clin$eval_start[i])
            );
          obs_clin$n_rlps_d1_12m[i] <- nrow(
            subset(a, !is.na(date_relapse) & EDSS_delta >= 1 &
              date_relapse >= obs_clin$eval_start[i])
          )
          obs_clin$EDSS_current[i] <- unique(a$EDSS_current[which.max(a$date_obs)])

          if(verbose) cat(
            '\n ==> for obs',
            as.character(obs_clin$obs_date[i]), ' since ',
            as.character(obs_clin$obs_12m[i]), '(presc start ',
            as.character(obs_clin$presc_start[i]), 'eval val.',
            as.character(obs_clin$eval_start[i]), ')\n');
          if(verbose) print(a);

          ###}}} -- vim fold
        }      

        ###}}} -- vim fold
      };

      if(verbose) cat('\n ==> ObsClin\n');
      if(verbose) print(obs_clin[c(
          "prog_id", "prescription",
          "presc_start","eval_start","obs_start", 
          # "presc_end", "eval_end", "obs_end",
          "obs_date", "presc_years", #"obs_12m",
          "dT2_12m", "dGd_12m", "n_rlps_12m", "n_rlps_d1_12m"
          )]);

      # half of the obs_clin are actually missing clinical eval, I remove
      # every obs_clin without corresponding obs_12m
      obs_clin <- subset(obs_clin, !is.na(obs_12m));

      #         #OLD# [FC/190225/14:09]# Severity {{{
      # 
      #         obs_clin$Cat <- with(
      #           data =  obs_clin, dplyr::case_when(
      #             (dT2_12m >=3 | dGd_12m >= 2) & (n_rlps_12m >= 2 & n_rlps_d1_12m >= 2) ~ "D",
      #             (dT2_12m >=2 | dGd_12m >= 1) & (n_rlps_12m >= 2 & n_rlps_d1_12m >= 2) ~ "C",
      #             (dT2_12m >=2 | dGd_12m >= 1) & (n_rlps_d1_12m >= 1) ~ "B",
      #             (dT2_12m >=1 | dGd_12m >= 1) & (n_rlps_12m >= 1) ~ "A",
      #             TRUE ~ as.character(0)
      #             )
      #           );
      # 
      #         ###}}} -- vim fold
      #[FC 190722 12:14]# Severity {{{

      obs_clin$Cat <- with(
        data =  obs_clin, dplyr::case_when(
          (dT2_12m >=3 | dGd_12m >= 2) & (n_rlps_12m >= 2 & n_rlps_d1_12m >= 2) ~ "D",
          (dT2_12m >=2 | dGd_12m >= 1) & (n_rlps_12m >= 2 & n_rlps_d1_12m >= 2) ~ "C",
          (dT2_12m >=2 | dGd_12m >= 1) & (n_rlps_d1_12m >= 1) ~ "B",
          (dT2_12m >=1 | dGd_12m >= 1) & (n_rlps_12m >= 1) ~ "A",
          TRUE ~ as.character(0)
        )
        );

      ###}}} -- vim fold
      #[FC/190225/14:09]# Time Cat {{{

      obs_clin$years_cat <- cut(
        obs_clin$presc_years,
        breaks = c(0.74, 1.25, 1.74, 2.25, 2.74, 3.25, 3.74, 4.25, 20)
        );
      obs_clin$presc_months <- obs_clin$presc_years * 12;
      obs_clin$presc_months_cat <- cut(
        obs_clin$presc_months,
        breaks = c(
          8.88, 15.00,
          20.88, 27.00,
          32.88, 39.00,
          44.88, 51.00,
          56.88, 63.00,
          200.00
          ),
        labels = c(
          "12", "18", "24", "30", "36", "42", "48", "54", "60", "63+"
        )
        );

      ###}}} -- vim fold

      if(verbose) cat('\n ==> ObsClin (curated) \n');
      if(verbose) print(obs_clin[c(
          "prog_id", "prescription",
          "presc_start","eval_start","obs_start", 
          # "presc_end", "eval_end", "obs_end",
          "obs_date", "presc_years", #"obs_12m",
          "dT2_12m", "dGd_12m", "n_rlps_12m", "n_rlps_d1_12m"
          )]);
      if(verbose) cat('\n\n');

      return(obs_clin);

    }else{

      if(verbose) cat('\nNo corresponding obs_raw to the prescription\n');
      return(NULL)

      ###}}} -- vim fold
    }

  }else{

    if(verbose) cat('\nNo valid prescription\n');
    if(verbose) cat('\n\n');
    return(NULL);
  }

  ###}}} -- vim fold
}

# 3/ Application
results <- mclapply(RRMS$Obs_12m, FUN = PrescObs12m, mc.cores = numCores);

# 4/ Tidy
RRMS$Obs_12m <- do.call(rbind, results)
RRMS$Obs_12m <- RRMS$Obs_12m[
  with(RRMS$Obs_12m, order(prog_id, presc_start, prescription, obs_date)),
  ];
rownames(RRMS$Obs_12m) <- 1:nrow(RRMS$Obs_12m);

###}}} -- vim fold

#[FC/190225/17:38]# RRMS$Obs12mEss {{{

# 1/ input as a list
RRMS$Obs_12m$IdObsMonths <- with(
  RRMS$Obs_12m, paste(prog_id, presc_start, presc_months_cat)
  ); 
inputList <- by(
  data = RRMS$Obs_12m, INDICES = list(RRMS$Obs_12m$IdObsMonths),
  function(x) x
  );

# 2/ function
clean12m <- function(x){
  if(nrow(x) > 1 & unique(x$presc_months_cat) != "63+"){
    dif <- abs(as.numeric(as.character(x$presc_months_cat)) - x$presc_months);
    x <- x[which.min(dif),];
  }else{}
  return(x);
}

# 3/ parallel application
RRMS$Obs12mEss <- do.call(rbind,
  mclapply(inputList, FUN = clean12m, mc.cores = numCores)
  );

###}}} -- vim fold

###}}} -- vim fold

## @knitr data_ADPIRA ----------------------------------------------------------
#[FC/191113/14:04]# ADPIRA {{{

ADPIRA <- list()
#[FC/191113/14:26]# disCtrl {{{

# Select from uninterrupted follow-up data.
Obs12mEss <- RRMS$Obs12mEss[
  with(RRMS$Obs12mEss, order(prog_id, presc_start, presc_months)),
  ]
dim(Obs12mEss)

# Restrict to study time point.
Obs12mEss <- subset(
  x = Obs12mEss,
  presc_months_cat %in% c("12", "24", "36", "48", "60"),
  select = c(
    presc_id, treatment_line, patient_id,
    prescription, presc_start, presc_months_cat, EDSS_current,
    n_rlps_12m, n_rlps_d1_12m, dT2_12m, dGd_12m
  )
)
dim(Obs12mEss)

# Add 2 helper variables.
Obs12mEss <- within(
    data = Obs12mEss,
    expr = {
      dMRI_12m <- dT2_12m + dGd_12m;
      presc_months_cat <- as.numeric(as.character(presc_months_cat))
    }
    )
dim(Obs12mEss)

# The EDSS start necessary to evaluate disease worsening is added.
disCtrl <- merge(
  x = unique(RRMS$presc_raw[c("presc_id", "EDSS_start")]),
  y = Obs12mEss,
  by = "presc_id"
  )

dim(disCtrl)
# Some [presc_id x presc_months] associated with multiple EDSS start:
# discarded.
not_unique_EDSS_start <- split(
  disCtrl,
  f = interaction(disCtrl$presc_id, disCtrl$presc_months_cat, drop = TRUE)
)
not_unique_EDSS_start <- not_unique_EDSS_start[
  sapply(not_unique_EDSS_start, nrow) > 1
  ]

disCtrl <- subset(
  disCtrl, 
  ! interaction(presc_id, presc_months_cat, drop = TRUE) %in%
    names(not_unique_EDSS_start)
)
dim(disCtrl)

# Some annual EDSS evaluation are missing:
# discarded.
disCtrl <- subset(disCtrl, !is.na(EDSS_current));
dim(disCtrl)

# Finally, comparing to baseline, we evaluate if the disease is worse,
#  if any relapse happened, or if any sign of brain MRI activity.
disCtrl <- within(
  disCtrl,
  {
    mri_activity <- ( dT2_12m + dGd_12m ) > 0
    relapse_activity <-  n_rlps_12m > 0
    worse <- ifelse(
      test = EDSS_start < 1,
      yes  = (EDSS_current - EDSS_start) >= 1.5 ,
      no   = ifelse(
        test = EDSS_start < 6,
        yes  = (EDSS_current - EDSS_start) >= 1,
        no   = (EDSS_current - EDSS_start) >= .5
      )
    )
  }
  );
dim(disCtrl)
# [1] 16112    16

###}}} -- vim fold
#[FC/191113/14:26]# ADPIRA$R0 // No rlps {{{

tmp12 <- subset(
  x = disCtrl,
  subset = presc_months_cat == 12 &  n_rlps_12m == 0
  );
tmp24 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 24 &  n_rlps_12m == 0 & presc_id %in% tmp12$presc_id
  );
tmp36 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 36 &  n_rlps_12m == 0 & presc_id %in% tmp24$presc_id
  );
tmp48 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 48 &  n_rlps_12m == 0 & presc_id %in% tmp36$presc_id
  );
tmp60 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 60 &  n_rlps_12m == 0 & presc_id %in% tmp48$presc_id
  );

ADPIRA$R0 <- norlp <- cbind(
  data.frame(ARM = "R0: no relapse", ARMCD = "R0"),
  rbind(tmp12, tmp24, tmp36, tmp48, tmp60)
)

###}}} -- vim fold
#[FC/191113/14:26]# ADPIRA$R1 // onerlp {{{

tmp12 <- subset(
  x = disCtrl,
  subset = presc_months_cat == 12 &  n_rlps_12m > 0
  );
tmp24 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 24 &  n_rlps_12m == 0 & presc_id %in% tmp12$presc_id
  );
tmp36 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 36 &  n_rlps_12m == 0 & presc_id %in% tmp24$presc_id
  );
tmp48 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 48 &  n_rlps_12m == 0 & presc_id %in% tmp36$presc_id
  );
tmp60 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 60 &  n_rlps_12m == 0 & presc_id %in% tmp48$presc_id
  );

ADPIRA$R1 <- onerlp <- cbind(
  data.frame(ARM = "R1: relapse first year", ARMCD = "R1"),
  rbind(tmp12, tmp24, tmp36, tmp48, tmp60)
)

###}}} -- vim fold
#[FC/191113/14:26]# ADPIRA$`R1+` // onebadrlp {{{

tmp12 <- subset(
  x = disCtrl,
  subset = presc_months_cat == 12 &  n_rlps_d1_12m > 0
  )
tmp24 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 24 &  n_rlps_12m == 0 & presc_id %in% tmp12$presc_id
  )
tmp36 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 36 &  n_rlps_12m == 0 & presc_id %in% tmp24$presc_id
  )
tmp48 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 48 &  n_rlps_12m == 0 & presc_id %in% tmp36$presc_id
  )
tmp60 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 60 &  n_rlps_12m == 0 & presc_id %in% tmp48$presc_id
  )

ADPIRA$`R1+` <- onebadrlp <- cbind(
  data.frame(ARM = "R1+: intense relapse first year", ARMCD = "R1+"),
  rbind(tmp12, tmp24, tmp36, tmp48, tmp60)
)

###}}} -- vim fold
#[FC/191113/14:26]# ADPIRA$`R1-` // onelowrlp {{{

tmp12 <- subset(
  x = disCtrl,
  subset = presc_months_cat == 12 & n_rlps_12m > 0 & n_rlps_d1_12m == 0
  );
tmp24 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 24 &  n_rlps_12m == 0 & presc_id %in% tmp12$presc_id
  );
tmp36 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 36 &  n_rlps_12m == 0 & presc_id %in% tmp24$presc_id
  );
tmp48 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 48 &  n_rlps_12m == 0 & presc_id %in% tmp36$presc_id
  );
tmp60 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 60 &  n_rlps_12m == 0 & presc_id %in% tmp48$presc_id
  );

ADPIRA$`R1-` <- onelowrlp <-  cbind(
  data.frame(ARM = "R1-: moderate relapse first year", ARMCD = "R1-"),
  rbind(tmp12, tmp24, tmp36, tmp48, tmp60)
)

###}}} -- vim fold
#[FC/191113/14:26]# ADPIRA$`R2` // onerlp {{{

tmp12 <- subset(
  x = disCtrl,
  subset = presc_months_cat == 12 &  n_rlps_12m == 0
  );
tmp24 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 24 &  n_rlps_12m > 0 & presc_id %in% tmp12$presc_id
  );
tmp36 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 36 &  n_rlps_12m == 0 & presc_id %in% tmp24$presc_id
  );
tmp48 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 48 &  n_rlps_12m == 0 & presc_id %in% tmp36$presc_id
  );
tmp60 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 60 &  n_rlps_12m == 0 & presc_id %in% tmp48$presc_id
  );


ADPIRA$`R2` <- onerlp <-cbind(
  data.frame(ARM = "R2: relapse second year", ARMCD = "R2"),
  rbind(tmp12, tmp24, tmp36, tmp48, tmp60)
 )

###}}} -- vim fold
#[FC 200622 07:28]# ADPIRA$R0_MRI0 // No rlps no MRI activity {{{

tmp12 <- subset(
  x = disCtrl,
  subset = presc_months_cat == 12 & !relapse_activity & !mri_activity
  );
tmp24 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 24 & presc_id %in% tmp12$presc_id &
    !relapse_activity & !mri_activity
  );
tmp36 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 36 & presc_id %in% tmp24$presc_id &
    !relapse_activity & !mri_activity
  );
tmp48 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 48 & presc_id %in% tmp36$presc_id &
    !relapse_activity & !mri_activity
  );
tmp60 <- subset(
  x = disCtrl,
  subset =
    presc_months_cat == 60 & presc_id %in% tmp48$presc_id &
    !relapse_activity & !mri_activity
  );

ADPIRA$`R0+MRI0` <- pirma <-  cbind(
  data.frame(ARM = "R0+MRI0: no relapse, no MRI activity", ARMCD = "R0+MRI0"),
  unique(rbind(tmp12, tmp24, tmp36, tmp48, tmp60))
  )

###}}} -- vim fold

ADPIRA <- do.call(rbind, ADPIRA)

with(ADPIRA, table(ARMCD, presc_months_cat))
# DMF and PEG prescription were rare, they are excluded.
ADPIRA <- subset(x = ADPIRA, !(prescription %in% c("DMF", "PEG")))


dim(ADPIRA)

#[FC/200721/22:05]# Additional description {{{

add_desc <- unique(
  subset(
    x = RRMS$presc_raw, 
    subset = presc_id %in% ADPIRA$presc_id,
    select = c(
      "presc_id", "switch_from", "naive", "birthyear", "sex",
      "symptom_date", "diagnosis_date", "first_presc_date"
    )
  )
)
add_desc <- split(add_desc, f = add_desc$presc_id)
add_desc <- do.call(
  rbind,
  lapply(
    add_desc, function(x) {
      if (nrow(x) > 1 && length(unique(x$switch_from)) > 1) {
        x$switch_from <- NA
        x[1,]
      } else {
        x 
      }
    }
  )
)

ADPIRA <- merge(x = ADPIRA, y = add_desc, by = "presc_id");

dim(ADPIRA)

with(ADPIRA, tapply(ARMCD, presc_months_cat, length))
###}}} -- vim fold

ADPIRA <- within(
  data = ADPIRA,
  {
  age <- as.numeric(
    substr(presc_start, start = 1, stop = 4)
    ) - birthyear
  }
)


ADPIRA <- ADPIRA[with(ADPIRA, order(ARMCD, presc_months_cat)),]

if(
  all(
  range(
    table(
      with(ADPIRA, interaction(ARMCD, presc_months_cat, presc_id, drop = TRUE))
    )
    ) != 1
)
  ) stop("ADPIRA preparation: the key is not unique.")



# Dataset level metadata
attr(ADPIRA, "metadata") <- t(data.frame(
    name = "Analysis data set for PIRA study.",
    date = Sys.Date(),
    Authorisationa = "Data shared in accordance with participants' consent",
    readFile = "File Encoding UTF8, ';' as separator and dots for decimals",
    key  = paste0(
      "One record per patient, drug, prescription start date, study Arm,",
      " visit."
      ),
    content = "Data corresponding to the analysis reported in Kapica et al. 2020"
    ))

# Column level metadata
ADPIRA <- within(
  data = ADPIRA,
  expr = {
    attr(presc_id, "label") <-  "Unique prescription identifier (stat. ind.)."
    attr(patient_id, "label") <-  "Unique patient identifier."
    attr(prescription, "label") <-  paste0(
      "Drug prescribed, one of Interferon beta (INF), Glatiramer Acetate (GA),",
      " Fingolimod (FTY), Natalizumab (NAT)."
    )
    attr(presc_start, "label") <-  "Prescription start date (ISO 8601)."
    attr(treatment_line, "label") <-  "Treatment line."
    attr(ARM, "label") <-  "Arm of the study, the prescription panel."
    attr(ARMCD, "label") <-  "Short code for the study Arm."
    attr(EDSS_start, "label") <- "EDSS at prescription start."
    attr(presc_months_cat, "label") <- paste0(
      "Monitoring time point in months since prescription start",
     " (12, 24, 36, 48, 60 months)"
      )
   attr(EDSS_current, "label") <- "EDSS at the time of the visit."
  attr(n_rlps_12m, "label") <-  "Number of confirmed relapse within 12 preceding months."
  attr(n_rlps_d1_12m, "label") <-  "Number of confirmed relapse within 12 months associated with maintained EDSS progression of at least 1 point."
  attr(dT2_12m, "label") <-  "Brain MRI activity: number of new T2 lesion detected within 12 months preceeding the visit."
  attr(dGd_12m, "label") <-  "Brain MRI activity: number of new Gd+ lesion detected within 12 months preceeding the visit."
  attr(dMRI_12m, "label") <-  "Brain MRI activity: number of new T2 or Gd+ lesion detected within 12 months preceeding the visit."
  attr(worse, "label") <-  "The disease was worse than at baseline (TRUE/FALSE)."
  attr(relapse_activity, "label") <-  "Relapse activity was detected (TRUE/FALSE)."
  attr(mri_activity, "label") <-  "Brain MRI activity was detected (TRUE/FALSE)."
  attr(switch_from, "label") <-  "The previous prescription, 'naive' if first prescription, '1st in prog' if not naive and new program.
  "
  attr(naive, "label") <-  "Naive prescription (TRUE/FALSE)"
  attr(birthyear, "label") <-  "Birthyear"
  attr(sex, "label") <-  "Sex."
  attr(symptom_date, "label") <-  "Date of first symptoms (ISO 8601)."
  attr(diagnosis_date, "label") <-  "Date of diagnosis (ISO 8601)."
  attr(first_presc_date, "label") <-  "Date of first prescription (ISO 8601)."

  }
)

###}}} -- vim fold

ADPIRA$AgeSympt <- as.numeric(substr(ADPIRA$symptom_date, start = 1, stop = 4)) - ADPIRA$birthyear
ADPIRA$Sympt2Presc <- as.numeric(as.Date(ADPIRA$presc_start) - as.Date(ADPIRA$symptom_date))

usethis::use_data(ADPIRA, overwrite = TRUE)

# [modeline]: # ( vim: set foldlevel=0: )
## nolint end ----source("inst/r_chunk/data_adpira.R")
