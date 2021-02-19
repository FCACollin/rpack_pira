#==============================================================================#
#
# title:    RRMS and PIRA
# encoding: "utf-8"
# author:   "Francois C A COLLIN, Ph.D."
# date:     200120
# Note:     vim-user friendly script
#
#==============================================================================#

## @knitr setup ----

library(RPack.PIRA)
library(ggplot2);
library(parallel);
library(emmeans)
library(multcomp)
numCores <- detectCores()

input <- list(
  ggTheme = c(
    "flat",      "flat dark", "camoflauge", "chalk", "copper",
    "dust",      "earth",     "fresh",      "grape", "grass",
    "greyscale", "light",      "lilac",     "pale",  "sea", 
    "sky",        "solarized", "dust"
    )[8]
  )

ggthemr::ggthemr(input$ggTheme) 

## @knitr TODO

# remove all data which could identify a person.

head(ADPIRA)
ADPIRA$AgeSympt <- as.numeric(substr(ADPIRA$symptom_date, start = 1, stop = 4)) - ADPIRA$birthyear
ADPIRA$Sympt2Presc <- as.numeric(as.Date(ADPIRA$presc_start) - as.Date(ADPIRA$symptom_date))

## @knitr table_01 ----

X <- split(ADPIRA, f = list(ADPIRA$presc_months_cat, ADPIRA$ARM))

tmp2 <- do.call(
  rbind, lapply(
    X, function(x){

      data.frame(
        Arm       = unique(x$ARMCD),
        Month     = unique(x$presc_months),
        n         = nrow(x),
        "F:M"     = round(sum(x$sex == "female")/sum(x$sex == "male"), 2),
        age       = round(median(x$age)),
        AgeSympt  = round(median(x$AgeSympt), 2),
        Sympt2Presc = round(median(x$Sympt2Presc)/365.25, 2),
        rlp_n      = sum(x$n_rlps_12m > 0),
        EDSS_start   = round(median(x$EDSS_start), 1),
        EDSS_current = round(median(x$EDSS_current), 1),
        Worse_n    = sum(x$worse)
      )

    }
  )
)

knitr::kable(
  tmp2,
  caption = paste(
    "Description of the prescription per scenario and month of observation"
    ),
  row.names = FALSE
)


## @knitr fig_01 ----
# EDSS worsening? 

ggthemr::ggthemr(input$ggTheme) 

norlp <- subset(ADPIRA, ARMCD == "R0")
X     <- split(x = norlp, f = norlp$presc_months_cat)
Xline <- split(
  x = norlp, f = with(norlp, paste(presc_months_cat, treatment_line))
)
Xpres <- split(
  x = norlp, f = with(norlp, paste(presc_months_cat, prescription)),
  drop = TRUE
)

FUN <- function(x, drug = FALSE, case = NA, line = FALSE){

  y <- data.frame(
    Time = unique(x$presc_months_cat),
    n    = nrow(x),                    # n cases
    x    = sum(x$worse),                # positive cases
    line = ifelse(line & !drug, as.character(unique(x$treatment_line)), ''),
    drug = ifelse(drug, unique(x$prescription), ''),
    case = case,
    stringsAsFactors = FALSE
  )

  y <- within(
    data = y,
    expr = {
      p      <- x/n;
      halfCI <- 1.96 *sqrt( (p*(1-p))/ n);
      halfCI <- sqrt( (p*(1-p))/ n);
      up     <- ifelse(p + halfCI < 1, p + halfCI, 1);
      lo     <- ifelse(p - halfCI > 0, p - halfCI, 0);
    }
  )

  return(y)

}

dtaplot <- rbind(
  do.call(rbind, lapply(X, FUN, case = "1) All"))
  , do.call(rbind, lapply(Xline, FUN, case = "2) line", line = TRUE))
  , do.call(
    rbind, lapply(Xpres, FUN, case = "3) drug", line = TRUE, drug = TRUE)
  )
)

dtaplot <- subset(dtaplot, (up - lo) < .1)

dtaplot <- rbind(
  dtaplot,
  data.frame(
    Time = c(48, 48, 60, 60, 60, 48, 60),
    n = 0,
    x = NA,
    line = c("", "", "", "", "", "II", "II"),
    drug = c("FTY", "NAT", "FTY", "NAT", "GLA", "", ""),
    case = c("3) drug", "3) drug", "3) drug", "3) drug", "3) drug", "2) line", "2) line"),
    lo = NA,
    up = NA,
    halfCI = NA,
    p = 0.001
  )
)
dtaplot$Colour <- with(
  dtaplot, ifelse(
    case == "1) All", "All", 
    ifelse(case == "2) line", line, drug)
  )
  )
dtaplot$Colour <- factor(
  dtaplot$Colour, levels = c("INF", "GLA", "FTY", "NAT", "I", "II", "All")
)
dtaplot$case <- factor(
  dtaplot$case, levels = c("1) All", "2) line", "3) drug"),
  labels = c("All", "Line", "DMT")
)

gg <- {
  ggplot(
    data = dtaplot,
    mapping = aes(
      x = Time, y = 100 * p,
      size = n,
      colour = Colour,
      fill = Colour,
      label = n,
      ymax = 100 * up, ymin = 100 * lo
    )
    ) + geom_col(
    position = "dodge",
    color = NA, size = 1, width = 9.5,
    alpha = .8
    ) + geom_errorbar(
    size = .5, alpha = 1, width = 2, position = position_dodge(width = 9.5),
    color = "gray45" 
    ) + scale_alpha_continuous(
    range = c(min = .3, max = .9)
    ) + coord_cartesian(
    ylim = c(-1, max(15, dtaplot$up *100, na.rm = TRUE)),
    xlim = c(6, 65)
    ) + geom_text(
    mapping = aes(y = 100*up),
    alpha = 1, size = 2.8, position = position_dodge(width = 9.5),
    vjust = -1.2,
    color = "black",
    fontface = "italic"
    ) + geom_label(
    mapping = aes(label = Colour), y = -.75,
    alpha = 1, size = 2, position = position_dodge(width = 9.5),
    colour = "white"
    ) + scale_x_continuous(
    breaks = c(6, seq(0 , 60, 12))
    ) + ylab(
    expression("Freq. worsen disease (% "%+-%" std. err.)")
    ) + xlab(
    "Time since prescription start (months)"
    ) + facet_grid(
    case~.
    ) + theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.background = element_blank()
    )
}

gg

ggthemr::ggthemr_reset()

## @knitr fig_02 ----

ggthemr::ggthemr(input$ggTheme) 

norlp <- subset(ADPIRA, ARMCD == "R0")
onerlp <- subset(ADPIRA, ARMCD == "R1")
onebadrlp <- subset(ADPIRA, ARMCD == "R1+")
onelowrlp <- subset(ADPIRA, ARMCD == "R1-")

Xnr <- split(x = norlp, f = norlp$presc_months_cat)
Xor <- split(x = onerlp, f = onerlp$presc_months_cat)
Xbr <- split(x = onebadrlp, f = onebadrlp$presc_months_cat)
Xlr <- split(x = onelowrlp, f = onelowrlp$presc_months_cat)

FUN <- function(x, drug = FALSE, case = NA, line = FALSE){

  y <- data.frame(
    Time = unique(x$presc_months_cat),
    n    = nrow(x),                    # n cases
    x    = sum(x$worse),                # positive cases
    line = ifelse(line & !drug, as.character(unique(x$treatment_line)), ''),
    drug = ifelse(drug, unique(x$prescription), ''),
    case = case,
    stringsAsFactors = FALSE
    );

  y <- within(
    data = y,
    expr = {
      p      <- x/n;
      halfCI <- 1.96 *sqrt( (p*(1-p))/ n);
      halfCI <- sqrt( (p*(1-p))/ n);
      up     <- ifelse(p + halfCI < 1, p + halfCI, 1);
      lo     <- ifelse(p - halfCI > 0, p - halfCI, 0);
    }
    );

  return(y);

}

dtaplot <- rbind(
  do.call(rbind, lapply(Xnr, FUN, case = "1) R0"))
  , do.call(rbind, lapply(Xor, FUN, case = "2) R1 (all)"))
  , do.call(rbind, lapply(Xbr, FUN, case = "3) R1+ (severe)"))
  , do.call(rbind, lapply(Xlr, FUN, case = "4) R1- (moderate)"))
  );

dtaplot <- subset(
  dtaplot, 
  (up - lo) < .15  & (up + lo) > 0 & Time %in% seq(12, 36, 12)

)
dtaplot <- rbind(
  dtaplot,
  data.frame(
    Time = c(36, 36),
    n = c(0, 0),
    x = NA,
    line = "",
    drug = "",
    case = c("3) R1+ (severe)", "4) R1- (moderate)"),
    lo = NA,
    up = NA,
    halfCI = NA,
    p = 0.001
    )
  )
dtaplot
gg <- {
  ggplot(
    data = dtaplot,
    mapping = aes(
      x = Time, y = 100*p,
      size = n,
      colour = case,
      fill = case,
      label = n,
      ymax = 100*up, ymin = 100*lo
    )
    ) + geom_col(
    position = "dodge",
    color = NA, size = 1, width = 9.5
    ) + geom_errorbar(
    size = .5, alpha = 1, width = 2, position = position_dodge(width = 9.5),
    color = "gray45" 
    ) + scale_alpha_continuous(
    range = c(min = .3, max = .9)
    ) + coord_cartesian(
    ylim = c(-1, max(15, dtaplot$up *100+3, na.rm = TRUE)),
    xlim = c(6, 41)
    ) + geom_text(
    y = -1,
    alpha = 1, size = 2.8, position = position_dodge(width = 9.5),
    color = "black",
    fontface = "italic"
    ) + scale_x_continuous(
    breaks = c(6, seq(0 , 36, 12))
    ) + ylab(
    expression("Freq. worsen disease (% "%+-%" std. err.)")
    ) + xlab(
    "Time since prescription start (months)"
    ) + theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.background = element_blank()
    )
}

gg
ggthemr::ggthemr_reset()

## @knitr fig_03 ----

#[FC/191113/14:27]# graph EDSS worsening?

ggthemr::ggthemr(input$ggTheme) 

norlp <- subset(ADPIRA, ARMCD == "R0")
onerlp <- subset(ADPIRA, ARMCD == "R1")
oneY2rlp <- subset(ADPIRA, ARMCD == "R2")

Xnr     <- split(x = norlp, f = norlp$presc_months_cat)
Xor     <- split(x = onerlp, f = onerlp$presc_months_cat)
X2r     <- split(x = oneY2rlp, f = oneY2rlp$presc_months_cat)

FUN <- function(x, drug = FALSE, case = NA, line = FALSE){

  y <- data.frame(
    Time = unique(x$presc_months_cat),
    n    = nrow(x),                    # n cases
    x    = sum(x$worse),                # positive cases
    line = ifelse(line & !drug, as.character(unique(x$treatment_line)), ''),
    drug = ifelse(drug, unique(x$prescription), ''),
    case = case,
    stringsAsFactors = FALSE
    );

  y <- within(
    data = y,
    expr = {
      p      <- x/n;
      halfCI <- 1.96 *sqrt( (p*(1-p))/ n);
      halfCI <- sqrt( (p*(1-p))/ n);
      up     <- ifelse(p + halfCI < 1, p + halfCI, 1);
      lo     <- ifelse(p - halfCI > 0, p - halfCI, 0);
    }
    );

  return(y);

}

dtaplot <- rbind(
  do.call(rbind, lapply(Xnr, FUN, case = "1) R0"))
  , do.call(rbind, lapply(Xor, FUN, case = "2) R1 (all)"))
  , do.call(rbind, lapply(X2r, FUN, case = "3) R2"))
  );

dtaplot <- subset(
  dtaplot, 
  (up - lo) < .10  & (up + lo) > 0 & Time %in% seq(12, 36, 12)
)

gg <- {
  ggplot(
    data = dtaplot,
    mapping = aes(
      x = Time, y = 100*p,
      size = n,
      colour = case,
      fill = case,
      label = n,
      ymax = 100*up, ymin = 100*lo
    )
    ) + geom_col(
    position = "dodge",
    color = NA, size = 1, width = 9.5
    ) + geom_errorbar(
    size = .5, alpha = 1, width = 2, position = position_dodge(width = 9.5),
    color = "gray45" 
    ) + scale_alpha_continuous(
    range = c(min = .3, max = .9)
    ) + coord_cartesian(
    ylim = c(-1, max(15, dtaplot$up *100+3, na.rm = TRUE)),
    xlim = c(6, 41)
    ) + geom_text(
    y = -1,
    alpha = 1, size = 2.8, position = position_dodge(width = 9.5),
    color = "black",
    fontface = "italic"
    ) + scale_x_continuous(
    breaks = c(6, seq(0 , 36, 12))
    ) + ylab(
    expression("Freq. worsen disease (% "%+-%" std. err.)")
    ) + xlab(
    "Time since prescription start (months)"
    ) + theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.background = element_blank()
    )
}

gg
ggthemr::ggthemr_reset()

## @knitr fig_04 ----

#[FC/191113/14:27]# graph MRI worsening?

ggthemr::ggthemr(input$ggTheme) 

norlp <- subset(ADPIRA, ARMCD == "R0")
onerlp <- subset(ADPIRA, ARMCD == "R1")
oneY2rlp <- subset(ADPIRA, ARMCD == "R2")

Xnr     <- split(x = norlp, f = norlp$presc_months_cat)
Xor     <- split(x = onerlp, f = onerlp$presc_months_cat)
X2r     <- split(x = oneY2rlp, f = oneY2rlp$presc_months_cat)

FUN <- function(x, drug = FALSE, case = NA, line = FALSE){

  y <- data.frame(
    Time = unique(x$presc_months_cat),
    n    = nrow(x),                    # n cases
    x    = sum(x$dMRI_12m >= 1),                # positive cases
    line = ifelse(line & !drug, as.character(unique(x$treatment_line)), ''),
    drug = ifelse(drug, unique(x$prescription), ''),
    case = case,
    stringsAsFactors = FALSE
    );

  y <- within(
    data = y,
    expr = {
      p      <- x/n;
      halfCI <- 1.96 *sqrt( (p*(1-p))/ n);
      halfCI <- sqrt( (p*(1-p))/ n);
      up     <- ifelse(p + halfCI < 1, p + halfCI, 1);
      lo     <- ifelse(p - halfCI > 0, p - halfCI, 0);
    }
    );

  return(y);

}

dtaplot <- rbind(
  do.call(rbind, lapply(Xnr, FUN, case = "1) R0"))
  , do.call(rbind, lapply(Xor, FUN, case = "2) R1"))
  , do.call(rbind, lapply(X2r, FUN, case = "3) R2"))
  );

dtaplot <- subset(
  dtaplot, 
  (up - lo) < .10  & (up + lo) > 0 & Time %in% seq(12, 36, 12)
)

dtaplot <- rbind(
  dtaplot,
  data.frame(
    Time = 36,
    n = 0,
    x = NA,
    line = "",
    drug = "",
    case = c("2) R1"),
    lo = NA,
    up = NA,
    halfCI = NA,
    p = 0.002
    )
  )

gg <- {
  ggplot(
    data = dtaplot,
    mapping = aes(
      x = Time, y = 100*p,
      size = n,
      colour = case,
      fill = case,
      label = n,
      ymax = 100*up, ymin = 100*lo
    )
    ) + geom_col(
    position = "dodge",
    color = NA, size = 1, width = 9.5
    ) + geom_errorbar(
    size = .5, alpha = 1, width = 2, position = position_dodge(width = 9.5),
    color = "gray45" 
    ) + scale_alpha_continuous(
    range = c(min = .3, max = .9)
    ) + coord_cartesian(
    ylim = c(-1, max(15, dtaplot$up *100+3, na.rm = TRUE)),
    xlim = c(6, 41)
    ) + geom_text(
    y = -1,
    alpha = 1, size = 2.8, position = position_dodge(width = 9.5),
    color = "black",
    fontface = "italic"
    ) + scale_x_continuous(
    breaks = c(6, seq(0 , 36, 12))
    ) + ylab(
    expression("Freq. worsen disease (% "%+-%" std. err.)")
    ) + xlab(
    "Time since prescription start (months)"
    ) + theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.background = element_blank()
    )

}

gg
ggthemr::ggthemr_reset()

## @knitr fig_05 ----
#[FC/200121/14:45]# graph MRI worsening? 

ggthemr::ggthemr(input$ggTheme) 

norlp <- subset(ADPIRA, ARMCD == "R0")

X     <- split(x = norlp, f = norlp$presc_months_cat)
Xline <- split(x = norlp, f = with(norlp, paste(presc_months_cat, treatment_line)))
Xpres <- split(
  x = norlp, f = with(norlp, paste(presc_months_cat, prescription)),
  drop = TRUE
)

FUN <- function(x, drug = FALSE, case = NA, line = FALSE){

  y <- data.frame(
    Time = unique(x$presc_months_cat),
    n    = nrow(x),                            # n cases
    x    = sum(x$dMRI_12m >=1),                # positive cases
    line = ifelse(line & !drug, as.character(unique(x$treatment_line)), ''),
    drug = ifelse(drug, unique(x$prescription), ''),
    case = case,
    stringsAsFactors = FALSE
    );

  y <- within(
    data = y,
    expr = {
      p      <- x/n;
      halfCI <- 1.96 *sqrt( (p*(1-p))/ n);
      halfCI <- sqrt( (p*(1-p))/ n);
      up     <- ifelse(p + halfCI < 1, p + halfCI, 1);
      lo     <- ifelse(p - halfCI > 0, p - halfCI, 0);
    }
    );

  return(y);

}

dtaplot <- rbind(
  do.call(rbind, lapply(X, FUN, case = "1) All"))
  , do.call(rbind, lapply(Xline, FUN, case = "2) line", line = TRUE))
  , do.call(rbind, lapply(Xpres, FUN, case = "3) drug", line = TRUE, drug = TRUE))
)

dtaplot <- subset(
  dtaplot, 
  !(drug %in% c("DMF", "PEG")) 
  & (up - lo) < .1
)

dtaplot <- rbind(
  dtaplot,
  data.frame(
    Time = c(48, 48, 48, 60, 60, 60, 48, 60),
    n = 0,
    x = NA,
    line = c("", "", "", "", "", "", "II", "II"),
    drug = c("GLA", "FTY", "NAT", "FTY", "NAT", "GLA", "", ""),
    case = c(
      "3) drug", "3) drug", "3) drug", "3) drug", "3) drug", "3) drug",
      "2) line", "2) line"
      ),
    lo = NA,
    up = NA,
    halfCI = NA,
    p = 0.001
  )
)

dtaplot$Colour <- with(
  dtaplot, ifelse(
    case == "1) All", "All", 
    ifelse(case == "2) line", line, drug)
  )
)

dtaplot$Colour <- factor(
  dtaplot$Colour, levels = c("INF", "GLA", "FTY", "NAT", "I", "II", "All")
)
dtaplot$case <- factor(
  dtaplot$case, levels = c("1) All", "2) line", "3) drug"),
  labels = c("All", "Line", "DMT")
)


gg <- {
  ggplot(
    data = dtaplot,
    mapping = aes(
      x = Time, y = 100*p,
      size = n,
      colour = Colour,
      fill = Colour,
      label = n,
      ymax = 100*up, ymin = 100*lo
    )
    ) + geom_col(
    position = "dodge",
    color = NA, size = 1, width = 9.5,
    alpha = .8
    ) + geom_errorbar(
    size = .5, alpha = 1, width = 2, position = position_dodge(width = 9.5),
    color = "gray45" 
    ) + scale_alpha_continuous(
    range = c(min = .3, max = .9)
    ) + coord_cartesian(
    ylim = c(-1.5, 32),
    xlim = c(6, 65)
    ) + geom_text(
    mapping = aes(y = 100*up),
    alpha = 1, size = 2.8, position = position_dodge(width = 9.5),
    vjust = -1.2,
    color = "black",
    fontface = "italic"
    ) + geom_label(
    mapping = aes(label = Colour), y = -1.5,
    alpha = 1, size = 2, position = position_dodge(width = 9.5),
    colour = "white"
    ) + scale_x_continuous(
    breaks = c(6, seq(0 , 60, 12))
    ) + ylab(
    "Freq. MRI activity (%, +/- std. err.)"
    ) + xlab(
    "Time since prescription start (months)"
    ) + facet_grid(
    case~.
    ) + theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.background = element_blank()
  )
}

gg

ggthemr::ggthemr_reset()

# @knitr fig_06 ----

ggthemr::ggthemr(input$ggTheme) 
pirma <- subset(ADPIRA, ARMCD == "R0+MRI0")
X     <- split(x = pirma, f = pirma$presc_months_cat)
Xline <- split(x = pirma, f = with(pirma, paste(presc_months_cat, treatment_line)))
Xpres <- split(
  x = pirma, f = with(pirma, paste(presc_months_cat, prescription)),
  drop = TRUE
)

FUN <- function(x, drug = FALSE, case = NA, line = FALSE){

  y <- data.frame(
    Time = unique(x$presc_months_cat),
    n    = nrow(x),                    # n cases
    x    = sum(x$worse),               # positive cases
    line = ifelse(line & !drug, as.character(unique(x$treatment_line)), ''),
    drug = ifelse(drug, unique(x$prescription), ''),
    case = case,
    stringsAsFactors = FALSE
    );

  y <- within(
    data = y,
    expr = {
      p      <- x/n;
      halfCI <- 1.96 *sqrt( (p*(1-p))/ n);
      halfCI <- sqrt( (p*(1-p))/ n);
      up     <- ifelse(p + halfCI < 1, p + halfCI, 1);
      lo     <- ifelse(p - halfCI > 0, p - halfCI, 0);
    }
    );

  return(y);

}

dtaplot <- rbind(
  do.call(rbind, lapply(X, FUN, case = "1) All"))
  , do.call(rbind, lapply(Xline, FUN, case = "2) line", line = TRUE))
  , do.call(rbind, lapply(Xpres, FUN, case = "3) drug", line = TRUE, drug = TRUE))
  );

dtaplot <- subset(
  dtaplot, 
  !(drug %in% c("DMF", "PEG")) 
  & (up - lo) < .1
)

dtaplot <- rbind(
  dtaplot,
  data.frame(
    Time = c(48, 48, 48, 60, 60, 60, 48, 60),
    n = 0,
    x = NA,
    line = c("", "", "", "", "", "", "II", "II"),
    drug = c("GLA", "FTY", "NAT", "FTY", "NAT", "GLA", "", ""),
    case = c(
      "3) drug", "3) drug", "3) drug", "3) drug", "3) drug", "3) drug",
      "2) line", "2) line"
      ),
    lo = NA,
    up = NA,
    halfCI = NA,
    p = 0.0005
  )
)

dtaplot$Colour <- with(
  dtaplot, ifelse(
    case == "1) All", "All", 
    ifelse(case == "2) line", line, drug)
  )
  )

dtaplot$Colour <- factor(
  dtaplot$Colour, levels = c("INF", "GLA", "FTY", "NAT", "I", "II", "All")
)
dtaplot$case <- factor(
  dtaplot$case, levels = c("1) All", "2) line", "3) drug"),
  labels = c("All", "Line", "DMT")
)


gg <- {
  ggplot(
    data = dtaplot,
    mapping = aes(
      x = Time, y = 100*p,
      size = n,
      colour = Colour,
      fill = Colour,
      label = n,
      ymax = 100*up, ymin = 100*lo
    )
    ) + geom_col(
    position = "dodge",
    color = NA, size = 1, width = 9.5,
    alpha = .8
    ) + geom_errorbar(
    size = .5, alpha = 1, width = 2, position = position_dodge(width = 9.5),
    color = "gray45" 
    ) + scale_alpha_continuous(
    range = c(min = .3, max = .9)
    ) + coord_cartesian(
    ylim = c(-.75, 12),
    xlim = c(6, 65)
    ) + geom_text(
    mapping = aes(y = 100*up),
    alpha = 1, size = 2.8, position = position_dodge(width = 9.5),
    vjust = -1.2,
    color = "black",
    fontface = "italic"
    ) + geom_label(
    mapping = aes(label = Colour), y = -.75,
    alpha = 1, size = 2, position = position_dodge(width = 9.5),
    colour = "white"
    ) + scale_x_continuous(
    breaks = c(6, seq(0 , 60, 12))
    ) + ylab(
    expression("Freq. worsen disease (% "%+-%" std. err.)")
    ) + xlab(
    "Time since prescription start (months)"
    ) + facet_grid(
    case~.
    ) + theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.background = element_blank()
    );
}

gg

ggthemr::ggthemr_reset()

## @knitr mod_glm_fit ----------------------------------------------------------

# > "*overdispersion when residual deviance is larger than residual degrees of
# > freedom*" [@Crawley2013]

res_glm <- list()

#[FC/200728/07:18]# R0 {{{

res_glm$R0$data <- with(
  data = subset(ADPIRA, ARMCD == "R0"),
  expr = {
    data.frame(
      presc_id,
      Time = factor(presc_months_cat),
      ARMCD = ARMCD,
      # Time = factor(presc_months_cat, levels = seq(12,60,12), ordered = TRUE),
      # Time = presc_months_cat,
      line = treatment_line,
      drug = prescription,
      sex = sex,
      age = age,
      EDSS = EDSS_start,
      worse
    )
  }
)

res_glm$R0$glm <- glm(
  worse ~ Time + (age + EDSS), data = res_glm$R0$data,
  family = binomial(link = "logit")
)

###}}} -- vim fold
#[FC/200728/07:18]# R1 {{{

res_glm$R1$data <- with(
  data = subset(ADPIRA, ARMCD == "R1" & ! presc_months_cat %in% c(48, 60)),
  expr = {
    data.frame(
      presc_id,
      Time = factor(presc_months_cat),
      ARMCD = ARMCD,
      line = treatment_line,
      drug = prescription,
      sex = sex,
      age = age,
      EDSS = EDSS_start,
      worse
    )
  }
)

res_glm$R1$glm <- glm(
  worse ~ Time * (age + EDSS + line + sex)
  -Time:sex -Time:drug -Time:line -line -Time:age -Time:EDSS -age,
  data = res_glm$R1$data,
  family = binomial(link = "logit")
)

###}}} -- vim fold
#[FC 200807 07:14]# R1+ {{{

res_glm$`R1+`$data <- with(
  data = subset(ADPIRA, ARMCD == "R1+" & ! presc_months_cat %in% c(48, 60)),
  expr = {
    data.frame(
      presc_id,
      Time = factor(presc_months_cat),
      ARMCD = ARMCD,
      line = treatment_line,
      drug = prescription,
      sex = sex,
      age = age,
      EDSS = EDSS_start,
      worse
    )
  }
)

res_glm$`R1+`$glm <- glm(
  worse ~ Time + EDSS + sex,
  #   worse ~ Time * (age + EDSS + line + sex + drug)
  #   -Time:line -Time:EDSS -Time:age -line -Time:drug -drug -age -Time:sex,
  data = res_glm$`R1+`$data,
  family = binomial(link = "logit")
)

###}}} -- vim fold
#[FC 200807 07:14]# R1- {{{

res_glm$`R1-`$data <- with(
  data = subset(ADPIRA, ARMCD == "R1-" & ! presc_months_cat %in% c(36, 48, 60)),
  expr = {
    data.frame(
      presc_id,
      Time = factor(presc_months_cat),
      ARMCD = ARMCD,
      line = treatment_line,
      drug = prescription,
      sex = sex,
      age = age,
      EDSS = EDSS_start,
      worse
    )
  }
)

res_glm$`R1-`$glm <- glm(
  worse ~ Time,
  data = res_glm$`R1-`$data,
  family = binomial(link = "logit")
)

###}}} -- vim fold
#[FC/200728/07:18]# R2 {{{

res_glm$R2$data <- with(
  data = subset(ADPIRA, ARMCD == "R2"),
  expr = {
    data.frame(
      presc_id,
      Time = factor(presc_months_cat),
      ARMCD = ARMCD,
      line = treatment_line,
      drug = prescription,
      sex = sex,
      age = age,
      EDSS = EDSS_start,
      worse
    )
  }
)

res_glm$R2$glm <- glm(
  worse ~ Time * (age + EDSS + drug + sex) -Time:age -Time:line -Time:drug
  -drug -Time:sex -sex -Time:EDSS, 
  data = res_glm$R2$data,
  family = binomial(link = "logit")
)

###}}} -- vim fold
#[FC/200728/07:18]# R0+MRI0 {{{

res_glm$`R0+MRI0`$data <- with(
  data = subset(ADPIRA, ARMCD == "R0+MRI0"),
  expr = {
    data.frame(
      presc_id,
      Time = factor(presc_months_cat),
      ARMCD = ARMCD,
      line = treatment_line,
      drug = prescription,
      sex = sex,
      age = age,
      EDSS = EDSS_start,
      worse
    )
  }
)

res_glm$`R0+MRI0`$glm <- glm(
  worse ~ Time + age + EDSS,
  data = res_glm$`R0+MRI0`$data,
  family = binomial(link = "logit")
)

###}}} -- vim fold

## @knitr mod_glm_devianceTable ----

covariates <- c("sex", "line", "drug", "age", "EDSS")
X <- lapply(res_glm, function(x) x$glm)

devianceTables <- lapply(
  X, function(x) {

    covariates <- covariates[! covariates %in% attr(terms(x), "term.labels")]

    y <- lapply(
      covariates, function(y) {
        effect <- y
        y <- update(x, as.formula(paste(" . ~ . +", y)))
        y <- as.data.frame(anova(x, y, test = "Chisq"))
        rownames(y) <- c(
          paste0("[#] ", paste(attr(terms(x), "term.labels"), collapse = "+"))
          , paste0("# + ", effect))
        y
      })

    unique(do.call(rbind, y))

  }
)

devianceTables <- Map(
  function(tbl, name) {
    tbl <- cbind(
      Arm = name,
      Covariates = rownames(tbl),
      tbl
    )
    rownames(tbl) <- NULL
    tbl
  },
  devianceTables, names(devianceTables)
)

devianceTables <- do.call(rbind, devianceTables)
rownames(devianceTables) <- NULL

knitr::kable(
  devianceTables,
  caption = paste(
    "Candiadate covariates. ",
    "The deviance table compares within each Arm the deviance of ",
    "the model [#] to the model including an additional covariate (#+)."
  )
)

## @knitr mod_glm_effecttests --------------------------------------------------

X <- lapply(res_glm, function(x) x$glm)

effectTest <- lapply(
  X = X,
  FUN = function(x) {
    as.data.frame(broom::tidy(x, exponentiate = TRUE, conf.int = TRUE))
  }
)

effectTest <- Map(
  function(tbl, name) {
    tbl <- cbind(
      Arm = name,
      tbl
    )
    rownames(tbl) <- NULL
    tbl
  },
  effectTest, names(effectTest)
)

effectTest <- do.call(rbind, effectTest)
rownames(effectTest) <- NULL

knitr::kable(
  effectTest,
  caption = "Effect tests."
)

## @knitr mod_glm_adj_means --------------------------------------------------

at <- list(EDSS = 2, age = 30, sex = "female")

em <- list()

#[FC/200804/12:56]# R0 {{{

em$R0 <- as.data.frame(
  multcomp::cld(emmeans::emmeans(
      object = res_glm$R0$glm, specs = ~ EDSS + age + Time, at = at,
      type = "response"
      ))
)

em$R0 <- within(
  em$R0,
  expr = {
    covariate <- paste0("[EDSS=", EDSS, "] [Age=", age, "]")
    Arm <- "R0"
  }
)

###}}} -- vim fold
#[FC/200804/12:57]# R1 {{{

em$R1 <- as.data.frame(
  multcomp::cld(
    emmeans::emmeans(
      object = res_glm$R1$glm, specs = ~ EDSS + sex + Time, at = at,
      type = "response"
    )
  )
)

em$R1 <- within(
  em$R1,
  expr = {
    covariate <- paste0("[EDSS=", EDSS, "] [Sex=", sex, "]")
    Arm <- "R1"
  }
)

###}}} -- vim fold
#[FC/200804/12:57]# R1+ {{{

em$`R1+` <- as.data.frame(
  multcomp::cld(
    emmeans::emmeans(
      object = res_glm$`R1+`$glm, specs = ~ EDSS + sex + Time, at = at,
      type = "response"
    )
  )
)

em$`R1+` <- within(
  em$`R1+`,
  expr = {
    covariate <- paste0("[EDSS=", EDSS, "] [Sex=", sex, "]")
    Arm <- "`R1+`"
  }
)

###}}} -- vim fold
#[FC/200804/12:57]# R1- {{{

em$`R1-` <- as.data.frame(
  multcomp::cld(
    emmeans::emmeans(
      object = res_glm$`R1-`$glm, specs = ~ Time, at = at,
      type = "response"
    )
  )
)

em$`R1-` <- within(
  em$`R1-`,
  expr = {
    covariate <- "none"
    Arm <- "`R1-`"
  }
)

###}}} -- vim fold
#[FC/200804/12:57]# R2 {{{

em$R2 <- as.data.frame(
  multcomp::cld(
    emmeans::emmeans(
      object = res_glm$R2$glm, specs = ~ EDSS + age + Time, at = at,
      type = "response"
      ))
)

em$R2 <- within(
  em$R2,
  expr = {
    covariate <- paste0("[EDSS=", EDSS, "] [Age=", age, "]")
    Arm <- "R2"
  }
)

###}}} -- vim fold
#[FC/200804/12:57]# `R0+MRI0` {{{

em$`R0+MRI0` <- as.data.frame(
  multcomp::cld(
    emmeans::emmeans(
      object = res_glm$`R0+MRI0`$glm, specs = ~ EDSS + age + Time, at = at,
      type = "response"
      ))
)

em$`R0+MRI0` <- within(
  em$`R0+MRI0`,
  expr = {
    covariate <- paste0("[EDSS=", EDSS, "] [Age=", age, "]")
    Arm <- "R0+MRI0"
  }
)

###}}} -- vim fold

em <- do.call(
  rbind,
  lapply(
    em, function(x) {
      x[
        c(
          "Arm", "Time", "covariate", "prob",
          "SE", "asymp.LCL", "asymp.UCL", ".group"
        )
        ]
    }
  )
)
rownames(em) <- NULL
em <- em[with(em, order(Arm, Time)),]

knitr::kable(em, caption = "Adjusted Means.")

