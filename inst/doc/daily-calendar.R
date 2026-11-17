## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ACWR)

## -----------------------------------------------------------------------------
data("training_load_rest", package = "ACWR")
head(training_load_rest)

# Sessions per week
table(ID = training_load_rest$ID, Week = training_load_rest$Week)

## -----------------------------------------------------------------------------
res <- ACWR(db = training_load_rest,
            ID = "ID",
            TL = "TL",
            days = "Day",
            training_dates = "Training_Date",
            ACWR_method = c("EWMA", "RAC", "RAU"))

# Last session of each athlete
last <- res[!duplicated(res$ID, fromLast = TRUE),
            c("ID", "Training_Date", "EWMA_ACWR", "RAC_ACWR", "RAU_ACWR")]
format(last, digits = 3)

## -----------------------------------------------------------------------------
day <- c(0, 2, 4, 7, 9, 11, 14, 16, 18, 21, 23, 25, 28:33)
dates <- as.Date("2024-01-01") + day
load <- rep(500, length(day))

rac <- RAC(TL = load, training_dates = dates)
rau <- RAU(TL = load, training_dates = dates)
ewma <- EWMA(TL = load, training_dates = dates)

# Ignoring the dates reproduces the per-session EWMA of 0.1.0
ewma_sessions <- suppressMessages(EWMA(TL = load))

n <- length(load)
data.frame(
  method = c("RAC", "RAU", "EWMA"),
  version_0.1.0 = c(1.00, 1.86, round(ewma_sessions$EWMA_ACWR[n], 2)),
  version_0.2.0 = round(c(rac$RAC_ACWR[n], rau$RAU_ACWR[n],
                          ewma$EWMA_ACWR[n]), 2)
)

## -----------------------------------------------------------------------------
a1 <- training_load_rest[training_load_rest$ID == 1, ]
rac_default <- RAC(TL = a1$TL, training_dates = a1$Training_Date)
rac_partial <- RAC(TL = a1$TL, training_dates = a1$Training_Date,
                   partial = TRUE)

data.frame(day = a1$Day,
           default = round(rac_default$RAC_ACWR, 2),
           partial = round(rac_partial$RAC_ACWR, 2))[c(1:3, 15:18), ]

