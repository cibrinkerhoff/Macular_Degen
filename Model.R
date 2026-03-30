library(nlme)
library(tidyverse)
library(vroom)

# Load data
dat <- vroom("ARMD.txt")

# Make TAU (0) the baseline for Trt
dat <- dat %>%
  mutate(Trt = factor(Trt, levels = c(0, 1), labels = c("Control", "Treated")))

# Full model with all predictors + all interactions with Trt
full_mod <- gls(
  Vision ~ Baseline * Trt + Time * Trt,
  data = dat,
  correlation = corSymm(form = ~ 4 | Subject)
)

# Individual interaction models
base_trt_mod <- gls(
  Vision ~ Baseline * Trt + Time,
  data = dat,
  correlation = corSymm(form = ~ 4 | Subject)
)

time_trt_mod <- gls(
  Vision ~ Baseline + Time * Trt,
  data = dat,
  correlation = corSymm(form = ~ 4 | Subject)
)

# No interaction model
no_int_mod <- gls(
  Vision ~ Baseline + Time + Trt,
  data = dat,
  correlation = corSymm(form = ~ 4 | Subject)
)

# F-tests comparing models
anova(no_int_mod, base_trt_mod)   # test Baseline:Trt
anova(no_int_mod, time_trt_mod)   # test Time:Trt
anova(no_int_mod, full_mod)       # test both interactions together




#FINAL MODEL

time_trt_mod <- gls(
  Vision ~ Baseline + Time * Trt,
  data = dat,
  correlation = corSymm(form = ~ 4 | Subject)
)
