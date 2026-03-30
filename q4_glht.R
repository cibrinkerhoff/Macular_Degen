library(nlme)
library(tidyverse)
library(vroom)

# Load data
dat <- vroom("ARMD.txt")

# Make TAU (0) the baseline for Trt
dat <- dat %>%
  mutate(Trt = factor(Trt, levels = c(0, 1), labels = c("Control", "Treated")))


#FINAL MODEL

time_trt_mod <- gls(
  Vision ~ Baseline + Time * Trt,
  data = dat,
  correlation = corSymm(form = ~ 4 | Subject)
)



#On average and after controlling for the baseline score, do patients on interferon- have a
#significantly higher vision score at 52 weeks than those not on the treatment?


library(multcomp)

glht_52 <- glht(
  time_trt_mod,
  linfct = c(
    "TrtTreated + Time:TrtTreated * 52 = 0"
  )
)

summary(glht_52)
confint(glht_52)

# Yes they do, on average we expect the vision score to be 8.241 points higher. Our 95% CI is (4.2415 12.2408)
