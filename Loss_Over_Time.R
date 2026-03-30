library(nlme)
library(tidyverse)
library(vroom)

# Load & prep data
dat <- vroom("ARMD.txt")
dat <- dat %>%
  mutate(Trt = factor(Trt, levels = c(0, 1), labels = c("Control", "Treated")))

# Fit final model
time_trt_mod <- gls(
  Vision ~ Baseline + Time * Trt,
  data = dat,
  correlation = corSymm(form = ~ 4 | Subject)
)

# Extract coefficients
coefs <- coef(time_trt_mod)

# Slopes for each group
slope_control <- coefs["Time"]
slope_treated <- coefs["Time"] + coefs["Time:TrtTreated"]

# Slowing effect (%)
slowing_pct <- (1 - slope_treated / slope_control) * 100

# Weekly vision loss over 52 weeks
loss_control <- slope_control * 52
loss_treated <- slope_treated * 52

# Print results
cat("=== Vision Loss Slopes (letters per week) ===\n")
cat(sprintf("Control:  %.4f\n", slope_control))
cat(sprintf("Treated:  %.4f\n", slope_treated))
cat(sprintf("Interaction (Time:Trt): %.4f\n", coefs["Time:TrtTreated"]))
cat("\n=== Over 52 Weeks ===\n")
cat(sprintf("Control total loss:  %.2f letters\n", loss_control))
cat(sprintf("Treated total loss:  %.2f letters\n", loss_treated))
cat(sprintf("\nInterferon-alpha slows degeneration by %.1f%% relative to control\n", slowing_pct))

# Summary table
results <- data.frame(
  Group     = c("Control", "Treated"),
  Slope     = c(slope_control, slope_treated),
  Loss_52wk = c(loss_control, loss_treated)
)
print(results)

# Optional: plot fitted trajectories
times <- c(0, 4, 12, 24, 52)
baseline_mean <- mean(dat$Baseline)

traj <- data.frame(
  Time  = rep(times, 2),
  Group = rep(c("Control", "Treated"), each = length(times)),
  Vision = c(
    baseline_mean + slope_control * times,
    baseline_mean + slope_treated * times
  )
)

ggplot(traj, aes(x = Time, y = Vision, color = Group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Control" = "#E24B4A", "Treated" = "#378ADD")) +
  labs(
    title = "Fitted Vision Trajectories by Treatment Group",
    x = "Week", y = "Vision Score (letters)",
    color = NULL
  ) +
  theme_minimal(base_size = 13)