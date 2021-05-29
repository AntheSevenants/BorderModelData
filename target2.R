options(scipen=999)

# --- Libraries ----
library(tidyverse)
library(effects)
library(tikzDevice)

# ---- Loading the target dataset ----
d <- read_delim("target2_stage1.csv", ";")

# ---- Defining functions ----

# Logit: from probabilities to logit scale
logit <- function(p) {
  log(p/(1 - p))
}

# Inverse logit: from logit scale to probabilities scale
ilogit <- function(lgt) {
  exp(lgt)/(1 + exp(lgt))
}

# ---- Transforming the data ----

# I need to one-index every step, because logarithms don't like zeroes...
d <- d %>% mutate(step = step + 1)

# Generate the logits for avg_sound_be
d <- d %>% mutate(avg_sound_be_logit = logit(avg_sound_be))

# Performance trick... for now
#d <- d %>% filter(step < 1001)

# ---- Explorative plots ----

# First, let's just plot the data as-is
d %>%
  ggplot(aes(x = step, y = avg_sound_be, color = target_accel_count)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Very clear -> these are not linear data! Maybe logit can help us make the data linear
# for the regression analysis?
ggplot(d, aes(x = step, y = avg_sound_be_logit, color = target_accel_count)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Not quite. What if we model is logarithmically?
ggplot(d, aes(x = log(step), y = avg_sound_be_logit, color = target_accel_count)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Somewhat better...

# ---- Regression ----

# Run the regression
fit <- lm(avg_sound_be_logit ~ log(step) * target_accel_count, data = d)

# Output
summary(fit)

# Add the predicted data to the dataset
d$predicted <- predict(fit)

# What do the predictions look like on the logit scale?
d %>%
ggplot(aes(x = log(step), y = avg_sound_be_logit, color = target_accel_count)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = predicted, group=target_accel_count), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color="Target acceleration count")

# What do the predictions look like on the probability scale?
tikz(file = "target2_real.tex", width=6, height=3, standAlone = TRUE)
d %>%
  filter(step == 1 | step %% 50 == 0) %>%
  ggplot(aes(x = step, y = ilogit(avg_sound_be_logit), color = target_accel_count, group = target_accel_count)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_line(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = ilogit(predicted), group=target_accel_count), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color="Target acceleration\ncount")
endofffile <- dev.off()
system("lualatex target2_real.tex; rm *.aux; rm *.log")

# Poor man's "all effects" plot
tikz(file = "target2_predict.tex", width=6, height=3, standAlone = TRUE)
d %>% 
  filter(step == 1 | step %% 50 == 0) %>%
  filter(target_accel_count %% 5 == 0) %>%
  mutate(target_accel_count = as.factor(target_accel_count)) %>%
  ggplot(aes(x = step, y=ilogit(predicted), color = target_accel_count)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0,0,0,0),"cm")) +
  geom_line(size = 0.1) +
  geom_line(aes(group = target_accel_count), size = 0.1) +
  labs(y="Predicted sound values", x="Step", color="Target acceleration\ncount")
#facet_wrap(~ target_accel_count) # This looks bad
endofffile <- dev.off()
system("lualatex target2_predict.tex; rm *.aux; rm *.log")

# Todo: maybe make colours prettier