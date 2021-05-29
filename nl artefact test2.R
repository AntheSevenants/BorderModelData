options(scipen=999)

# --- Libraries ----
library(tidyverse)
library(effects)
library(tikzDevice)

# ---- Loading the artefact dataset ----
d <- read_delim("nl_artefact_test2_stage1.csv", ";")

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

# Generate the logits for sphere_Breda
d <- d %>% mutate(sphere_Breda_logit = logit(sphere_Breda))

# ---- Explorative plots ----

# First, let's just plot the data as-is
ggplot(d, aes(x = step, y = sphere_Breda, color = target_accel_count)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071")

# Very clear -> these are not linear data! Maybe logit can help us make the data linear
# for the regression analysis?
ggplot(d, aes(x = step, y = sphere_Breda_logit, color = target_accel_count)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Better.

# ---- Regression ----

# Run the regression
fit <- lm(sphere_Breda_logit ~ log(step) * target_accel_count, data = d)

# Output
summary(fit)

# Add the predicted data to the dataset
d$predicted <- predict(fit)

# What do the predictions look like on the logit scale?
ggplot(d, aes(x = step, y = sphere_Breda_logit, color = target_accel_count)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = predicted, group=target_accel_count), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color="Travel acceleration count")

# What do the predictions look like on the probability scale?
ggplot(d, aes(x = step, y = ilogit(sphere_Breda_logit), color = target_accel_count)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = ilogit(predicted), group=target_accel_count), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color="Travel acceleration count")

# Poor man's "all effects" plot
tikz(file = "nl_artefact_test2.tex", width=6, height=3, standAlone = TRUE)
d %>% 
  filter(step == 1 | step %% 20 == 0) %>%
  filter((target_accel_count %% 5 == 0 & target_accel_count != 0) | target_accel_count == 1) %>%
  mutate(target_accel_count = as.factor(target_accel_count)) %>%
  ggplot(aes(x = step, y=ilogit(predicted), color = target_accel_count)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  geom_line(size = 0.1) +
  geom_line(aes(group = target_accel_count)) +
  labs(y="Predicted sound values", x="Step", color="Travel acceleration\ncount")
  #facet_wrap(~ target_accel_count) # This looks bad
endofffile <- dev.off()
system("lualatex nl_artefact_test2.tex; rm *.aux; rm *.log")

# Todo: maybe make colours prettier