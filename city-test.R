options(scipen=999)

# --- Libraries ----
library(tidyverse)
library(effects)
library(tikzDevice)

# ---- Loading the contact dataset ----
d <- read_delim("contact_stage1.csv", ";")

# ---- Transforming the data ----

# Multiply the media receptiveness by 1000, so regression is easier to understand
d <- d %>% mutate(abroad_travel_chance_be_thousand = abroad_travel_chance_be * 10000)
# Find the default parameterse
d <- d %>% filter(abroad_travel_chance_be_thousand == 50)

# Define cities
cities <- c("Tilburg", "Breda", "Antwerpen", "Turnhout", "Leuven", "Mechelen", "Dendermonde")

# Create combined dataframe
d_cities <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("step", "sound_avg", "city"))))

for (city in cities) {
  city_column_name <- paste("sphere_", city, sep="")
  d_city <- d %>% select(c("step", city_column_name)) %>% mutate(sound_avg = !!as.name(city_column_name), city=city) %>% select(-c(city_column_name))
  d_cities <- rbind(d_cities, d_city)
}

d_cities <- d_cities %>% mutate(city = case_when(
  city == "Antwerpen" ~ "Antwerp",
  TRUE ~ city
))

# ---- Explorative plots ----

# First, let's just plot the data as-is

# Stage 1 data
tikz(file = "city_test.tex", width=6, height=3, standAlone = TRUE)
d_cities %>% 
  filter(step == 1 | step %% 50 == 0) %>%
  filter(step <= 10000) %>%
  ggplot(aes(x = step, y = sound_avg, color = city)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_line(size=0.1) +
  labs(y="Sound values", x="Step", color="Influence sphere")
endofffile <- dev.off()
system("lualatex city_test.tex; rm *.aux; rm *.log")

