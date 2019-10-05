# statewide harvest ----

source("code/helper.r")

# data ----
harvest <- read_csv("data/harvest.csv")
sport_release <- read_csv("data/regional_sport_harvest_reease.csv")

xaxis <- tickr(sport_release, year, 5)

# fig sw2 ----
harvest %>% 
  filter(species == "black", fishery=="comm") %>% 
  mutate(Region = case_when(region=="southeast" ~ "eastern GOA", 
                            TRUE ~ "central/western GOA")) %>% 
  ggplot(aes(year, catch, fill = Region)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

ggsave("figs/black_catch_comm_state.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig sw3 ----
harvest %>% 
  filter(species == "yelloweye", fishery=="comm") %>% 
  mutate(Region = case_when(region=="southeast" ~ "eastern GOA", 
                            TRUE ~ "central/western GOA")) %>% 
  ggplot(aes(year, catch, fill = Region)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

ggsave("figs/yelloweye_catch_comm_state.png", width = 6.5, height = 5, units = "in", dpi = 200)


# fig sw4 ----
sport_release %>% 
  ggplot(aes(year, retain, fill = area)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(start = 0.1, end = 0.6, name = "", labels = c("eastern GOA", "central/western GOA")) +
  scale_y_continuous(name = "Retained catch\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  theme(legend.justification=c(1,1), legend.position=c(.3,1))

ggsave("figs/catch_retained_sport_state.png", width = 6.5, height = 5, units = "in", dpi = 200)   


# fig sw5 ----
sport_release %>% 
  drop_na %>% 
  mutate(catch = retain + release, 
         perc = release / catch * 100) %>% 
  ggplot(aes(year, perc, color = area)) +
  geom_line() +
  scale_color_grey(start = 0.1, end = 0.6, name = "", labels = c("eastern GOA", "central/western GOA")) +
   scale_y_continuous(name = "% of catch released\n", limits = c(0, 100)) +
   scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
   theme(legend.justification=c(1,1), legend.position=c(1,1))

ggsave("figs/perc_catch_released_sport_state.png", width = 6.5, height = 5, units = "in", dpi = 200)   






