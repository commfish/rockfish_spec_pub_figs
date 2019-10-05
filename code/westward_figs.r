# westward figures 

source("code/helper.r")

# data ----
harvest <- read_csv("data/harvest.csv")

xaxis <- tickr(harvest, year, 5)

# fig w8 ----
harvest %>%
  filter(species == "black", fishery == "comm", region == "westward") %>%
  ggplot(aes(year, catch, fill = gear)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "", labels = c("Jig", "Longline", "Non-pelagic", "Other")) +
  theme(legend.position = c(0.8, 0.8))

ggsave("figs/black_catch_gear_comm_westward.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig w9 ----
harvest %>%
  filter(species == "black", fishery == "sport", region == "westward") %>%
  group_by(region, year) %>%
  mutate(total = sum(catch, na.rm = T)) %>%
  ggplot(aes(year, total)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (number of fish)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_color_grey()

ggsave("figs/black_catch_sport_westward.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig w10 ----
# brf lengths - commfish 

# fig w11 ----
# brf ages - commfish

# fig w12 ----
# brf lengths - sportfish 


# fig w13 ----
# brf ages - sportfish 

# fig w14 ----
# yelloweye catch by gear type
harvest %>%
  filter(species == "yelloweye", fishery == "comm", region == "westward") %>% 
  mutate(gear = case_when(gear=="non_pel" ~ "Non-pelagic/bottom trawl",
                          gear=="longline" ~ "Longline",
                          TRUE ~ "Other")) %>% 
  ggplot(aes(year, catch, fill = gear)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "") +
  theme(legend.position = c(0.8, 0.8))

ggsave("figs/yellow_catch_gear_comm_westward.png", width = 6.5, height = 5, units = "in", dpi = 200)

