# westward figures 

source("code/helper.r")

# data ----
harvest <- read_csv("data/harvest.csv")
bio <- read_csv("data/kodiak_brf.csv") %>% 
  dplyr::select(-Age, -Year, -Sex) %>% 
  rename_all(tolower)
sport_bio <- read_csv("data/central_bio.csv")
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

ggsave("figs/f11_black_catch_gear_comm_westward.png", width = 6.5, height = 5, units = "in", dpi = 200)

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

ggsave("figs/f12_black_catch_sport_westward.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig w10 ----
# brf lengths - commfish 
xaxis <- tickr(bio, length, 10)

bio %>% 
  dplyr::select(sex, year, length, age) %>% 
  mutate(Sex = case_when(sex == 1~ "male", 
                         sex==2 ~ "female"), 
         Year = factor(year)) %>% 
  drop_na(Sex) %>% 
  ggplot(aes(length, Year, height = ..density..)) + 
  geom_density_ridges(scale = 2.2, alpha = .6) +
  facet_wrap(~Sex) +
  scale_fill_grey() +
  xlab("\nLength (cm)") +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(0,1))

ggsave("figs/f13_black_length_comm_westward.png", width = 6.5, height = 8, units = "in", dpi = 200)


# fig w11 ----
# brf ages - commfish
xaxis <- tickr(harvest, year, 5)
bio %>% 
  dplyr::select(sex, year, length, age) %>% 
  mutate(Sex = case_when(sex == 1~ "male", 
                         sex==2 ~ "female"), 
         Year = factor(year)) %>% 
  filter(age>0) %>% 
  drop_na(Sex) %>% 
  group_by(year, Sex, age) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, age, size = n)) + 
  geom_point() +
  scale_size_area() +
  facet_wrap(~Sex) +
  theme(legend.justification=c(0,1), 
        legend.position=c(.36,1),
        legend.title = element_blank()) +
  ylab("Age") +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) 

ggsave("figs/f14_black_age_comm_westward.png", width = 6.5, height = 5, units = "in", dpi = 200)




# fig w12 ----
# brf lengths - sportfish 
xaxis <- tickr(sport_bio, `Length CM`, 20, start = 20)

sport_bio %>% 
  filter(Species==142, `Division Code` == "SF") %>% 
  dplyr::select(sex = `Sex Code`, year = `Sample Year`, length = `Length CM`, 
                Area = `Mgmt Area Code`, age = Age) %>% 
  filter(Area == "Kod") %>% 
  mutate(Sex = ifelse(sex == 1, "male", "female"), 
         Year = factor(year), 
         length = length * 0.983 - 1.421) %>% 
  group_by(Year, Sex, length) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(length, Year, height = ..density..)) + 
  geom_density_ridges(scale = 1.5, alpha = .6) +
  facet_wrap(~Sex) +
  scale_fill_grey() +
  scale_x_continuous(name = "\nLength (cm)", 
                     labels = xaxis$labels, 
                     breaks = xaxis$breaks,
                     limits = c(15, 70)) +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 1.2)))

ggsave("figs/f15_black_length_sport_westward.png", width = 6.5, height = 8, units = "in", dpi = 200)

# fig w13 ----
# brf ages - sportfish 
sport_bio %>% 
  filter(Species==142, `Division Code` == "SF") %>% 
  dplyr::select(sex = `Maturity Sex`, year = `Sample Year`, length = `Length CM`, 
                Area = `Mgmt Area Code`, age = Age) %>% 
  filter(Area == "Kod") %>% 
  mutate(Sex = ifelse(sex == 1, "male", "female"), 
         Year = factor(year)) %>% 
  group_by(year, Sex, age) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, age, size = n)) + 
  geom_point(alpha = .6) +
  scale_size_area() +
  facet_wrap(~Sex) +
  theme(legend.justification=c(0,1), legend.position=c(.536,1)) +
  ylab("Age") +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) 

ggsave("figs/f16_black_age_sport_westward.png", width = 6.5, height = 5, units = "in", dpi = 200)


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

ggsave("figs/f17_yellow_catch_gear_comm_westward.png", width = 6.5, height = 5, units = "in", dpi = 200)

