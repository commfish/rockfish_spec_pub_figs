# central figures 

source("code/helper.r")

# data ----
harvest <- read_csv("data/harvest.csv")
bio <- read_csv("data/central_bio.csv")

xaxis <- tickr(harvest, year, 5)

# fig c6 ----
harvest %>%
  filter(species == "black", fishery == "comm", region == "central") %>% 
  ggplot(aes(year, catch, fill = District)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "") +
  theme(legend.position = c(0.8, 0.8))

ggsave("figs/c6_black_catch_district_comm_central.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig c7 ----
harvest %>%
  filter(species == "black", fishery == "sport", region == "central") %>%
  group_by(region, year) %>%
  mutate(total = sum(catch, na.rm = T)) %>%
  ggplot(aes(year, total)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (number of fish)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_color_grey()

ggsave("figs/c7_black_catch_sport_central.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig c8 ----
bio %>% 
  filter(Species==142, `Division Code` == "CF") %>% 
  dplyr::select(sex = `Maturity Sex`, year = `Sample Year`, length = `Length CM`, 
                Area = `Mgmt Area Code`, age = Age) %>% 
  filter(Area == "H") %>% 
  mutate(Sex = ifelse(sex == 1, "male", "female"), 
         Year = factor(year)) %>% 
  drop_na(Sex) %>% 
  group_by(Year, Sex, length) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(length, Year, height = ..density..)) + 
  geom_density_ridges(scale = 2.2, alpha = .6) +
  facet_wrap(~Sex) +
  scale_fill_grey() +
  xlab("\nLength (cm)") +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(0,1))

ggsave("figs/c8_black_length_comm_cook_inlet.png", width = 6.5, height = 8, units = "in", dpi = 200)

# fig c9 ----
bio %>% 
  filter(Species==142, `Division Code` == "CF") %>% 
  dplyr::select(sex = `Maturity Sex`, year = `Sample Year`, length = `Length CM`, 
                Area = `Mgmt Area Code`, age = Age) %>% 
  filter(Area == "H") %>% 
  mutate(Sex = ifelse(sex == 1, "male", "female"), 
         Year = factor(year)) %>% 
  drop_na(Sex) %>% 
  group_by(year, Sex, age) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, age, size = n)) + 
  geom_point() +
  scale_size_area() +
  facet_wrap(~Sex) +
  theme(legend.justification=c(0,1), legend.position=c(.36,1)) +
  ylab("Age") +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) 

ggsave("figs/c9_black_age_comm_cook_inlet.png", width = 6.5, height = 5, units = "in", dpi = 200)


# fig c10 ----
harvest %>%
  filter(species == "yelloweye", fishery == "comm", region == "central") %>% 
  ggplot(aes(year, catch, fill = District)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "") +
  theme(legend.position = c(0.8, 0.8))

ggsave("figs/c10_yelloweye_catch_district_comm_central.png", width = 6.5, height = 5, units = "in", dpi = 200)


# fig c11 ----
harvest %>%
  filter(species == "yelloweye", fishery == "sport", region == "central") %>%
  group_by(region, year) %>%
  mutate(total = sum(catch, na.rm = T)) %>%
  ggplot(aes(year, total)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (number of fish)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_color_grey()

ggsave("figs/c11_yelloweye_catch_sport_central.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig c12 ----
bio %>% 
  filter(Species==145, `Division Code` == "CF") %>% 
  dplyr::select(sex = `Maturity Sex`, year = `Sample Year`, length = `Length CM`, 
                Area = `Mgmt Area Code`, age = Age) %>% 
  # filter(Area == "H") %>% 
  mutate(Sex = ifelse(sex == 1, "male", "female"), 
         Year = factor(year),
         Area = case_when(Area=="E" ~ "PWS",
                          Area == "H" ~ "CI")) %>% 
  drop_na(Sex) %>% 
  drop_na(Area) %>% 
  group_by(Year, Sex, Area) %>% 
  mutate(n = n()) %>% 
  filter(n >= 30, !is.na(Area)) %>% 
  group_by(Year, Sex, Area, length) %>% 
  summarise(n = n()) %>% 
  left_join(expand.grid(Year = factor(1991:2017), Area = c("PWS", "CI")), .) %>% 
  filter(Area %in% c("PWS", "CI")) %>% 
   ggplot(aes(length, Year, height = ..density.., fill = Sex)) + 
  geom_density_ridges(scale = 2.2, alpha = .6) +
  facet_wrap(~Area) +
  scale_fill_grey() +
  xlab("\nLength (cm)") +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(.85,.32))

ggsave("figs/c12_yelloweye_length_comm_cook_inlet.png", width = 6.5, height = 8, units = "in", dpi = 200)

# fig c13 ----
bio %>% 
  filter(Species==145, `Division Code` == "CF") %>% 
  dplyr::select(sex = `Maturity Sex`, year = `Sample Year`, length = `Length CM`, 
                Area = `Mgmt Area Code`, age = Age) %>% 
  # filter(Area == "H") %>% 
  mutate(Sex = ifelse(sex == 1, "male", "female"), 
         Year = factor(year),
         Area = case_when(Area=="E" ~ "PWS",
                          Area == "H" ~ "CI")) %>% 
  drop_na(Sex) %>% 
  drop_na(Area) %>% 
  group_by(year, Sex, age, Area) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, age, size = n)) + 
  geom_point() +
  scale_size_area() +
  facet_wrap(~Sex) +
  theme(legend.justification=c(0,1), legend.position=c(.1,1)) +
  ylab("Age") +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) 

ggsave("figs/c13_yelloweye_age_comm_cook_inlet.png", width = 6.5, height = 5, units = "in", dpi = 200)

