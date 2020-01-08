# southeast figures 

source("code/helper.r")

# data ----
harvest <- read_csv("data/harvest.csv")
outside <- read_csv("data/84-2019 outside ye harvest.csv")
inside <- read_csv("data/84-2019 inside ye harvest.csv")
br_bio <- read_csv("data/br_bio.csv") %>% 
  rename_all(tolower)

sport_bio <- read_csv("data/sport_brf_bio_se.csv", guess_max = 50000)

read_csv('data/black_groundfish.csv', guess_max = 50000) %>% 
  dplyr::select(year, Pounds, Round_Pounds, CFEC_Permit) %>% 
  bind_rows(read_csv("data/black_troll.csv", guess_max = 50000) %>% 
  dplyr::select(year, Pounds, Round_Pounds, CFEC_Permit)) %>% 
  mutate(Round_Pounds = case_when(Round_Pounds < Pounds ~ Pounds,
                                  is.na(Round_Pounds) ~ Pounds,
                                  TRUE ~ Round_Pounds),
         fishery = case_when(CFEC_Permit %in% c("M05B","M15B", "M25B", "M26B") ~ "directed",
                             CFEC_Permit %in% c("S05B", "S15B") ~ "salmon troll", 
                             CFEC_Permit == "M07B" & year==1987 ~ "Question",
                             TRUE ~ "incidental")) %>% 
  filter(fishery!="Question") %>%
  dplyr::select(year, catch = Round_Pounds, fishery) -> se_brf

# fig se3 ----

xaxis <- tickr(se_brf, year, 5)

se_brf %>% 
  ggplot(aes(year, catch, fill = fishery)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

ggsave("figs/f31_black_catch_comm_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)


# fig se4 ----

xaxis <- tickr(harvest, year, 5, start = 1995)
harvest %>% 
  filter(species == "pelagic", fishery == "sport", region == "southeast") %>% 
  ggplot(aes(year, catch)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (numbers)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "")

ggsave("figs/f32_pelagic_catch_sport_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig se5 ----
xaxis <- tickr(br_bio, length_millimeters/10, 10, start = 20)
br_bio %>% 
  filter(species_code==142) %>% 
  dplyr::select(sex = sex_code, year, length = length_millimeters, age) %>% 
  mutate(Sex = case_when(sex == 1~ "male", 
                         sex==2 ~ "female"), 
         Year = factor(year),
         length = length / 10) %>% 
  drop_na(Sex) %>% 
  ggplot(aes(length, Year, height = ..density..)) + 
  geom_density_ridges(scale = 1.5, alpha = .6) +
  facet_wrap(~Sex) +
  scale_fill_grey() +
  scale_x_continuous(name = "\nLength (cm)", 
                     labels = xaxis$labels, 
                     breaks = xaxis$breaks,
                     limits = c(20, 65)) +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 2)))

ggsave("figs/f33_black_length_comm_southeast.png", width = 6.5, height = 8, units = "in", dpi = 200)


# fig se6 ----
xaxis <- tickr(se_brf, year, 5)
br_bio %>% 
  filter(species_code==142) %>% 
  dplyr::select(sex = sex_code, year, length = length_millimeters, age) %>% 
  mutate(Sex = case_when(sex == 1 ~ "male", 
                         sex==2 ~ "female"), 
         Year = factor(year),
         length = length / 10) %>% 
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

ggsave("figs/f34_black_age_comm_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig seX ----
xaxis <- tickr(sport_bio, round(length/10), 10, start = 10)

sport_bio %>% 
  filter(Species == "black") %>% 
  dplyr::select(sex, year, length, age) %>% 
  mutate(Sex = case_when(sex == "M" ~ "male", 
                         sex == "F" ~ "female"), 
         Year = factor(year),
         length = length / 10) %>% 
  # drop_na(Sex) %>% 
  ggplot(aes(length, Year, height = ..density..)) + 
  geom_density_ridges(scale = 1.5, alpha = .6) +
  # facet_wrap(~Sex) +
  scale_fill_grey() +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(name = "\nLength (cm)", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 1.2)))

ggsave("figs/f35_black_length_sport_southeast.png", width = 6.5, height = 8, units = "in", dpi = 200)

# fig seXX ----

sport_bio %>% 
  filter(Species == "black") %>% 
  dplyr::select(sex, year, length, age) %>% 
  mutate(Sex = case_when(sex == "M" ~ "male", 
                         sex == "F" ~ "female"), 
         Year = factor(year),
         length = length / 10) %>% 
  drop_na(Sex) %>% 
  drop_na(age) %>% 
  group_by(year, Sex, age) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, age, size = n)) + 
  geom_point() +
  scale_size_area() +
  facet_wrap(~Sex) +
  theme(legend.justification=c(0,1), legend.position=c(.36,1)) +
  ylab("Age") +
  scale_x_continuous(breaks = 2016:2018)

ggsave("figs/sexx_black_age_sport_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig seXX ----

sport_bio %>% 
  filter(Species == "black") %>% 
  dplyr::select(sex, year, length, age) %>% 
  mutate(Sex = case_when(sex == "M" ~ "male", 
                         sex == "F" ~ "female"), 
         Year = factor(year),
         length = length / 10) %>% 
  drop_na(Sex) %>%
  ggplot(aes(length, Year, height = ..density..)) + 
  geom_density_ridges(scale = 1.5, alpha = .6) +
  facet_wrap(~Sex) +
  scale_fill_grey() +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(name = "\nLength (cm)", labels = xaxis$labels, breaks = xaxis$breaks)  +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 1.2)))

ggsave("figs/seXXX_black_length_sport_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)


# fig se7 ----
inside %>% 
  mutate(fishery_type = case_when(fishery_type == "Directed" ~ "Directed",
                                  YEAR < 1990 ~ "Mixed",
                                  TRUE ~ "Incidental")) %>% 
  group_by(YEAR, fishery_type) %>% 
  summarise(catch = sum(ROUND_POUNDS)) %>% 
  ggplot(aes(YEAR, catch, fill = fishery_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

ggsave("figs/f36_yelloweye_catch_direct_incidental_inside_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig se8 ----

xaxis <- tickr(se_brf, year, 5)
outside %>% 
  filter(SPECIES_CODE == 145) %>% 
  mutate(fishery_type = case_when(fishery_type == "Directed" ~ "Directed",
                                  YEAR < 1990 ~ "Mixed",
                                  TRUE ~ "Incidental")) %>% 
  group_by(YEAR, fishery_type) %>% 
  summarise(catch = sum(ROUND_POUNDS)) %>% 
  ggplot(aes(YEAR, catch, fill = fishery_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (lbs)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

ggsave("figs/f37_yelloweye_catch_direct_incidental_outside_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig se9 ----
harvest %>% 
  filter(species == "yelloweye", fishery == "sport", region == "southeast") %>% 
  mutate(District = factor(District, levels = c("SSEI", "SSEO", "CSEO", "NSEO", "NSEI"))) %>% 
  ggplot(aes(year, catch, fill = District)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Harvest (numbers)\n", labels = scales::comma) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_fill_grey(name = "")

ggsave("figs/f38_yelloweye_catch_district_sport_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)


# fig se10 ----

read_csv("data/dsr_bio_1983-1995.csv") %>% 
  bind_rows(read_csv("data/dsr_bio_1996-2018.csv")) %>% 
  dplyr::select(area = G_MANAGEMENT_AREA_CODE, year = YEAR, age = AGE, Sex = SEX_CODE, length = LENGTH_MILLIMETERS) %>%  
  mutate(Year = factor(year), Sex = case_when(Sex == 1 ~ "male", 
                                              Sex == 2 ~ "female"),
         Area = case_when(area %in% c("NSEI", "SSEI") ~ "Inside",
                          area %in% c("SSEO", "CSEO", "NSEO", "EYKT") ~ "Outside"),
         length = length / 10) %>% 
  drop_na(Sex) %>% 
  drop_na(Area) %>% 
  ggplot(aes(length, Year, height = ..density..)) + 
  geom_density_ridges(scale = 1.4, alpha = .6) +
  facet_wrap(~Sex) +
  scale_fill_grey() +
  xlab("\nLength (cm)") +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 1.5)))

ggsave("figs/f39_dsr_length_comm_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)



# fig se11 ----

read_csv("data/dsr_bio_1983-1995.csv") %>% 
  bind_rows(read_csv("data/dsr_bio_1996-2018.csv")) %>% 
  dplyr::select(area = G_MANAGEMENT_AREA_CODE, year = YEAR, age = AGE, Sex = SEX_CODE) %>%  
  mutate(Year = factor(year), Sex = case_when(Sex == 1 ~ "male", 
                                              Sex == 2 ~ "female"),
         Area = case_when(area %in% c("NSEI", "SSEI") ~ "Inside",
                          area %in% c("SSEO", "CSEO", "NSEO", "EYKT") ~ "Outside")) %>% 
  drop_na(Sex) %>% 
  drop_na(Area) %>% 
  drop_na(age) %>% 
  group_by(year, age, Area, Sex) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, age, size = n, color = Sex)) + 
  geom_point(alpha = 0.6) + 
  scale_size_area() +
  facet_wrap(~Area) +
  scale_color_grey() +
  theme(legend.justification=c(0,1), legend.position=c(.36,1)) +
  scale_x_continuous(name = "\nYear", labels = xaxis$labels, breaks = xaxis$breaks) +
  ylab("Age")

ggsave("figs/f40_dsr_age_comm_southeast.png", width = 6.5, height = 5, units = "in", dpi = 200)

# fig seXX ----


sport_bio %>% 
  filter(Species == "Yelloweye") %>% 
  dplyr::select(sex, year, length, age) %>% 
  mutate(Sex = case_when(sex == "M" ~ "male", 
                         sex == "F" ~ "female"), 
         Year = factor(year),
         length = round(length / 10)) %>% 
  dplyr::filter(year!=2005) -> sport_ye

xaxis <- tickr(sport_ye, length, 20, start = 20)

sport_ye %>%
  ggplot(aes(length, Year, height = ..density..)) + 
  geom_density_ridges(scale = 1.5, alpha = .6) +
  # facet_wrap(~Sex) +
  scale_fill_grey() +
  ylab("Year\n") +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(name = "\nLength (cm)", 
                     labels = xaxis$labels, 
                     breaks = xaxis$breaks,
                     limits = c(20, 110))  +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 2)))

ggsave("figs/f41_yelloweye_length_sport_southeast.png", width = 6.5, height = 8, units = "in", dpi = 200)
