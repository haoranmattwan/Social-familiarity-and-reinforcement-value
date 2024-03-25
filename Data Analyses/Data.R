# Save as Data.R

library(here())

raw_dat <- read_csv("dat.csv")
# Data Trimming
dat <- raw_dat %>%
  mutate(active_ses_time = (active_ses_time / 60) / 60,
         opening_rt = opening / active_ses_time,
         interact_rt = interact / active_ses_time,
         lq = lhs(interact_rt),
         fmlr = ifelse(familiarity == 1, "Cagemate", "Non-cagemate"),
         cond = case_when(cond == "10sec" ~ "10 Sec",
                          cond == "30sec" ~ "30 Sec",
                          cond == "60sec" ~ "60 Sec"),
         duration = case_when(cond == "10 Sec" ~ 10,
                              cond == "30 Sec" ~ 30,
                              cond == "60 Sec" ~ 60),
         unit_price = fr / duration) %>%
  select(-c(opening, interact, familiarity, ref)) %>%
  relocate(pair, fmlr, cond, fr, interact_rt, lq) %>%
  arrange(pair, fmlr, cond) %>% group_by(pair, fmlr, cond) %>%
  mutate(id = cur_group_id()) %>% group_by(pair, fmlr, cond, fr) %>%
  summarise_all(mean) %>%
  dummy_cols(select_columns = c("fmlr")) %>%
  unite("fmlr_cond",fmlr:cond, remove = F) %>% dummy_cols(select_columns = c("fmlr_cond")) %>%
  select(-fmlr_cond) %>%
  rename("f" = "fmlr_Cagemate", "u" = "fmlr_Non-cagemate",
         "f1" = "fmlr_cond_Cagemate_10 Sec","f3" = "fmlr_cond_Cagemate_30 Sec","f6" = "fmlr_cond_Cagemate_60 Sec",
         "u1" = "fmlr_cond_Non-cagemate_10 Sec","u3" = "fmlr_cond_Non-cagemate_30 Sec","u6" = "fmlr_cond_Non-cagemate_60 Sec") %>%
  mutate(s1 = ifelse(cond == "10 Sec", 1, 0),
         s3 = ifelse(cond == "30 Sec", 1, 0),
         s6 = ifelse(cond == "60 Sec", 1, 0))
