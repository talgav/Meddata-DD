source("~/R/code_snips/opendir.R")
library(tidyverse)

raw_med <- read_csv("data/all_uvc_data_101119.csv", col_types = cols(depth = col_double()))

length_NAs <- raw_med %>% filter(is.na(sp.length)) %>% print(.)
# write_csv(length_NAs, "problems/length_NAs.csv")

protect_NAs <- raw_med %>% filter(is.na(protection)) %>% print(.)
# write_csv(protect_NAs, "problems/protect_NAs.csv")

enforce_NAs <- raw_med %>% filter(is.na(enforcement)) %>% print(.)
# write_csv(enforce_NAs, "problems/enforce_NAs.csv")

mpaha_NAs <- raw_med %>% filter(is.na(total.mpa.ha)) %>% print(.)
# write_csv(mpaha_NAs, "problems/mpaha_NAs.csv")

notake_NAs <- raw_med %>% filter(is.na(size.notake)) %>% print(.)
# write_csv(notake_NAs, "problems/notake_NAs.csv")

mpacreate_NAs <- raw_med %>% filter(is.na(yr.creation)) %>% print(.)
# write_csv(mpacreate_NAs, "problems/mpacreate_NAs.csv")

depth_NAs <- raw_med %>% filter(is.na(depth)) %>% print(.)
# write_csv(depth_NAs, "problems/depth_NAs.csv")

ab_NAs <- raw_med %>% filter(is.na(a), is.na(b)) %>% distinct(species) %>% print(.)
# write_csv(ab_NAs, "problems/ab_NAs.csv")

temp_NAs <- raw_med %>% filter(is.na(tmean), is.na(trange)) %>% print(.)
# write_csv(temp_NAs, "problems/temp_NAs.csv")

sal_NAs <- raw_med %>% filter(is.na(sal_mean)) %>% print(.)
# write_csv(sal_NAs, "problems/sal_NAs.csv")


