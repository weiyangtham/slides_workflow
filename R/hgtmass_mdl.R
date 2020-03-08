library(tidyverse)
library(magrittr)

starwars %<>% mutate(human = (species == "Human") & !is.na(species))

hgtmass = starwars %>%
  group_by(human) %>%
  nest() %>%
  mutate(mdl = map(data, ~lm(height ~ mass, data = .)),
         coefs = map(mdl, ~broom::tidy(., conf.int = T))) %>%
  unnest(coefs) %>%
  filter(str_detect(term, "mass")) %>%
  select(human, estimate:conf.high)

write_csv(hgtmass, here::here("results/hgtmass.csv"))

