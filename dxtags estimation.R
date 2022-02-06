library(DTAXG)
library(tidyverse)

head(sman)

mic.pce <- sman %>% 
  select(pcr, mic) %>% 
  mutate(across(everything(), ~case_when(. == "Positive" ~ 1, . == "Negative" ~ 0)))

head(mic.pce)

DTAXG2(mic.pce$mic, 
       mic.pce$pcr, 
       prior.pi = c(.979, .9), 
       prior.se.group1 = c(.9, .1), 
       prior.se.group2 = c(.9, .85), 
       prior.sp.group1 = c(.9, .82), 
       prior.sp.group2 = c(.85, .9), 
       n.sample = 4000, 
       n.burnin = 1500)

       table(mic.pce) %>% prop.table()
       