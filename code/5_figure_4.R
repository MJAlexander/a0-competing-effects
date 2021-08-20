## Relationship between ratio of IMR/U5MR and a0 based on HMD
## Makes Figure 4
## Author: MA and LR

library(tidyverse)
library(here)

d <- read_rds(here("data/hmd_data.rds"))

d %>% 
  filter(ratio>0.5) %>% 
  filter(imr<.08)%>%
  ggplot(aes(ratio, a0, color = imr)) + geom_point() + 
  scale_color_viridis_c() + 
  theme_bw() + 
  ylab("Average age of death (years)") + xlab("Ratio of infant to under-five mortality")
ggsave(here("fig", "a0_ratio_scatter.pdf"), width = 8, height = 6)

