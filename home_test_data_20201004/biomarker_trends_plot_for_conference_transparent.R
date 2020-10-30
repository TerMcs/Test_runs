transparent.plot <- p + ggpubr::theme_transparent()
ggsave(filename = "transparent-background.png",
       plot = transparent.plot,
       bg = "transparent", 
       width = 2, height = 1.5, units = "in")

install.packages("ggpubr")
library(europepmc)
library(cowplot)
library(tidyverse)

biomarker_trend <- europepmc::epmc_hits_trend(query = '("biomarker*" AND "back pain")', 
                                              period = 2009:2020)

biomarker_trend %>%
  ggplot(aes(x = factor(year), y = (query_hits / all_hits))) +
  geom_col(fill = "#56B4E9", width = 0.6, alpha = 0.9) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal_hgrid(12) +
  labs(x = "Year", y = "Proportion of all published articles")

transparent_biomarker_trend <- biomarker_trend + ggpubr::theme_transparent()