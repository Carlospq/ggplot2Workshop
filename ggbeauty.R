## Author: Gian Marco Franceschini

librarian::shelf(tidyverse, data.table, glue, patchwork, quiet = TRUE)
options(stringsAsFactors = FALSE)
`%!in%` <- Negate(`%in%`)

library(palmerpenguins)

aa = penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = year, fill = island))+
  geom_bar(position = "fill", color = "black")+
  labs(x = "Year of collection", y = "Fraction of subjects", title = "Through sampled years, all species share only one island", fill = "Island")+
  facet_grid(sex ~ species)+
  scale_fill_manual(values = c("#A7D49B", "#233D4D", "#FE7F2D"))+
  theme_bw()+
  theme(legend.position = "none")+
  NULL

aa


bb = penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = sex, y = body_mass_g))+
    geom_boxplot(aes(fill = sex))+
    geom_jitter(width = .2)+
    labs(x = "Sex", y = "Body mass", title = "Body mass comparison by sex")+
    ggpubr::stat_compare_means(comparisons = list(c("male", "female")), label = "p.signif")+
    scale_fill_manual(values = c("#EB346B", "#2873C9"))+
    facet_wrap(~species+island)+
    lims(y = c(2500, 7000))+
    theme(legend.position = "none")+
    NULL

bb


cc = penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = bill_depth_mm, y = flipper_length_mm, fill = species))+
  geom_density_2d(aes(color = species))+
  geom_point(shape = 21, size = 3)+
  geom_smooth(method = "lm", color = "black", linetype = 2)+
  labs(x = "Bill depth", y = "Flipper length", title = "Anatomical features correlation")+
  ggpubr::stat_cor(geom = "label", label.x = 18, size = 3)+
  scale_fill_manual(values = c("#9AD1D4", "#007EA7", "#CCDBDC"))+
  scale_color_manual(values = c("#9AD1D4", "#007EA7", "#CCDBDC"))+
  facet_wrap(~"All islands")+
  NULL

cc


aa / (bb + cc) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Penguins!", tag_levels = "A")




