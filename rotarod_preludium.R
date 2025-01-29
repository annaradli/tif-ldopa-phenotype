library(tidyverse)
#library(rstatix)
library(readxl)
library(lemon)
library(extrafont)

rotarod_input <- read_excel("rotarod_preludium.xlsx") %>% 
  select(1, 3, 6) %>%
  mutate_at(vars(1), as.character) %>%
  filter(mouse_ID != "2895")


rotarod_input$genotype <- as.factor(rotarod_input$genotype)

t_test_rotarod <- rotarod_input %>%
  t_test(`time_to_fall[s]` ~ genotype)

####
#t_test
rotarod_input %>%
  t_test(`time_to_fall[s]` ~ genotype)

#new plot

rotarod_plot_new <- rotarod_input %>%
  group_by(genotype) %>%
  summarise(mean = mean(`time_to_fall[s]`),
            SEM = sd(`time_to_fall[s]`)/sqrt(n())) %>%
  ggplot(., aes(x = genotype, y = mean, fill = genotype, color = genotype)) +
  geom_col(position = position_dodge(), width = 0.6,  size = 2) +
  geom_linerange(aes(ymin = mean - SEM, ymax = mean + SEM), size = 1, color = "black") +
  geom_point(data = rotarod_input,
             aes(y = `time_to_fall[s]`, x = genotype, fill = genotype, color = genotype), alpha = 0.2, shape = 19, size = 5) +
  scale_fill_manual(values = c("white","white"),
                    labels = c("controls","mutants")) +
  scale_color_manual(values = c("#a6a09f","#1f7fd1"),
                     labels = c("controls","mutants")) +
  labs(y = "Time to fall [s] \n") +
  theme_classic() +
  theme(aspect.ratio = 2.5,
        legend.position = "none",
        title = element_text(size = 20,  face = "bold"),
        text = element_text(family = "Calibri"),
        axis.title.y = element_text(face = "bold", size = 20),
        axis.title.x = element_blank(),
        axis.line.x = element_line(colour = "white"),
        axis.text = element_text(size = 16, color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.background = element_rect(fill = "#dfcbf5", colour = "white"),
        strip.text = element_text(size = 12)) +
  #facet_wrap(~Weeks, scales = 'free_x', strip.position = "bottom") +
  # geom_text(data = weight2_together %>% filter(timepoint == "ldopa-1"),  genotype = "*", x = 2, y = 37, size = 10, inherit.aes = FALSE) +
  # facet_row(~timepoint, scales = "free_x", strip.position = "bottom", space = "free", genotypeler = as_genotypeler(weight2_genotypes)) +
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 250), gap = 0.05)

ggsave(plot = rotarod_plot_new, paste0("rotarod_preludium.pdf"), dpi = 300, width = 9, height = 7, device = cairo_pdf)
