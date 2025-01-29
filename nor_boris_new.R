library(tidyverse)
library(rstatix)
library(readxl)
library(lemon)
library(extrafont)

#import data
placement <- read_excel("nor-objects.xlsx")
placement <- placement %>% mutate(position_novel = case_when(
  novel_object == "bricks" & placement_3 == "bricks" ~ 3,
  novel_object == "bricks" & placement_4 == "bricks" ~ 4,
  novel_object == "bottle" & placement_3 == "bottle" ~ 3,
  novel_object == "bottle" & placement_4 == "bottle" ~ 4,
)) 

nor_boris <- read.csv("nor_boris_input.csv", sep = ";")

nor_boris <- nor_boris %>%
  full_join(placement) %>%
  rename(total_exploration_time = total_exploration_time_.s.) %>%
  mutate(exploration_time_novel = case_when(
           position_novel == 3 ~  time_exploration_3left,
           position_novel == 4 ~ time_exploration_4right),
         recognition_index = (exploration_time_novel/total_exploration_time)*100,
         label = factor(paste(genotype, treatment),
                        levels = c("control saline", "control L-DOPA", "mutant saline", "mutant L-DOPA"))) %>%
  filter(cage_ID!= "5-3") %>%
  filter(total_exploration_time > 5)

aov_nor <- summary(aov(recognition_index ~ genotype*treatment, data = nor_boris))

nor_boris %>% group_by(label) %>% summarise(n_animals = length(unique(cage_ID)))

#####
#t_test

nor_boris %>%
  t_test(recognition_index~label) %>%
  mutate(p_adjust_BH = p.adjust(p, "BH"))

#####
#plot - new palette

nor_boris_plot <- nor_boris %>%
  group_by(label) %>%
  summarise(mean = mean(recognition_index),
            SEM = sd(recognition_index, na.rm = TRUE)/sqrt(n())) %>%
  ggplot(., aes(x = label, y = mean, fill = label)) +
  geom_col(position = position_dodge(), width = 0.6) +
  geom_linerange(aes(ymin = mean - SEM, ymax = mean + SEM), size = 1) +
  geom_point(data = nor_boris,
             aes(y = recognition_index, x = label, fill = label), alpha = 0.2, shape = 19, size = 5) +
  scale_fill_manual(values = c( "#a6a09f","#595857",
                               "#1f7fd1","#054780"),
                    labels = c( "controls + saline", "controls + L-DOPA",
                                "mutants + saline", "mutants + L-DOPA")) +
  labs(y = "Novel object recognition index [%] \n") +
  scale_x_discrete(labels = c("control saline" = "controls + saline",
                              "control L-DOPA" = "controls + L-DOPA",
                              "mutant saline" = "mutants + saline",
                              "mutant L-DOPA" = "mutants + L-DOPA")) + 
  theme_classic() +
  geom_hline(yintercept = 50, linetype ="dashed") +
  theme(aspect.ratio = 2.5,
        legend.position = "none",
        title = element_text(size = 20,  face = "bold"),
        text = element_text(family = "Calibri"),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_blank(),
        axis.line.x = element_line(colour = "white"),
        axis.text = element_text(size = 16, color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.background = element_rect(fill = "#cccbc8", colour = "white"),
        strip.text = element_text(size = 12)) +
  #facet_wrap(~Weeks, scales = 'free_x', strip.position = "bottom") +
  # geom_text(data = weight2_together %>% filter(timepoint == "ldopa-1"),  label = "*", x = 2, y = 37, size = 10, inherit.aes = FALSE) +
  # facet_row(~timepoint, scales = "free_x", strip.position = "bottom", space = "free", labeller = as_labeller(weight2_labels)) +
  coord_capped_cart(bottom='none', left='none', gap = 0.05)

ggsave(plot = nor_boris_plot, paste0("nor_boris_preludium.pdf"), dpi = 300, width = 9, height = 7, device = cairo_pdf)
