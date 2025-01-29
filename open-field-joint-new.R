### open field test analysis - cohorts 1 & 2 - new - analysis from March 2022

library(tidyverse)
library(rstatix)
library(RColorBrewer)
library(readxl)
library(broom)
library(tidyr)
library(purrr)
library(lemon)

### import and format data

input_open_field <- read.csv("open-field.csv", sep = ";") 

colnames(input_open_field)
# input_open_field <- sapply(input_open_field, gsub, pattern = ",", replacement= ".")


input_open_field <-  input_open_field %>% 
  mutate_all(funs(str_replace(., ",", ".")))

#rename columns
input_open_field <- input_open_field %>% 
  rename(trial = X,
         mouse_cageID= X.1,
         mouse_ID = X.2,
         Treatment = X.4,
         Genotype = X.3) %>%
  filter(mouse_ID != "2895")

  
#drop the unnecessary columns, add total_thigmotaxis column = cumulative duration area time - cumulative duration center zone time

as_tibble(input_open_field)
colnames(input_open_field)

open_field <- input_open_field %>% 
  select(2:5, 6, 7, 11, 26) %>%
  rename(
    total_distance = `Distance.moved.Center.point.Total.cm`,
    mean_velocity = `Velocity.Center.point.Mean.cm.s`,
    total_arena_time = `In.zone.Arena...Center.point.Cumulative.Duration.s` ,
    total_center_time = `In.zone.center...Center.point.Cumulative.Duration.s`)


open_field$total_distance <- as.numeric(open_field$total_distance)
open_field$mean_velocity <- as.numeric(open_field$mean_velocity)
open_field$total_arena_time <- as.numeric(open_field$total_arena_time)
open_field$total_center_time <- as.numeric(open_field$total_center_time)

open_field <- open_field %>%
  mutate(percent_thigmotaxis_time = (total_arena_time - total_center_time)/total_arena_time * 100,
         percent_center_time = total_center_time/total_arena_time*100,
         Group = paste(Genotype, Treatment))

open_field$Group <- factor(open_field$Group, levels = c("control saline", 
                                                        "control L-DOPA",
                                                        "mutant saline",
                                                        "mutant L-DOPA"))

### two-way ANOVA
#Parameters of interest:
# total_distance
# percent_thigmotaxis_time
# percent_center_time
# mean_velocity


open_field$mouse_ID <- as.factor(open_field$mouse_ID)

###all ANOVA
summary(aov(total_distance ~ Genotype*Treatment, data = open_field)) #interaction
summary(aov(percent_thigmotaxis_time ~ Genotype*Treatment, data = open_field)) #interaction
# summary(aov(percent_center_time ~ Genotype*Treatment, data = open_field)) #none
summary(aov(mean_velocity ~ Genotype*Treatment, data = open_field)) #interaction



colnames(open_field)

open_field_long <- open_field %>%
  select(-1, -8, -7) %>%
  gather(Parameter, value, 4:7)

unique(open_field_long$Parameter)

#ANOVAs - list
lapply(split(open_field_long, open_field_long$Parameter), function(i){
  summary(aov(value ~ Genotype*Treatment, data = i))
})

open_field_long %>% group_by(Genotype, Treatment) %>% summarise(n = length(unique(mouse_ID)))


open_field %>%
  t_test(total_distance~Group)  %>%
  mutate(p_adjust_BH = p.adjust(p, "BH")) %>%
  filter(p_adjust_BH < 0.05)

open_field %>%
  t_test(mean_velocity~Group)  %>%
  mutate(p_adjust_BH = p.adjust(p, "BH")) %>%
  filter(p_adjust_BH < 0.05)
open_field %>%
  t_test(percent_thigmotaxis_time~Group) %>%
  mutate(p_adjust_BH = p.adjust(p, "BH")) %>%
  filter(p_adjust_BH < 0.05)

####
#new plots
open.total_distance_new <- open_field %>%
  select(1:5, 11) %>% 
  group_by(Genotype, Treatment, Group) %>%
  summarise(mean = mean(total_distance),
            SEM = sd(total_distance)/sqrt(n())) %>%
  ggplot(., aes(x = Group, y = mean, fill = Group, group = Group)) +
  geom_col(position = position_dodge(), width = 0.6) +
  geom_linerange(aes(ymin = mean - SEM, ymax = mean + SEM), size = 1) +
  geom_point(data = open_field,
             aes(x = Group, y = total_distance, fill = Group), alpha = 0.2, shape = 19, size = 5) +
  scale_fill_manual(values = c( "#a6a09f","#595857",
                                "#1f7fd1","#054780"),
                    labels = c( "controls + saline", "controls + L-DOPA",
                                "mutants + saline", "mutants + L-DOPA")) +
  labs(y = "Total distance walked [cm] \n") +
  scale_x_discrete(labels = c("control saline" = "controls + saline",
                              "control L-DOPA" = "controls + L-DOPA",
                              "mutant saline" = "mutants + saline",
                              "mutant L-DOPA" = "mutants + L-DOPA")) + 
  theme_classic() +
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
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 8000), gap = 0.05)

ggsave(plot = open.total_distance_new, paste0("open-field-distance.pdf"), dpi = 300, width = 9, height = 7, device = cairo_pdf)

#mean velocity

open.mean_velocity_new <- open_field %>%
  select(1:4, 6, 11) %>% 
  group_by(Genotype, Treatment, Group) %>%
  summarise(mean = mean(mean_velocity),
            SEM = sd(mean_velocity)/sqrt(n())) %>%
  ggplot(., aes(x = Group, y = mean, fill = Group, group = Group)) +
  geom_col(position = position_dodge(), width = 0.6) +
  geom_linerange(aes(ymin = mean - SEM, ymax = mean + SEM), size = 1) +
  geom_point(data = open_field,
             aes(x = Group, y = mean_velocity, fill = Group), alpha = 0.2, shape = 19, size = 5) +
  scale_fill_manual(values = c( "#a6a09f","#595857",
                                "#1f7fd1","#054780"),
                    labels = c( "controls + saline", "controls + L-DOPA",
                                "mutants + saline", "mutants + L-DOPA")) +
  labs(y = "Mean velocity [cm/s] \n") +
  scale_x_discrete(labels = c("control saline" = "controls + saline",
                              "control L-DOPA" = "controls + L-DOPA",
                              "mutant saline" = "mutants + saline",
                              "mutant L-DOPA" = "mutants + L-DOPA")) + 
  theme_classic() +
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
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 15), gap = 0.05)
  
  ggsave(plot = open.mean_velocity_new, paste0("open-field-velocity.pdf"), dpi = 300, width = 9, height = 7, device = cairo_pdf)
  
#thigmotaxis
  
  open.percent_thigmotaxis_time_new <- open_field %>%
    select(1:4, 9, 11) %>% 
    group_by(Genotype, Treatment, Group) %>%
    summarise(mean = mean(percent_thigmotaxis_time),
              SEM = sd(percent_thigmotaxis_time)/sqrt(n())) %>%
    ggplot(., aes(x = Group, y = mean, fill = Group, group = Group)) +
    geom_col(position = position_dodge(), width = 0.6) +
    geom_linerange(aes(ymin = mean - SEM, ymax = mean + SEM), size = 1) +
    geom_point(data = open_field,
               aes(x = Group, y = percent_thigmotaxis_time, fill = Group), alpha = 0.2, shape = 19, size = 5) +
    scale_fill_manual(values = c( "#a6a09f","#595857",
                                  "#1f7fd1","#054780"),
                      labels = c( "controls + saline", "controls + L-DOPA",
                                  "mutants + saline", "mutants + L-DOPA")) +
    labs(y = "Time spent by cage walls [%] \n") +
    scale_x_discrete(labels = c("control saline" = "controls + saline",
                                "control L-DOPA" = "controls + L-DOPA",
                                "mutant saline" = "mutants + saline",
                                "mutant L-DOPA" = "mutants + L-DOPA")) + 
    theme_classic() +
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
    geom_text(label = "#", aes(x = 1, y = 75), size = 6) +
    geom_text(label = "#", aes(x = 2, y = 75), size = 6) +
    geom_text(label = "#", aes(x = 3, y = 75), size = 6) +
    geom_hline(yintercept = 50, linetype ="dashed") +
    #facet_wrap(~Weeks, scales = 'free_x', strip.position = "bottom") +
    # geom_text(data = weight2_together %>% filter(timepoint == "ldopa-1"),  label = "*", x = 2, y = 37, size = 10, inherit.aes = FALSE) +
    # facet_row(~timepoint, scales = "free_x", strip.position = "bottom", space = "free", labeller = as_labeller(weight2_labels)) +
    coord_capped_cart(bottom='none', left='none', ylim = c(0, 100), gap = 0.05)
  
  ggsave(plot = open.percent_thigmotaxis_time_new , paste0("open-field-thigmotaxis.pdf"), dpi = 300, width = 9, height = 7, device = cairo_pdf)
  
#number of animals
open_field %>% group_by(Group) %>% summarise(n_animals = length(unique(mouse_ID)))