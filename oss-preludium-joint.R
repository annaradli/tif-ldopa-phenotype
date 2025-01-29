### oss2 analysis - cohorts 1&2

library(tidyverse)
library(rstatix)
library(RColorBrewer)
library(patchwork)
library(lemon)
# library(readxl)
# library(plotly)
library(ggforce)

### import and format data

input_oss2 <- read.csv("oss-joint.csv")
genotypes <- read.csv("genotypes.csv", sep = ";")

# add correct and incorrect, add info on animals
oss2_long <- input_oss2 %>% mutate(correct_nosepokes = case_when(
  rewarded_side == "L" ~ left_nosepokes,
  rewarded_side == "R" ~ right_nosepokes
),
incorrect_nosepokes = case_when(
  rewarded_side == "R" ~ left_nosepokes,
  rewarded_side == "L" ~ right_nosepokes
),
total_nosepokes = left_nosepokes+right_nosepokes) %>%
  filter(animal_ID != 2895) #remove 5-3 which has uncertain genetic status

oss2_long <- left_join(oss2_long, genotypes, by.x = "animal_Id", by.y = "mouse_ID") %>%
  select(-mouse_ID, -birthday, -cage) 

#timepoint as factor
oss2_long$timepoint <- factor(oss2_long$timepoint , levels = c("training1", 
                                                             "training2",
                                                             "training3",
                                                             "training4",
                                                             "training5",
                                                             "test1-1",
                                                             "test1-2",
                                                             "test1-3",
                                                             "test2-1",
                                                             "test2-2",  
                                                             "test2-3",
                                                             "test3-1",
                                                             "test3-2",
                                                             "test3-3",
                                                             "test4-1",
                                                             "test4-2",
                                                             "test4-3"
                                                        ))

oss2_long$animal_ID <- as.factor(oss2_long$animal_ID)

oss2_long %>% filter(is.na(correct_nosepokes)) %>% group_by(animal_ID, weeks_post_tamoxifen) %>% summarise(obs = paste(unique(timepoint), collapse = ",")) #NAs only for prematurely killed and 14 weeks post tamoxifen
oss2_long %>% filter(is.na(incorrect_nosepokes)) %>% group_by(animal_ID, weeks_post_tamoxifen) %>% summarise(obs = paste(unique(timepoint), collapse = ",")) #NAs only for prematurely killed and 14 weeks post tamoxifen

oss2_long %>% drop_na() %>% group_by(animal_ID) %>% summarise(n_weeks = paste(unique(weeks_post_tamoxifen), collapse = ",")) %>% print(n = nrow(.))

#average for each week
oss2_long <- oss2_long %>%
  group_by(animal_ID, cage_ID, genotype, treatment, weeks_post_tamoxifen, history) %>%
  summarise(mean_correct = mean(correct_nosepokes),
            mean_incorrect = mean(incorrect_nosepokes),
            mean_nosepokes = mean(total_nosepokes),
            mean_rewards = mean(rewards))

class(oss2_long$weeks_post_tamoxifen)

#filter animals that died before the end of mutation/treatment period

oss2_mutation <- oss2_long %>% filter(weeks_post_tamoxifen < 14) %>% filter(!cage_ID %in% c("2-1", "7-1", "7-2", "9-1", "9-2", "5-3"))

unique(oss2_mutation$animal_ID)


#from treatment - remove all "killed" (1-2 not removed in the mutation period was killed on the 1st day of treatment)
oss2_treatment <- oss2_long %>% filter(weeks_post_tamoxifen == 14) %>% filter(history != "killed") %>% filter(cage_ID != "5-3")

#check whether all animals have complete data - yes
# oss2_mutation %>% drop_na() %>% group_by(animal_ID, weeks_post_tamoxifen) %>% summarize(obs = length(correct_nosepokes)) %>%
#   group_by(animal_ID) %>% summarise(unique_obs = length(obs)) %>% filter(unique_obs < 14) 
# 
# oss2_treatment %>% drop_na() %>% group_by(animal_ID, timepoint) %>% summarize(obs = length(correct_nosepokes)) %>%
#   group_by(animal_ID) %>% summarise(unique_obs = length(obs)) %>% filter(unique_obs < 3) 

#checking completeness - another variant - for means per weeks_post_tamoxifen
oss2_mutation %>% drop_na() %>% group_by(animal_ID, weeks_post_tamoxifen) %>% summarize(obs = length(mean_correct)) %>%
     group_by(animal_ID) %>% summarise(unique_obs = length(obs)) %>% filter(unique_obs < 4) #complete

oss2_treatment %>% drop_na() %>% group_by(animal_ID, weeks_post_tamoxifen) %>% summarize(obs = length(mean_correct)) %>%
  group_by(animal_ID) %>% summarise(unique_obs = length(obs)) %>% filter(unique_obs < 1) #complete


#AOVs
oss2_mutation$weeks_post_tamoxifen <- as.factor(oss2_mutation$weeks_post_tamoxifen)
oss2_treatment$weeks_post_tamoxifen <- as.factor(oss2_treatment$weeks_post_tamoxifen)

summary(aov(mean_correct ~ genotype*weeks_post_tamoxifen + Error(animal_ID/weeks_post_tamoxifen), data = oss2_mutation))
summary(aov(mean_incorrect ~ genotype*weeks_post_tamoxifen + Error(animal_ID/weeks_post_tamoxifen), data = oss2_mutation))

oss2_mutation %>% group_by(weeks_post_tamoxifen, genotype) %>% summarise(n = length(unique(animal_ID)))

oss2_treatment %>% group_by(genotype) %>% summarise(mean = mean(mean_incorrect, na.rm = TRUE), 
                                                   SEM = sd(mean_incorrect, na.rm = TRUE)/sqrt(n()))

summary(aov(mean_correct ~ genotype*treatment, data = oss2_treatment))
summary(aov(mean_incorrect ~ genotype*treatment, data = oss2_treatment))

oss2_treatment %>% group_by( genotype, treatment) %>% summarise(n = length(unique(animal_ID)))

#join dataframes for plotting
oss2_together <- bind_rows(oss2_mutation, oss2_treatment) 
oss2_together$weeks_post_tamoxifen <- as.numeric(as.character(oss2_together$weeks_post_tamoxifen))
oss2_together$treatment <- ifelse(oss2_together$weeks_post_tamoxifen < 14, str_replace_all(oss2_together$treatment, c("L-DOPA" = "NA", "saline" = "NA")), oss2_together$treatment)

oss2_together$Group <- factor(factor(paste(oss2_together$genotype, oss2_together$treatment),
                               levels = c("control NA", "control saline", "control L-DOPA",
                                          "mutant NA", "mutant saline", "mutant L-DOPA")))


oss2_correct <- oss2_together %>%
  select(animal_ID, genotype, treatment, weeks_post_tamoxifen, Group, mean_correct) 

oss2_incorrect <- oss2_together %>%
  select(animal_ID, genotype, treatment, weeks_post_tamoxifen, Group, mean_incorrect)



oss2_correct %>%
  group_by(weeks_post_tamoxifen) %>%
  t_test(mean_correct~Group)  %>%
  mutate(p_adjust_BH = p.adjust(p, "BH")) %>%
  filter(p_adjust_BH < 0.05)


oss2_incorrect %>%
  group_by(weeks_post_tamoxifen) %>%
  t_test(mean_incorrect~Group)  %>%
  mutate(p_adjust_BH = p.adjust(p, "BH")) %>%
  filter(p_adjust_BH < 0.05)



#new plots

oss2_correct_plot_new <- oss2_correct %>%
  group_by(weeks_post_tamoxifen, Group) %>%
  summarise(mean = mean(mean_correct),
            SEM = sd(mean_correct, na.rm = TRUE)/sqrt(n())) %>%
  ggplot(., aes(x = Group, y = mean, fill = Group, colour = Group, group = weeks_post_tamoxifen)) +
  geom_linerange(aes(ymin = mean - SEM, ymax = mean + SEM), size = 1.5) +
  geom_point(size = 5, stroke = 2.2, shape = 21) +
  scale_fill_manual(values = c("white", "#a6a09f","#595857",
                               "white", "#1f7fd1","#054780"),
                    labels = c("controls", "controls + saline", "controls + L-DOPA",
                               "mutants", "mutants + saline", "mutants + L-DOPA")) +
  scale_color_manual(values = c("#a6a09f", "#a6a09f","#595857",
                                "#1f7fd1","#1f7fd1","#054780"),
                     labels = c("controls", "controls + saline", "controls + L-DOPA",
                                "mutants", "mutants + saline", "mutants + L-DOPA")) +
  labs(y = "Active nosepokes \n",
       x = "Weeks after tamoxifen treatment") +
  theme_classic() +
  theme(aspect.ratio = 18, 
        legend.position = "none", #first "right" to have the legend
        title = element_text(size = 20,  face = "bold"),
        text = element_text(family = "Calibri"),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.line.x = element_line(colour = "white"),
        axis.text = element_text(size = 16, colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.background = element_rect(fill = "#cccbc8", colour = "white"),
        strip.text = element_text(size = 16)) +
  #geom_text(data = weight2_together %>% filter( weeks_after_tamoxifen == "14"), label = "*", x = 2, y = 30, size = 10, inherit.aes = FALSE) +
  facet_row(~weeks_post_tamoxifen, scales = "free_x", strip.position = "bottom", space = "free", labeller = as_labeller(as.factor(oss2_correct$weeks_post_tamoxifen))) +
  coord_capped_cart(bottom ='none', left='none', ylim = c(0, 200),  gap = 0.05) 

ggsave(plot = oss2_correct_plot_new, paste0("oss_active_publikacja.pdf"), dpi = 300, width = 9, height = 7, device = cairo_pdf)

#
oss2_incorrect_plot_new <- oss2_incorrect %>%
  group_by(weeks_post_tamoxifen, Group) %>%
  summarise(mean = mean(mean_incorrect),
            SEM = sd(mean_incorrect, na.rm = TRUE)/sqrt(n())) %>%
  ggplot(., aes(x = Group, y = mean, fill = Group, colour = Group, group = weeks_post_tamoxifen)) +
  geom_linerange(aes(ymin = mean - SEM, ymax = mean + SEM), size = 1.5) +
  geom_point(size = 5, stroke = 2.2, shape = 21) +
  scale_fill_manual(values = c("white", "#a6a09f","#595857",
                               "white", "#1f7fd1","#054780"),
                    labels = c("controls", "controls + saline", "controls + L-DOPA",
                               "mutants", "mutants + saline", "mutants + L-DOPA")) +
  scale_color_manual(values = c("#a6a09f", "#a6a09f","#595857",
                                "#1f7fd1","#1f7fd1","#054780"),
                     labels = c("controls", "controls + saline", "controls + L-DOPA",
                                "mutants", "mutants + saline", "mutants + L-DOPA")) +
  labs(y = "Inactive nosepokes \n",
       x = "Weeks after tamoxifen treatment") +
  theme_classic() +
  theme(aspect.ratio = 18, 
        legend.position = "none", #first "right" to have the legend
        title = element_text(size = 20,  face = "bold"),
        text = element_text(family = "Calibri"),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.line.x = element_line(colour = "white"),
        axis.text = element_text(size = 16, colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.background = element_rect(fill = "#cccbc8", colour = "white"),
        strip.text = element_text(size = 16)) +
  #geom_text(data = weight2_together %>% filter( weeks_after_tamoxifen == "14"), label = "*", x = 2, y = 30, size = 10, inherit.aes = FALSE) +
  facet_row(~weeks_post_tamoxifen, scales = "free_x", strip.position = "bottom", space = "free", labeller = as_labeller(as.factor(oss2_incorrect$weeks_post_tamoxifen))) +
  coord_capped_cart(bottom ='none', left='none', ylim = c(0, 200),  gap = 0.05) 

ggsave(plot = oss2_incorrect_plot_new, paste0("oss_inactive_publikacja.pdf"), dpi = 300, width = 9, height = 7, device = cairo_pdf)