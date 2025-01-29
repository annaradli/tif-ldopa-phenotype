### weight analysis - cohorts 1&2

library(tidyverse)
library(rstatix)
library(RColorBrewer)
library(patchwork)
library(lemon)
library(ggforce)
library(extrafont)
# library(readxl)
# library(plotly)

### import and format data
weight_1 <- read.csv("weight_cohort1.csv")
weight_2 <- read.csv("weight_cohort2.csv")
weight_1_ldopa <- read.csv("weight_cohort1_ldopa.csv")
weight_2_ldopa <- read.csv("weight_cohort2_ldopa.csv")
genotypes <- read.csv("genotypes.csv", sep = ";")


#######MUTATION#############

#reshape, drop the unnecessary data and X's from the date
weight_1 <- weight_1 %>% select(-birthdate) %>% gather(date, weight, 5:12) 
weight_1$date <- str_replace(weight_1$date, "^[A-Z]*", "")

weight_2 <- weight_2 %>% gather(date, weight, 5:12) 
weight_2$date <- str_replace(weight_2$date, "^[A-Z]*", "")
weight_2$date <- str_replace(weight_2$date, "13.09.21", "13.09.2021")

  
weight_mutation <- bind_rows(weight_1, weight_2)
weight_mutation$mouse_ID <- as.factor(weight_mutation$mouse_ID)
weight_mutation <- weight_mutation %>% 
  mutate(
  timepoint = case_when(
    date %in%  c("22.06.2021", "13.09.2021") ~"tam_start",
    date %in%  c("10.08.2021", "02.11.2021") ~"weight-1",
    date %in%  c("18.08.2021", "09.11.2021") ~"weight-2",
    date %in%  c("25.08.2021", "16.11.2021") ~"weight-3",
    date %in%  c("31.08.2021", "23.11.2021") ~"weight-4",
    date %in%  c("08.09.2021", "30.11.2021") ~"weight-5",
    date %in%  c("14.09.2021", "07.12.2021") ~"weight-6",
    date %in%  c("22.09.2021", "15.12.2021") ~"weight-7",
  ),
  weeks_after_tamoxifen = case_when(
    timepoint == "tam_start" ~ 0,
    timepoint == "weight-1" ~ 7,
    timepoint == "weight-2" ~ 8,
    timepoint == "weight-3" ~ 9,
    timepoint == "weight-4" ~ 10,
    timepoint == "weight-5" ~ 11,
    timepoint == "weight-6" ~ 12,
    timepoint == "weight-7" ~ 13,
  ))


#join with genotypes 

genotypes_select <- genotypes %>% select(mouse_ID, history, treatment)
genotypes_select$mouse_ID <- as.factor(genotypes_select$mouse_ID)
weight_mutation <- left_join(weight_mutation, genotypes_select, by = "mouse_ID")
weight_mutation$weeks_after_tamoxifen <- as.factor(weight_mutation$weeks_after_tamoxifen )
  

##############TREATMENT##################

#reshape, drop the unnecessary data and X's from the date

weight_1_ldopa <- weight_1_ldopa %>% select(-4) %>% gather(date, weight, 6:19) 
weight_1_ldopa$date <- str_replace(weight_1_ldopa$date, "^[A-Z]*", "")
weight_1_ldopa$date <- str_replace(weight_1_ldopa$date, ".21", ".2021")

weight_2_ldopa <- weight_2_ldopa %>% select(-4) %>% gather(date, weight, 6:19) 
weight_2_ldopa$date <- str_replace(weight_2_ldopa$date, "^[A-Z]*", "")
weight_2_ldopa$date <- str_replace(weight_2_ldopa$date, ".21", ".2021")
weight_2_ldopa$date <- str_replace(weight_2_ldopa$date, ".22", ".2022")

weight_ldopa <- rbind(weight_1_ldopa, weight_2_ldopa)
weight_ldopa <- weight_ldopa %>% rename(mouse_ID = ID.myszy)
weight_ldopa$mouse_ID <- as.factor(weight_ldopa$mouse_ID)


weight_ldopa <- weight_ldopa %>% 
  mutate(
  
  timepoint = case_when(
    date %in%  c("28.09.2021", "21.12.2021") ~"ldopa-1",
    date %in%  c("29.09.2021", "22.12.2021") ~"ldopa-2",
    date %in%  c("30.09.2021", "23.12.2021") ~"ldopa-3",
    date %in%  c("01.10.2021" , "24.12.2021") ~"ldopa-4",
    date %in%  c("02.10.2021" , "25.12.2021") ~"ldopa-5",
    date %in%  c("03.10.2021" , "26.12.2021") ~"ldopa-6",
    date %in%  c("04.10.2021" , "27.12.2021") ~"ldopa-7",
    date %in%  c("05.10.2021" , "28.12.2021") ~"ldopa-8",
    date %in%  c("06.10.2021" , "29.12.2021") ~"ldopa-9",
    date %in%  c("07.10.2021" , "30.12.2021") ~"ldopa-10",
    date %in%  c("08.10.2021" , "31.12.2021") ~"ldopa-11",
    date %in%  c("09.10.2021" , "01.01.2022") ~"ldopa-12",
    date %in%  c("10.10.2021" ,	"02.01.2022") ~"ldopa-13",
    date %in%  c("11.10.2021" ,	"03.01.2022") ~"ldopa-14"
  ),
  weeks_after_tamoxifen = case_when(
    timepoint %in% c("ldopa-1", "ldopa-2", "ldopa-3", "ldopa-4",
                     "ldopa-5", "ldopa-6", "ldopa-7") ~ 14,
    timepoint %in% c("ldopa-8", "ldopa-9", "ldopa-10", "ldopa-11",
                     "ldopa-12", "ldopa-13", "ldopa-14") ~ 15,
  ))

#timepoint as a factor
#no ldopa-1 because it was just befor the first injection
weight_ldopa$timepoint <- factor(weight_ldopa$timepoint , levels = c("ldopa-1",
                                                                     "ldopa-2",
                                                                     "ldopa-3",
                                                                     "ldopa-4",
                                                                     "ldopa-5",
                                                                     "ldopa-6",
                                                                     "ldopa-7",
                                                                     "ldopa-8",
                                                                     "ldopa-9",
                                                                     "ldopa-10",
                                                                     "ldopa-11",
                                                                     "ldopa-12",
                                                                     "ldopa-13",
                                                                     "ldopa-14"
))



#find mice with NAs and exclude them
missing_data_ldopa <- weight_ldopa %>% filter(is.na(weight)) %>% distinct(mouse_ID) #5 mice: "2877", "3303", "3306", "3254", "3255"
missing_data$mouse_ID <- as.factor(missing_data$mouse_ID)
weight_mutation$weeks_after_tamoxifen <- as.factor(weight_mutation$weeks_after_tamoxifen)
weight_mutation <- filter(weight_mutation, !mouse_ID %in% c("2877", "3303", "3306", "3254", "3255")) 

genotypes_select2 <- genotypes_select %>% select(mouse_ID, history)
weight_ldopa <- left_join(weight_ldopa, genotypes_select2, by = "mouse_ID")

#move first timepoint to the weight_mutation - as it was before the first dose of L-DOPA
weight_14 <- weight_ldopa %>% filter(timepoint == "ldopa-1") %>% select(-treatment)
weight_14$weeks_after_tamoxifen <- as.factor(weight_14$weeks_after_tamoxifen)
weight_mutation <- bind_rows(weight_mutation, weight_14)
weight_ldopa <- weight_ldopa %>% filter(timepoint != "ldopa-1")



####################################
#MUTATION

unique(weight_mutation$timepoint)
#find mice with NA's and remove them
missing_data <- weight_mutation %>% filter(is.na(weight)) %>% distinct(mouse_ID) #5 mice 
#exclude them and 5-3 (=2895)
weight_mutation <- weight_mutation %>%
  filter(!mouse_ID %in% c("2877", "3303", "3306", "3254", "3255", "2895")) 
weight_mutation$treatment <- str_replace_all(weight_mutation$treatment, c("L-DOPA" = "NA", "saline" = "NA"))
weight_mutation <- weight_mutation %>% replace_na(list(treatment = "NA")) #had to add it cause 1 mouse was NA and wasnt included!
weight_mutation <- as_tibble(weight_mutation)
unique(weight_mutation$treatment)

#check the completeness
weight_mutation %>% group_by(weeks_after_tamoxifen) %>% summarise(n_animals = length(unique(mouse_ID))) #24 animals everywhere

weight_mutation %>% group_by(mouse_ID) %>% summarise(n_weeks = paste(unique(weeks_after_tamoxifen), collapse = ",")) %>% print(n = nrow(.))

weight_mutation$weeks_after_tamoxifen <- as.factor(weight_mutation$weeks_after_tamoxifen)

summary(aov(weight ~ genotype*weeks_after_tamoxifen + Error(mouse_ID/weeks_after_tamoxifen), data = weight_mutation))
weight_mutation %>% group_by(weeks_after_tamoxifen, genotype) %>% summarise(n = length(unique(mouse_ID)))

###################################
#TREATMENT ==> new - mean for the whole period!

unique(weight_ldopa$timepoint)
#check NAs
weight_ldopa %>% filter(is.na(weight)) %>% distinct(cage_ID) #5 animals, all prematurely dead (1-2 extra in relation to mutation, but no mention of 2-1)
#remove them cause all they have is NAs, no real measuremnents after all (or 5-3 as an outlier!)

weight_ldopa <- weight_ldopa %>% filter(!cage_ID %in% c("1-2", "7-1", "7-2", "9-1", "9-2", "2-1", "5-3"))
weight_ldopa %>% filter(is.na(weight)) #no NAs after filtering
weight_ldopa %>% group_by(weeks_after_tamoxifen) %>% summarise(n = length(unique(mouse_ID))) #23 animals

#check the missingness of data for the treatment dataset
weight_ldopa %>% filter(is.na(weight)) %>% distinct(mouse_ID) #none



#rename to 'saline'
weight_ldopa$treatment <- str_replace(weight_ldopa$treatment, "saline", "saline")

# weight_ldopa$weeks_after_tamoxifen <- as.factor(weight_ldopa$weeks_after_tamoxifen)

# weight_ldopa <- weight_ldopa %>%
#   mutate(timepoint = case_when(
#     weeks_after_tamoxifen == "14" ~ "mean_ldopa14",
#     weeks_after_tamoxifen == "15" ~ "mean_ldopa15",
#   ))

weight_ldopa <- weight_ldopa %>%
  mutate(timepoint = "mean_ldopa14-15") %>%
  group_by(mouse_ID, genotype, treatment, cage, cage_ID, timepoint) %>%
  summarise(mean_weight = mean(weight))

#ANOVA
summary(aov(mean_weight ~ genotype*treatment, data = weight_ldopa)) #genotype

#treatment with L-DOPA
#summary(aov(weight~genotype*treatment*timepoint + Error(mouse_ID/timepoint), data = weight_ldopa)) #genotype, timepoint, genotype*timepoint, 3-way interaction

#mean for each week of treatment
# weight_ldopa <- weight_ldopa %>%
#   group_by(mouse_ID, weeks_after_tamoxifen, genotype, treatment, timepoint) %>%
#   summarise(mean_weight = mean(weight))
# 
# weight_ldopa$weeks_after_tamoxifen <- as.factor(weight_ldopa$weeks_after_tamoxifen)
# 
# summary(aov(mean_weight ~ genotype*treatment*weeks_after_tamoxifen + Error(mouse_ID/weeks_after_tamoxifen), data = weight_ldopa))
# weight_ldopa %>% group_by(genotype, treatment) %>% summarise(n = length(unique(mouse_ID)))

######
#join
weight_ldopa_sel <- weight_ldopa %>%
  rename(weight_plot = mean_weight)

weight_mutation_sel <- weight_mutation %>%
  rename(weight_plot = weight)

weight2_together <- bind_rows(weight_mutation_sel, weight_ldopa_sel)
# weight2_together %>% filter(timepoint == "mean_ldopa14")
weight2_together %>% distinct(timepoint)

weight2_together$group <- factor(paste(weight2_together$genotype, weight2_together$treatment),
                                        levels = c("control NA", "control saline", "control L-DOPA",
                                                   "mutant NA", "mutant saline", "mutant L-DOPA"))
weight2_together$timepoint <- factor(weight2_together$timepoint, 
                                     levels = c("tam_start",    
                                                "weight-1",
                                                "weight-2",
                                                "weight-3",     
                                                "weight-4",    
                                                "weight-5",     
                                                "weight-6",     
                                                "weight-7",     
                                                "ldopa-1",     
                                                "mean_ldopa14-15"))


weight2_labels <- c("tam_start" = "0",    
                    "weight-1" = "7",
                    "weight-2" = "8",
                    "weight-3" = "9",     
                    "weight-4" = "10",    
                    "weight-5" = "11",     
                    "weight-6" = "12",     
                    "weight-7" = "13",     
                    "ldopa-1" = "14",     
                    "mean_ldopa14-15" = "14-15")


weight2_together %>%
  group_by(timepoint) %>%
  t_test(weight_plot ~ group) %>%
  mutate(p_adjusted = p.adjust(p, "BH")) %>%
  print(n = nrow(.)) %>%
  filter(p_adjusted < 0.05) #ldopa-1

#new plot
weight2_together_plot_new <- weight2_together %>%
  group_by(weeks_after_tamoxifen, group, timepoint) %>%
  summarise(mean = mean(weight_plot),
            SEM = sd(weight_plot, na.rm = TRUE)/sqrt(n())) %>%
  ggplot(., aes(x = group, y = mean, fill = group, colour = group, group = weeks_after_tamoxifen)) +
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
  
  labs(y = "Body weight [g] \n",
       x = "Weeks after tamoxifen treatment") +
  theme_classic() +
  theme(aspect.ratio = 28, 
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
geom_text(data = weight2_together %>% filter( weeks_after_tamoxifen == "14"), label = "*", x = 2, y = 30, size = 10, inherit.aes = FALSE) +
  facet_row(~timepoint, scales = "free_x", strip.position = "bottom", space = "free", labeller = as_labeller(weight2_labels)) +
  coord_capped_cart(bottom ='none', left='none', ylim = c(15, 35),  gap = 0.05) 

ggsave(plot = weight2_together_plot_new, paste0("weight-preludium-publikacja.pdf"), dpi = 300, width = 11, height = 7, device = cairo_pdf)  

  
