#PRELUDIUM

library(tidyverse)
library(lubridate)
library(lemon)
library(readxl)
library(patchwork)
library(ggforce)
library(gridExtra)
library(extrafont)
library(RColorBrewer)
library(rstatix)

#import files
genotypes_preludium <- read.csv("genotypes_preludium.csv", sep = ";") %>% rename(Animal = "mouse_ID")
genotypes_preludium$Animal <- as.character(as.numeric(genotypes_preludium$Animal))
list.files(pattern = "xlsx")
import_coh12 <- as.data.frame(read_excel("10-09-2021-AR-TIF-KOHORTA1-SESJA1_RunStatistics.xlsx"))
import_coh13 <- as.data.frame(read_excel("27-09-2021 ANIA TIF KOHORTA 1_RunStatistics-ALL PARAMETERS.xlsx"))
import_coh14 <- as.data.frame(read_excel("8-10-2021 TIF-DAT ANIA kohorta 1 2021 L-DOPA_RunStatistics.xlsx")) #L-DOPA
import_coh22 <- as.data.frame(read_excel("2-12-2021 kohorta 2 TIF DAT sesja 2_RunStatistics.xlsx"))
import_coh23 <- as.data.frame(read_excel("17-12-2021 kohorta 2 Ania TIF-DAT_RunStatistics.xlsx"))
import_coh24 <- as.data.frame(read_excel("31-12-2021 kohorta 2 tif-dat l-dopa_RunStatistics.xlsx")) #L-DOPA

parameters_list <- c("BOS",
                     "StrideLength",
                     "Swing_",
                     "SwindSpeed",
                     "StepCycle",
                     "DutyCycle",
                     "Stance",
                     "_MaxIntensity_",
                     "_MeanIntensity_",
                     "Cadence",
                     "Stand_",
                     "PrintArea",
                     "Average_Speed",
                     "RegularityIndex",
                     "BodySpeedVariation",
                     "MaxContactArea",
                     "StepSequence",
                     "Couplings",
                     "PrintPositions",
                     "Experiment",
                     "Animal",
                     "NumberOfPatterns",
                     "WalkWay_Length_(cm)",
                     "WalkWay_Width_(cm)")

#limit the df's and connect them
coh12 <- import_coh12 %>%
select(contains(parameters_list)) %>%
  select(!contains("CStat")) %>%
  select(!contains("FP_")) %>%
  select(!contains("HP_")) %>%
  filter(`StepSequence_NumberOfPatterns` > 0) %>% #criteria
  full_join(genotypes_preludium, "Animal") %>%
  filter(cohort == 1) %>%
  mutate(across(.cols = everything(),
                .fns = ~ str_replace(., "^-$", "NA"))) %>%
  mutate(session_date = "2021-09-10")

coh13 <- import_coh13 %>%
  select(contains(parameters_list)) %>%
  select(!contains("CStat")) %>%
  select(!contains("FP_")) %>%
  select(!contains("HP_")) %>%
  filter(`StepSequence_NumberOfPatterns` > 0) %>% #criteria
  full_join(genotypes_preludium, "Animal") %>%
  filter(cohort == 1) %>%
  mutate(across(.cols = everything(),
                .fns = ~ str_replace(., "^-$", "NA"))) %>%
  mutate(session_date = "2021-09-27")

coh14 <- import_coh14 %>%
  select(contains(parameters_list)) %>%
  select(!contains("CStat")) %>%
  select(!contains("FP_")) %>%
  select(!contains("HP_")) %>%
  filter(`StepSequence_NumberOfPatterns` > 0) %>% #criteria
  full_join(genotypes_preludium, "Animal") %>%
  filter(cohort == 1) %>%
  mutate(across(.cols = everything(),
                .fns = ~ str_replace(., "^-$", "NA"))) %>%
  mutate(session_date = "2021-10-8")


cohort1 <- bind_rows(coh12, coh13, coh14) %>%
  mutate(Days = round(as.numeric(difftime(as.POSIXct(strptime(session_date, "%Y-%m-%d")), as.POSIXct(strptime("26/6/2021","%d/%m/%Y"), tz = "CET"), units = "days"))),
         Weeks = round(as.numeric(difftime(as.POSIXct(strptime(session_date, "%Y-%m-%d")), as.POSIXct(strptime("26/6/2021","%d/%m/%Y"), tz = "CET"), units = "weeks"))))

unique(cohort1$`WalkWay_Length_(cm)`)

coh22 <- import_coh22 %>%
  select(contains(parameters_list)) %>%
  select(!contains("CStat")) %>%
  select(!contains("FP_")) %>%
  select(!contains("HP_")) %>%
  filter(`StepSequence_NumberOfPatterns` > 0) %>% #criteria
  full_join(genotypes_preludium, "Animal") %>%
  filter(cohort == 2) %>%
  mutate(across(.cols = everything(),
                .fns = ~ str_replace(., "^-$", "NA"))) %>%
  mutate(session_date = "2021-12-2")

coh23 <- import_coh23 %>%
  select(contains(parameters_list)) %>%
  select(!contains("CStat")) %>%
  select(!contains("FP_")) %>%
  select(!contains("HP_")) %>%
  filter(`StepSequence_NumberOfPatterns` > 0) %>% #criteria
  full_join(genotypes_preludium, "Animal") %>%
  filter(cohort == 2) %>%
  mutate(across(.cols = everything(),
                .fns = ~ str_replace(., "^-$", "NA"))) %>%
  mutate(session_date = "2021-12-17")

coh24 <- import_coh24 %>%
  select(contains(parameters_list)) %>%
  select(!contains("CStat")) %>%
  select(!contains("FP_")) %>%
  select(!contains("HP_")) %>%
  filter(`StepSequence_NumberOfPatterns` > 0) %>% #criteria
  full_join(genotypes_preludium, "Animal") %>%
  filter(cohort == 2) %>%
  mutate(across(.cols = everything(),
                .fns = ~ str_replace(., "^-$", "NA"))) %>%
  mutate(session_date = "2021-12-31")

cohort2 <- bind_rows(coh22, coh23, coh24) %>% 
  mutate(Days = round(as.numeric(difftime(as.POSIXct(strptime(session_date, "%Y-%m-%d")), as.POSIXct(strptime("18/9/2021","%d/%m/%Y"), tz = "CET"), units = "days"))),
         Weeks = round(as.numeric(difftime(as.POSIXct(strptime(session_date, "%Y-%m-%d")), as.POSIXct(strptime("18/9/2021","%d/%m/%Y"), tz = "CET"), units = "weeks"))))

unique(cohort2$Days) #ok

unique(cohort2$`WalkWay_Length_(cm)`)
unique(cohort1$`WalkWay_Length_(cm)`) #the difference is 0.05121232 cm so the difference between the calibration settings is not significant

#merge all dataframes
catwalk_preludium <- bind_rows(cohort1, cohort2) %>%
  filter(cage_ID != "5-3") #exclude the mouse with uncertain genotype

#get rid of problematic column names and average
catwalk_preludium.average <- catwalk_preludium %>% 
  rename("LF_PrintArea_(cm)_Mean" = `LF_PrintArea_(cm²)_Mean`,
         "RF_PrintArea_(cm)_Mean" = `RF_PrintArea_(cm²)_Mean`,
         "RH_PrintArea_(cm)_Mean" = `RH_PrintArea_(cm²)_Mean`,
         "LH_PrintArea_(cm)_Mean" = `LH_PrintArea_(cm²)_Mean`,
         "RF_MaxContactArea_(cm)_Mean" = `RF_MaxContactArea_(cm²)_Mean`,
         "RH_MaxContactArea_(cm)_Mean" = `RH_MaxContactArea_(cm²)_Mean`,
         "LF_MaxContactArea_(cm)_Mean" = `LF_MaxContactArea_(cm²)_Mean`,
         "LH_MaxContactArea_(cm)_Mean" = `LH_MaxContactArea_(cm²)_Mean`) %>%
  unnest(1:79) %>%
  mutate_at(1:79, as.numeric) %>%
  mutate(StrideLength_H = rowMeans(select(., contains("H_StrideLength")), na.rm = TRUE),
         Swing_H = rowMeans(select(., contains("H_Swing_")), na.rm = TRUE),
         StepCycle_H = rowMeans(select(., contains("H_StepCycle_")), na.rm = TRUE),
         DutyCycle_H = rowMeans(select(., contains("_DutyCycle_")), na.rm = TRUE),
         SingleStance_H = rowMeans(select(., contains("H_SingleStance_")), na.rm = TRUE),
         InitialDualStance_H = rowMeans(select(., contains("H_InitialDualStance_")), na.rm = TRUE),
         TerminalDualStance_H = rowMeans(select(., contains("H_TerminalDualStance_")), na.rm = TRUE),
         MaxIntensity_H = rowMeans(select(., contains("H_MaxIntensity_")), na.rm = TRUE),
         MeanIntensity_H = rowMeans(select(., contains("H_MeanIntensity_")), na.rm = TRUE),
         Stand_H = rowMeans(select(., contains("H_Stand_")), na.rm = TRUE),
         PrintArea_H = rowMeans(select(., contains("H_PrintArea_")), na.rm = TRUE),
         BodySpeedVariation_H = rowMeans(select(., contains("H_BodySpeedVariation_")), na.rm = TRUE),
         MaxContactArea_H = rowMeans(select(., contains("H_MaxContactArea_")), na.rm = TRUE),
         StrideLength_F = rowMeans(select(., contains("F_StrideLength")), na.rm = TRUE),
         Swing_F = rowMeans(select(., contains("F_Swing_")), na.rm = TRUE),
         StepCycle_F = rowMeans(select(., contains("F_StepCycle_")), na.rm = TRUE),
         DutyCycle_F = rowMeans(select(., contains("_DutyCycle_")), na.rm = TRUE),
         SingleStance_F = rowMeans(select(., contains("F_SingleStance_")), na.rm = TRUE),
         InitialDualStance_F = rowMeans(select(., contains("F_InitialDualStance_")), na.rm = TRUE),
         TerminalDualStance_F = rowMeans(select(., contains("F_TerminalDualStance_")), na.rm = TRUE),
         MaxIntensity_F = rowMeans(select(., contains("F_MaxIntensity_")), na.rm = TRUE),
         MeanIntensity_F = rowMeans(select(., contains("F_MeanIntensity_")), na.rm = TRUE),
         Stand_F = rowMeans(select(., contains("F_Stand_")), na.rm = TRUE),
         PrintArea_F = rowMeans(select(., contains("F_PrintArea_")), na.rm = TRUE),
         BodySpeedVariation_F = rowMeans(select(., contains("F_BodySpeedVariation_")), na.rm = TRUE),
         MaxContactArea_F = rowMeans(select(., contains("F_MaxContactArea_")), na.rm = TRUE)) %>%
  select(-starts_with(c("LH", "RH", "RF", "LF")))
  
catwalk_preludium_long <- catwalk_preludium.average %>% 
  rename(NumberOfPatterns = `StepSequence_NumberOfPatterns`,
         RegularityIndex = `StepSequence_RegularityIndex_(%)`) %>%
  pivot_longer(c(1:27, 42:67), names_to = "parameter", values_to = "value") %>% 
  select(-Experiment, -session_date, -`WalkWay_Length_(cm)`, -`WalkWay_Width_(cm)`, -birthday, -cage, -cage_ID) %>%
  drop_na(value)

#which animals are prematurely dead
killed <- genotypes_preludium %>% filter(history == "killed") %>% distinct(Animal)

#see which animals don't have the full observations for all weeks
missing_cp <- catwalk_preludium_long  %>% group_by(Animal) %>% summarise(n_obs = length(unique(Weeks))) %>% filter(n_obs < 3) #6 animals
catwalk_preludium_long %>% filter(Animal %in% missing_cp$Animal) %>%  group_by(Animal) %>% summarise(sessions = paste(unique(Weeks), collapse = ", "))

#2875, 3254, 3255, 3303, 3306 to be removed from the mutation dataset (2875 stays in treatment)
#3254, 3255, 3303, 3306, 2876 to be removed from the treatment dataset

#are there animals that lack only some measurements/parameters in some sessions?
catwalk_preludium_long %>% group_by(Animal, parameter) %>% summarise(n_Days = length(unique(Days))) %>% filter(n_Days < 3) %>% distinct(Animal)
#no, just the 6 problematic mentioned above - no need to remove by parameter from analysis

#prepare ANOVAs
catwalk_preludium_long$Days <- as.factor(catwalk_preludium_long$Days)
catwalk_preludium_long$Weeks <- as.factor(catwalk_preludium_long$Weeks)
catwalk_preludium_long <- catwalk_preludium_long %>% filter(parameter != "NumberOfPatterns") #this one parameter is unnecessary
class(catwalk_preludium_long$Animal)

#############################################
#WEEKS


####
####
#mutation
catwalk_preludium.mutation <- catwalk_preludium_long %>% filter(Weeks != "15") %>% 
  filter(!Animal %in% c("2875", "3254", "3255", "3303", "3306")) %>% #Animals without at least 1 session
  group_by(Animal, genotype, Weeks, parameter) %>% summarise(mean_value = mean(value, na.rm = TRUE))

# #ANOVAs - list
lapply(split(catwalk_preludium.mutation, catwalk_preludium.mutation$parameter), function(i){
  summary(aov(mean_value ~ genotype*Weeks + Error(Animal/Weeks), data = i))
})
#save list to one file
anova_list_catwalk_preludium.mutation = lapply(split(catwalk_preludium.mutation, catwalk_preludium.mutation$parameter), function(i){
  summary(aov(mean_value ~ genotype*Weeks + Error(Animal/Weeks), data = i))
})
capture.output(anova_list_catwalk_preludium.mutation, file = "anova_list_catwalk_preludium.mutation.csv")


#all results in 1 tibble
aov_catwalk_preludium.mutation <- catwalk_preludium.mutation %>%
  nest(data = c(Animal, genotype, Weeks, mean_value)) %>%
  mutate(model = map(data, ~aov(mean_value ~ genotype*Weeks + Error(Animal/Weeks), .)),
         tidy = map(model, broom::tidy)) %>%
  select(parameter, tidy) %>%
  unnest(tidy) %>%
  mutate(p_adjusted = p.adjust(p.value, "BH"))

#View(aov_catwalk_preludium.mutation %>% filter(p.value < 0.05))

#View(aov_catwalk_preludium.mutation %>% mutate(p_adjusted = p.adjust(p.value, "BH")) %>% filter(p_adjusted < 0.05))



####
####
#treatment

catwalk_preludium.treatment <- catwalk_preludium_long %>% filter(Weeks %in% c( "15")) %>%
  group_by(Animal, genotype, treatment,  parameter) %>% summarise(mean_value = mean(value, na.rm = TRUE))

unique(catwalk_preludium.treatment$Animal) #all good animals in it --> no need to remove, because they are already not present

#ANOVAs - list
lapply(split(catwalk_preludium.treatment, catwalk_preludium.treatment$parameter), function(i){
  summary(aov(mean_value ~ genotype*treatment, data = i))
})
#save to a file
anova_list_catwalk_preludium.treatment = lapply(split(catwalk_preludium.treatment, catwalk_preludium.treatment$parameter), function(i){
  summary(aov(mean_value ~ genotype*treatment, data = i))
})
capture.output(anova_list_catwalk_preludium.treatment, file = "anova_list_catwalk_preludium.treatment.csv")

#all results in 1 tibble
aov_catwalk_preludium.treatment <- catwalk_preludium.treatment %>%
  nest(data = c(Animal, genotype, treatment,  mean_value)) %>%
  mutate(model = map(data, ~aov(mean_value ~ genotype*treatment, .)),
         tidy = map(model, broom::tidy)) %>%
  select(parameter, tidy) %>%
  unnest(tidy) %>%
  mutate(p_adjusted = p.adjust(p.value, "BH"))

#View(aov_catwalk_preludium.treatment %>% filter(p.value < 0.05))

#View(aov_catwalk_preludium.treatment%>% mutate(p_adjusted = p.adjust(p.value, "BH")) %>% filter(p_adjusted < 0.05))



#NUMBER OF ANIMALS
catwalk_preludium.mutation %>% group_by(genotype) %>% summarise(n = length(unique(Animal)))
catwalk_preludium.treatment %>% group_by(genotype, treatment) %>% summarise(n = length(unique(Animal)))

unique(catwalk_preludium_long$Weeks)

####significant p-values
aov_catwalk_preludium.mutation %>% filter(p.value < 0.05 & str_detect(term, "genotype"))
aov_catwalk_preludium.treatment %>% filter(p.value < 0.05 & str_detect(term, "genotype"))
aov_catwalk_preludium.treatment %>% filter(p.value < 0.05 & str_detect(term, "treatment"))
