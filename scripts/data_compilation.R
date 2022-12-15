library(dplyr)


# Load data ---------------------------------------------------------------

ts_results <- read.csv("raw_data/telomere_results_linregpcr.csv", row.names = 1) %>%
  rename(sample_id = sample) # telomere data by extract #
extractions <- read.csv("raw_data/extraction_list.csv") %>% rename(sample_id = extract_label) # Study F = 0 parents / 1 nestlings


# Physiology data
phys <- read.csv("raw_data/Captures_Hormone_Bleeding_Blood_DNA 11.18.2020.csv", stringsAsFactors = FALSE) %>%
  mutate(Site = case_when(Site == "Unit 1" ~ "Unit_1",
                          Site == "Unit 2" ~ "Unit_2",
                          TRUE ~ Site)) %>%
  rename(telo_nb = Telomere_Sample_nb) %>%
  mutate(Nestling_Fate = na_if(Nestling_Fate,"")) %>%
  mutate(band_date = paste(Individual_Band, year, day_of_year, sep = "_")) %>%  # add year
  mutate(site_nest = paste(Site, Nest, Exp_Year, sep = "_"))


# Conor's nest data
nestdata2018 <- read.table("raw_data/data_by_female.txt", sep = "\t", header = T) %>%
  mutate(site_nest = paste("Unit", unitbox, year, sep = "_")) %>%
  mutate(full_treatment = case_when(full_treatment == "Control_Dull" ~ "Dull_Control",
                                    full_treatment == "Predator_Dull" ~ "Dull_Predator",
                                    full_treatment == "Predator_Control" ~ "Control_Predator",
                                    TRUE ~ full_treatment)) %>%
  mutate (predator = case_when(full_treatment == "Control_Control" ~ "Control",
                               full_treatment == "Dull_Control" ~ "Control",
                               full_treatment == "Control_Predator" ~ "Predation",
                               full_treatment == "Dull_Predator" ~ "Predation",
                               full_treatment == "Control_Stress" ~ "Stress",
                               full_treatment == "Dull_Stress" ~ "Stress")) %>%
  mutate (dull = case_when(full_treatment == "Control_Control" ~ "Control",
                           full_treatment == "Dull_Control" ~ "Dull",
                           full_treatment == "Control_Predator" ~ "Control",
                           full_treatment == "Dull_Predator" ~ "Dull",
                           full_treatment == "Control_Stress" ~ "Control",
                           full_treatment == "Dull_Stress" ~ "Dull"))

# Nest data for all years
nests <- read.csv("raw_data/Nest_Records 11.18.2020.csv", stringsAsFactors = FALSE) %>%
  mutate(Site = case_when(Site == "CU1" ~ "Unit_1",
                          Site == "CU2" ~ "Unit_2",
                          Site == "CU4" ~ "Unit_4",
                          TRUE ~ Site)) %>%
  mutate(site_nest = paste(Site, Nest, Exp_Year, sep = "_"))

nestling_data <- read.delim("raw_data/data_by_nestling.txt", sep = "\t", header = T, fill = T)
dim(nestling_data)

# Compile data ------------------------------------------------------------

# Merge telomere results with extraction information
comp_data_test <- merge(extractions, ts_results, all.x = T, all.y = F) %>%
  filter(study_f == 1) %>% dplyr::select(-c(day_of_year, year, Capture_Date))
#mutate(band_date = paste(band, year, day_of_year, sep = "_"))  # 2019 samples don't have the day in them for some reason

# PHYS DATA
# Merge telomere results with physiology; join based on telo_nb
# Check and make sure telo_nbs have matches
#comp_data_test$telo_nb %in% phys$telo_nb #all true
intersect(names(comp_data_test), names(phys))
comp_data_test <- left_join(comp_data_test, phys)


# Check that individual band numbers match
comp_data_test$Individual_Band == comp_data_test$band #all true

# ADD NEST DATA
# Add in nest data based on the column "site_nest"
# First check that all the site nests exist in the nest df

comp_data_test <- inner_join(comp_data_test, nestdata2018)

# CROSS FOSTERING DATA
comp_data_test %>% filter(!band %in% nestling_data$band) %>% select(exclude, site_nest, experiment, full_treatment) # 18 rows missing data. Will do a left join
comp_data_test <- left_join(comp_data_test, nestling_data)

dim(comp_data_test)
# Inspect treatments
# Treatments in 2018 were color: dulled or control-dulled (manipulation of female breast feathers)
# x stress: simulated predation by mink, flight handicapping, or control
# Treatments in 2019 were color x stress (but flight handicapping was excluded)
# Desired treatments: Dull_Control, Dull_Predator, Dull_Stress, Control_Control,
# Control_Predator, Control_Stress

table(comp_data_test$full_treatment)
table(comp_data_test$full_treatment, comp_data_test$year)

comp_data_test <-
  comp_data_test %>%
  mutate (Nestling_Fate = case_when (Nestling_Fate == "Dead" ~ "Died",
                                     TRUE ~ Nestling_Fate)) %>%
  filter(exclude != "Yes")

# Double check that all the treatments look good
table(comp_data_test$full_treatment, comp_data_test$predator)
table(comp_data_test$full_treatment, comp_data_test$dull)

# do some sanity checks to make sure nest metadata matches
nest_summary <- table(comp_data_test$site_nest) %>% t %>% as.data.frame %>% rename(site_nest = Var2)
nest_summary <- merge(nest_summary, select(nests, site_nest, Brood_Size_Hatching, Brood_Size_Day6)) %>% mutate(check = Brood_Size_Day6 == Freq)

filter(nest_summary, Freq > Brood_Size_Hatching) # 0 nests have more nestlings than brood size at hatching
filter(nest_summary, Freq > Brood_Size_Day6) #0 nests have more nestlings than brood size at day 6
filter(nest_summary, Freq < Brood_Size_Day6) #32 nests have fewer blood samples than brood size, presumably blood sample not taken and/or telo didn't work.


table(comp_data_test$in_xfost_exp) # all rows in x foster experiment
comp_data_test$Bleed1_Glucose == comp_data_test$d12_bgluc # No glucose in 2018 but otherwise these match

# grab relevant columns
comp_data <- comp_data_test %>% select(band, year, site_nest, ts_ratio1, ts_ratio2,
                                       ts_ratio3, ts_ratio4, Age, Mass, Flat_Wing,
                                       Bleed1_CORT_corrected_value, Bleed1_Glucose,
                                       Bleed2_CORT_corrected_value, Bleed2_Glucose,
                                       Bleed3_CORT_corrected_value, Bleed3_Glucose,
                                       experiment, color, fbbright,
                                       predator, dull, gen_dad,
                                       full_treatment, soc_mom, gen_mom, raised_nest,
                                       sex, d12_base, d12_stress, d12_dex, d15_acth,
                                       Current_Brood_Size,
                                       maxbrood, Nestling_Fate, sex
)

comp_data <- filter(comp_data, predator !="Stress") %>% droplevels()
dim(comp_data)

# Results of nest study  --------------------------------------------------

nestdata2018 <- nestdata2018 %>% mutate(full_treatment = case_when(full_treatment == "Control_Dull" ~ "Dull_Control",
                                                                   full_treatment == "Predator_Dull" ~ "Dull_Predator",
                                                                   full_treatment == "Predator_Control" ~ "Control_Predator",
                                                                   TRUE ~ full_treatment)) %>%
  mutate (predator = case_when(full_treatment == "Control_Control" ~ "Control",
                               full_treatment == "Dull_Control" ~ "Control",
                               full_treatment == "Control_Predator" ~ "Predation",
                               full_treatment == "Dull_Predator" ~ "Predation",
                               full_treatment == "Control_Stress" ~ "Stress",
                               full_treatment == "Dull_Stress" ~ "Stress")) %>%
  mutate (dull = case_when(full_treatment == "Control_Control" ~ "Control",
                           full_treatment == "Dull_Control" ~ "Dull",
                           full_treatment == "Control_Predator" ~ "Control",
                           full_treatment == "Dull_Predator" ~ "Dull",
                           full_treatment == "Control_Stress" ~ "Control",
                           full_treatment == "Dull_Stress" ~ "Dull")) %>%
  filter(exclude != "Yes", predator != "Stress")

intersect(colnames(nestling_data), colnames(nestdata2018))
nestling_data <- left_join(nestling_data, nestdata2018) %>% filter(predator != "Stress")
dim(nestling_data)


# Summary stats telomeres  ------------------------------------------------

head(comp_data_test)

mean(comp_data_test$used_260_280, na.rm = T)
mean(comp_data_test$used_260_230)

range(comp_data_test$used_260_280)
range(comp_data_test$used_260_230)

fivenum(comp_data_test$eff_gapdh)
fivenum(comp_data_test$eff_telo)
fivenum(comp_data_test$plate_eff_gapdh)
fivenum(comp_data_test$plate_eff_telo)

mean(comp_data_test$eff_gapdh, na.rm = T)
mean(comp_data_test$eff_telo, na.rm = T)
mean(comp_data_test$plate_eff_gapdh, na.rm = T)
mean(comp_data_test$plate_eff_telo,na.rm = T)


# write out data ----------------------------------------------------------

write.csv(comp_data, file = "compiled_data/comp_data.csv", row.names = F)
write.csv(nestling_data, file = "compiled_data/nestling_data.csv", row.names = F)
write.csv(nestdata2018, file = "compiled_data/nestdata2018.csv", row.names = F)
