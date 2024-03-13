# McNew et al. 2023.
# Developmental stage-dependent effects of perceived predation
# risk on nestling tree swallows
# Response to reviewers additional analyses

library(rptR)
library(dplyr)

# read data
raw_telos <- read.csv("raw_data/raw_compiled_telomere_data_repeatability.csv")

# Create dataframes of gapdh and telomere repeated samples
head(raw_telos)
dup_gapdh <- raw_telos %>%
  filter(pcr_primer == "gapdh") %>%
  group_by(sample) %>%
  filter(n() > 1, sample !="H20") %>%
  droplevels() %>%
  mutate(index = 1:n()) %>%
  mutate(sample_n = paste(sample, index, sep = "_")) %>%
  select(sample_n, Cq, plate_eff, Cq_gold) %>%
  rename(Cq_gapdh = Cq, plate_eff_gapdh = plate_eff, Cq_gold_gapdh = Cq_gold)


rpt(Cq_gapdh ~ (1|sample),
    data = dup_gapdh, grname = "sample", datatype = "Gaussian", nboot= 1000, npermut = 0)

# Find repeated telomere samples
dup_telo <- raw_telos %>%
  filter(pcr_primer == "telo") %>%
  group_by(sample) %>%
  filter(n() > 1, sample !="H20") %>% # grab samples that were run more than once
  droplevels() %>%
  mutate(index = 1:n()) %>% #create an index of the number of times a sample was run
  mutate(sample_n = paste(sample, index, sep = "_")) %>%  # create sample x run ID
  select(sample_n, Cq, plate_eff, Cq_T_087) %>% # pull columns for TS calculation
  rename(Cq_telo = Cq, plate_eff_telo = plate_eff, Cq_T_087_telo = Cq_T_087)
View(dup_gapdh)

rpt(Cq_telo ~ (1|sample),
    data = dup_telo, grname = "sample", datatype = "Gaussian", nboot= 1000, npermut = 0)


rep_bind <- inner_join(dup_gapdh, dup_telo)
rep_bind <- rep_bind %>% mutate(ts_ratio = (plate_eff_telo ^ (Cq_T_087_telo - Cq_telo))/
                               (plate_eff_gapdh ^ (Cq_gold_gapdh - Cq_gapdh)))

# Run repeatability
rpt(ts_ratio ~ (1|sample),
    data = rep_bind, grname = "sample", datatype = "Gaussian", nboot= 100, npermut = 0)
View(rep_bind)
