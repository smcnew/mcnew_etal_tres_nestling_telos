# New script 2022 analyze nestlings
# Last update: Dec 2022
# Sabrina McNew

library(lme4)
library(lmerTest)
library(sjPlot)
library(tidyr)
library(coxme) # for survival analysis
library(survminer)
library(survival)

# plotting packages
#devtools::install_github("thomasp85/patchwork")
#devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
library(ggpattern) # make stripes on violin plots
library(patchwork) # plot layouts
library(ggplot2)
library(scales)

library(dplyr)

# data --------------------------------------------------------------------

comp_data <- read.csv(file = "compiled_data/comp_data.csv")
nestling_data <- read.csv(file = "compiled_data/nestling_data.csv")
nestdata2018 <- read.csv(file = "compiled_data/nestdata2018.csv")
d18 <- filter(comp_data, year == "2018")
d19 <- filter(comp_data, year == "2019")

# sample sizes per year


table(nestdata2018$full_treatment, nestdata2018$year)

filter(nestdata2018, year == 2019) %>% group_by(dull, predator)  %>% tally
filter(nestdata2018, year == 2019) %>%  group_by(dull) %>% tally
head(nestdata2018)

# Graphing parameters -----------------------------------------------------
ggthemr(palette = 'light', layout = "clean", text_size = 12, spacing = 1)
#ggthemr_reset() # go back to ggplot defaults

# Optional plot parameters for ggpattern:
# pattern_size = width of stripes
# pattern_spacing = how close stripes are
# scale_fill_manual(values = cols) # change the colors of the geoms
# #
# Wing and headbill ~ treatment  ---------------------------------------------

# effects in 2018 of predator on wing and head; no difference in 2019
mod5 <- lmer(d12_wing ~ predator + dull + scale(fbbright) + fage + (1|site_nest) + (1|gen_mom),
             data = nestling_data[nestling_data$year == "2018",]) #%>% summary
mod6 <- lmer(d12_wing ~ predator + dull* scale(fbbright) + fage+  (1|site_nest) + (1|gen_mom),
             data = nestling_data[nestling_data$year == "2019",]) #%>% summary


mod7 <- lmer(d12_head ~ predator + dull + scale(fbbright) + fage +  (1|site_nest) ,
             data = nestling_data[nestling_data$year == "2018",]) #%>% summary
mod8 <- lmer(d12_head ~ predator + dull * scale(fbbright) + fage +  (1|site_nest) + (1|gen_mom),
             data = nestling_data[nestling_data$year == "2019",])

tab_model(mod5, mod6,
          dv.labels = c("Wing length (2018)", "Wing length (2019)"),
          pred.labels = c("Intercept (Controls)", "Predation",
                          "Dull", "Brightness (scaled)", "Female age", "Dull x Brightness"))

tab_model(mod7, mod8,
          dv.labels = c("Head-bill (2018)", "Head-bill (2019)"),
          pred.labels = c("Intercept (Controls)", "Predation",
                          "Dull", "Brightness (scaled)", "Female age", "Dull x Brightness"))



# Nestling size plots  ---------------------------------------------------


# Wing 2018
pwing2018 <- nestling_data %>% filter(year == "2018") %>%
  ggplot(., aes(
    x = predator,
    y = d12_wing,
    fill = predator,
    pattern = dull
  )) +
  ylim(15, 70)+
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  labs(
    x = "Stress treatment",
    y = "Wing length (mm)",
    pattern = "Signal treatment",
    fill = "Predator treatment",
    title = "2018"
  ) +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)


pwing2019 <- nestling_data %>% filter(year == "2019") %>%
  ggplot(., aes(
    x = predator,
    y = d12_wing,
    fill = predator,
    pattern = dull
  )) +
  ylim(15,70)+
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  labs(
    x = "Stress treatment",
    y = "Wing length (mm)",
    pattern = "Signal treatment",
    fill = "Predator treatment",
    title = "2019"
  ) +
  theme(legend.key.size = unit(0.9, 'cm'), plot.title = element_text(hjust=0.5)) +
  guides(pattern = guide_legend(order = 2, override.aes = list(pattern = c("none", "stripe"))),
         fill = "none" #guide_legend(order = 1, override.aes = list(pattern = "none")),
  ) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

phead2018 <- nestling_data %>% filter(year == "2018") %>%
  ggplot(., aes(
    x = predator,
    y = d12_head,
    fill = predator,
    pattern = dull
  )) +
  ylim(18,30)+
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  labs(
    x = "Stress treatment",
    y = "Head + bill (mm)",
    pattern = "Signal treatment",
    fill = "Predator treatment"
  ) +
  theme(legend.position = "none") +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)


phead2019 <- nestling_data %>% filter(year == "2019") %>%
  ggplot(., aes(
    x = predator,
    y = d12_head,
    fill = predator,
    pattern = dull
  )) +
  ylim(18,30)+
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  labs(
    x = "Stress treatment",
    y = "Head + bill length (mm)",
    pattern = "Signal treatment",
    fill = "Predator treatment"
  ) +
  theme(legend.key.size = unit(0.9, 'cm')) +
  guides(pattern = guide_legend(order = 2, override.aes = list(pattern = c("none", "stripe"))),
         fill = "none" # guide_legend(order = 1, override.aes = list(pattern = "none")),
  ) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

pdf("output_plots/headwing.pdf")
(pwing2018 + pwing2019)/
  (phead2018 + phead2019 ) +  plot_annotation(tag_levels = 'A')
dev.off()
# Nestling mass ~ treatment ----------------------------------------------------
# Yes effect of mass early

mod1 <- lm(d6_av_mass ~ predator + dull * scale(fbbright) + fage, data = nestdata2018[nestdata2018$year == "2018",])
mod2 <- lm(d6_av_mass ~ predator + dull + scale(fbbright) + fage, data = nestdata2018[nestdata2018$year == "2019",])
tab_model(mod1, mod2, dv.labels = c("Av. 6 day old mass (2018)", "Av. 6. day old mass (2019)"),
          pred.labels = c("Intercept (Controls)", "Predation",
                          "Dull", "Brightness (scaled)", "Female age", "Dull x Brightness"))

#No effects of mass later
mod3 <- lmer(d12_mass ~ predator + dull  +scale(fbbright) + fage + (1|site_nest),
     data = nestling_data[nestling_data$year == "2018",])
mod4 <- lmer(d12_mass ~  predator + dull * scale(fbbright) + fage  + (1|site_nest),
     data = nestling_data[nestling_data$year == "2019",])
tab_model(mod3, mod4, dv.labels = c("Day 12 mass 2018", "Day 12 mass 2019"),
          pred.labels = c("Intercept (Controls)", "Predation",
                          "Dull", "Brightness (scaled)", "Female age", "Dull x Brightness"))

#
# Supplemental plots mass x brightness  -----------------------------------
pmass6_bright18 <- nestdata2018 %>% filter(year == 2018) %>%
  ggplot(aes(x = fbbright, y = d6_av_mass, color = dull)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  labs(x = "Female brightness (pre-treatment)",
       y = "Day 6 average nestling mass",
       color = "Signal treatment",
       title = "2018")

pmass6_bright19 <- nestdata2018 %>% filter(year == 2019) %>%
  ggplot(aes(x = fbbright, y = d6_av_mass, color = dull)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  labs(x = "Female brightness (pre-treatment)",
       y = "Day 6 average nestling mass",
       color = "Signal treatment",
       title = "2019")

pmass_bright18 <- nestling_data %>% filter(year == 2018) %>%
  ggplot(aes(x = fbbright, y = d12_mass, color = dull)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  labs(x = "Female brightness (pre-treatment)",
       y = "Day 12 nestling mass",
       color = "Signal treatment",
       title = "2018")

pmass_bright19 <- nestling_data %>% filter(year == 2019) %>%
  ggplot(aes(x = fbbright, y = d12_mass, color = dull)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  labs(x = "Female brightness (pre-treatment)",
       y = "Day 12 nestling mass length",
       color = "Signal treatment",
       title = "2019")


pdf("output_plots/mass_bright.pdf", width = 9, height = 7)
(pmass6_bright18 + pmass6_bright19) /
  (pmass_bright18 + pmass_bright19) +  plot_annotation(tag_levels = 'A')
dev.off()

# Box plot broken up by initial brightness
nestling_data$brightcat <- cut(nestling_data$fbbright, 3, labels = c("dull", "medium", "bright"))
nestling_data <- nestling_data %>% group_by(year) %>%
   mutate(brightcat = cut(fbbright, 3, labels = c("dull", "average", "bright"))) %>%
  ungroup()


#inspect categories
nestling_data %>% filter(year == 2018) %>%
  ggplot(aes(x = fbbright, fill = brightcat)) + geom_histogram()

ps1 <- nestling_data %>% filter(year == 2018, !is.na(brightcat)) %>%
  ggplot(aes(x = brightcat, y = d12_wing, fill = dull)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Female brightness (pre-treatment)",
    y = "Nestling wing length (mm)",
    fill = "Signal treatment",
    title = "2018"
  ) +
  scale_fill_manual(values = c("#ffb84d" ,"#aaa488")) +
  theme(plot.title = element_text(hjust=0.5), legend.key.size = unit(0.9, 'cm')) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

ps2 <- nestling_data %>% filter(year == 2019) %>%
  ggplot(aes(x = brightcat, y = d12_mass, fill = dull)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Female brightness (pre-treatment)",
    y = "Nestling wing length (mm)",
    fill = "Signal treatment",
    title = "2019"
  ) +
  scale_fill_manual(values = c("#ffb84d" ,"#aaa488")) +
  theme(plot.title = element_text(hjust=0.5), legend.key.size = unit(0.9, 'cm')) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)


ps3 <- nestling_data %>% filter(year == 2018, !is.na(brightcat)) %>%
  ggplot(aes(x = brightcat, y = d12_head, fill = dull)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Female brightness (pre-treatment)",
    y = "Nestling head + bill length (mm)",
    fill = "Signal treatment",
    title = "2018"
  ) +
  scale_fill_manual(values = c("#ffb84d" ,"#aaa488")) +
  theme(plot.title = element_text(hjust=0.5), legend.key.size = unit(0.9, 'cm')) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

ps4 <- nestling_data %>% filter(year == 2019, !is.na(brightcat)) %>%
  ggplot(aes(x = brightcat, y = d12_head, fill = dull)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Female brightness (pre-treatment)",
    y = "Nestling head + bill length (mm)",
    fill = "Signal treatment",
    title = "2019"
  ) +
  scale_fill_manual(values = c("#ffb84d" ,"#aaa488")) +
  theme(plot.title = element_text(hjust=0.5), legend.key.size = unit(0.9, 'cm')) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)


ps5 <- nestling_data %>% filter(year == 2018, !is.na(brightcat)) %>%
  ggplot(aes(x = brightcat, y = d12_mass, fill = dull)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Female brightness (pre-treatment)",
    y = "Nestling mass (g)",
    fill = "Signal treatment",
    title = "2018"
  ) +
  scale_fill_manual(values = c("#ffb84d" ,"#aaa488")) +
  theme(plot.title = element_text(hjust=0.5), legend.key.size = unit(0.9, 'cm')) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

ps6 <- nestling_data %>% filter(year == 2019) %>%
  ggplot(aes(x = brightcat, y = d12_mass, fill = dull)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Female brightness (pre-treatment)",
    y = "Nestling mass (g)",
    fill = "Signal treatment",
    title = "2019"
  ) +
  scale_fill_manual(values = c("#ffb84d" ,"#aaa488")) +
  theme(plot.title = element_text(hjust=0.5), legend.key.size = unit(0.9, 'cm')) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)
pdf("output_plots/sizexbrightnessxdulling2.pdf", width = 8, height = 12)
(ps1 + ps2) /
(ps3 + ps4) /
(ps5 +ps6)
dev.off()

# Supplemental plots of brightness and dulling interactions  -------------

pwing_bright18 <- nestling_data %>% filter(year == 2018) %>%
  ggplot(aes(x = fbbright, y = d12_wing, color = dull)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  labs(x = "Female brightness (pre-treatment)",
       y = "Day 12 nestling wing length",
       color = "Signal treatment",
       title = "2018")

pwing_bright19 <- nestling_data %>% filter(year == 2019) %>%
  ggplot(aes(x = fbbright, y = d12_wing, color = dull)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  labs(x = "Female brightness (pre-treatment)",
       y = "Day 12 nestling wing length",
       color = "Signal treatment",
       title = "2019")


phead_bright18 <- nestling_data %>% filter(year == 2018) %>%
  ggplot(aes(x = fbbright, y = d12_head, color = dull)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  labs(x = "Female brightness (pre-treatment)",
       y = "Day 12 nestling head+bill length",
       color = "Signal treatment",
       title = "2018")

phead_bright19 <- nestling_data %>% filter(year == 2019) %>%
  ggplot(aes(x = fbbright, y = d12_head, color = dull)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  labs(x = "Female brightness (pre-treatment)",
       y = "Day 12 nestling head+bill length",
       color = "Signal treatment",
       title = "2019")


pdf("output_plots/wing_bright.pdf", width = 9, height = 7)
(pwing_bright18 + pwing_bright19) /
(phead_bright18 + phead_bright19) +  plot_annotation(tag_levels = 'A')
dev.off()


# Nestling mass plots ---------------------------------------------------

# Mass day 6
pmass18 <- nestdata2018 %>% filter(year == "2018") %>%
  ggplot(., aes(
    x = predator,
    y = d6_av_mass,
    fill = predator,
    pattern = dull
  )) +
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  ylim(3,15)+
  labs(
    x = "Stress treatment",
    y = "Mass (g) at 6 days",
    pattern = "Signal treatment",
    fill = "Stress treatment",
    title = "2018"
  ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)



pmass19 <- nestdata2018 %>% filter(year == "2019") %>%
  ggplot(., aes(
    x = predator,
    y = d6_av_mass,
    fill = predator,
    pattern = dull
  )) +
  ylim(3,15)+
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  labs(
    x = "Stress treatment",
    y = "Mass (g) at 6 days",
    pattern = "Signal treatment",
    fill = "Stress treatment",
    title = "2019"
  ) +
  theme(legend.key.size = unit(0.9, 'cm'),
        plot.title = element_text(hjust=0.5)) +
  guides(pattern = guide_legend(order = 2, override.aes = list(pattern = c("none", "stripe"))),
         fill = "none") +
  #guides(pattern = guide_legend(order = 2, override.aes = list(pattern = c("none", "stripe"))),
  #       fill = guide_legend(order = 1, override.aes = list(pattern = "none"))) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

# MASS DAY 12 FOR 2018
pmass18_d12 <-nestling_data %>% filter(year == "2018") %>%
  ggplot(., aes(
    x = predator,
    y = d12_mass,
    fill = predator,
    pattern = dull
  )) +
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  ylim(8,25)+
  labs(
    x = "Stress Treatment",
    y = "Mass (g) at 12 days",
    pattern = "Signal treatment",
    fill = "Stress treatment"
  ) +
  theme(legend.position = "none") +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

# MASS DAY 12 FOR 2019
pmass19_d12 <- nestling_data %>% filter(year == "2019") %>%
  ggplot(., aes(
    x = predator,
    y = d12_mass,
    fill = predator,
    pattern = dull
  )) +
  ylim(8,25)+
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  labs(
    x = "Stress treatment",
    y = "Mass (g) at 12 days",
    pattern = "Signal treatment",
    fill = "Stress treatment"
  ) +
  theme(legend.key.size = unit(.9, 'cm')) +
  guides(pattern = guide_legend(order = 2, override.aes = list(pattern = c("none", "stripe"))),
         fill = "none",#guide_legend(order = 1, override.aes = list(pattern = "none")),
         ) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1 )

pdf("output_plots/nestling_mass.pdf")
(pmass18 + pmass19) /
(pmass18_d12 + pmass19_d12) +  plot_annotation(tag_levels = 'A')
dev.off()


# Cort ~ treatments -----------------------------------------------------------------
d18

mod9 <- lmer(d12_base ~ predator + dull + scale(fbbright) + fage + (1|site_nest), data = d18) #%>% summary
mod10 <- lmer(d12_stress ~ predator + dull  +  scale(fbbright) + fage + (1|site_nest), data = d18) #%>% summary
mod11 <- lmer(d12_dex ~ predator + dull  + scale(fbbright) + fage + (1|site_nest), data = d18) #%>% summary

mod12 <- lmer(d12_base ~ predator + dull + scale(fbbright) + fage + (1|site_nest), data = d19 ) #%>% summary
mod13 <- lmer(d12_stress ~ predator + dull + scale(fbbright) + fage + (1|site_nest), data = d19 ) #%>% summary
mod14 <- lmer(d12_dex ~ predator + dull+ scale(fbbright) + fage + (1|site_nest), data = d19 ) #%>% summary


tab_model(mod9, mod10, mod11,
          dv.labels = c("base cort", "stress cort", "post-dex cort"),
          pred.labels = c("Intercept (Controls)", "Predation", "Dull", "Female Brightness (scaled)",
          "Female Age") )

tab_model(mod12, mod13, mod14,
          dv.labels = c("base cort", "stress cort", "post-dex cort"),
          pred.labels = c("Intercept (Controls)", "Predation", "Dull", "Female Brightness (scaled)", "Female Age") )

#
# Cort plots  -------------------------------------------------------------

nestlingcort <- nestling_data %>% select(band, year, d12_base, d12_stress, d12_dex,
                                         predator, dull) %>%
  pivot_longer(cols = c( d12_base, d12_stress, d12_dex),
               names_to = "stress", values_to = "cort") %>% as.data.frame %>%
  mutate(stress = factor(stress, levels = c("d12_base", "d12_stress", "d12_dex")))


cort2018 <- nestlingcort %>% filter(year == "2018") %>%
  ggplot(., aes(
    x = predator,
    y = log10(cort),
    fill = stress,
    pattern = dull
  )) +

#  ylim(15, 70)+
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  scale_fill_manual (labels = c("Base", "Stress-induced", "Negative-feedback"), values = c("#3a6589", "#aaa488", "#b2432f")) +
  labs(
    x = "Stress treatment",
    y = expression(paste("Corticosterone (log ng/", mu, "l)")),
    pattern = "Signal treatment",
    fill = "Stress Series",
    title = "2018"
  ) +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

cort2019 <- nestlingcort %>% filter(year == "2019") %>%
  ggplot(., aes(
    x = predator,
    y = log10(cort),
    fill = stress,
    pattern = dull
  )) +

  #  ylim(15, 70)+
  geom_boxplot_pattern(pattern_fill = "grey50",
                       pattern_colour = "grey50",
                       outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  scale_fill_manual (labels = c("Base", "Stress-induced", "Negative-feedback"), values = c("#3a6589", "#aaa488", "#b2432f")) +
  labs(
    x = "Stress treatment",
    y = expression(paste("Corticosterone (log ng/", mu, "l)")),
    pattern = "Signal treatment",
    fill = "Stress series",
    title = "2019"
  ) +
  theme(legend.key.size = unit(0.9, 'cm'), plot.title = element_text(hjust=0.5)) +
  guides(pattern = guide_legend(order = 2, override.aes = list(pattern = c("none", "stripe"))),
         fill = guide_legend(order = 1, override.aes = list(pattern = "none")),
  ) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)


pdf("output_plots/cort.pdf", height = 4.5, width = 9)
(cort2018 + cort2019) +  plot_annotation(tag_levels = 'A')
dev.off()



# Telomere ~ treatment  -----------------------------------------------------------

head(comp_data)
# Stats
mod17 <- lmer(ts_ratio1 ~ predator + dull + scale(fbbright) +
                fage + (1|gen_mom) + (1|site_nest),
     data = d18)

mod18 <- lmer(ts_ratio1~ predator  + dull + scale(fbbright) +
                fage +  (1|gen_mom) + (1|site_nest),
     data = d19) #%>% summary
tab_model(mod17, mod18,
         dv.labels = c("Relative telomere length (2018)", "Relative telomere length (2019)"),
         pred.labels = c("Intercept (Controls)", "Predation", "Dull", "Scaled brightness", "Female Age"))
# Figure nestlings telos ~ treatment

# 2018
p1 <-
  ggplot(d18, aes(x = predator, y = ts_ratio1, fill = predator, pattern = dull)) +
  geom_boxplot_pattern(
    pattern_fill = "grey50",
    pattern_colour = "grey50",
    outlier.shape = NA) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  labs(x = "Stress treatment", y = "Relative telomere length",
       pattern = "Signal Treatment",
       fill = "Predation",
       title = "2018") +
  theme(legend.position = "none",  plot.title = element_text(hjust=0.5)) +
  ylim(0,.8) +
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
  #       fill = guide_legend(override.aes = list(pattern = "none")))+
  # add points
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)

# 2019
p2 <-
  ggplot(d19, aes(
    x = predator,
    y = ts_ratio1,
    fill = predator,
    pattern = dull
  )) +
  geom_boxplot_pattern(
    pattern_fill = "grey50",
    pattern_colour = "grey50",
    outlier.shape = NA
  ) +
  scale_pattern_manual (values = c(Dull = "stripe", Control = "none")) +
  labs(
    x = "Stress treatment",
    y = "Relative telomere length",
    pattern = "Signal treatment",
    fill = "Predator treatment",
    title = "2019"
  ) +
  theme(legend.key.size = unit(0.9, 'cm'),  plot.title = element_text(hjust=0.5)) +
  guides(pattern = guide_legend(order = 2, override.aes = list(pattern = c("none", "stripe"))),
         fill = "none"# guide_legend(order = 1, override.aes = list(pattern = "none"))
         ) +
  # add points
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),
              colour = "black", size = 1)
guides(pattern = guide_legend(order = 2, override.aes = list(pattern = c("none", "stripe"))),
       fill = guide_legend(order = 1, override.aes = list(pattern = "none")),
)
pdf("output_plots/telomere_plot_both.pdf")
(p1 + p2)/(p1 + p2) +  plot_annotation(tag_levels = 'A')
dev.off()


# Telomeres ~ growth and cort ------------------------

d18 <- d18 %>% mutate(Nestling_Fate = na_if(Nestling_Fate,""))
d19 <- d19 %>% mutate(Nestling_Fate = na_if(Nestling_Fate,""))
as.factor(d19$Nestling_Fate) %>% summary

# Growth stuff: no relationship with telomeres

# par(mfrow= c(2,2))
# plot(Flat_Wing ~ ts_ratio1, d18)
# plot(Flat_Wing ~ ts_ratio3, d18)
# plot(Mass ~ ts_ratio1, d18)
# plot(Mass ~ ts_ratio3, d18)
#
# par(mfrow= c(2,2))
# plot(Flat_Wing ~ ts_ratio1, d19)
# plot(Flat_Wing ~ ts_ratio3, d19)
# plot(Mass ~ ts_ratio1, d19)
# plot(Mass ~ ts_ratio3, d19)


mod19 <- lmer(Flat_Wing ~ ts_ratio1 + (1|site_nest), data = d18)# %>% summary
mod20 <- lmer(Flat_Wing ~ ts_ratio1 + (1|site_nest), data = d19)# %>% summary

mod21 <- lmer(Mass ~ ts_ratio1 + (1|site_nest), data = d19)# %>% summary
mod22 <- lmer(Mass ~ ts_ratio1 + (1|site_nest), data = d18)# %>% summary

tab_model(mod19, mod20,
          dv.labels = c("Wing length 2018", "Wing length 2019"),
          pred.labels = c("Intercept (Controls)", "relative telomere length"))

tab_model( mod21, mod22,
          dv.labels = c("Mass 2018", "Mass 2019"),
          pred.labels = c("Intercept (Controls)", "relative telomere length"))
# No difference in telos between lived/died nestlings
mod23 <- glmer(as.factor(Nestling_Fate) ~
                 ts_ratio1 + (1|site_nest), data = d18, family = "binomial")#%>% summary

mod24 <- glmer(as.factor(Nestling_Fate) ~
              ts_ratio1 + (1|site_nest), data = d19, family = "binomial")# %>% summary
tab_model(mod23, mod24,
          dv.labels = c("Fledging success 2018", "Fledging success 2019"),
          pred.labels = c("Intercept (Controls)", "relative telomere length") )

mod25 <- lmer(ts_ratio1 ~ d12_base + d12_stress + d12_dex + (1|site_nest), data = d18)
mod26 <- lmer(ts_ratio1 ~ d12_base + d12_stress + d12_dex + (1|site_nest), data = d19)
tab_model(mod25, mod26,
          dv.labels = c("RTL 2018", "RTL 2019"),
          pred.labels = c("Intercept", "Baseline cort", "Stress cort", "Post-dex cort"))
plot(ts_ratio1 ~ log(d12_base), data = d19)

# Survival analysis and fledging success -----------------------------------------------------------

# Surv analysis + plot
nsurv <- nestdata2018 %>% select(site_nest, year, clutch, maxbrood, numd6,
                                 numband, num_d15, numfled, full_treatment,
                                 dull, predator, fbbright, fage)

nsurv <- nsurv %>% mutate(maxbrood = ifelse(is.na(maxbrood), 0, maxbrood)) %>%
                     mutate(died_hatch = clutch - maxbrood,
                            died_5 = maxbrood - numd6,
                            died_11 = numd6 - numband,
                            died_14 = numband - num_d15,
                            died_16 = num_d15  - numfled )



  #nsurv %>% filter(if_any(everything(), ~ . < 0)) # Some errors in data set, now corrected
  # issues to fix
  # filter(phys, site_nest == "Unit_2_124_2018" & Age == 12) # 2 nestlings at d 12
  # filter(phys, site_nest == "Unit_2_103_2019"& Age == 12) # 6 nestlings at d 12
  # filter(phys, site_nest == "Unit_2_95_2018"& Age == 12) # max brood = 5
  # filter(phys, site_nest == "Unit_2_121_2019"& Age == 12) # 1 nestling d12
  # filter(phys, site_nest == "Unit_2_41_2019"& Age == 12) # 4 nestling d12
  # filter(phys, site_nest == "Unit_2_31_2019"& Age == 12) # 4 nestling d12
  # filter(phys, site_nest == "Unit_2_54_2019"& Age == 12) # 6 nestling d12
  # filter(phys, site_nest == "Unit_2_37_2018") # 4 nestling d12
  # filter(nests, site_nest == "Unit_2_37_2018") # unclear but clutch should be 6 probably, 6 on d6 4 banded

  # pivot_longer(mydata,
  #              cols, # columns that should pivot from wide to long (unquoted)
  #              names_to, # name of the new category column as a quoted string
  #              values_to # name of the new value column as a quoted string
  # )
  # check and make sure things add up
nsurv$check <-nsurv$clutch == nsurv$numfled + nsurv$died_hatch + nsurv$died_5 + nsurv$died_11 + nsurv$died_14 + nsurv$died_16

nsurv <- nsurv %>% select(full_treatment, site_nest, died_hatch, died_5, died_11,
                            died_14, died_16, numfled, year, dull, predator, fbbright, fage) %>%
                      pivot_longer(cols = c(died_hatch, died_5, died_11, died_14, died_16, numfled),
                      names_to = "end",
                      values_to = "number") %>%
                    filter(number > 0) %>%
                    uncount(number, .remove = TRUE) %>%
    mutate(age = case_when(end == "died_hatch" ~ 1,
                           end == "died_5" ~ 5,
                           end == "died_11" ~ 11,
                           end == "died_14" ~ 14,
                           end == "died_16" ~ 16,
                           end == "numfled" ~ 23)) %>%
    mutate(died = case_when(end == "numfled" ~ 0,
                            TRUE ~ 1 ))

# A check to make sure all the totals line up

check <- table(nsurv$site_nest) %>% as.data.frame %>% rename(site_nest = Var1)
check <- left_join(check, select(nestdata2018, c(site_nest, clutch))) %>%
    mutate(check = Freq == clutch)
summary(check$check) # all good if TRUE

nestdata2018  <- nestdata2018 %>% mutate(numdied = clutch - numfled)

# make sure totals from nsurv can be mutated back to the same numbers as in nestdata2018
back_check <- nsurv %>% group_by(site_nest, died, year) %>% tally %>% pivot_wider(
    id_cols = c(site_nest, year),
    names_from = died,
    values_from = n,
    names_prefix = "died",
    values_fill = 0
  )
back_check <-select(nestdata2018, site_nest, numfled, numdied) %>% left_join(., back_check)
back_check$numdied == back_check$died1 # should be all true
back_check$numfled == back_check$died0 # should be all true

nsurv %>% filter(died == 1) %>% filter(year == 2018 & predator == "Predation") %>% tally #group_by(age) %>% tally


# Survival plots
survplot18 <- survfit(Surv(age, died) ~ predator, data=nsurv[nsurv$year == 2018,]) %>%
  ggsurvplot(., conf.int = TRUE, legend.labs = c("Control", "Predation")) + labs (x = "Days")
survplot19 <- survfit(Surv(age, died) ~ predator, data=nsurv[nsurv$year == 2019,]) %>%
  ggsurvplot(., conf.int = TRUE, legend.labs = c("Control", "Predation")) +
  labs(x = "Days")

pdf("output_plots/survival_plots.pdf", height =5, width = 9)
survplot18$plot + survplot19$plot + plot_annotation(tag_levels = 'A')
dev.off()

# Cox Proportional Hazards Models
mod25 <- coxme(Surv(age, died) ~ predator + dull + scale(fbbright) + fage + (1|site_nest), data = nsurv[nsurv$year == 2018,]) %>% summary
mod26 <- coxme(Surv(age, died) ~ predator + dull + scale(fbbright) + fage + (1|site_nest), data = nsurv[nsurv$year == 2019,]) %>% summary
summary(mod25)
tab_model(mod25, mod26, dv.labels = c("Hazard ratio nestling survival 2018",
                                      "Hazard ratio nestling survival 2019"),
                                      pred.labels  = c("Predation", "Dulling", "Female brightness (scaled)", "Female age"))

# Fledging success glms
mod27 <- glmer(died ~ predator + dull + scale(fbbright) + fage + (1|site_nest), data = nsurv[nsurv$year == 2018,], family = "binomial") #%>% summary
mod28 <- glmer(died ~ predator + dull+ scale(fbbright) + fage + (1|site_nest), data = nsurv[nsurv$year == 2019,], family = "binomial") #%>% summary
tab_model(mod27, mod28, dv.labels = c("Fledging success 2018", "Fledging success 2019"),
          pred.labels = c("Intercept (Controls)", "Predation", "Dull", "Female brightness (scaled)", "Female age"))
# Number of fledglings per treatment
aggregate(numfled ~ predator + year, data = nestdata2018, mean)
aggregate(maxbrood ~ predator + year, data = nestdata2018, mean)

aggregate(numfled ~ predator + year, data = nestdata2018, sum)
aggregate(maxbrood ~ predator + year, data = nestdata2018, sum)
29/95
62/89
# Check for differences in hatching success
nestdata2018$nohatch <- nestdata2018$clutch - nestdata2018$maxbrood
aggregate(maxbrood/clutch ~ predator + year, data = nestdata2018, mean)
mod29 <- glmer(cbind(maxbrood, nohatch) ~ dull + (1|site_nest),
      data = nestdata2018[nestdata2018$year == 2018,], family = "binomial")
mod30 <- glmer(cbind(maxbrood, nohatch) ~ predator  + (1|site_nest),
               data = nestdata2018[nestdata2018$year == 2019,], family = "binomial")
tab_model(mod29, mod30, dv.labels = c("Hatching success 2018", "Hatching success 2019"),
          pred.labels = c("Intercept (Controls)", "Dulling", "Predation"))

# Does original female brightness matter? no
#glmer(cbind(maxbrood, nohatch) ~ predator  + dull*fbbright + (1|site_nest),
#      data = nestdata2018[nestdata2018$year == 2018,], family = "binomial") %>% summary

# investigate differences in fledging success between years
nestdata2018  <- nestdata2018 %>% mutate(numdied = clutch - numfled) # nestlings that died after hatching
glmer(cbind(numdied, numfled) ~ as.factor(year) + (1|site_nest), data = nestdata2018, family = "binomial") %>% tab_model
aggregate(numfled/maxbrood ~ predator + year, data = nestdata2018, mean)

glmer(cbind(numdied, numfled) ~ predator + (1|site_nest), data = nestdata2018[nestdata2018$year == 2019,], family = "binomial") %>% summary


glmer(cbind(numdied, numfled) ~ predator + fbbright +
        (1|site_nest), data = nestdata2018[nestdata2018$year == 2019,], family = "binomial") %>% summary





# Response to reviewers ---------------------------------------------------

# determining mean number of fledglings per year
nestdata2018 %>%
  filter(predator == "Control") %>%
  group_by(year) %>%
  summarize(mean_fledge = mean(numfled))
head(nestdata2018)

# Did clutch/brood size affect measures of nestling size?
lmer(d12_head ~ predator + dull + scale(fbbright)  + maxbrood +(1|site_nest) + (1|gen_mom),
data = nestling_data[nestling_data$year == "2018",]) %>% summary
lmer(d12_wing ~ predator + dull + scale(fbbright)  + maxbrood +(1|site_nest) + (1|gen_mom),
     data = nestling_data[nestling_data$year == "2018",]) %>% summary
lmer(d12_mass ~ predator + dull + scale(fbbright)  + maxbrood +(1|site_nest) + (1|gen_mom),
     data = nestling_data[nestling_data$year == "2018",]) %>% summary


lmer(d12_head ~ predator + dull + scale(fbbright)  + maxbrood +(1|site_nest) + (1|gen_mom),
     data = nestling_data[nestling_data$year == "2019",]) %>% summary
lmer(d12_wing ~ predator + dull + scale(fbbright)  + maxbrood +(1|site_nest) + (1|gen_mom),
     data = nestling_data[nestling_data$year == "2019",]) %>% summary
lmer(d12_mass ~ predator + dull + scale(fbbright)  + maxbrood +(1|site_nest) + (1|gen_mom),
     data = nestling_data[nestling_data$year == "2019",]) %>% summary

# does including telomere effiency affect results?
lmer(ts_ratio1 ~ predator + dull + (1|gen_mom) + (1|site_nest) + eff_gapdh +
       eff_telo,  data = d18) %>% summary

lmer(ts_ratio1 ~ predator + dull + (1|gen_mom) + (1|site_nest) + eff_gapdh +
       eff_telo ,
     data = d19) %>% summary
cor(comp_data$ts_ratio1, comp_data$eff_telo, use = "complete.obs")

par(mfrow = c(1,2))
plot(ts_ratio1 ~ eff_telo, data= d18)
plot(ts_ratio1 ~ eff_telo, data= d19)

# Change in magnitude of cort: does this matter?
lmer(ts_ratio1 ~  (d12_stress- d12_base)  + (1|site_nest), data = d18) %>% summary()
lmer(ts_ratio1 ~  (d12_stress- d12_base)  + (1|site_nest), data = d19) %>% summary()

lmer(ts_ratio1 ~  (d12_dex- d12_base)  + (1|site_nest), data = d18) %>% summary()
lmer(ts_ratio1 ~  (d12_dex- d12_base)  + (1|site_nest), data = d19) %>% summary()

lmer(ts_ratio1 ~  (d12_dex- d12_stress)  + (1|site_nest), data = d18) %>% summary()
lmer(ts_ratio1 ~  (d12_dex- d12_stress)  + (1|site_nest), data = d19) %>% summary()

# Does age of mother matter?
head(nestling_data)
lmer(d12_wing ~ predator + dull + scale(fbbright)  + fage + (1|site_nest) + (1|gen_mom),
             data = nestling_data[nestling_data$year == "2018",]) %>% summary
lmer(d12_wing ~ predator + dull* scale(fbbright) + fage + (1|site_nest) + (1|gen_mom),
             data = nestling_data[nestling_data$year == "2019",]) %>% summary


lmer(d12_head ~ predator + dull + scale(fbbright)+  (1|site_nest) ,
             data = nestling_data[nestling_data$year == "2018",]) #%>% summary
lmer(d12_head ~ predator + dull * scale(fbbright) +  (1|site_nest) + (1|gen_mom),
             data = nestling_data[nestling_data$year == "2019",])

# summary stats for abstract
head(nestling_data)
