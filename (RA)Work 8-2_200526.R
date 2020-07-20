########################
#### BASIC SETTINGS ####
########################

# Load library
library(haven)
library(tidyverse)
library(plyr)
library(tidytext)
library(ggrepel)

# Load data
dat <- read_dta("bootstrapping results with new violent arrests, N=500.dta")

head(dat)

# Convert to a long format and make new columns for each metric
head(dat[,655:660])
dat_long <- dat %>% 
  gather(category, severity, bw_rat_pc_cs1_A:nww_diff_pva_cs5_K) %>% 
  mutate(cs = ifelse(str_detect(category, "cs1_"), "All UOF",
                     ifelse(str_detect(category, "cs2_"), "Lethal Force",
                            ifelse(str_detect(category, "cs3_"), "Common UOF",
                                   ifelse(str_detect(category, "cs4_"), "Ranked, CPE", "Ranked, Oakland"))))) %>% 
  mutate(dept = ifelse(str_detect(category, "_A"), "A", 
                       ifelse(str_detect(category, "_B"), "B",
                              ifelse(str_detect(category, "_C"), "C",
                                     ifelse(str_detect(category, "_D"), "D",
                                            ifelse(str_detect(category, "_E"), "E",
                                                   ifelse(str_detect(category, "_F"), "F",
                                                          ifelse(str_detect(category, "_G"), "G",
                                                                 ifelse(str_detect(category, "_H"), "H",
                                                                        ifelse(str_detect(category, "_I"), "I",
                                                                               ifelse(str_detect(category, "_J"), "J", "K"))))))))))) %>% 
  mutate(minority = ifelse(str_detect(category, "bw_"), "Black-to-White", "Nonwhite-to-White")) %>% 
  mutate(benchmark = ifelse(str_detect(category, "_pc_"), "Population", 
                            ifelse(str_detect(category, "_pa_"), "Arrests", "Violent Arrests"))) %>% 
  mutate(metrics = ifelse(str_detect(category, "_rat_"), "Ratio", "Difference"))


# Collapse the data by averaging severity across each metric
sum_df <- ddply(dat_long, c("cs", "dept", "minority", "benchmark", "metrics"), summarise, 
                N = sum(!is.na(severity)),
                mean = mean(severity, na.rm = TRUE),
                sd = sd(severity, na.rm = TRUE))

# Make index column for Dept F and Dept K
sum_df <- sum_df %>% 
  mutate(dept_col = ifelse(dept == "F", "F", 
                           ifelse(dept == "K", "K", "Others")))

# Make index column for CS2: Lethal force
sum_df <- sum_df %>% 
  mutate(cs_cs2 = ifelse(cs == "Lethal Force", 1, 0))

# Reorder CS level
sum_df$cs <- factor(sum_df$cs, levels = c("All UOF", "Lethal Force", "Common UOF", "Ranked, CPE", "Ranked, Oakland"), 
                    labels = c("All UOF", "Lethal Force", "Common UOF", "Ranked, CPE", "Ranked, Oakland"))

head(sum_df)
sum_df <- sum_df %>% 
  mutate(abs_mean = abs(mean), dist_1 = abs(1-mean))

# Save data
write.csv(sum_df, "data_withSE_200525.csv")


####################
#### Make Plots ####
####################

## Set up position dodges
pd <- position_dodge(width = 0.6)
pd2 <- position_dodge(width = 0.8)

## Figure 1a. Ratio x BW x Population
sum_df %>% 
  filter(metrics == "Ratio", minority == "Black-to-White", benchmark == "Population") %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Ratio of Black to White Force Severity (Population Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = c(.97, .97), legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.direction = "horizontal")


## Figure 1b: Ratio x BW x Population (Axes distinct)
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Ratio", minority == "Black-to-White", benchmark == "Population", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Ratio of Black to White Force Severity (Population Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# CS2
sum_df %>% 
  filter(metrics == "Ratio", minority == "Black-to-White", benchmark == "Population", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Figure 2: Ratio x BW x Arrests (Axes distinct)
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Ratio", minority == "Black-to-White", benchmark == "Arrests", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Ratio of Black to White Force Severity (Arrest Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))


# CS2
sum_df %>% 
  filter(metrics == "Ratio", minority == "Black-to-White", benchmark == "Arrests", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Figure 3: Ratio x NW-W x Population
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Ratio", minority == "Nonwhite-to-White", benchmark == "Population", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Ratio of Non-white to White Force Severity (Population Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# CS2
sum_df %>% 
  filter(metrics == "Ratio", minority == "Nonwhite-to-White", benchmark == "Population", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Figure 4: Ratio x NW-W x Arrests
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Ratio", minority == "Nonwhite-to-White", benchmark == "Arrests", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Ratio of Non-white to White Force Severity (Arrest Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(1, 3, 5, 7), labels = c("1", "3", "5", "7"))


# CS2
sum_df %>% 
  filter(metrics == "Ratio", minority == "Nonwhite-to-White", benchmark == "Arrests", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


##############################################################################################################

## Figure E1: Difference x BW x Population
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Difference", minority == "Black-to-White", benchmark == "Population", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Difference between Black and White Force Severity (Pop. Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000), labels = c("0", "0.5k", "1k", "1.5k", "2k"))


# CS2
sum_df %>% 
  filter(metrics == "Difference", minority == "Black-to-White", benchmark == "Population", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Figure E2: Difference x BW x Arrests
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Difference", minority == "Black-to-White", benchmark == "Arrests", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Difference between Black and White Force Severity (Arrest Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000), labels = c("0", "10k", "20k", "30k"))

# CS2
sum_df %>% 
  filter(metrics == "Difference", minority == "Black-to-White", benchmark == "Arrests", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Figure E3: Difference x NW-W x Population
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Difference", minority == "Nonwhite-to-White", benchmark == "Population", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Difference between Non-white and White Force Severity (Pop. Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0, 500, 1000), labels = c("0", "0.5k", "1k"))

# CS2
sum_df %>% 
  filter(metrics == "Difference", minority == "Nonwhite-to-White", benchmark == "Population", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Figure E4: Difference x NW-W x Arrests
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Difference", minority == "Nonwhite-to-White", benchmark == "Arrests", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Difference between Non-white and White Force Severity (Arrest Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000), labels = c("0", "10k", "20k", "30k", "40k"))


# CS2
sum_df %>% 
  filter(metrics == "Difference", minority == "Nonwhite-to-White", benchmark == "Arrests", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


##############################################################################################################

## Figure F1: Ratio x BW x Violent Arrests (VA)
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Ratio", minority == "Black-to-White", benchmark == "Violent Arrests", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Ratio of Black to White Force Severity (Violent Arrest Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# CS2
sum_df %>% 
  filter(metrics == "Ratio", minority == "Black-to-White", benchmark == "Violent Arrests", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Figure F2: Ratio x NW-W x Violent Arrests (VA)
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Ratio", minority == "Nonwhite-to-White", benchmark == "Violent Arrests", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Ratio of Non-white to White Force Severity (Violent Arrest Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# CS2
sum_df %>% 
  filter(metrics == "Ratio", minority == "Black-to-White", benchmark == "Violent Arrests", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin = ifelse(mean-1.96*sd>0, mean-1.96*sd, 0), ymax = mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept=1, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Ratio = 1") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Figure F3: Difference x BW x Violent Arrests (VA)
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Difference", minority == "Black-to-White", benchmark == "Violent Arrests", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, 
                    color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Difference between Black and White Force Severity (VA Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(-400000, -200000, 0, 200000), labels = c("-400k", "-200k", "0", "200k"))


# CS2
sum_df %>% 
  filter(metrics == "Difference", minority == "Black-to-White", benchmark == "Violent Arrests", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(-2000, 0, 2000, 6000, 10000), labels = c("-2k", "0", "2k", "6k", "10k"))


## Figure F4: Difference x NWW x VA
# CS1, 3, 4, 5
sum_df %>% 
  filter(metrics == "Difference", minority == "Nonwhite-to-White", benchmark == "Violent Arrests", cs_cs2 == 0) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% Confidence Interval") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y="Difference between Non-white and White Force Severity (VA Benchmark)", shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.position = "none",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(-200000, 0, 200000, 400000, 600000), labels = c("-200k", "0", "200k", "400k", "600k"))

# CS2
sum_df %>% 
  filter(metrics == "Difference", minority == "Nonwhite-to-White", benchmark == "Violent Arrests", cs_cs2 == 1) %>% 
  ggplot(aes(x = cs, y = mean)) +
  geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd, color = dept), position = pd, width=.7) +
  geom_point(aes(shape=dept, color = dept), size=3, position = pd) +
  scale_color_manual(values=c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red")) +
  scale_shape_manual(values = 0:11) +
  theme_bw() +
  geom_point(aes(size = minority, shape = NA), colour = "grey50") +
  scale_size_discrete(labels = "95% CI") +
  geom_hline(aes(yintercept = 0, linetype = minority), color="#414141") +
  scale_linetype_manual(values = "dashed", labels = "Difference = 0") +
  guides(shape=guide_legend(order = 1), color=guide_legend(order = 1, override.aes = list(linetype = NULL, size = 3, color = c("grey50", "grey50", "grey50", "grey50", "grey50", "blue", "grey50", "grey50", "grey50", "grey50", "red"))), 
         size=guide_legend(NULL, override.aes=list(shape="I", size = 5), order = 2), linetype=guide_legend(order=3)) +
  labs(x=NULL, y=NULL, shape = NULL, linetype = NULL, size = NULL, color = NULL) +
  theme(legend.spacing.y = unit(-.17, 'cm'), legend.justification = c("right", "top"), 
        legend.box.just = "left", legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(-2000, 0, 2000, 6000, 10000), labels = c("-2k", "0", "2k", "6k", "10k"))

