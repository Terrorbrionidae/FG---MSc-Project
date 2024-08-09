## HEADER ####
## who: Fraser Gray (22382300)
## what: MSc Research Project Script
## when: 2024-08-22

## CONTENTS ####
## 00 Setup
## 01 Review Temp & Humidity Data
## 02 Review Larvae Mass
## 03 Compare phase 1 PVC and frass mass
## 04 Compare death and plastic mass

## 00 Setup ####
  # Set libraries, install if necessary and load
  req_packages <- c("openxlsx", "tidyverse", "ggpubr", "ggpattern", "ggtext", "multcompView",
                    "scales", "MASS", "lmtest") 
  install.packages(setdiff(req_packages, rownames(installed.packages())))  
  lapply(req_packages, library, character.only = TRUE)
  
  # Set working directory
  setwd("Insert Directory")
  
  #load data
  Env_dta <- read.xlsx("22382300 - Temp & Humidity.xlsx")
  Exp_dta <- read.xlsx("22382300 - Exp Data.xlsx")
  
  #fix date & datetime format for Env_dta
  Env_dta$Date <- convertToDate(Env_dta$Date)
  Env_dta$DateTime <- convertToDateTime(Env_dta$DateTime)
  
  #factor experimental data variables
  Exp_dta$Treatment <- factor(Exp_dta$Treatment, levels = c(1,2,3,4,5,6,7,8))
  Exp_dta$Time <- factor(Exp_dta$Time, levels = c("Start", "End"))

## 01 Review Temp & Humidity Data ####
  #Set line positions for events
  Event_pos <- as.numeric(c(Env_dta$DateTime[1], Env_dta$DateTime[370], 
                             Env_dta$DateTime[425], Env_dta$DateTime[882], 
                             Env_dta$DateTime[936]))
  
  #set annotate positions & labels
  Ann_posx <- c(as_datetime("2024-04-12 22:00:00"), as_datetime("2024-05-01 17:00:00"), 
               as_datetime("2024-05-14 22:00:00"))
  Ann_posy <- c(22, 22, 22) 
  Ann_lab <- c("Phase 1 \n Start", "Phase 1 \n Data Collection \n + \n Phase 2 \n Start", 
               "Phase 2 \n Data Collection")
  
  #set segment positions
  Seg_posxs <- c(as_datetime("2024-04-26 09:00:00"), as_datetime("2024-05-17 17:00:00"))
  Seg_posxe <- c(as_datetime("2024-04-28 16:00:00"), as_datetime("2024-05-19 23:00:00"))
  Seg_posy <- c(22, 22)
  Seg_df <- data.frame(Seg_posxs, Seg_posxe, Seg_posy)
  
  #plot temperature data
  tp <- ggplot(data = Env_dta, aes(x = DateTime, y = Temperature)) + 
          geom_line(colour = "#FF6633", linewidth = 0.8) +
          ylim(16,24) +
          geom_vline(xintercept = Event_pos, linetype = "dashed") +
          annotate("text", x = Ann_posx, y = Ann_posy, label= Ann_lab, size = 3.5) +
          geom_segment(data = Seg_df, aes(x = Seg_posxs, y = Seg_posy, xend = Seg_posxe, yend = Seg_posy), 
                       arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both")) +
          labs(x = "Date & Time (Hourly Readings)", y = "Temperature (\u00B0C)") + 
          theme_light()
  
  #plot humidity data
  hp <- ggplot(data = Env_dta, aes(x = DateTime, y = Humidity)) + 
          geom_line(colour = "#298c8c", linewidth = 0.8) +
          geom_vline(xintercept = Event_pos, linetype = "dashed") +
          labs(x = "Date & Time (Hourly Readings)", y = "Humidity (%)") + 
          theme_light()
  
  #arrange both in a grid
  ggarrange(tp + rremove("x.text") + rremove("xlab"), hp, 
            labels = c("A", "B"), nrow = 2)
  
## 02 Review Larvae Mass ####
  
  ##Make chart##
  
  #Filter out treatments with no mealworms
  Exp_dta_M <- Exp_dta %>% filter(Species != "None")
  
  #Set facet label names
  Phase.labs <- c("Phase 1 (15 Days)", "Phase 2 (19 Days)")
  names(Phase.labs) <- c(1, 2)
  
  #Create geom segment and geom text data frame
  Seg_df <- data.frame(Seg_posxs = c(1.5, 5.5, 0, 4.5), Seg_posxe = c(4.5, 8.5, 2.5, 6.5), 
                       Seg_posy = c(5, 10, 11, 11), Lab_posx = c(3,7, 1.5, 5.5), 
                       Lab_posy = c(4, 11, 10, 10), Lab_txt = c(rep("With PVC", 2), rep("No Larvae", 2)),
                       Phase = c(1, 1, 2, 2), Species = rep("Tenebrio molitor", 4), Time = rep("Start", 4))
  
  #Data frame for tukey letters (see results further below)
  Tuk_df <- data.frame(Tuk_posx = c(seq(from = 0.7, to = 3.7, by = 1), seq(from = 1, to = 4, by = 1), 
                                    seq(from = 4.7, to = 7.7, by = 1), seq(from = 5.05, to = 8.05, by = 1),
                                    2.7,3.05,3.7,4.05,6.7,7.05,7.7,8.05), 
                       Tuk_posy = c(rep(9,4), rep(12.5,4), rep(7.5,8), 16,18.5,16.25,17.5,16.25,17.5,16,18.25), 
                       Tuk_txt = c(rep("a",4), rep("b",4), rep("c",8), rep("ab",5), "a","b","a"), 
                       Phase = c(rep(1,16), rep(2,8)), Species = rep("Tenebrio molitor", 24), Time = rep("Start", 24))

  #Boxplot of larvae mass before and after each phase
  ggplot(data = Exp_dta_M, aes(x = Treatment, y = W_Mass, fill = Species, pattern = Time)) + 
    geom_boxplot_pattern() +
    geom_vline(xintercept = seq(from = 1.5, to = 7.5, by = 1), linetype = "dashed", colour = "grey") +
    facet_wrap(~Phase, ncol = 1, scales = "free_y", strip.position = "right", labeller = labeller(Phase = Phase.labs)) +
    geom_segment(data = Seg_df, aes(x = Seg_posxs, y = Seg_posy, xend = Seg_posxe, yend = Seg_posy), 
                 arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both")) +
    geom_text(data = Seg_df, aes(x = Lab_posx, y = Lab_posy, label = Lab_txt), size = 3.5) +
    geom_text(data = Tuk_df, aes(x = Tuk_posx, y = Tuk_posy, label = Tuk_txt), size = 3.5) +
    scale_fill_manual(labels = c("*Tenebrio molitor*", "*Zophobas atratus*"), values = c("#9fc8c8", "#298c8c")) +
    scale_pattern_manual(labels = c("Start of Experiment", "End of Experiment"), values = c("none","circle")) +
    labs(x = "Treatment", y = "Total Larvae Mass (grams)") +
    theme_light() +
    theme(legend.text = element_markdown(), panel.grid = element_blank(), strip.text = element_text(colour = 'black'))
  
  ##Do Test## 
  
  #Anova for influence of measurement time and treatment on larvae mass
  Exp_dta_P1 <- Exp_dta %>% filter(Phase != 2)  #do first for Phase 1
  aov_1 <- aov(W_Mass ~ Time * Treatment, data = Exp_dta_P1)
  anova(aov_1)
  tukey <- TukeyHSD(aov_1)
  tukey
  aov_1L <- multcompLetters4(aov_1, tukey)
  aov_1L <- data.frame(letters = aov_1L$`Time:Treatment`$Letters)
  aov_1L
  
  Exp_dta_P2 <- Exp_dta %>% filter(Phase != 1)  #do next for Phase 2
  aov_2 <- aov(W_Mass ~ Time * Treatment, data = Exp_dta_P2)
  anova(aov_2)
  tukey <- TukeyHSD(aov_2)
  tukey
  aov_2L <- multcompLetters4(aov_2, tukey)
  aov_2L <- data.frame(letters = aov_2L$`Time:Treatment`$Letters)
  aov_2L
  
## 03 Compare phase 1 PVC and frass mass ####
  
  ##Make frass mass chart##
  
  #Filter only end of phase 1
  Exp_dta_P1 <- Exp_dta %>% filter(Phase != 2, Time == "End")
  
  #Create geom segment and geom text data frame
  Seg_df <- data.frame(Seg_posxs = c(1.5, 5.5), Seg_posxe = c(4.5, 8.5), 
                       Seg_posy = c(0.5, 2.5), Lab_posx = c(3, 7), 
                       Lab_posy = c(0.25, 2.75), Lab_txt = rep("With PVC", 2), 
                       Species = rep("Tenebrio molitor",2))
  
  #Data frame for tukey letters (see results further below)
  Tuk_df <- data.frame(Tuk_posx = seq(from = 0.7, to = 7.7, by = 1), 
                       Tuk_posy = c(1.95,2.1,2.05,1.95,1.7,1.8,1.8,1.7), 
                       Tuk_txt = c("ab","a","a","ab","c","bc","bc","c"), 
                       Species = rep("Tenebrio molitor", 8))
  
  #Boxplot of frass mass at end of phase 1
  fm <-  ggplot(data = Exp_dta_P1, aes(x = Treatment, y = as.numeric(F_Mass), fill = Species)) + 
            geom_boxplot() +
            geom_vline(xintercept = seq(from = 1.5, to = 7.5, by = 1), linetype = "dashed", colour = "grey") +
            scale_fill_manual(labels = c("*Tenebrio molitor*", "*Zophobas atratus*"), values = c("#9fc8c8", "#298c8c")) +
            geom_segment(data = Seg_df, aes(x = Seg_posxs, y = Seg_posy, xend = Seg_posxe, yend = Seg_posy), 
                          arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both")) +
            geom_text(data = Seg_df, aes(x = Lab_posx, y = Lab_posy, label = Lab_txt), size = 3.5) +
            geom_text(data = Tuk_df, aes(x = Tuk_posx, y = Tuk_posy, label = Tuk_txt), size = 3.5) +
            scale_y_continuous(limits = c(0,3), labels = label_number(accuracy = 0.1)) +
            labs(x = "Treatment", y = "Mass of Frass at \n Phase 1 End (grams)") +
            theme_light() +
            theme(legend.text = element_markdown(), panel.grid = element_blank()) +
            guides(fill = guide_legend(nrow = 2))
  
  #Anova for influence of treatment on frass mass
  aov_3 <- aov(as.numeric(F_Mass) ~ Treatment, data = Exp_dta_P1)
  anova(aov_3)
  tukey <- TukeyHSD(aov_3)
  tukey
  aov_3L <- multcompLetters4(aov_3, tukey)
  aov_3L <- data.frame(letters = aov_3L$`Treatment`$Letters)
  aov_3L
  
  ##Make PVC mass chart##
  
  #Filter only on phase 1 rows with plastic mass data
  Exp_dta_PVC <- Exp_dta %>% filter(P_Mass != "N/A")
  Exp_dta_PVC$P_Mass[Exp_dta_PVC$P_Mass == 0] <- "N/A" #set trt 1 & 5 to N/A & force to not plot
  
  #Create geom segment and geom text data frame
  Seg_df <- data.frame(Seg_posxs = c(0.4, 4.5), Seg_posxe = c(1.5, 5.5), 
                       Seg_posy = c(0.1, 0.1), Lab_posx = c(1, 5), 
                       Lab_posy = c(0.05, 0.05), Lab_txt = rep("No PVC", 2), 
                       Species = rep("Tenebrio molitor",2))
  
  #Data frame for tukey letters (see results further below)
  Tuk_df <- data.frame(Tuk_posx = c(1.7,2.7,3.7,5.7,6.7,7.7), 
                       Tuk_posy = c(0.2,0.2,0.19,0.4,0.38,0.39), 
                       Tuk_txt = c("bc","c","c","a","abc","ab"), 
                       Species = rep("Tenebrio molitor", 6))
  
  #Boxplot of PVC mass at end of phase 1
  pm <- ggplot(data = Exp_dta_PVC, aes(x = Treatment, y = as.numeric(P_Mass), fill = Species)) + 
            geom_boxplot() +
            geom_vline(xintercept = seq(from = 1.5, to = 7.5, by = 1), linetype = "dashed", colour = "grey") +
            geom_segment(data = Seg_df, aes(x = Seg_posxs, y = Seg_posy, xend = Seg_posxe, yend = Seg_posy), 
                         arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both")) +
            geom_text(data = Seg_df, aes(x = Lab_posx, y = Lab_posy, label = Lab_txt), size = 3.5) +
            geom_text(data = Tuk_df, aes(x = Tuk_posx, y = Tuk_posy, label = Tuk_txt), size = 3.5) +
            scale_y_continuous(labels = label_number(accuracy = 0.1)) +
            scale_fill_manual(labels = c("*Tenebrio molitor*", "*Zophobas atratus*"), values = c("#9fc8c8", "#298c8c")) +
            labs(x = "Treatment", y = "Mass Removed from \n PVC Square (grams)") +
            theme_light() +
            theme(legend.text = element_markdown(), panel.grid = element_blank())
  
  #Anova for influence of treatment on PVC mass
  
  aov_4 <- aov(as.numeric(P_Mass) ~ Treatment, data = Exp_dta_PVC %>% filter(P_Mass != "N/A"))
  anova(aov_4)
  tukey <- TukeyHSD(aov_4)
  tukey
  aov_4L <- multcompLetters4(aov_4, tukey)
  aov_4L <- data.frame(letters = aov_4L$`Treatment`$Letters)
  aov_4L
  
  ##Arrange both in a grid##
  ggarrange(fm + rremove("x.text") + rremove("xlab"), pm, 
            labels = c("A", "B"), nrow = 2, common.legend = TRUE, legend="right")
  
## 04 Compare death and plastic mass####
  ##Create plot
  
  #Filter only on phase 1 rows with plastic mass data
  Exp_dta_plastic <- Exp_dta %>% filter(P_Mass != "N/A")
  
  #Comparison of removed plastic and deceased larvae at end of phase 1
  ggplot(data = Exp_dta_plastic, aes(x = as.numeric(P_Mass), y = as.numeric(W_Dead), colour = Species)) + 
    geom_point(shape = 21, size = 2, colour = "black", aes(fill = Species)) +
    geom_smooth(method = "glm.nb", formula = y ~ x) +
    facet_wrap(~Species, ncol = 1, scales = "free_y", strip.position = "right") +
    labs(x = "Mass Removed From PVC Square (grams)", y = "Number of Deceased Larvae at Phase 1 End") +
    scale_colour_manual(values = c("#9fc8c8", "#298c8c")) +
    scale_fill_manual(values = c("#9fc8c8", "#298c8c")) +
    theme_light() +
    theme(legend.position = "none", strip.text = element_text(face = "italic", colour = 'black'))
  
  ##Do test
  
  #First Poisson
  Exp_dta_TM <- Exp_dta %>% filter(P_Mass != "N/A")
  m1 <- glm(as.numeric(W_Dead) ~ as.numeric(P_Mass)*Species, data = Exp_dta_TM, 
            family = poisson(link = log))
  summary(m1)
  
  #Check for overdispersion
  E2 <- resid(m1, type = "pearson")
  N  <- nrow(Exp_dta_TM)
  p  <- length(coef(m1)) + 1 # '+1' is due to theta
  sum(E2^2) / (N - p)
  
  #negative binomial glm  
  m2 <- glm.nb(as.numeric(W_Dead) ~ as.numeric(P_Mass)*Species,
               data=Exp_dta_TM)
  summary(m2)
  #Check for overdispersion
  E2 <- resid(m2, type = "pearson")
  N  <- nrow(Exp_dta_TM)
  p  <- length(coef(m2)) + 1 # '+1' is due to theta
  sum(E2^2) / (N - p)
  
  #test for differences
  lrtest(m1, m2) # poisson to neg bin
  
  
  
  
  
  
  
  