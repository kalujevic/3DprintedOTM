##########################################################
############# Data exploration K. Alujevic ###############
#################### September 2022 ######################
##########################################################

################ Sceloporus occidentalis #################

library(ggplot2)
library(tidyverse)
library(multcomp)

ggplot(data=Extracted_data,aes(x=Mass,y=MeanT)) + 
  geom_point(aes(colour=OTM_type,shape=OTM_type),size=2) + 
  geom_line(aes(colour=OTM_type))+
  #stat_smooth(aes(colour=OTM_type),method='lm',linetype='dashed') +
  facet_wrap(~Microsite)


## plotting the relationship between lizard temperature and each of the models separated per microsite

Extracted_data_restructured$Microsite <- ifelse(Extracted_data_restructured$Microsite == "rock_shade", 
                                                "Rock Shade", "Rock Sun")

plot1 <- ggplot(data=Extracted_data_restructured,aes(x=LizardT,y=MeanT)) + 
            geom_point(aes(colour=OTM_type,shape=OTM_type),size=2) + 
            stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
            facet_wrap(~Microsite,scales='free') +
            ggtitle(~bold("OTM validation")~bolditalic("Sceloporus occidentalis")) +
            scale_x_continuous(name='Lizard Temperature (°C)') + 
            scale_y_continuous(name='Model Temperature (°C)') + 
            theme_bw() +
            theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
                  axis.title = element_text(size=12, face = "bold"),
                  legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
                  legend.text = element_text(size=10),
                  strip.text.x = element_text(size = 12, face = "bold"),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank())

plot1 + geom_abline(intercept = 0, slope = 1)

ggsave("Sceloporus_occidentalis_validation_per_microsite.pdf", height = 6, width = 8)

# no outlier:

Extracted_data_restructured %>% filter(Species=="Sceloporus_occidentalis" & !row_number() %in% c(81, 82, 83, 84)) %>% 
  ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type),size=2) + 
  stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  facet_wrap(~Microsite,scales='free') +
  ggtitle(~bold("OTM validation")~bolditalic("Sceloporus occidentalis")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)

ggsave("Sceloporus_occidentalis_validation_per_microsite_no_outlier.pdf", height = 6, width = 8)


## plotting the relationship between lizard temperature and each of the models across both microsites together

plot2 <- ggplot(data=Extracted_data_restructured,aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type,shape=Microsite),size=2) + 
  stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation")~bolditalic("Sceloporus occidentalis")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

plot2 + geom_abline(intercept = 0, slope = 1)

ggsave("Sceloporus_occidentalis_validation_across_microsites.pdf", height = 6, width = 6)


##plotting the variance around each model and lizard

plot3 <- ggplot(data=Extracted_data,aes(x=OTM_type,y=SD)) + 
            geom_boxplot(aes(fill=OTM_type)) + 
            facet_wrap(~Microsite,scales='free') +
            ggtitle('OTM validation Sceloporus occidentalis') +
            scale_y_continuous(name='Standard deviation within equilibration') + 
            theme(strip.text.x = element_text(size = 10))


# no outlier:

Extracted_data_restructured %>% filter(Species=="Sceloporus_occidentalis" & !row_number() %in% c(81, 82, 83, 84)) %>% 
  ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type, shape=Microsite),size=2) + 
  stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation")~bolditalic("Sceloporus occidentalis")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)

ggsave("Sceloporus_occidentalis_validation_across_microsites_no_outlier.pdf", height = 6, width = 6)



################ Podarcis muralis #################


## plotting the relationship between lizard temperature and each of the models across microsites

Extracted_data_restructured %>%
  filter (Species=="Podarcis_muralis") %>%
  ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type,shape=Microsite),size=2) + 
  stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation")~bolditalic("Podarcis muralis")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)
  
ggsave("Podarcis_muralis_validation_across_microsites.pdf", height = 6, width = 8)



################ Anolis gundlachi #################


## plotting the relationship between lizard temperature and each of the models across microsites

Extracted_data_restructured %>%
  filter (Species=="Anolis_gundlachi") %>%
  ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type,shape=Microsite),size=2) + 
  stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation")~bolditalic("Anolis gundlachi")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)

ggsave("Anolis_gundlachi_validation_across_microsites.pdf", height = 6, width = 8)


################ Anolis pulchellus #################


## plotting the relationship between lizard temperature and each of the models across microsites

Extracted_data_restructured %>%
  filter (Species=="Anolis_pulchellus") %>%
  ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type,shape=Microsite),size=2) + 
  stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation")~bolditalic("Anolis pulchellus")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)

ggsave("Anolis_pulchellus_validation_across_microsites.pdf", height = 6, width = 8)



################ Anolis sagrei #################


## plotting the relationship between lizard temperature and each of the models across microsites

Extracted_data_restructured %>%
  filter (Species=="Anolis_sagrei") %>%
  ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type,shape=Microsite),size=2) + 
  stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation")~bolditalic("Anolis sagrei")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)

ggsave("Anolis_sagrei_validation_across_microsites.pdf", height = 6, width = 8)



################ Anolis gundlachi #################


## plotting the relationship between lizard temperature and each of the models across microsites

Extracted_data_restructured %>%
  filter (Species=="Anolis_cristatellus") %>%
  ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type,shape=Microsite),size=2) + 
  stat_smooth(aes(colour=OTM_type,fill=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation")~bolditalic("Anolis cristatellus")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)

ggsave("Anolis_christatellus_validation_across_microsites.pdf", height = 6, width = 8)



############### All species merged #################

Extracted_data_restructured %>%
  ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type),size=2) + 
  stat_smooth(aes(colour=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation all species pooled")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)


ggsave("All_species_validation_across_microsites.pdf", height = 6, width = 8)

#should we discard this outlier at less than 10C?
# --> same but without outtlier:

Extracted_data_restructured %>%  filter(!row_number() %in% c(81, 82, 83, 84)) %>% 
ggplot(aes(x=LizardT,y=MeanT)) + 
  geom_point(aes(colour=OTM_type),size=2) + 
  stat_smooth(aes(colour=OTM_type),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("OTM validation all species pooled")) +
  scale_x_continuous(name='Lizard Temperature (°C)') + 
  scale_y_continuous(name='Model Temperature (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  geom_abline(intercept = 0, slope = 1)

ggsave("All_species_validation_across_microsites_no_outlier.pdf", height = 6, width = 8)

################### Absolute difference between lizard temp and each of the model temps #####################

## across all species:

Extracted_data_restructured %>%
  ggplot(aes(x=OTM_type,y=Abs_differenceT)) + 
  geom_boxplot(aes(fill=OTM_type),color='black', size=0.5) + 
  ggtitle(~bold("Absolute difference between lizard and model temperatures")) +
  scale_y_continuous(name='|ΔT| (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
 

ggsave("Absolute_difference_between_lizard_model_temperatures_all.pdf", height = 6, width = 10)


#a<-lm(Abs_differenceT~OTM_type, data=Extracted_data_restructured)
#summary(a)


#Exclude copper pipe when looking across all:

No_copperpipe <- Extracted_data_restructured %>% filter(!OTM_type=="CopperPipe") 

No_copperpipe %>%
  ggplot(aes(x=OTM_type,y=Abs_differenceT)) + 
  geom_boxplot(aes(fill=OTM_type),color='black', size=0.5) + 
  ggtitle(~bold("Absolute difference between lizard and model temperatures")) +
  scale_y_continuous(name='|ΔT| (°C)') + 
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#C77CFF")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


ggsave("Absolute_difference_between_lizard_model_temperatures_all_no_copperpipe.pdf", height = 6, width = 8)


# only Sceloporus occidentalis:

Soccidendtalis <- Extracted_data_restructured %>% filter(Species=="Sceloporus_occidentalis") 

Soccidendtalis$OTM_type <- as.factor(Soccidendtalis$OTM_type)

SO <- aov(Abs_differenceT ~ OTM_type, data = Soccidendtalis) #I didn't test assumptions!! (do this for the conference)

summary(SO)

# Tukey HSD test:
#post_test <- glht(SO, linfct = mcp(OTM_type = "Tukey"))
#summary(post_test)

TukeyHSD(SO)
#diff        lwr       upr     p adj
#CopperPipe-ABS        0.59312620 -0.7418028 1.9280552 0.6506730
#CopperPLA-ABS         0.61418085 -0.7207481 1.9491098 0.6248031
#PLA-ABS               0.07688192 -1.2580470 1.4118109 0.9987643
#CopperPLA-CopperPipe  0.02105465 -1.3138743 1.3559836 0.9999744
#PLA-CopperPipe       -0.51624428 -1.8511732 0.8186847 0.7419037
#PLA-CopperPLA        -0.53729893 -1.8722279 0.7976300 0.7176067


## by species:

Extracted_data_restructured %>% filter(!OTM_type=="CopperPipe") %>%
  ggplot(aes(x=OTM_type,y=Abs_differenceT)) + 
  geom_boxplot(aes(fill=OTM_type),color='black', size=0.5) + 
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#C77CFF")) +
  ggtitle(~bold("Absolute difference between lizard and model temperatures")) +
  scale_y_continuous(name='|ΔT| (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  facet_wrap(~ Species)


ggsave("Absolute_difference_between_lizard_model_temperatures_per_species_nocopper.pdf", height = 6, width = 10)


###################  Difference between lizard temp and each of the model temps #####################

## across all species:

No_copperpipe <- Extracted_data_restructured %>% filter(!OTM_type=="CopperPipe") 

No_copperpipe %>%
  ggplot(aes(x=OTM_type,y=DifferenceT)) + 
  geom_boxplot(aes(fill=OTM_type),color='black', size=0.5) + 
  ggtitle(~bold("Difference between lizard and model temperatures")) +
  scale_y_continuous(name='ΔT (°C)') + 
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#C77CFF")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


ggsave("Difference_between_lizard_model_temperatures_all_no_copperpipe.pdf", height = 6, width = 8)

#do this but per sun and shade (need to add a category for sun and shade)!


## by species:

Extracted_data_restructured %>% filter(!OTM_type=="CopperPipe") %>%
  ggplot(aes(x=OTM_type,y=DifferenceT)) + 
  geom_boxplot(aes(fill=OTM_type),color='black', size=0.5) + 
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#C77CFF")) +
  ggtitle(~bold("Difference between lizard and model temperatures")) +
  scale_y_continuous(name='ΔT (°C)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  facet_wrap(~ Species)


ggsave("Difference_between_lizard_model_temperatures_per_species_nocopper.pdf", height = 6, width = 10)




###########################################################################################################


######################## PRINTING COSTS ###########################

## across all models:

Models_printing_cost %>%
  ggplot(aes(x=SVL_mm,y=Time_min)) + 
  geom_point(aes(colour=Material),size=2) + 
  stat_smooth(aes(colour=Material,fill=Material),method='lm',linetype='dashed', alpha=0.25) +
  ggtitle(~bold("Printing time")) +
  scale_y_continuous(name='Printing time (min)') + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, colour="black",face = "bold",hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


ggsave("Printing_time_SVL.pdf", height = 6, width = 10)
