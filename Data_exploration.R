##########################################################
############# Data exploration K. Alujevic ###############
#################### September 2022 ######################
##########################################################

################ Sceloporus occidentalis #################

library(ggplot2)
library(tidyverse)

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

#should we discard this outlier at less than 10C?

ggsave("All_species_validation_across_microsites.pdf", height = 6, width = 8)



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


a<-lm(Abs_differenceT~OTM_type, data=Extracted_data_restructured)
summary(a)

## by species:

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
        panel.grid.major = element_blank()) +
  facet_wrap(~ Species)


ggsave("Absolute_difference_between_lizard_model_temperatures_per_species.pdf", height = 6, width = 11)


