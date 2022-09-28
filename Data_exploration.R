##########################################################
############# Data exploration K. Alujevic ###############
#################### September 2022 ######################
##########################################################

################ Sceloporus occidentalis #################

library(ggplot2)

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



