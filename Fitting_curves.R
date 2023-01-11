
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(drc)
library(nlme)

library(devtools)
devtools::install_github("onofriAndreaPG/aomisc")

library(aomisc)

# reorganizing the data into long format:
OTM_raw_data_long <- OTM_raw_data %>% 
                        pivot_longer(!Time, names_to='Curves', values_to='Temp')

#splitting one column into 3:
OTM_raw_data_long[c('Species', 'Trial', 'OTM')] <- str_split_fixed(OTM_raw_data_long$Curves, '_', 3)

#OTM_raw_data_long %>% 
#  ggplot(aes(x = Time, y = Temp, col = OTM)) +
#  geom_line() +
#  facet_wrap(~Species)


# Fitting the asymptotic regression model:

#https://www.statforbiology.com/nonlinearregression/usefulequations#asymptotic_regression_model

#subset only one curve for one trial:
SO_T1_Lizard <- subset(OTM_raw_data_long, Curves=="SO_T1_Lizard") %>%
  filter(!(is.na(.$Temp)))

model <- drm(Temp ~ as.matrix(Time), fct = DRC.asymReg(), na.action = na.omit, data = SO_T1_Lizard)

plot(model, log="", main = "Asymptotic regression")

summary(model)
#Parameter estimates:
  
#  Estimate Std. Error t-value   p-value    
#init:(Intercept)    2.2199e+01 1.9511e-02 1137.76 < 2.2e-16 ***
#  m:(Intercept)       2.1611e-02 2.5881e-04   83.50 < 2.2e-16 ***
#  plateau:(Intercept) 1.7325e+01 1.8585e-02  932.21 < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error:
  
#  0.05539003 (130 degrees of freedom)



#plateau:(Intercept) Estimate is the temperature at which the curve plateaus!

#coef(model)

#selects the third coefficient which is the plateau (the only one I need) --> this is my equilibration temperature!
as.numeric(model$coefficients[3])


# creating a dataframe to write these extracted parameters into:

#selecting only unique values for curves
Curves <- OTM_raw_data_long[!duplicated(OTM_raw_data_long$Curves), ]$Curves

eqTemp <- rep(NA, length(Curves))

eqTime <- rep(NA, length(Curves))

equilibration <- data.frame(Curves, eqTemp, eqTime) 


# time to equilibrate:

#give me the Time that is associated with Temp == plateau

#SO_T1_Lizard %>% filter(Temp <= model$coefficients[3]) %>% filter(Time == min(Time, na.rm = T))

#better approach --> create a new column where you'll calculate absolute difference between temp at any time and temp for plateau, and look for the smallest difference; that difference is the point where the plateau starts
eq_time <- SO_T1_Lizard %>% mutate(diff = abs(Temp - model$coefficients[3])) %>% 
  filter(diff == min(diff,na.rm = T)) %>% select(Time) %>% as.numeric()

# how do we know this is the first? --> FIXED THIS LATER IN THE FOR LOOP WHEN I'M USING ALL THE DATA



###################################

#Doing this for the full dataset:

for (i in 1:nrow(equilibration))
  {
    #filter the dataset for each curve
    filter_data <- OTM_raw_data_long %>% filter(Curves == equilibration$Curves[i]) %>%
                  filter(!is.na(Temp))
    
    #run model to fit the curve
    fit <- drm(Temp ~ as.matrix(Time), fct = DRC.asymReg(), na.action = na.omit, data = filter_data)
    
    #extract point where the curve flattens:
    asimptote <- as.numeric(fit$coefficients[3])
    
    #extract point where we have eq_temp and eq_time
    eq_point <- filter_data %>% mutate(diff = abs(Temp - asimptote)) %>% 
      filter(diff < 1.25 * min(diff,na.rm = T)) %>%  # taking all the values that are 25% bigger than the min to make sure I capture the point when it is flattening and not later when it is flat for long --> test this 25% on a few curves to make sure this is a good estimate (maybe 15% or 35% is better?)
      filter(Time == min(Time, na.rm = T))  # taking the earliest time amongst those temps
      # %>% select(Time) %>% as.numeric() # this part wasn't working anymore so I added the next line
    
    eq_time <- eq_point$Time
    eq_temp <- eq_point$Temp
    
    
    #write time to eq into new dataframe
    equilibration$eqTime[i] <- eq_time
    equilibration$eqTemp[i] <- eq_temp
}
# so some of the curves didn't fit well and the outputs are weird, so go through each and see which should be included and which not!!!

#export dataframe
write.csv(equilibration, "Equilibration_indices.csv")


######## trying to inspect fits to see which ones are not fitted well:

# path <- "C:/Users/karla/Desktop/All_OTMs_December/"

#splitting one column into 3:
#equilibration[c('Species', 'Trial', 'OTM')] <- str_split_fixed(equilibration$Curves, '_', 3)

#equilibration_SO <- equilibration %>% filter(Species == "SO")

for (i in 1:nrow(equilibration))
{
  #filter the dataset for each curve
  filter_data <- OTM_raw_data_long %>% filter(Curves == equilibration$Curves[i]) %>%
    filter(!is.na(Temp)) 
  
  fit <- drm(Temp ~ as.matrix(Time), fct = DRC.asymReg(), na.action = na.omit, data = filter_data)
  
  jpeg(paste("fit_", Curves[i], ".jpeg", sep="")) 
  
  plot(fit, log="", main=Curves[i])
  
  dev.off()
}


## after inspecting these fits, I will remove certain curves because the function cannot be fit properly. I'm manually adding a column Fit to "equilibration" where I will list YES if fit is good and NO if it should be discarded. I am also adding a column Incl_eqTime to decide if I should consider it for analyses with time to equilibrate:


####### RUNNING IT ALL WITH CLEANED DATA #########

OTM_raw_data_long_cleaned <- OTM_raw_data_cleaned %>% 
                                pivot_longer(!Time, names_to='Curves', values_to='Temp')

for (i in 1:nrow(equilibration))
{
  #filter the dataset for each curve
  filter_data <- OTM_raw_data_long_cleaned %>% filter(Curves == equilibration$Curves[i]) %>%
    filter(!is.na(Temp))
  
  #run model to fit the curve
  fit <- drm(Temp ~ as.matrix(Time), fct = DRC.asymReg(), na.action = na.omit, data = filter_data)
  
  #extract point where the curve flattens:
 asimptote <- as.numeric(fit$coefficients[3])
  
  #extract point where we have eq_temp and eq_time
  eq_point <- filter_data %>% mutate(diff = abs(Temp - asimptote)) %>% 
    filter(diff < 1.25 * min(diff,na.rm = T)) %>%  # taking all the values that are 25% bigger than the min to make sure I capture the point when it is flattening and not later when it is flat for long --> test this 25% on a few curves to make sure this is a good estimate (maybe 15% or 35% is better?)
    filter(Time == min(Time, na.rm = T))  # taking the earliest time amongst those temps
  # %>% select(Time) %>% as.numeric() # this part wasn't working anymore so I added the next line
  
  eq_time <- eq_point$Time
  eq_temp <- eq_point$Temp
  
  
  #write extracted indices into new dataframe
  equilibration$eqTime[i] <- eq_time
  equilibration$eqTemp[i] <- eq_temp
  
}

write.csv(equilibration, "Equilibration_indices_25.csv") ####### TRY WITH LESS THAN 25%!!

## trying with 1.15 instead if 1.25:

for (i in 1:nrow(equilibration))
{
  #filter the dataset for each curve
  filter_data <- OTM_raw_data_long_cleaned %>% filter(Curves == equilibration$Curves[i]) %>%
    filter(!is.na(Temp))
  
  #run model to fit the curve
  fit <- drm(Temp ~ as.matrix(Time), fct = DRC.asymReg(), na.action = na.omit, data = filter_data)
  
  #extract point where the curve flattens:
  asimptote <- as.numeric(fit$coefficients[3])
  
  #extract point where we have eq_temp and eq_time
  eq_point <- filter_data %>% mutate(diff = abs(Temp - asimptote)) %>% 
    filter(diff < 1.05 * min(diff,na.rm = T)) %>%  # taking all the values that are 15% bigger than the min to make sure I capture the point when it is flattening and not later when it is flat for long --> test this 25% on a few curves to make sure this is a good estimate (maybe 15% or 35% is better?)
    filter(Time == min(Time, na.rm = T))  # taking the earliest time amongst those temps
  # %>% select(Time) %>% as.numeric() # this part wasn't working anymore so I added the next line
  
  eq_time <- eq_point$Time
  eq_temp <- eq_point$Temp
  
  
  #write extracted indices into new dataframe
  equilibration$eqTime[i] <- eq_time
  equilibration$eqTemp[i] <- eq_temp
  
}

write.csv(equilibration, "Equilibration_indices_15.csv") 


##plotting each curve to inspect:

for (i in 1:nrow(equilibration))
{
  #filter the dataset for each curve
  filter_data <- OTM_raw_data_long_cleaned %>% filter(Curves == equilibration$Curves[i]) %>%
    filter(!is.na(Temp)) 
  
  fit <- drm(Temp ~ as.matrix(Time), fct = DRC.asymReg(), na.action = na.omit, data = filter_data)
  
  jpeg(paste("fit_", Curves[i], ".jpeg", sep="")) 
  
  plot(fit, log="", main=Curves[i])
  
  dev.off()
}


################

## correct (with extracting data from the fit and not raw data based on the fit):

OTM_raw_data_long_cleaned <- OTM_raw_data_cleaned %>% 
                                pivot_longer(!Time, names_to='Curves', values_to='Temp')


Curves <- OTM_raw_data_long[!duplicated(OTM_raw_data_long$Curves), ]$Curves

eqTemp <- rep(NA, length(Curves))

eqTime <- rep(NA, length(Curves))

equilibration <- data.frame(Curves, eqTemp, eqTime) 


for (i in 1:nrow(equilibration))
{
  #filter the dataset for each curve
  filter_data <- OTM_raw_data_long_cleaned %>% filter(Curves == equilibration$Curves[i]) %>%
    filter(!is.na(Temp))
  
  #run model to fit the curve
  fit <- drm(Temp ~ Time, fct = DRC.asymReg(), na.action = na.omit, data = filter_data)
  
  #extract point where the curve flattens:
  asimptote <- as.numeric(fit$coefficients[3])
  
  #extract point where we have eq_temp and eq_time
 # eq_point <- filter_data %>% mutate(diff = abs(Temp - asimptote)) %>% 
  #  filter(diff < 1.05 * min(diff,na.rm = T)) %>%  # taking all the values that are 15% bigger than the min to make sure I capture the point when it is flattening and not later when it is flat for long --> test this 25% on a few curves to make sure this is a good estimate (maybe 15% or 35% is better?)
  #  filter(Time == min(Time, na.rm = T))  # taking the earliest time amongst those temps
  # %>% select(Time) %>% as.numeric() # this part wasn't working anymore so I added the next line
  
  prediction_df <- tibble(time=filter_data$Time) #created a df to store all the times (all y for the fit function) 
  
  prediction_df$prediction_temp <- predict(fit, as.matrix(prediction_df$time)) #using the model fit we predicted temp for all times (all x for y)
  
  prediction_df$diff <- abs(prediction_df$prediction_temp - asimptote) #subtracted the asimptote to temp prediction for all x
  
  eq_point  <- prediction_df %>% filter(diff < 0.025*asimptote)  # extracting those times for which the difference between the temp and the asimptote is smaller than 2.5 of the asimptote
  
  if(nrow(eq_point) == 0){
    eq_point <- prediction_df %>% filter(diff < 0.1*asimptote)
    
    if(nrow(eq_point) == 0){
    eq_time <- eq_temp <- NA}
    else{
      eq_point <- eq_point %>% filter(time==min(time,na.rm=T))
      eq_time <- eq_point$time #storing time
      eq_temp <- eq_point$prediction_temp #storing temp
    }
   
  } else{
    eq_point <- eq_point %>% filter(time==min(time,na.rm=T))
    eq_time <- eq_point$time #storing time
    eq_temp <- eq_point$prediction_temp #storing temp
  }

  
  #write extracted indices into new dataframe
  equilibration$eqTime[i] <- eq_time
  equilibration$eqTemp[i] <- eq_temp
  
  print(i)
}

write.csv(equilibration, "Equilibration_indices.csv") 

## after inspecting the fits, I removed certain curves because the function cannot be fit properly. I'm manually adding a column Fit to "Equilibration_indices_assignedYN.csv" where I will list Y if fit is good and NO if it should be discarded. I am also adding a column Incl_eqTime to decide if I should consider it for analyses with time to equilibrate:

# using this data to plot and do analyses



###### creating a df to store data (raw and fitted for plotting a trial as an example):

## using trial AC_T42:

filter_data_42 <- OTM_raw_data_long_cleaned %>% filter(Trial=="T42") %>% filter(!is.na(Temp)) 
OTMS <- unique(filter_data_42$OTM)
out <- list()

for (i in 1:length(OTMS)) {
  #filter the dataset for each curve
   temp <- filter_data_42 %>% filter(OTM == OTMS[i]) 
    
  #run model to fit the curve
  fit <- drm(Temp ~ Time, fct = DRC.asymReg(), na.action = na.omit, data = temp)
  
  prediction_df_42 <- tibble(time=temp$Time) #created a df to store all the times (all y for the fit function) 
  
  prediction_df_42$prediction_temp <- predict(fit, as.matrix(prediction_df_42$time)) #using the model fit we predicted temp for all times (all x for y)
  
  prediction_df_42$raw_temp <- temp$Temp
  
  prediction_df_42$Trial <- temp$Curves
  
  prediction_df_42$OTM <- temp$OTM
  
  out[[i]] <- prediction_df_42
  
}

fin_42 <- data.table::rbindlist(out)



## using trial PM_T11:

filter_data_11 <- OTM_raw_data_long_cleaned %>% filter(Species=="PM") %>% filter(Trial=="T11") %>% filter(!is.na(Temp)) 

OTMS <- unique(filter_data_11$OTM)

out <- list()

for (i in 1:length(OTMS)) {
  #filter the dataset for each curve
  temp <- filter_data_11 %>% filter(OTM == OTMS[i]) 
  
  #run model to fit the curve
  fit <- drm(Temp ~ Time, fct = DRC.asymReg(), na.action = na.omit, data = temp)
  
  prediction_df_11 <- tibble(time=temp$Time) #created a df to store all the times (all y for the fit function) 
  
  prediction_df_11$prediction_temp <- predict(fit, as.matrix(prediction_df_11$time)) #using the model fit we predicted temp for all times (all x for y)
  
  prediction_df_11$raw_temp <- temp$Temp
  
  prediction_df_11$Trial <- temp$Curves
  
  prediction_df_11$OTM <- temp$OTM
  
  out[[i]] <- prediction_df_11
  
}

fin_11 <- data.table::rbindlist(out)
