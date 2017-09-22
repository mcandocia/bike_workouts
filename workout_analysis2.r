#setwd('C:\\Users\\mscan\\Downloads\\workout_gpx\\strava_gpx')
setwd('/ntfsl/data/workouts')
library(readODS)
library(lubridate)
library(plyr)
library(dplyr)
library(leaps)
library(KernSmooth)
library(ggplot2)
library(scales)
library(sjPlot)
library(reshape2)

#workouts = read_ods('bike_and_run_gpx_info.ods',col_types=NA)
workouts = read_ods('workout_gpx/strava_gpx/bike_and_run_gpx_info.ods', col_types=NA)
workouts$date = strptime(workouts$date, '%m/%d/%y')
workouts$time = strptime(workouts$time, '%r')
workouts$hour = strftime(workouts$time, '%H')
workouts$minute = strftime(workouts$time, '%M')
workouts$strava_estimated_calories = as.numeric(workouts$strava_estimated_calories)
workouts$elevation_change_feet = as.numeric(workouts$elevation_change_feet)
workouts$distance = as.numeric(workouts$distance)
workouts$date = as.POSIXct(workouts$date)
workouts$time = as.POSIXct(workouts$time)
workouts$filename = tolower(workouts$filename)
workouts$filename = paste0(tolower(workouts$filename),'.gpx')


convert_time_to_minutes <- function(x){
  x = ifelse(grepl('.*:.*:.*',x), x, paste0('00:',x))
  hour = as.numeric(gsub('^([0-9]+):.*','\\1', x))
  minute = as.numeric(gsub('^.*?:([0-9]+):.*','\\1',x))
  second = as.numeric(gsub('.*:([0-9]+)$','\\1',x))
  return(60 * hour + minute + second/60.)
}

workouts$duration_numeric = convert_time_to_minutes(workouts$duration)
workouts$elapsed_time_numeric = convert_time_to_minutes(workouts$duration)

workouts$average_speed = 60*workouts$distance/workouts$duration_numeric 
workouts$average_elapsed_speed = 60*workouts$distance/workouts$elapsed_time_numeric
rownames(workouts) = workouts$filename 
################
###REGRESSION###
################
runs = workouts %>% filter(type=='run')
rides = workouts %>% filter(type=='bike', !grepl('mountains',comments)) 

#basic model
#grepl removes one
ride_cal_model = lm(strava_estimated_calories ~ distance + 
                      duration_numeric + 
                      average_speed:distance +
                      elevation_change_feet,
                    data=rides)

srcm = step(ride_cal_model, direction='both', k=log(76))
summary(srcm)

data_line = data.frame(x=c(0,3300), y=c(0,3300))

ggplot() +
  geom_point(data=cbind(rides, regression_estimate = srcm$fitted.values), aes(x=strava_estimated_calories, y=regression_estimate, size=distance, alpha=0.8, color=device)) + 
  ggtitle('Strava Calorie Estimate vs. Regression Model Estimate',
          subtitle=bquote(paste("R"^"2","=0.90"))) + xlab('Strava Calories Estimate') + ylab("Regression Calories Estimate") + 
  theme_bw() + 
  geom_line(data=data_line, aes(x=x, y=y,alpha=0.8)) + 
  scale_alpha_identity() + 
  scale_x_continuous(breaks=seq(0,3500,200)) + scale_y_continuous(breaks=seq(0,3000,200)) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) + 
  guides(size=guide_legend('Distance (miles)'),
         color=guide_legend("Recording Device"))


#using device
ride_cal_model_device = lm(strava_estimated_calories ~ distance + duration_numeric + 
                      average_speed:distance +
                      elevation_change_feet + device , 
                    data=rides)

srcmd = step(ride_cal_model_device, direction='both', k=log(76))
summary(srcmd)


ggplot() +
  geom_point(data=cbind(rides, regression_estimate = srcmd$fitted.values), aes(x=strava_estimated_calories, y=regression_estimate, size=distance, alpha=0.8, shape=device)) + 
  ggtitle('Strava Calorie Estimate vs. Regression Model Estimate', 
          subtitle=bquote(paste("Using GPS device as variable | R"^"2","=0.93"))) + xlab('Strava Calories Estimate') + ylab("Regression Calories Estimate") + 
  theme_bw() + 
  geom_line(data=data_line, aes(x=x, y=y, color="red",alpha=0.8)) + 
  scale_color_identity() + scale_alpha_identity() + 
  scale_x_continuous(breaks=seq(0,3500,200)) + scale_y_continuous(breaks=seq(0,3000,200)) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) + 
  guides(size=guide_legend('Distance (miles)'),
         shape=guide_legend("Recording Device"))



#using device with interactions
ride_cal_model_interactions = lm(strava_estimated_calories ~ distance*device + duration_numeric*device + 
                      average_speed:distance*device +
                      elevation_change_feet*device + device , 
                    data=rides%>% filter(!grepl('mountains',comments)))

srcmi = step(ride_cal_model_interactions, direction='both', k=log(76))
summary(srcmi)

ggplot() +
  geom_point(data=cbind(rides, regression_estimate = srcmi$fitted.values), aes(x=strava_estimated_calories, y=regression_estimate, size=distance, alpha=0.8, color=device)) + 
  ggtitle('Strava Calorie Estimate vs. Regression Model Estimate', 
          subtitle=bquote(paste("Using GPS device as variable with interaction effects | R"^"2","=0.96"))) + xlab('Strava Calories Estimate') + ylab("Regression Calories Estimate") + 
  theme_bw() + 
  geom_line(data=data_line, aes(x=x, y=y,alpha=0.8)) + 
  scale_alpha_identity() + 
  scale_x_continuous(breaks=seq(0,3500,200)) + scale_y_continuous(breaks=seq(0,3000,200)) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) + 
  guides(size=guide_legend('Distance (miles)'),
         color=guide_legend("Recording Device"))

#HTML tables for models...
sjt.lm(srcm, srcmd, srcmi,
       depvar.labels=c("Calories","Calories (w/Device)", "Calories (w/Device Interactions)"), 
       pred.labels=c("Ride Duration","Ride Distance","Strava App Used?","Distance * Average Speed", "Distance (If Strava App Used)", "Distance * Average Speed (If Strava App Used)"),
       file='initial_regression_tables.html')


#combined data frame

dfr1 = cbind(rides, regression_estimate = srcm$fitted.values) %>% mutate(model='Calorie Estimate')
dfr2 = cbind(rides, regression_estimate = srcmd$fitted.values) %>% mutate(model='Calorie Estimate\n(with Device as variable)')
dfr3 = cbind(rides, regression_estimate = srcmi$fitted.values) %>% mutate(model='Calorie Estimate\n(with Device interactions)')

dfr = rbind(dfr1, dfr2, dfr3)
ggplot() +
  geom_point(data=dfr, aes(x=strava_estimated_calories, y=regression_estimate, size=distance, alpha=0.8, color=device)) + 
  ggtitle('Strava Calorie Estimate vs. Regression Model Estimate', 
          subtitle=bquote(paste("using 3 different regression models"))) + xlab('Strava Calories Estimate') + ylab("Regression Calories Estimate") + 
  theme_bw() + 
  geom_line(data=data_line, aes(x=x, y=y,alpha=0.8)) + 
  scale_alpha_identity() + 
  scale_x_continuous(breaks=seq(0,3500,500)) + scale_y_continuous(breaks=seq(0,3000,200)) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) + 
  guides(size=guide_legend('Distance (miles)'),
         color=guide_legend("Recording Device")) + 
  facet_grid(.~model)


##################
###GPS ANALYSIS###
##################

#combine the raw CSVs that were processed in Python using BeautifulSoup
csv_workout_files = dir('raw_csv')

read_workout_csv <- function(x, smooth=1, degree=2){
  df = read.csv(paste0('raw_csv/',x))
  df$source = x 
  if (smooth > 1){
    n_entries = nrow(df)
    df = df %>% mutate(distance_smooth=locpoly(1:n_entries, distance, degree=degree, gridsize=n_entries, bandwidth=smooth)$y)
  }
  else{
    df$distance_smooth = df$distance
  }
  return(df)
}

adjust_speed <- function(data, smooth, degree, use_timediff=TRUE){
  #adds a smoothened speed/pace to data
  if (use_timediff){
    if (smooth > 0)
      data$distance_smooth = locpoly(1:nrow(data), data$distance/c(1, diff(data$time)), degree=degree, gridsize=nrow(data), bandwidth=smooth)$y
    else
      data$distance_smooth = data$distance/c(1, diff(data$time))
  }
  else{
    if (smooth > 0)
      data$distance_smooth = locpoly(1:nrow(data), data$distance, degree=degree, gridsize=nrow(data), bandwidth=smooth)$y
    else
      data$distance_smooth = data$distance 
  }    
  
  # print(head(data$distance_smooth))
  if (use_timediff){
    data = data %>% mutate(speed = distance*3600, speed_smooth=distance_smooth*3600, 
                           pace=1/(60*distance), pace_smooth = 1/(60*distance_smooth),
                           pace_smooth = pmin(pace_smooth, 25),
                           speed_smooth2 = speed_smooth^2,
                           speed_smooth3 = speed_smooth^3, 
                           speed2 = speed^2, 
                           speed3 = speed^3)
  }
  else {
    data = data %>% mutate(speed = distance*3600, speed_smooth=distance_smooth*3600, 
                           pace=1/(60*distance), pace_smooth = 1/(60*distance_smooth),
                           speed_smooth2 = speed_smooth^2,
                           speed_smooth3 = speed_smooth^3, 
                           speed2 = speed^2, 
                           speed3 = speed^3)
  }
  if (use_timediff){
    #return distance to normal
    data = data %>% mutate(distance = distance * c(1, diff(time)),
                           distance_smooth = distance_smooth * c(1, diff(time)))
  }
  return(data)
}

amalgamated_workouts <<- ldply(csv_workout_files,read_workout_csv, smooth=0, degree=2)
amalgamated_workouts$time = with(amalgamated_workouts, as.POSIXct(as.character(time), format='%Y-%m-%dT%H:%M:%SZ'))
ca_workouts_td = amalgamated_workouts %>% group_by('source') %>% adjust_speed(smooth=5, degree=2, use_timediff=TRUE)
ca_workouts_td_us = amalgamated_workouts %>% group_by('source') %>% adjust_speed(smooth=0, degree=2, use_timediff=TRUE)
ca_workouts = amalgamated_workouts %>% group_by('source') %>% adjust_speed(smooth=5, degree=2, use_timediff=FALSE)

#plot the smoothed and un-smoothed versions of a time-differenced workout
smooth_comparison = rbind(ca_workouts_td %>% filter(source=='bike_0006.csv') %>% mutate(smooth='smoothing'),
                          ca_workouts_td_us %>% filter(source=='bike_0006.csv') %>% mutate(smooth='w/o smoothing')
)

#smoothing comparison plot
ggplot(smooth_comparison, aes(x=time, y=speed_smooth, color=smooth, alpha=0.9)) + geom_line() + 
  scale_alpha_identity() + facet_grid(smooth~.) + xlab('Time of Day') + ylab('Speed (mph)') + 
  ggtitle("Comparison of Bike Ride Speed With and Without Kernel Smoothing", subtitle="quadratic polynomial smoothing with bandwidth of n=5 used") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) +
  guides(color=guide_legend("Smoothing Used?")) + 
  scale_color_manual(values=c('smoothing'="#66A61E",'w/o smoothing' ="#E6AB02"))


###utility functions 
relu <- function(x) ifelse(x > 0, x, 0)
#conversion constants
#mph -> m/s = 1609/3600 
mphconv = 1609/3600
#ft -> m = 1609/5280
ftconv = 1609/5280
#joules to Calories
jtc = 0.000239006

meters_to_feet = 3.28084

combine_data <- function(data, C1=0, C2=0, C3=1, mass=80, returnEnergyCalcOnly=FALSE, use_timediff=FALSE){
  #C2 = rolling resistance coefficient (in watts/mph)
  #C3 = area * drag coefficient 
  #check to seee if times are too long
  data = data %>% dplyr::mutate(speed_smooth = speed_smooth * (speed_smooth < 45) * (speed_smooth > 0) ) %>%
    group_by(source) %>% 
    dplyr::mutate(
      #kinetic energy
      timediff=ifelse(use_timediff, as.numeric(c(1, diff(time))), 1),
      speed_smooth = speed_smooth * (timediff < 5),#in case a long distance got recorded through intermission
      KE_diff = mass*mphconv^2*(speed_smooth^2 - 
                                  c(0,speed_smooth[1:(length(speed_smooth) -1)]^2))/2,
      #take into account elevation change
      PE_diff=0 + 
        meters_to_feet*c(0,identity(elevation_change[2:length(elevation_change)] * 9.8 * ftconv)) * mass,
      #rolling resistance
      roll_resistance = 0 + C1 * (mphconv * speed_smooth[1:length(speed_smooth)]) * timediff,
      quad_resistance = 0 + C2 * (mphconv * speed_smooth[1:length(speed_smooth)])^2 * timediff,
      #air resistance
      air_resistance= 0+ C3 * (mphconv * speed_smooth[1:length(speed_smooth)])^3 * timediff,
      energy_increase = relu(KE_diff + PE_diff + roll_resistance + quad_resistance + air_resistance)
    )  
  if (returnEnergyCalcOnly){
    return(data)
  }
  data = data %>% dplyr::summarise(#sum_v = sum(speed_smooth),
    #sum_v2 = sum(speed_smooth^2),
    #sum_v3 = sum(speed_smooth^3), 
    sum_e = sum(
      energy_increase
    ))
  
  data$filename = substr(data$source, 1, nchar(data$source) -4)
  data = data %>% dplyr::left_join(workouts, 'filename')
  return(data)
}

combine_and_plot_data <- function(mass=70, C1, C3, smooth=0,optimizer=FALSE, use_timediff=FALSE){
  #optimizer=TRUE will turn this into an optimization function
  #require(ggplot2)
  nd = combine_data(amalgamated_workouts %>% filter(substr(source,1,4)=='bike') %>% group_by(source) %>% adjust_speed(smooth=smooth, degree=2, use_timediff=use_timediff), 
                    C1=C1,C3=C3,C2=0, mass=mass, returnEnergyCalcOnly=TRUE, use_timediff=use_timediff)
  nd_summary = nd %>% summarise(total_energy=sum(energy_increase) * jtc,
                                air_energy = sum(air_resistance) * jtc,
                                roll_energy = sum(roll_resistance) * jtc)
  #join with cal info
  nd_summary = nd_summary %>% left_join(workouts %>% transmute(source=gsub('.gpx','.csv',filename), calories = strava_estimated_calories,
                                                                device=device, distance=distance),
                                        'source')
  if (!optimizer){
    with(nd_summary, plot(calories, total_energy, pch=as.numeric(factor(device)) + 2))
    abline(a=0,b=0.21,col='#FF0000AA')
    with(nd_summary %>%filter(calories > 1500), text(calories, total_energy+12, labels=source, cex=0.4))
    MSE = with(nd_summary, mean((calories*0.21 - total_energy)^2))
    MAE = with(nd_summary, mean(abs(calories*0.21 - total_energy)))
    print(paste0("MSE: ",MSE))
    print(paste0("MAE: ",MAE))
    R2 = 1-MSE/var(nd_summary$calories*0.21)
    print(paste0("R^2: ", R2))
    return(nd_summary)
  }
  else{
    MSE = with(nd_summary, mean((calories*0.21 - total_energy)^2))
    R2 = 1-MSE/var(nd_summary$calories*0.21)
    return(R2)
  }
}

optim_fn <- function(x){
  combine_and_plot_data(mass=82, C1=x[1], C3=x[2], smooth=10, optimizer=TRUE, use_timediff=TRUE)
}
#optimizer
optim(par=list(C1=5,C3=0.1), optim_fn, control=list(fnscale=-1, trace=2), lower=0.09, upper=12)
  
td = combine_and_plot_data(mass=82,C1=6.7537, C3=0.09, smooth=10, use_timediff=TRUE)

ggplot() + geom_point(data=td, aes(x=calories, y=total_energy/0.21, color=device,size=distance)) +
  geom_line(data=data_line, aes(x=x,y=y,fill='red')) + 
  scale_fill_identity() + theme_bw() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) +
  ggtitle("Strava Calorie Estimate vs. Optimized Calculation",
          subtitle=bquote(paste("coefficients: drag = 0.09*v"^3, "(m/s) W | rolling resistance = 6.754 * v (m/s) W | model R"^2,"=0.83"))) + 
  xlab("Strava Calories Estimate") + ylab("Calculation Calories")


#more realistic coefficients:

tdr = combine_and_plot_data(mass=82,C1=3, C3=0.2, smooth=10, use_timediff=TRUE)
ggplot() + geom_point(data=tdr, aes(x=calories, y=total_energy/0.21, color=device,size=distance)) +
  geom_line(data=data_line, aes(x=x,y=y,fill='red')) + 
  scale_fill_identity() + theme_bw() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) +
  ggtitle("Strava Calorie Estimate vs. Realistic Calculation",
          subtitle=bquote(paste("coefficients: drag = 0.2*v"^3, "(m/s) W | rolling resistance = 3.0 * v (m/s) W | model R"^2,"=0.73"))) + 
  xlab("Strava Calories Estimate") + ylab("Calculation Calories")

#separate categories and plot their relative effects

ttd = td %>% transmute(source=source, device=device, air_percent = air_energy/pmax(total_energy, air_energy), roll_percent = roll_energy/pmax(roll_energy, total_energy))
molten_td = melt(ttd, id.vars=(c('source','device'))) %>% mutate(calculation='optimized calculation')

ttdr = tdr %>% transmute(source=source, device=device, air_percent = air_energy/total_energy, roll_percent = roll_energy/total_energy)
molten_tdr = melt(ttdr, id.vars=(c('source','device'))) %>% mutate(calculation="realistic calculation")

molten = rbind(molten_td, molten_tdr) %>% mutate(variable = variable %>% plyr::mapvalues(from=c("air_percent", "roll_percent"), to=c('air resistance\npower percentage', 'roll resistance\npower percentage')))

ggplot(data=molten, aes(x=variable, y=value, fill=variable)) + geom_boxplot() +
  facet_grid(.~device + calculation) + xlab("") + ylab("Proportion of Total Average Power") + scale_y_continuous(label=percent) +
  ggtitle("Proportions of Cycling Power Expended Grouped by Devices and Calculations",
          subtitle="air resistance and rolling resistances compared | Cateye Stealth 50 and Strava iPhone app compared") + 
  guides(fill=guide_legend("")) + 
  theme_bw() + theme(plot.title=element_text(hjust=0.5),
                     plot.subtitle=element_text(hjust=0.5)) + 
  scale_fill_manual(values=c('air resistance\npower percentage'="#1B9E77",'roll resistance\npower percentage' ="#D95F02"))
