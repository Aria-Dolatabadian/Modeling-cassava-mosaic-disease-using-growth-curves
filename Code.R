#Modeling cassava mosaic disease using growth ​curves.

#Cassava mosaic disease

## Load data into vector
weeksAfterPlantingM <- c(  4,  6,  8, 10, 12, 14, 16,
                          18, 20, 22, 24, 26, 28);
diseaseIncidenceForCassavaM <- c(0.04, 0.11, 0.18, 0.32, 0.53,
                                 0.64, 0.70, 0.73, 0.75, 0.77,
                                 0.78, 0.79, 0.79);
diseaseIncidenceForCassavaAndMaizeM <-
                               c(0.03, 0.10, 0.17, 0.27, 0.36,
                                 0.43, 0.47, 0.50, 0.51, 0.60,
                                 0.60, 0.61, 0.61);
diseaseIncidenceForCassavaAndCowpeaM <-
                               c(0.03, 0.09, 0.17, 0.27, 0.35,
                                 0.42, 0.43, 0.43, 0.44, 0.55,
                                 0.56, 0.5700, 0.57);
diseaseIncidenceForCassavaAndMaizeAndCowpeaM <-
                               c(0.0200, 0.08, 0.16, 0.27, 0.35,
                                 0.41, 0.41, 0.42, 0.42, 0.52,
                                 0.53, 0.54, 0.55);

## Set default values for drawing a plot
default_type = 'o';
default_pch = 22;
default_xlim = c(4, 28);
default_ylim = c(0, 1);
default_xlab = 'Weeks After Planting';
default_ylab = 'Disease Incidence (%)';
default_color1 = 'orange';
default_color2 = 'blue';
default_color3 = 'green';
default_color4 = 'black';
default_lty_for_actual_data = 1;
default_lty_for_fitted_data = 2;
default_lwd_for_actual_data = 2;
default_lwd_for_fitted_data = 1;
## Plot disease incidence (y) vs. weeks after planting
# Plot disease incidence for cassava alone
plot(
     weeksAfterPlantingM, diseaseIncidenceForCassavaM,
     type = default_type,
     pch = default_pch,
     lwd = default_lwd_for_actual_data,
     xlim = default_xlim,
     ylim = default_ylim,
     xlab = default_xlab,
     ylab = default_ylab,
     col = default_color1 
);
title(main = 'Cassava mosaic disease progress');
## Draw a fitted curve given monomolecular model
# Note: intercept and slope have been already
#    estimated by Fondong et al. (2002)
esti_intercept = -0.21; 
esti_slope = 0.08;
temp = esti_intercept + esti_slope * weeksAfterPlantingM;
yM = (exp(temp) - 1) / exp(temp);
lines(
      weeksAfterPlantingM, yM,
      col = default_color1,
      lwd = default_lwd_for_fitted_data,
      lty = default_lty_for_fitted_data);
# Plot disease incidence for cassava intercropped with maize
lines(
      weeksAfterPlantingM,
      diseaseIncidenceForCassavaAndMaizeM,
      type = default_type,
      lwd = default_lwd_for_actual_data,
      col = default_color2
      );
# Draw a fitted curve given monomolecular model
esti_intercept = -0.19; 
esti_slope = 0.05;
temp = esti_intercept + esti_slope * weeksAfterPlantingM;
yM = (exp(temp) - 1) / exp(temp);
lines(
      weeksAfterPlantingM, yM,
      col = default_color2,
      lwd = default_lwd_for_fitted_data,
      lty = default_lty_for_fitted_data
);
# Plot disease incidence for cassava intercropped with cowpea
lines(
      weeksAfterPlantingM,
      diseaseIncidenceForCassavaAndCowpeaM,
      type = default_type,
      col = default_color3,
      lwd = default_lwd_for_actual_data
);
# Draw a fitted curve given monomolecular model
esti_intercept = -0.15; 
esti_slope = 0.05;
temp = esti_intercept + esti_slope * weeksAfterPlantingM;
yM = (exp(temp) - 1) / exp(temp);
lines(
      weeksAfterPlantingM, yM,
      col = default_color3,
      lwd = default_lwd_for_fitted_data,
      lty = default_lty_for_fitted_data
);
# Plot disease incidence for cassava intercropped
#    with maize and cowpea
lines(
      weeksAfterPlantingM,
      diseaseIncidenceForCassavaAndMaizeAndCowpeaM,
      type = default_type,
      col = default_color4,
      lwd = default_lwd_for_actual_data
);
# Draw a fitted curve given monomolecular model
esti_intercept = -0.19; 
esti_slope = 0.04;
temp = esti_intercept + esti_slope * weeksAfterPlantingM;
yM = (exp(temp) - 1) / exp(temp);
lines(
      weeksAfterPlantingM, yM,
      col = default_color4,
      lwd = default_lwd_for_fitted_data,
      lty = default_lty_for_fitted_data
);
## Add legend to the graph
legend(
      'topleft',
       c(
          'Cassava',
          'Cassava+maize',
          'Cassava+cowpea',
          'Cassava+maize+cowpea'
        ),
       pch = c(22),
       lty = 1,
       col = c('orange','blue','green','black'),
       title = 'Cropping Type',
       inset = 0.01
)





#Modeling the whitefly population over time.


## Load data into vectors
weeksAfterPlantingM <-
    c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
WFForCassavaM <-
    c(4, 7, 14, 33, 28, 39, 24, 14, 6, 10, 8, 7, 4);
WFForCassavaAndMaizeM <-
    c(3, 4, 6, 13, 8, 12, 7, 3, 3, 4, 3, 3, 4);
WFForCassavaAndCowpeaM <-
    c(2, 4, 5, 6, 8, 10, 16, 4, 4, 6, 4, 4, 3);
WFForCassavaAndMaizeAndCowpeaM <-
    c(2, 3, 7, 6, 8, 13, 10, 4, 4, 9, 8, 7, 6);
## Set default values for drawing a plot
default_type = 'o';
default_pch = 22;
default_xlim = c(4, 17);
default_ylim = c(0, 45);
default_xlab = 'Weeks after planting';
default_ylab = 'Number of adult whiteflies';
default_color1 = 'orange';
default_color2 = 'blue';
default_color3 = 'green';
default_color4 = 'black';
default_lty_for_actual_data = 1;
default_lty_for_fitted_data = 2;
default_lwd_for_actual_data = 2;
default_lwd_for_fitted_data = 1;
## Plot insect population vs. weeks after planting plot
# Plot insect populations for cassava alone
plot(
     weeksAfterPlantingM, WFForCassavaM,
     type = default_type,
     pch = default_pch,
     lwd = default_lwd_for_actual_data,
     xlim = default_xlim,
     ylim = default_ylim,
     xlab = default_xlab,
     ylab = default_ylab,
     col = default_color1
);
title(
   main=
     'Mean number of adult whiteflies on the leaves of cassava'
);
# Plot insect populations for cassava intercropped with maize
lines(
      weeksAfterPlantingM, WFForCassavaAndMaizeM,
      type = default_type,
      lwd = default_lwd_for_actual_data,
      col = default_color2
);
# Plot insect populations for cassava intercropped with cowpea
lines(
      weeksAfterPlantingM, WFForCassavaAndCowpeaM,
      type = default_type,
      col = default_color3,
      lwd = default_lwd_for_actual_data
);
# Plot insect populations for cassava intercropped
#    with maize and cowpea
lines(
      weeksAfterPlantingM,
      WFForCassavaAndMaizeAndCowpeaM,
      type = default_type,
      col = default_color4,
      lwd = default_lwd_for_actual_data
);
## Add legend to the graph
legend(
      'topright',
       c(
          'Cassava',
          'Cassava+maize',
          'Cassava+cowpea',
          'Cassava+maize+cowpea'
        ),
       pch = 22,
       lty = 1,
       col = c(
         'orange',

         'blue',
         'green',
         'black'
        ),
       title = 'Cropping Type',
       inset = 0.05
)


#Intercropping effect on disease severity

## Load data into vector
weeksAfterPlantingM <-
    c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28);
diseaseSevForCassavaM <-
    c(2.0, 3.0, 3.9, 3.6, 4.1, 4.4, 4.5,
      3.7, 3.3, 2.7, 2.3, 2.7, 2.8);
diseaseSevForCassavaAndMaizeM <-
    c(2.3, 2.6, 3.7, 3.3, 3.7, 4.0, 4.5,
      3.6, 3.3, 2.8, 2.3, 2.7, 2.7);
diseaseSevForCassavaAndCowpeaM <-
    c(1.9, 2.6, 3.3, 3.2, 3.4, 3.6, 4.5,
      3.9, 3.8, 3.1, 2.6, 2.8, 2.7);
diseaseSevForCassavaAndMaizeAndCowpeaM <-
    c(2.5, 2.6, 3.2, 3.2, 2.8, 3.6, 4.5,
      3.9, 3.9, 3.4, 2.7, 2.7, 2.7);
## Set default values for drawing a plot
default_type = 'o';
default_pch = 22;
default_xlim = c(4, 33);
default_ylim = c(1.0, 5);
default_xlab = 'Weeks after planting';
default_ylab = 'Disease Severity';
default_color1 = 'orange';
default_color2 = 'blue';
default_color3 = 'green';
default_color4 = 'black';
default_lty_for_actual_data = 1;
default_lty_for_fitted_data = 2;
default_lwd_for_actual_data = 2;
default_lwd_for_fitted_data = 1;
## Plot disease severity (y) vs. weeks after planting
# Plot disease severity for cassava alone
plot(
     weeksAfterPlantingM, diseaseSevForCassavaM,
     type = default_type,
     pch = default_pch,
     lwd = default_lwd_for_actual_data,
     xlim = default_xlim,
     ylim = default_ylim,
     xlab = default_xlab,
     ylab = default_ylab,
     col = default_color1
);
title(main = 'Disease symptom severity');
# Plot disease severity for cassava intercropped
#    with maize
lines(
      weeksAfterPlantingM,
      diseaseSevForCassavaAndMaizeM,
      type = default_type,
      lwd = default_lwd_for_actual_data,
      col = default_color2
);
# Plot disease severity for cassava intercropped
#    with cowpea
lines(
      weeksAfterPlantingM,
      diseaseSevForCassavaAndCowpeaM,
      type = default_type,
      col = default_color3,
      lwd = default_lwd_for_actual_data
);
# Plot disease severity for cassava intercropped
#    with maize and cowpea
lines(
      weeksAfterPlantingM,
      diseaseSevForCassavaAndMaizeAndCowpeaM,
      type = default_type,
      col = default_color4,
      lwd = default_lwd_for_actual_data
);
## Add legend to the graph
legend(
      'bottomright',
       c(
          'Cassava',
          'Cassava+maize',
          'Cassava+cowpea',
          'Cassava+maize+cowpea'
        ),
       pch = 22,
       lty = 1,
       col = c(
          'orange',
          'blue',
          'green',
          'black'
       ),
       title = 'CroppingType',
       inset = 0.05
)

