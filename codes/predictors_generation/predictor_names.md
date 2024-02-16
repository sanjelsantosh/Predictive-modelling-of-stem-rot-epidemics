Following list provides the list of predictors naming rule and their description
# Soil temperature (ST) based predictors:
(Predictor class/Acronym == Description)
TT	== Thermal time (base T = 0)
avgST	== Average of daily mean ST
avgSTmin ==	Average of daily minimum ST
avgSTmx	== Average of daily maximum ST 
nHrST25.30 ==	Number of hours with 25< ST°C < 30
nHrST25.35 ==	Number of hours with 25< ST°C < 35
nHrSTa30 ==	Number of hours with ST°C > 30

# Relative Humidity (RH) based predictors:
avgRH	== Average of daily mean RH
avgRHmin ==	Average of daily minimum RH
avgRHmx	== Average of daily maximum RH 
nHrRHa80	== Number of hours with RH > 80%
nHrRHa85	== Number of hours with RH > 85%
nHrRHa90	== Number of hours with RH > 90%

# Ineraction (ST, RH) based predictors:
nHrRHa80ST25.30	== Number of hours with RH>80% and 25<ST°C<30
nHrRHa85ST25.30	== Number of hours with RH>85% and 25<ST°C<30
nHrRHa90ST25.30	== Number of hours with RH>90% and 25<ST°C<30
avgHTR	== Average of mean daily RH / mean daily ST

# NASA's SMAP surface soil moisture based predictors:
avgGWET	== Average of daily ground wetness (GWET)
cumGWET	== Cumulative daily GWET
nDaysGWET50	== Number of hours with GWET > 50%

# Precipitation (rainfall + irrigation)	based predictors:
avgDRnI	Average of total daily precipitation 
cumDRnI	Cumulative daily precipitation 

# Evapotranspiration based predictors
avgETin	Average of daily evapotranspiration
cumETin	Cumulative daily evapotranspiration

# Solar radiation	based predictor
cumSR	Cumulative hourly solar radiation
