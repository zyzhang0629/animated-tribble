setwd("D:/UniversityOfLeeds/Dissertation/Data")
getwd()
library(dplyr)
#install.packages("zoo")
library(zoo)
#install.packages("lubridate")
library(lubridate)
library(ggplot2)
#install.packages("corrplot")
library(corrplot)
#install.packages("forecast")
library(forecast)
#install.packages("tseries")
library(tseries)
#install.packages("xts")
library(xts)
#install.packages("psych")
#install.packages("GPArotation")
library(psych)
library(GPArotation)
#install.packages("TSA")
library(TSA)

#Since only one monitoring station, LeedsCentre, has data on ozone,carbon monoxide and sulfur dioxide,
#not analyze these two pollutants now,If the model for the next three sites is similar, 
# add these two pollutants back into the model.
data_Dewsbury <- read.csv("Dewsbury_Ashworth_Grange.csv",header = TRUE)
data_LeedsCentre <- read.csv("Leeds_Centre.csv",header = TRUE)
data_Headingly <- read.csv("Leeds_Heading_Kerbside.csv",header = TRUE)
#data_Dewsbury only has data on PM10 and PM2.5 from July 1, 2022, 
#so it will split the data from July 1, 2022,and the data ends in 2024/6/3,after that,no data.
str(data_Dewsbury)#Verify that the data type for the first time column is date
data_Dewsbury_filtered <- data_Dewsbury %>%
  filter(Date >= as.Date("2022-07-01") & Date <= as.Date("2024-06-03"))
data_LeedsCentre_filtered <- data_LeedsCentre %>%
  filter(Date >= as.Date("2022-07-01") & Date <= as.Date("2024-06-03"))
data_Headingly_filtered <- data_Headingly %>%
  filter(Date >= as.Date("2022-07-01") & Date <= as.Date("2024-06-03"))

#Check and handle missing values
#Convert No data to NA first
#1.For Dewsbury
data_Dewsbury_filtered[data_Dewsbury_filtered == "No data"] <- NA
data_Dewsbury_filtered[data_Dewsbury_filtered == ""] <- NA
missing_values_Dewsbury <- colSums(is.na(data_Dewsbury_filtered))
print(missing_values_Dewsbury)
missing_matrix_Dewsbury <- as.numeric(is.na(data_Dewsbury_filtered))
missing_matrix_Dewsbury <- matrix(missing_matrix_Dewsbury, nrow = nrow(data_Dewsbury_filtered))
heatmap(missing_matrix_Dewsbury, Rowv = NA, Colv = NA, col = c("white", "black"), 
        scale = "none", margins = c(5, 10), xlab = "Variables", ylab = "Observations",
        main = "Missing Values Heatmap - Dewsbury")
#View the specific location of the missing value
missing_indices_Dewsbury <- which(is.na(data_Dewsbury_filtered), arr.ind = TRUE)
missing_indices_Dewsbury <- as.data.frame(missing_indices_Dewsbury)
#For the large area missing of the first three pollutants from 10 o 'clock on August 25, 
#2022 to 12 o 'clock on October 12, 2022, 
#consider using the corresponding data in 2022 to fill in
source_start <- 1330
source_end <- 2484
target_start <- 10090
target_end <- 10906

pollutant_indices <- c(3, 4, 5)
replacement_data <- data_Dewsbury_filtered[source_start:source_end, pollutant_indices]
replacement_data <- replacement_data[1:(target_end - target_start + 1), ]
data_Dewsbury_filtered[target_start:target_end, pollutant_indices] <- replacement_data
missing_values_1 <- colSums(is.na(data_Dewsbury_filtered))
print(missing_values_1)

columns_to_interpolate <- c("Nitric.oxide", "Nitrogen.dioxide", "Nitrogen.oxides.as.nitrogen.dioxide",
                            "PM10.particulate.matter..Hourly.measured.", "PM2.5.particulate.matter..Hourly.measured.",
                            "Modelled.Wind.Direction", "Modelled.Wind.Speed", "Modelled.Temperature")

for (col in columns_to_interpolate) {
  data_Dewsbury_filtered[[col]] <- na.approx(data_Dewsbury_filtered[[col]], na.rm = FALSE)
}

missing_values_after_interpolation <- sapply(data_Dewsbury_filtered[, columns_to_interpolate], function(x) sum(is.na(x)))
print(missing_values_after_interpolation)

#2.For Leeds Centre
data_LeedsCentre_filtered[data_LeedsCentre_filtered == "No data"] <- NA
data_LeedsCentre_filtered[data_LeedsCentre_filtered == ""] <- NA
missing_values_LeedsCentre <- colSums(is.na(data_LeedsCentre_filtered))
print(missing_values_LeedsCentre)
missing_matrix_LeedsCentre <- as.numeric(is.na(data_LeedsCentre_filtered))
missing_matrix_LeedsCentre <- matrix(missing_matrix_LeedsCentre, nrow = nrow(data_LeedsCentre_filtered))
heatmap(missing_matrix_LeedsCentre, Rowv = NA, Colv = NA, col = c("white", "black"), 
        scale = "none", margins = c(5, 10), xlab = "Variables", ylab = "Observations",
        main = "Missing Values Heatmap - LeedsCentre")
missing_indices_LeedsCentre <- which(is.na(data_LeedsCentre_filtered), arr.ind = TRUE)
missing_indices_LeedsCentre <- as.data.frame(missing_indices_LeedsCentre)
#If a large piece of data is missing, 
#replace it with the corresponding time data of the previous year
target_start1 <- 4796
target_end1 <- 5052
source_start1 <- 13844
source_end1 <- 14100
pollutant_indices1 <- c(3, 4, 5)
replacement_data1 <- data_LeedsCentre[source_start1:source_end1, pollutant_indices1]
replacement_data1 <- replacement_data1[1:(target_end1 - target_start1 + 1), ]
data_LeedsCentre_filtered[target_start1:target_end1, pollutant_indices1] <- replacement_data1
target_start2 <- 13616
target_end2 <- 14030
source_start2 <- 13916
source_end2 <- 14318
pollutant_indices2 <- c(3, 4, 5, 6, 7, 8)
replacement_data2 <- data_LeedsCentre[source_start2:source_end2, pollutant_indices2]
replacement_data2 <- replacement_data2[1:(target_end2 - target_start2 + 1), ]
data_LeedsCentre_filtered[target_start2:target_end2, pollutant_indices2] <- replacement_data2
missing_values_2 <- colSums(is.na(data_LeedsCentre_filtered))
print(missing_values_2)

columns_to_interpolate <- c("Nitric.oxide", "Nitrogen.dioxide", "Nitrogen.oxides.as.nitrogen.dioxide",
                            "Carbon.monoxide", "PM10.particulate.matter..Hourly.measured.", "PM2.5.particulate.matter..Hourly.measured.",
                            "Modelled.Wind.Direction", "Modelled.Wind.Speed", "Modelled.Temperature")

for (col in columns_to_interpolate) {
  data_LeedsCentre_filtered[[col]] <- na.approx(data_LeedsCentre_filtered[[col]], na.rm = FALSE)
}
missing_values_after_interpolation <- sapply(data_LeedsCentre_filtered[, columns_to_interpolate], function(x) sum(is.na(x)))
print(missing_values_after_interpolation)
#Since the pollutants in the sixth column are not found in the other two monitoring points, 
#they are temporarily deleted.
data_LeedsCentre_filtered <- data_LeedsCentre_filtered %>% select(-6)
#3.For Headingly
data_Headingly_filtered[data_Headingly_filtered == "No data"] <- NA
data_Headingly_filtered[data_Headingly_filtered == ""] <- NA
missing_values_Headingly <- colSums(is.na(data_Headingly_filtered))
print(missing_values_Headingly)
missing_matrix_Headingly <- as.numeric(is.na(data_Headingly_filtered))
missing_matrix_Headingly <- matrix(missing_matrix_Headingly, nrow = nrow(data_Headingly_filtered))
heatmap(missing_matrix_Headingly, Rowv = NA, Colv = NA, col = c("white", "black"), 
        scale = "none", margins = c(5, 10), xlab = "Variables", ylab = "Observations",
        main = "Missing Values Heatmap - Headingly")
missing_indices_Headingly <- which(is.na(data_Headingly_filtered), arr.ind = TRUE)
missing_indices_Headingly <- as.data.frame(missing_indices_Headingly)
#If a large piece of data is missing, 
#replace it with the corresponding time data of the previous year
target_start1 <- 4978
target_end1 <- 5642
source_start1 <- 14026
source_end1 <- 14702

pollutant_indices1 <- c(3, 4, 5)


replacement_data1 <- data_Headingly[source_start1:source_end1, pollutant_indices1]

replacement_data1 <- replacement_data1[1:(target_end1 - target_start1 + 1), ]

data_Headingly_filtered[target_start1:target_end1, pollutant_indices1] <- replacement_data1

target_start2 <- 4417
target_end2 <- 9347
source_start2 <- 13465
source_end2 <- 18395

pollutant_indices2 <- 6

replacement_data2 <- data_Headingly[source_start2:source_end2, pollutant_indices2,drop=FALSE]

replacement_data2 <- replacement_data2[1:(target_end2 - target_start2 + 1), , drop=FALSE]

data_Headingly_filtered[target_start2:target_end2, pollutant_indices2] <- replacement_data2

target_start3 <- 4417
target_end3 <- 5557
source_start3 <- 13465
source_end3 <- 14605


pollutant_indices3 <- 7
replacement_data3 <- data_Headingly[source_start3:source_end3, pollutant_indices3,drop=FALSE]
replacement_data3 <- replacement_data3[1:(target_end3 - target_start3 + 1), ,drop=FALSE]

data_Headingly_filtered[target_start3:target_end3, pollutant_indices3] <- replacement_data3

target_start4 <- 16105
target_end4 <- 16896
source_start4 <- 25129
source_end4 <- 25920

pollutant_indices4 <- 3:10

replacement_data4 <- data_Headingly[source_start4:source_end4, pollutant_indices4]

replacement_data4 <- replacement_data4[1:(target_end4 - target_start4 + 1), ]

data_Headingly_filtered[target_start4:target_end4, pollutant_indices4] <- replacement_data4

target_start5 <- 13273
target_end5 <- 14028
source_start5 <- 13561
source_end5 <- 14316

pollutant_indices5 <- 6

replacement_data5 <- data_Headingly[source_start5:source_end5, pollutant_indices5, drop = FALSE]

replacement_data5 <- replacement_data[1:(target_end5 - target_start5 + 1), , drop = FALSE]

data_Headingly_filtered[target_start5:target_end5, pollutant_indices5] <- replacement_data5

target_start6 <- 16105
target_end6 <- 16896
source_start6 <-16369
source_end6 <-17160

pollutant_indices6 <- 6

replacement_data6 <- data_Headingly[source_start6:source_end6, pollutant_indices6, drop = FALSE]

replacement_data6 <- replacement_data[1:(target_end6 - target_start6 + 1), , drop = FALSE]

data_Headingly_filtered[target_start6:target_end6, pollutant_indices6] <- replacement_data6

no_data_positions <- which(data_Headingly_filtered == "No data", arr.ind = TRUE)
print(no_data_positions)
data_Headingly_filtered[data_Headingly_filtered == "No data"] <- NA
missing_matrix_Headingly <- as.numeric(is.na(data_Headingly_filtered))
missing_matrix_Headingly <- matrix(missing_matrix_Headingly, nrow = nrow(data_Headingly_filtered))
heatmap(missing_matrix_Headingly, Rowv = NA, Colv = NA, col = c("white", "black"), 
        scale = "none", margins = c(5, 10), xlab = "Variables", ylab = "Observations",
        main = "Missing Values Heatmap - Headingly")
missing_indices_Headingly <- which(is.na(data_Headingly_filtered), arr.ind = TRUE)
missing_indices_Headingly <- as.data.frame(missing_indices_Headingly)
str(data_Headingly_filtered)


missing_values_3 <- colSums(is.na(data_Headingly_filtered))
print(missing_values_3)



columns_to_interpolate <- c("Nitric.oxide", "Nitrogen.dioxide", "Nitrogen.oxides.as.nitrogen.dioxide",
                             "PM10.particulate.matter..Hourly.measured.", "PM2.5.particulate.matter..Hourly.measured.",
                            "Modelled.Wind.Direction", "Modelled.Wind.Speed", "Modelled.Temperature")

for (col in columns_to_interpolate) {
  data_Headingly_filtered[[col]] <- na.approx(data_Headingly_filtered[[col]], na.rm = FALSE)
}
missing_values_after_interpolation <- sapply(data_Headingly_filtered[, columns_to_interpolate], function(x) sum(is.na(x)))
print(missing_values_after_interpolation)

#View data structures and summaries
str(data_Dewsbury_filtered)
summary(data_Dewsbury_filtered)
str(data_LeedsCentre_filtered)
summary(data_LeedsCentre_filtered)
str(data_Headingly_filtered)
summary(data_Headingly_filtered)
#Deal with the (<0 only for pollutants) using Linear interpolation 
data_Dewsbury_filtered <- data_Dewsbury_filtered %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide, 
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), 
                ~ifelse(. < 0, NA, .))) %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide, 
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), 
                ~na.approx(., na.rm = FALSE)))

summary(data_Dewsbury_filtered)
data_LeedsCentre_filtered <- data_LeedsCentre_filtered %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide, 
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), 
                ~ifelse(. < 0, NA, .))) %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide, 
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), 
                ~na.approx(., na.rm = FALSE)))

summary(data_LeedsCentre_filtered)
data_Headingly_filtered <- data_Headingly_filtered %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide, 
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), 
                ~ifelse(. < 0, NA, .))) %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide, 
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), 
                ~na.approx(., na.rm = FALSE)))

summary(data_Headingly_filtered)
#EDA

#Boxplot
data_Dewsbury_filtered$Site <- "Dewsbury_Ashworth_Grange"
data_LeedsCentre_filtered$Site <- "Leeds_Centre"
data_Headingly_filtered$Site <- "Leeds_Headingly_Kerbside"
data_combined <- rbind(data_Dewsbury_filtered, data_LeedsCentre_filtered, data_Headingly_filtered)
#For Nitrogen.dioxide
ggplot(data_combined, aes(x = Site, y = Nitrogen.dioxide)) +
  geom_boxplot() +
  labs(title = "Boxplot of Nitrogen Dioxide Levels at Three Sites",
       x = "Site",
       y = "Nitrogen Dioxide (µg/m³)") +
  theme_minimal()
#For Nitric.oxide
ggplot(data_combined, aes(x = Site, y = Nitric.oxide)) +
  geom_boxplot() +
  labs(title = "Boxplot of Nitric.oxide Levels at Three Sites",
       x = "Site",
       y = "Nitric.oxide (µg/m³)") +
  theme_minimal()
#For Nitrogen.oxides.as.nitrogen.dioxide
ggplot(data_combined, aes(x = Site, y = Nitrogen.oxides.as.nitrogen.dioxide)) +
  geom_boxplot() +
  labs(title = "Boxplot of Nitrogen.oxides.as.nitrogen.dioxide Levels at Three Sites",
       x = "Site",
       y = "Nitrogen.oxides.as.nitrogen.dioxide (µg/m³)") +
  theme_minimal()
#For PM10.particulate.matter..Hourly.measured.
ggplot(data_combined, aes(x = Site, y = PM10.particulate.matter..Hourly.measured.)) +
  geom_boxplot() +
  labs(title = "Boxplot of PM10.particulate.matter..Hourly.measured. Levels at Three Sites",
       x = "Site",
       y = "PM10.particulate.matter..Hourly.measured. (µg/m³)") +
  theme_minimal()
#For PM2.5.particulate.matter..Hourly.measured.
ggplot(data_combined, aes(x = Site, y = PM2.5.particulate.matter..Hourly.measured.)) +
  geom_boxplot() +
  labs(title = "Boxplot of PM2.5.particulate.matter..Hourly.measured. Levels at Three Sites",
       x = "Site",
       y = "PM2.5.particulate.matter..Hourly.measured. (µg/m³)") +
  theme_minimal()
#Using Linear interpolation to deal with outliers
interpolate_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(na.approx(y, na.rm = FALSE))
}
data_Dewsbury_filtered <- data_Dewsbury_filtered %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide,
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), interpolate_outliers))

summary(data_Dewsbury_filtered)
data_LeedsCentre_filtered <- data_LeedsCentre_filtered %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide,
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), interpolate_outliers))

summary(data_LeedsCentre_filtered)
data_Headingly_filtered <- data_Headingly_filtered %>%
  mutate(across(c(Nitric.oxide, Nitrogen.dioxide, Nitrogen.oxides.as.nitrogen.dioxide,
                  PM10.particulate.matter..Hourly.measured., 
                  PM2.5.particulate.matter..Hourly.measured.), interpolate_outliers))

summary(data_Headingly_filtered)
data_combined <- rbind(data_Dewsbury_filtered, data_LeedsCentre_filtered, data_Headingly_filtered)
#Sequence chart for all time(season trend)
data_combined$Datetime <- as.POSIXct(paste(data_combined$Date, data_combined$Time), format="%Y/%m/%d %H:%M:%S")
#For Nitrogen.dioxide
ggplot(data_combined, aes(x = Datetime, y = Nitrogen.dioxide)) +
  geom_line() +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  labs(title = "Time Series of Nitrogen Dioxide Levels at Three Sites",
       x = "Date",
       y = "Nitrogen Dioxide (µg/m³)") +
  theme_minimal()
#For Nitric.oxide
ggplot(data_combined, aes(x = Datetime, y = Nitric.oxide)) +
  geom_line() +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  labs(title = "Time Series of Nitric.oxide Levels at Three Sites",
       x = "Date",
       y = "Nitric.oxide (µg/m³)") +
  theme_minimal()
#For Nitrogen.oxides.as.nitrogen.dioxide
ggplot(data_combined, aes(x = Datetime, y = Nitrogen.oxides.as.nitrogen.dioxide)) +
  geom_line() +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  labs(title = "Time Series of Nitrogen.oxides.as.nitrogen.dioxide Levels at Three Sites",
       x = "Date",
       y = "Nitrogen.oxides.as.nitrogen.dioxide (µg/m³)") +
  theme_minimal()
#For PM10.particulate.matter..Hourly.measured.
ggplot(data_combined, aes(x = Datetime, y = PM10.particulate.matter..Hourly.measured.)) +
  geom_line() +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  labs(title = "Time Series of PM10.particulate.matter..Hourly.measured. Levels at Three Sites",
       x = "Date",
       y = "PM10.particulate.matter..Hourly.measured. (µg/m³)") +
  theme_minimal()
#For PM2.5.particulate.matter..Hourly.measured.
ggplot(data_combined, aes(x = Datetime, y = PM2.5.particulate.matter..Hourly.measured.)) +
  geom_line() +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  labs(title = "Time Series of PM2.5.particulate.matter..Hourly.measured. Levels at Three Sites",
       x = "Date",
       y = "PM2.5.particulate.matter..Hourly.measured. (µg/m³)") +
  theme_minimal()


#For day trend
data_Dewsbury_subset <- data_combined[1:96, ]
data_LeedsCentre_subset <- data_combined[16897:16992, ]
data_Headingly_subset <- data_combined[33793:33888, ]

data_Dewsbury_subset$Site <- "Dewsbury_Ashworth_Grange"
data_LeedsCentre_subset$Site <- "Leeds_Centre"
data_Headingly_subset$Site <- "Leeds_Headingly_Kerbside"

filtered_data <- rbind(data_Dewsbury_subset, data_LeedsCentre_subset, data_Headingly_subset)

filtered_data$Datetime <- as.POSIXct(paste(filtered_data$Date, filtered_data$Time), format="%Y/%m/%d %H:%M:%S")

filtered_data <- filtered_data %>%
  mutate(Date = as.Date(Datetime),
         Time = format(Datetime, "%H:%M:%S"))
#For Nitrogen.dioxide
ggplot(filtered_data, aes(x = Time, y = Nitrogen.dioxide, color = Site, group = Site)) +
  geom_line() +
  facet_wrap(~ Site + Date, scales = "free_y", ncol = 4) +
  labs(title = "Hourly Nitrogen Dioxide Levels at Three Sites",
       x = "Time",
       y = "Nitrogen Dioxide (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#For Nitric.oxide
ggplot(filtered_data, aes(x = Time, y = Nitric.oxide, color = Site, group = Site)) +
  geom_line() +
  facet_wrap(~ Site + Date, scales = "free_y", ncol = 4) +
  labs(title = "Hourly Nitric.oxide Levels at Three Sites",
       x = "Time",
       y = "Nitric.oxide (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#For Nitrogen.oxides.as.nitrogen.dioxide
ggplot(filtered_data, aes(x = Time, y = Nitrogen.oxides.as.nitrogen.dioxide, color = Site, group = Site)) +
  geom_line() +
  facet_wrap(~ Site + Date, scales = "free_y", ncol = 4) +
  labs(title = "Hourly Nitrogen.oxides.as.nitrogen.dioxide Levels at Three Sites",
       x = "Time",
       y = "Nitrogen.oxides.as.nitrogen.dioxide (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#For PM10.particulate.matter..Hourly.measured.
ggplot(filtered_data, aes(x = Time, y = PM10.particulate.matter..Hourly.measured., color = Site, group = Site)) +
  geom_line() +
  facet_wrap(~ Site + Date, scales = "free_y", ncol = 4) +
  labs(title = "Hourly PM10.particulate.matter..Hourly.measured. Levels at Three Sites",
       x = "Time",
       y = "PM10.particulate.matter..Hourly.measured. (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#For PM2.5.particulate.matter..Hourly.measured.
ggplot(filtered_data, aes(x = Time, y = PM2.5.particulate.matter..Hourly.measured., color = Site, group = Site)) +
  geom_line() +
  facet_wrap(~ Site + Date, scales = "free_y", ncol = 4) +
  labs(title = "Hourly PM2.5.particulate.matter..Hourly.measured. Levels at Three Sites",
       x = "Time",
       y = "PM2.5.particulate.matter..Hourly.measured. (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Model for a single pollutant
#For Nitric.oxide
#For Dewsbury

data_Dewsbury_filtered$Datetime <- paste(data_Dewsbury_filtered$Date, data_Dewsbury_filtered$Time)

data_Dewsbury_filtered$Datetime <- as.POSIXct(data_Dewsbury_filtered$Datetime, format="%Y/%m/%d %H:%M:%S", tz="UTC")

na_rows <- which(is.na(data_Dewsbury_filtered$Datetime))
print(data_Dewsbury_filtered[na_rows, ])

time_series_Nitric_oxide_1 <- data_Dewsbury_filtered %>%
  select(Datetime, Nitric.oxide) %>%
  arrange(Datetime)
str(time_series_Nitric_oxide_1)
plot(time_series_Nitric_oxide_1$Datetime, time_series_Nitric_oxide_1$Nitric.oxide, type='l', 
     xlab='Time', ylab='Nitric Oxide', main='Time Series of Nitric Oxide in Dewsbury')
#From the image, the sequence is non-stationary because it does not fluctuate around a constant value

acf(time_series_Nitric_oxide_1$Nitric.oxide)
#There is a significant peak about every 24 lag periods
adf_result <- adf.test(time_series_Nitric_oxide_1$Nitric.oxide)
print(adf_result)#stationary

kpss_result <- kpss.test(time_series_Nitric_oxide_1$Nitric.oxide)
print(kpss_result)#not stationary

#diff=1
nitric_oxide_diff <- diff(time_series_Nitric_oxide_1$Nitric.oxide, differences = 1)
diff_data <- data.frame(Datetime = time_series_Nitric_oxide_1$Datetime[-1], NitricOxideDiff = nitric_oxide_diff)

ggplot(diff_data, aes(x = Datetime, y = NitricOxideDiff)) +
  geom_line() +
  labs(title = "First Order Differenced Time Series of Nitric Oxide",
       x = "Date",
       y = "Differenced Nitric Oxide (µg/m³)") +
  theme_minimal()

seasonal_period <- 24

seasonal_diff <- diff(time_series_Nitric_oxide_1$Nitric.oxide, lag = seasonal_period, differences = 1)

seasonal_diff_data <- data.frame(Datetime = time_series_Nitric_oxide_1$Datetime[(seasonal_period + 1):length(time_series_Nitric_oxide_1$Datetime)], 
                                 SeasonalDifference = seasonal_diff)

plot(seasonal_diff_data$Datetime, seasonal_diff_data$SeasonalDifference, type = "l", 
     xlab = "Time", ylab = "Seasonal Difference", main = "Seasonal Differenced Time Series")

acf(seasonal_diff_data$SeasonalDifference, main = "ACF Plot of Seasonal Differenced Series")

adf_test <- adf.test(seasonal_diff)
print(adf_test)

pacf(seasonal_diff_data$SeasonalDifference, main = "PACF Plot of Seasonal Differenced Series")
Sys.setlocale("LC_TIME", "C")
seasonal_diff_data <- seasonal_diff_data[order(seasonal_diff_data$Datetime), ]


split_point <- floor(0.8 * nrow(seasonal_diff_data))


train_data <- seasonal_diff_data[1:split_point, ]
test_data <- seasonal_diff_data[(split_point + 1):nrow(seasonal_diff_data), ]


ts_train <- ts(train_data$SeasonalDifference, frequency = seasonal_period)


fit <- auto.arima(ts_train)
summary(fit)#ARIMA(1,0,2)(2,0,0)[12] with zero mean 


forecasted_values <- forecast(fit, h = nrow(test_data))


comparison_df <- data.frame(
  Datetime = test_data$Datetime,
  Actual = test_data$SeasonalDifference,
  Forecast = as.numeric(forecasted_values$mean)
)

ggplot(comparison_df, aes(x = Datetime)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Forecast, color = "Forecast")) +
  labs(title = 'Actual vs Forecasted Values', x = 'Time', y = 'Values') +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()
#The predicted value (blue) and the actual value (red) are very close in most of the time periods, 
#indicating that the model has a better predictive effect in these time periods. 
#However, where there are some spikes and outliers, the model's predictions are less effective. 
#This usually occurs when the actual value changes drastically or when an unusual event occurs.
mse <- mean((comparison_df$Forecast - comparison_df$Actual)^2)
rmse <- sqrt(mse)
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

recent_days <- 7
recent_data <- seasonal_diff_data[(nrow(seasonal_diff_data) - recent_days * 24 + 1):nrow(seasonal_diff_data), ]
forecast_72h <- forecast(fit, h = 48)
print(forecast_72h)
future_df <- data.frame(Datetime = seq(from = max(seasonal_diff_data$Datetime), by = "hour", length.out = 48),
                        Forecast = as.numeric(forecast_72h$mean))
ggplot() +
  geom_line(data = recent_data, aes(x = Datetime, y = SeasonalDifference, color = "Actual")) +
  geom_line(data = future_df, aes(x = Datetime, y = Forecast, color = "Forecast")) +
  labs(title = 'Forecast for the Next 48 Hours', x = 'Time', y = 'Values') +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal() 

#For Leeds centre
data_LeedsCentre_filtered$Datetime <- paste(data_LeedsCentre_filtered$Date, data_LeedsCentre_filtered$Time)

data_LeedsCentre_filtered$Datetime <- as.POSIXct(data_LeedsCentre_filtered$Datetime, format="%Y/%m/%d %H:%M:%S", tz="UTC")

na_rows <- which(is.na(data_LeedsCentre_filtered$Datetime))
print(data_LeedsCentre_filtered[na_rows, ])

time_series_Nitric_oxide_2 <- data_LeedsCentre_filtered %>%
  select(Datetime, Nitric.oxide) %>%
  arrange(Datetime)
str(time_series_Nitric_oxide_2)
plot(time_series_Nitric_oxide_2$Datetime, time_series_Nitric_oxide_2$Nitric.oxide, type='l', 
     xlab='Time', ylab='Nitric Oxide', main='Time Series of Nitric Oxide in LeedsCentre')
#From the image, the sequence is non-stationary because it does not fluctuate around a constant value

acf(time_series_Nitric_oxide_2$Nitric.oxide)
#There is a significant peak about 24 lag periods
adf_result <- adf.test(time_series_Nitric_oxide_2$Nitric.oxide)
print(adf_result)#stationary

kpss_result <- kpss.test(time_series_Nitric_oxide_2$Nitric.oxide)
print(kpss_result)#not stationary

#diff=1
nitric_oxide_diff <- diff(time_series_Nitric_oxide_2$Nitric.oxide, differences = 1)
diff_data <- data.frame(Datetime = time_series_Nitric_oxide_2$Datetime[-1], NitricOxideDiff = nitric_oxide_diff)

ggplot(diff_data, aes(x = Datetime, y = NitricOxideDiff)) +
  geom_line() +
  labs(title = "First Order Differenced Time Series of Nitric Oxide",
       x = "Date",
       y = "Differenced Nitric Oxide (µg/m³)") +
  theme_minimal()

seasonal_period <- 24

seasonal_diff <- diff(time_series_Nitric_oxide_2$Nitric.oxide, lag = seasonal_period, differences = 1)

seasonal_diff_data <- data.frame(Datetime = time_series_Nitric_oxide_2$Datetime[(seasonal_period + 1):length(time_series_Nitric_oxide_2$Datetime)], 
                                 SeasonalDifference = seasonal_diff)

plot(seasonal_diff_data$Datetime, seasonal_diff_data$SeasonalDifference, type = "l", 
     xlab = "Time", ylab = "Seasonal Difference", main = "Seasonal Differenced Time Series")

acf(seasonal_diff_data$SeasonalDifference, main = "ACF Plot of Seasonal Differenced Series")

adf_test <- adf.test(seasonal_diff)
print(adf_test)#stationary

pacf(seasonal_diff_data$SeasonalDifference, main = "PACF Plot of Seasonal Differenced Series")
Sys.setlocale("LC_TIME", "C")
seasonal_diff_data <- seasonal_diff_data[order(seasonal_diff_data$Datetime), ]

split_point <- floor(0.8 * nrow(seasonal_diff_data))


train_data <- seasonal_diff_data[1:split_point, ]
test_data <- seasonal_diff_data[(split_point + 1):nrow(seasonal_diff_data), ]


ts_train <- ts(train_data$SeasonalDifference, frequency = seasonal_period)


fit <- auto.arima(ts_train)
summary(fit)#ARIMA(1,0,1)(2,0,0)[24] with zero mean


forecasted_values <- forecast(fit, h = nrow(test_data))


comparison_df <- data.frame(
  Datetime = test_data$Datetime,
  Actual = test_data$SeasonalDifference,
  Forecast = as.numeric(forecasted_values$mean)
)

ggplot(comparison_df, aes(x = Datetime)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Forecast, color = "Forecast")) +
  labs(title = 'Actual vs Forecasted Values', x = 'Time', y = 'Values') +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()
mse <- mean((comparison_df$Forecast - comparison_df$Actual)^2)
rmse <- sqrt(mse)
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

recent_days <- 7
recent_data <- seasonal_diff_data[(nrow(seasonal_diff_data) - recent_days * 24 + 1):nrow(seasonal_diff_data), ]
forecast_72h <- forecast(fit, h = 48)
print(forecast_72h)
future_df <- data.frame(Datetime = seq(from = max(seasonal_diff_data$Datetime), by = "hour", length.out = 48),
                        Forecast = as.numeric(forecast_72h$mean))
ggplot() +
  geom_line(data = recent_data, aes(x = Datetime, y = SeasonalDifference, color = "Actual")) +
  geom_line(data = future_df, aes(x = Datetime, y = Forecast, color = "Forecast")) +
  labs(title = 'Forecast for the Next 48 Hours', x = 'Time', y = 'Values') +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal() 

#For Headingly
data_Headingly_filtered$Datetime <- paste(data_Headingly_filtered$Date, data_Headingly_filtered$Time)

data_Headingly_filtered$Datetime <- as.POSIXct(data_Headingly_filtered$Datetime, format="%Y/%m/%d %H:%M:%S", tz="UTC")

na_rows <- which(is.na(data_Headingly_filtered$Datetime))
print(data_Headingly_filtered[na_rows, ])

time_series_Nitric_oxide_3 <- data_Headingly_filtered %>%
  select(Datetime, Nitric.oxide) %>%
  arrange(Datetime)
str(time_series_Nitric_oxide_3)
plot(time_series_Nitric_oxide_3$Datetime, time_series_Nitric_oxide_3$Nitric.oxide, type='l', 
     xlab='Time', ylab='Nitric Oxide', main='Time Series of Nitric Oxide in Headingly')
#From the image, the sequence is non-stationary because it does not fluctuate around a constant value

acf(time_series_Nitric_oxide_3$Nitric.oxide)
#There is a significant peak about 24 lag periods
adf_result <- adf.test(time_series_Nitric_oxide_3$Nitric.oxide)
print(adf_result)#stationary

kpss_result <- kpss.test(time_series_Nitric_oxide_3$Nitric.oxide)
print(kpss_result)#not stationary

#diff=1
nitric_oxide_diff <- diff(time_series_Nitric_oxide_3$Nitric.oxide, differences = 1)
diff_data <- data.frame(Datetime = time_series_Nitric_oxide_3$Datetime[-1], NitricOxideDiff = nitric_oxide_diff)

ggplot(diff_data, aes(x = Datetime, y = NitricOxideDiff)) +
  geom_line() +
  labs(title = "First Order Differenced Time Series of Nitric Oxide",
       x = "Date",
       y = "Differenced Nitric Oxide (µg/m³)") +
  theme_minimal()

seasonal_period <- 24

seasonal_diff <- diff(time_series_Nitric_oxide_3$Nitric.oxide, lag = seasonal_period, differences = 1)

seasonal_diff_data <- data.frame(Datetime = time_series_Nitric_oxide_3$Datetime[(seasonal_period + 1):length(time_series_Nitric_oxide_2$Datetime)], 
                                 SeasonalDifference = seasonal_diff)

plot(seasonal_diff_data$Datetime, seasonal_diff_data$SeasonalDifference, type = "l", 
     xlab = "Time", ylab = "Seasonal Difference", main = "Seasonal Differenced Time Series")

acf(seasonal_diff_data$SeasonalDifference, main = "ACF Plot of Seasonal Differenced Series")

adf_test <- adf.test(seasonal_diff)
print(adf_test)#stationary

pacf(seasonal_diff_data$SeasonalDifference, main = "PACF Plot of Seasonal Differenced Series")
Sys.setlocale("LC_TIME", "C")
seasonal_diff_data <- seasonal_diff_data[order(seasonal_diff_data$Datetime), ]

split_point <- floor(0.8 * nrow(seasonal_diff_data))


train_data <- seasonal_diff_data[1:split_point, ]
test_data <- seasonal_diff_data[(split_point + 1):nrow(seasonal_diff_data), ]


ts_train <- ts(train_data$SeasonalDifference, frequency = seasonal_period)


fit <- auto.arima(ts_train)
summary(fit)#ARIMA(1,0,1)(2,0,0)[24] with zero mean 


forecasted_values <- forecast(fit, h = nrow(test_data))


comparison_df <- data.frame(
  Datetime = test_data$Datetime,
  Actual = test_data$SeasonalDifference,
  Forecast = as.numeric(forecasted_values$mean)
)

ggplot(comparison_df, aes(x = Datetime)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Forecast, color = "Forecast")) +
  labs(title = 'Actual vs Forecasted Values', x = 'Time', y = 'Values') +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()
mse <- mean((comparison_df$Forecast - comparison_df$Actual)^2)
rmse <- sqrt(mse)
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

recent_days <- 7
recent_data <- seasonal_diff_data[(nrow(seasonal_diff_data) - recent_days * 24 + 1):nrow(seasonal_diff_data), ]
forecast_72h <- forecast(fit, h = 48)
print(forecast_72h)
future_df <- data.frame(Datetime = seq(from = max(seasonal_diff_data$Datetime), by = "hour", length.out = 48),
                        Forecast = as.numeric(forecast_72h$mean))
ggplot() +
  geom_line(data = recent_data, aes(x = Datetime, y = SeasonalDifference, color = "Actual")) +
  geom_line(data = future_df, aes(x = Datetime, y = Forecast, color = "Forecast")) +
  labs(title = 'Forecast for the Next 48 Hours', x = 'Time', y = 'Values') +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal() 

#Correlation
#For Dewsbury
pollutant_data_Dewsbury <- data_Dewsbury_filtered[, c(3, 4, 5, 6,7)]
correlation_matrix_Dewsbury <- cor(pollutant_data_Dewsbury)

print(correlation_matrix_Dewsbury)
corrplot(correlation_matrix_Dewsbury, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7, cl.cex = 0.7, title = "Correlation Matrix of Pollutants_Dewsbury")
#For LeedsCentre
pollutant_data_LeedsCentre <- data_LeedsCentre_filtered[, c(3, 4, 5, 6,7)]
correlation_matrix_LeedsCentre <- cor(pollutant_data_LeedsCentre)

print(correlation_matrix_LeedsCentre)
corrplot(correlation_matrix_LeedsCentre, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7, cl.cex = 0.7, title = "Correlation Matrix of Pollutants_LeedsCentre")
#For Headingly
pollutant_data_Headingly<- data_Headingly_filtered[, c(3, 4, 5, 6,7)]
correlation_matrix_Headingly <- cor(pollutant_data_Headingly)

print(correlation_matrix_Headingly)
corrplot(correlation_matrix_Headingly, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7, cl.cex = 0.7, title = "Correlation Matrix of Pollutants_Headingly")
#Nitric.oxide is highly correlated with Nitrogen.oxides.as.nitrogen.dioxide, 
#Nitrogen.dioxide is highly correlated with Nitrogen.oxides.as.nitrogen.dioxide, 
#and PM2.5 and PM10 are highly correlated.

#Factor analysis
#For Dewsbury
pollutant_data_Dewsbury_standardized <- scale(pollutant_data_Dewsbury)
bartlett_test <- cortest.bartlett(pollutant_data_Dewsbury_standardized)
print(bartlett_test)#p<0.05

kmo_test <- KMO(pollutant_data_Dewsbury_standardized)
print(kmo_test)#>0.5

fa_parallel_Dewsbury <- fa.parallel(pollutant_data_Dewsbury_standardized, fa = "fa")
num_factors_Dewsbury <- fa_parallel_Dewsbury$nfact
fa_result_Dewsbury <- fa(pollutant_data_Dewsbury_standardized, nfactors = num_factors_Dewsbury, rotate = "varimax")
print(fa_result_Dewsbury)
fa.diagram(fa_result_Dewsbury, main = "Factor Analysis Diagram - Dewsbury")

fa_scores_Dewsbury <- factor.scores(pollutant_data_Dewsbury_standardized, fa_result_Dewsbury, method = "regression")
factor_scores <- fa_scores_Dewsbury$scores
summary(factor_scores)

factor1_scores <- factor_scores[, 1]
factor2_scores <- factor_scores[, 2]


fit_factor1 <- auto.arima(factor1_scores)
fit_factor2 <- auto.arima(factor2_scores)

summary(fit_factor1)#ARIMA(0,1,1) with drift 
summary(fit_factor2)#ARIMA(0,1,3)  

forecast_factor1 <- forecast(fit_factor1, h = 24)
forecast_factor2 <- forecast(fit_factor2, h = 24)

plot(forecast_factor1, main = "Forecast for Factor 1 Scores")
plot(forecast_factor2, main = "Forecast for Factor 2 Scores")

forecast_scores <- cbind(forecast_factor1$mean, forecast_factor2$mean)

forecast_scores <- as.matrix(forecast_scores)


loadings_matrix <- as.matrix(fa_result_Dewsbury$loadings)
predicted_values <- forecast_scores %*% t(loadings_matrix)

reverse_scale <- function(predicted_values, original_data) {
  col_means <- colMeans(original_data, na.rm = TRUE)
  col_sds <- apply(original_data, 2, sd, na.rm = TRUE)
  scaled_values <- sweep(predicted_values, 2, col_means, `*`)
  scaled_values <- sweep(scaled_values, 2, col_sds, `+`)
  return(scaled_values)
}

predicted_values_unscaled <- reverse_scale(predicted_values, pollutant_data_Dewsbury)
predicted_values_df <- data.frame(predicted_values_unscaled)
predicted_values_df$Datetime <- seq(from = max(data_Dewsbury_filtered$Datetime), by = "hour", length.out = 24)
colnames(predicted_values_df) <- c("Nitric.oxide", "Nitrogen.dioxide", "Nitrogen.oxides.as.nitrogen.dioxide", "PM10", "PM2.5", "Datetime")
print(predicted_values_df)

ggplot(predicted_values_df, aes(x = Datetime)) +
  geom_line(aes(y = Nitric.oxide, color = "Nitric.oxide")) +
  geom_line(aes(y = Nitrogen.dioxide, color = "Nitrogen.dioxide")) +
  geom_line(aes(y = Nitrogen.oxides.as.nitrogen.dioxide, color = "Nitrogen.oxides.as.nitrogen.dioxide")) +
  geom_line(aes(y = PM10, color = "PM10")) +
  geom_line(aes(y = PM2.5, color = "PM2.5")) +
  labs(title = 'Predicted Values of Pollutants in Dewsbury', x = 'Datetime', y = 'Value') +
  scale_color_manual(values = c("Nitric.oxide" = "red", "Nitrogen.dioxide" = "blue", "Nitrogen.oxides.as.nitrogen.dioxide" = "green", "PM10" = "purple", "PM2.5" = "orange")) +
  theme_minimal()
#For LeedsCentre
pollutant_data_LeedsCentre_standardized <- scale(pollutant_data_LeedsCentre)
bartlett_test <- cortest.bartlett(pollutant_data_LeedsCentre_standardized)
print(bartlett_test)#p<0.05

kmo_test <- KMO(pollutant_data_LeedsCentre_standardized)
print(kmo_test)#>0.5
fa_parallel_LeedsCentre <- fa.parallel(pollutant_data_LeedsCentre_standardized, fa = "fa")
num_factors_LeedsCentre <- fa_parallel_LeedsCentre$nfact
fa_result_LeedsCentre <- fa(pollutant_data_LeedsCentre_standardized, nfactors = num_factors_LeedsCentre, rotate = "varimax")
print(fa_result_LeedsCentre)
fa.diagram(fa_result_LeedsCentre, main = "Factor Analysis Diagram - LeedsCentre")

fa_scores_LeedsCentre <- factor.scores(pollutant_data_LeedsCentre_standardized, fa_result_LeedsCentre, method = "regression")
factor_scores <- fa_scores_LeedsCentre$scores
summary(factor_scores)

factor1_scores <- factor_scores[, 1]
factor2_scores <- factor_scores[, 2]


fit_factor1 <- auto.arima(factor1_scores)
fit_factor2 <- auto.arima(factor2_scores)



summary(fit_factor1)#ARIMA(0,1,0)
summary(fit_factor2)#ARIMA(1,0,2) with zero mean 


forecast_factor1 <- forecast(fit_factor1, h = 24)
forecast_factor2 <- forecast(fit_factor2, h = 24)


plot(forecast_factor1, main = "Forecast for Factor 1 Scores")
plot(forecast_factor2, main = "Forecast for Factor 2 Scores")


forecast_scores <- cbind(forecast_factor1$mean, forecast_factor2$mean)

forecast_scores <- as.matrix(forecast_scores)

loadings_matrix <- as.matrix(fa_result_LeedsCentre$loadings)
predicted_values <- forecast_scores %*% t(loadings_matrix)
predicted_values_df <- data.frame(predicted_values)
predicted_values_df$Datetime <- seq(from = max(data_LeedsCentre_filtered$Datetime), by = "hour", length.out = 24)
colnames(predicted_values_df) <- c("Nitric.oxide", "Nitrogen.dioxide", "Nitrogen.oxides.as.nitrogen.dioxide", "PM10", "PM2.5", "Datetime")


predicted_values_df_unscaled <- reverse_scale(predicted_values_df[, -6], pollutant_data_LeedsCentre)
predicted_values_df_unscaled <- data.frame(predicted_values_df_unscaled)
predicted_values_df_unscaled$Datetime <- predicted_values_df$Datetime

print(predicted_values_df_unscaled)

ggplot(predicted_values_df_unscaled, aes(x = Datetime)) +
  geom_line(aes(y = Nitric.oxide, color = "Nitric.oxide")) +
  geom_line(aes(y = Nitrogen.dioxide, color = "Nitrogen.dioxide")) +
  geom_line(aes(y = Nitrogen.oxides.as.nitrogen.dioxide, color = "Nitrogen.oxides.as.nitrogen.dioxide")) +
  geom_line(aes(y = PM10, color = "PM10")) +
  geom_line(aes(y = PM2.5, color = "PM2.5")) +
  labs(title = 'Predicted Values of Pollutants in LeedsCentre', x = 'Datetime', y = 'Value') +
  scale_color_manual(values = c("Nitric.oxide" = "red", "Nitrogen.dioxide" = "blue", "Nitrogen.oxides.as.nitrogen.dioxide" = "green", "PM10" = "purple", "PM2.5" = "orange")) +
  theme_minimal()

#For Headingly
pollutant_data_Headingly_standardized <- scale(pollutant_data_Headingly)
bartlett_test <- cortest.bartlett(pollutant_data_Headingly_standardized)
print(bartlett_test)#p<0.05

kmo_test <- KMO(pollutant_data_Headingly_standardized)
print(kmo_test)#>0.5
fa_parallel_Headingly <- fa.parallel(pollutant_data_Headingly_standardized, fa = "fa")
num_factors_Headingly <- fa_parallel_Headingly$nfact
fa_result_Headingly <- fa(pollutant_data_Headingly_standardized, nfactors = num_factors_Headingly, rotate = "varimax")
print(fa_result_Headingly)
fa.diagram(fa_result_Headingly, main = "Factor Analysis Diagram - Headingly")

fa_scores_Headingly <- factor.scores(pollutant_data_Headingly_standardized, fa_result_Headingly, method = "regression")
factor_scores <- fa_scores_Headingly$scores
summary(factor_scores)

factor1_scores <- factor_scores[, 1]
factor2_scores <- factor_scores[, 2]
factor3_scores <- factor_scores[, 3]

fit_factor1 <- auto.arima(factor1_scores)
fit_factor2 <- auto.arima(factor2_scores)
fit_factor3 <- auto.arima(factor3_scores)


summary(fit_factor1)#ARIMA(3,1,3)
summary(fit_factor2)#ARIMA(1,1,5)  
summary(fit_factor3)#ARIMA(1,1,1) 

forecast_factor1 <- forecast(fit_factor1, h = 24)
forecast_factor2 <- forecast(fit_factor2, h = 24)
forecast_factor3 <- forecast(fit_factor3, h = 24)


plot(forecast_factor1, main = "Forecast for Factor 1 Scores")
plot(forecast_factor2, main = "Forecast for Factor 2 Scores")
plot(forecast_factor3, main = "Forecast for Factor 3 Scores")

forecast_scores <- cbind(forecast_factor1$mean, forecast_factor2$mean, forecast_factor3$mean)

forecast_scores <- as.matrix(forecast_scores)

loadings_matrix <- as.matrix(fa_result_Headingly$loadings)
predicted_values <- forecast_scores %*% t(loadings_matrix)
predicted_values_df <- data.frame(predicted_values)
predicted_values_df$Datetime <- seq(from = max(data_Headingly_filtered$Datetime), by = "hour", length.out = 24)
colnames(predicted_values_df) <- c("Nitric.oxide", "Nitrogen.dioxide", "Nitrogen.oxides.as.nitrogen.dioxide", "PM10", "PM2.5", "Datetime")


predicted_values_df_unscaled <- reverse_scale(predicted_values_df[, -6], pollutant_data_Headingly)
predicted_values_df_unscaled <- data.frame(predicted_values_df_unscaled)
predicted_values_df_unscaled$Datetime <- predicted_values_df$Datetime

print(predicted_values_df_unscaled)

ggplot(predicted_values_df_unscaled, aes(x = Datetime)) +
  geom_line(aes(y = Nitric.oxide, color = "Nitric.oxide")) +
  geom_line(aes(y = Nitrogen.dioxide, color = "Nitrogen.dioxide")) +
  geom_line(aes(y = Nitrogen.oxides.as.nitrogen.dioxide, color = "Nitrogen.oxides.as.nitrogen.dioxide")) +
  geom_line(aes(y = PM10, color = "PM10")) +
  geom_line(aes(y = PM2.5, color = "PM2.5")) +
  labs(title = 'Predicted Values of Pollutants in Headingly', x = 'Datetime', y = 'Value') +
  scale_color_manual(values = c("Nitric.oxide" = "red", "Nitrogen.dioxide" = "blue", "Nitrogen.oxides.as.nitrogen.dioxide" = "green", "PM10" = "purple", "PM2.5" = "orange")) +
  theme_minimal()

