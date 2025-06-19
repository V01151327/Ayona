# I. Installing the packages ####
# Function
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
# loading
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA") #vector
lapply(libraries, install_and_load)
# II. Putting the data set ####
data = read.csv("C:/Users/91859/Desktop/NSSO68.csv")
# III. Filtering for a the state Bihar ####
state_name <- "BHR"
state_data <- data %>%
  filter(state_1 == state_name)

unique(data$state_1)
unique(state_data$state_1)
# write.csv
write.csv(state_data,"C:/Users/91859/Desktop/NSSO68.csv")
# IV. Display the data set ####
cat("Dataset Information:\n")
print(names(state_data))
print(head(state_data))
print(dim(state_data))

sum(is.na(state_data))
# V. Check for missing values ####
missing_info <- colSums(is.na(state_data))
cat("Missing Values In  formation:\n")
print(missing_info)
# VI. Selecting relevant columns ####
state_subset <- state_data %>%
  select(state_1, District, Region, Sector, State_Region,
         Meals_At_Home, ricetotal_v, wheattotal_v, Milktotal_v,
         pulsestot_v,nonvegtotal_v, fruitstt_v, No_of_Meals_per_day)
names(state_data)
# VII.  Filling the missing values with mean ####
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
missing_info <- colSums(is.na(state_subset))
cat("Missing Values Information:\n")
print(missing_info)
# filling the missing values of Meals_At_Home
state_subset$Meals_At_Home <- impute_with_mean(state_subset$Meals_At_Home)

missing_info <- colSums(is.na(state_subset))
cat("Missing Values Information:\n")
print(missing_info)
# filling the missing values of No_of_Meals_per_day
state_subset$No_of_Meals_per_day <- impute_with_mean(state_subset$No_of_Meals_per_day)

missing_info <- colSums(is.na(state_subset))
cat("Missing Values Information:\n")
print(missing_info)
# VIII. Eliminating the outliers ####
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c('Meals_At_Home', 'ricetotal_v', 'wheattotal_v', 'Milktotal_v',
                     'pulsestot_v','nonvegtotal_v', 'fruitstt_v', 'No_of_Meals_per_day')
for (col in outlier_columns) {
  state_subset <- remove_outliers(state_subset, col)
}
names(state_subset)

# IX. Creating total concumption variable ####
state_subset$total_consumption <- rowSums(state_subset[, c('ricetotal_v', 'wheattotal_v', 'Milktotal_v',
                                                           'pulsestot_v','nonvegtotal_v', 'fruitstt_v')], na.rm = TRUE)
# X. Summarize consumption ####
summarize_consumption <- function(group_col) {
  summary <- state_subset %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")
sector_summary <- summarize_consumption("Sector")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)

cat("Bottom Consuming Districts:\n")
print(tail(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)
# XII. Renaming the district ####
district_mapping <- c( "28"=" Patna"," 19"=" Samastipur","13"=" Darbhanga","35"="Gaya","34"="Aurangabad","33"="Jehanabad", "3"="Sheohar", "38"="Arwal")
# converting the sector Rural & urban"
sector_mapping <- c("2"="URBAN", "1"="RURAL")
# Including the names
state_subset$District <- as.character(state_subset$District)
state_subset$Sector <- as.character(state_subset$Sector)
state_subset$District <- ifelse(state_subset$District %in% names(district_mapping),
                                district_mapping[state_subset$District],
                                state_subset$District)
state_subset$Sector <- ifelse(state_subset$Sector %in% names(sector_mapping),
                              sector_mapping[state_subset$Sector],
                              state_subset$Sector)
# Summarizing all
district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")
sector_summary <- summarize_consumption("Sector")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)

cat("Top Consuming Districts:\n")
print(tail(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)

# XII. testing fir mean difference b/w rural & urban consumption ####
rural <- state_subset %>%
  filter(Sector == "RURAL") %>%
  select(total_consumption)

urban <- state_subset %>%
  filter(Sector == "URBAN") %>%
  select(total_consumption)

# z-TEST for Rural & Urban
library(BSDA)
z_test_result <- z.test(rural, urban, alternative = "two.sided",
                        mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)
# Reporting the test result
if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a difference between mean consumptions of urban and rural.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of urban and rural.\n")
}
# XIII. Testing for mean difference of top & Bottom District ####
top_district <- state_subset %>%
  filter(District == "Patna") %>%
  select(total_consumption)

bottom_district <- state_subset %>%
  filter(District == "Aurangabad") %>%
  select(total_consumption)
# Z- Test 
library(BSDA)
z_test_result <- z.test(top_district, bottom_district, alternative = "two.sided",
                        mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)
# Report
if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a difference between mean consumptions of top and bottom districts of andhrapradesh.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of top and bottom districts of andhrapradesh.\n")
}