library(tidyverse)
library(stringi)
library(ggplot2)
library(lubridate)

df_auto <- read_csv("dataLab/otomoto_web2020.csv")

# TASK 1

df_auto <- rename(df_auto, engine_capacity = capacity, 
                  fuel_type = fuel,
                  horse_power = power,
                  make = brand,
                  new_used = used,
                  gearbox = transmission,
                  aso = authorised_service)

# TASK 2

# OPTION 1 - REGEX
df_auto$mileage <- as.numeric((stri_replace_all_regex(df_auto$mileage, "[^0-9]", "")))
df_auto$engine_capacity <- as.numeric(stri_replace_all_regex(df_auto$engine_capacity, "\\D|3$", ""))
df_auto$horse_power <- as.numeric(stri_replace_all_regex(df_auto$horse_power, "[^0-9]", ""))

# OPTION 2 - FIXED
df_auto$mileage <- as.numeric(stri_replace_all(df_auto$mileage, fixed = c(" ", "km"), replacement = "", vectorize_all = FALSE))
df_auto$engine_capacity <- as.numeric(stri_replace_all(df_auto$engine_capacity, fixed = c(" ", "cm3"), replacement = "", vectorize_all = FALSE))
df_auto$horse_power <- as.numeric(stri_replace_all(df_auto$horse_power, fixed = c(" ", "KM"), replacement = "", vectorize_all = FALSE))


# TASK 3

# Only one of these two options should be chosen:

# OPTION 1 - REGEX
price <- stri_split(df_auto$price, regex = "\\s+", simplify = TRUE) %>%  
  as.data.frame() %>% 
  map_df(stri_trim)

colnames(price) <- c("Value", "Currency")
df_auto$price <- price$Value
df_auto$price <- as.numeric((stri_replace_all_regex(df_auto$price, "[^0-9]", "")))

# OPTION 2 - FIXED
# price <- stri_split(df_auto$price, fixed = " ", simplify = TRUE) %>%  
#   as.data.frame() %>% 
#   map_df(stri_trim)
#  
# colnames(price) <- c("Value", "Currency")
# df_auto$price <- price$Value
# df_auto$price <- as.numeric(stri_replace_all(df_auto$price, fixed = ",", replacement = "", vectorize_all = FALSE))


colnames(df_auto)[colnames(df_auto) == "price"] <- "priceValue"
Currency <- price$Currency
df_auto <- mutate(df_auto, priceCurrency = Currency, .after = priceValue)

unique_currency <- stri_unique(df_auto$priceCurrency)
# Now we know that we have 2 currencies: PLN and EUR

df_auto <- df_auto %>%
  mutate(priceValue = if_else(priceCurrency != "PLN", priceValue * 4.3484, priceValue)) %>%
  mutate(priceCurrency = if_else(priceCurrency != "PLN", "PLN", priceCurrency))


# TASK 4

colorAscii <- df_auto$color
df_auto <- mutate(df_auto, colorAscii = colorAscii, .after = color)


df_auto$colorAscii <- stri_trans_tolower(df_auto$colorAscii)
df_auto$colorAscii <- stri_trans_general(df_auto$colorAscii, id = "Latin-ASCII")
unique_colors <- stri_sort(stri_unique(df_auto$colorAscii))

translations_eng <- c("Beige", "White", "Maroon", "Brown", "Black", "Red", "Purple", "Other color", "Blue", "Silver", "Gray", "Green", "Gold", "Yellow")

df_auto$colorAscii <- stri_replace_all(df_auto$colorAscii, replacement = translations_eng, fixed=unique_colors, vectorize_all = FALSE)


# TASK 5

# OPTION 1 - REGEX
df_auto <- df_auto %>%
  mutate(NoOffer = stri_extract(url, regex = "(?<=-)[A-Za-z0-9]+(?=\\.html)"))

# OPTION 2 - FIXED
df_auto <- df_auto %>%
  mutate(NoOffer = str_extract(url, fixed("ID") %s+% "[A-Za-z0-9]+"))


# TASK 6

ft <- df_auto$features
ft <- stri_replace_all(ft, replacement="_", fixed=" ")
ft <- stri_replace_all_regex(ft, "[^a-zA-Z0-9_|]", "")
ft <- stri_split(ft, fixed="|")

ft_names <- df_auto$features
ft_names <- stri_split(ft_names, fixed="|") 

# Original feature names - unique_features
unique_features <- ft_names %>%  
  unlist() %>%
  stri_remove_empty_na() %>%
  stri_trim_both() %>%
  stri_unique()

# remove underscore only when last
remove_underscore <- function(lst) {
  stri_replace_all_regex(lst, "_$", "")
}

ft <- map(ft, remove_underscore)
df_auto$features <- ft

# Prepared feature names - unique_elements
unique_elements <- stri_unique(unlist(ft))
unique_elements <- stri_remove_empty_na(unique_elements) 

result_list <- map(unique_elements, ~as.numeric(sapply(df_auto$features, function(features_vector) .x %in% features_vector)))

df_features <- do.call(cbind, result_list)

colnames(df_features) <- unique_elements

df_features <- as.data.frame(df_features)

df_auto <- cbind(df_auto, df_features)

# 1st graph
count_freq_features <- colSums(df_features == 1)
freq_features <- data.frame(Features = unique_features, Frequency = count_freq_features) %>%
  arrange(-Frequency)

n <- 10

top_freq <- head(freq_features, n)

p <- ggplot(top_freq, aes(x = reorder(Features, Frequency), y = Frequency)) +
  geom_col(fill = "skyblue") +
  labs(x = "Feature", y = "Frequency", title = "Top 10 Most Frequent Features") +
  coord_flip()

p + scale_y_continuous(labels = scales::number_format(scale = 1))

# 2nd graph
count_feature_in_car <- rowSums(df_features == 1)

car_features <- data.frame(Make = df_auto$make, Frequency = count_feature_in_car)

average_features <- aggregate(Frequency ~ Make, data = car_features, FUN = mean)

average_features <- average_features %>%
  arrange(desc(Frequency)) %>%
  mutate(Make = reorder(Make, -Frequency))

top_freq <- head(average_features, n)

ggplot(top_freq, aes(x = reorder(Make, -Frequency), y = Frequency)) +
  geom_col(fill = "skyblue") +
  labs(x = "Car Make", y = "Average Number of Features", title = "Top 10 Average Number of Features by Car Make") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3rd graph
car_price <- data.frame(Price = df_auto$priceValue, Frequency = count_feature_in_car)

car_price <- car_price[order(-car_price$Frequency), ]

average_prices <- car_price %>%
  group_by(Frequency) %>%
  summarize(Average_Price = mean(Price))

ggplot(average_prices, aes(x = Frequency, y = Average_Price)) +
  geom_line() +
  labs(x = "Number of Features", y = "Average Price", title = "Average Price of Car by Number of Features")
