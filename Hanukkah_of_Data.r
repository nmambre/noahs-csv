# Load the necessary packages
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# DAY 1: The Investigator
# Read the CSV file
df_customers <- read.csv("5784/noahs-customers.csv")

# create first_name column
df_customers$first_name <- sapply(strsplit(df_customers$name, " "), "[", 1)
df_customers$last_name <- sapply(strsplit(df_customers$name, " "), "[", 2)

# create lookup table for name_digits
lookup <- c(" " = "0", "A"="2", "B"="2", "C"="2", "D"="3", "E"="3", "F"="3", "G"="4", "H"="4", "I"="4", "J"="5", "K"="5", "L"="5", "M"="6", "N"="6", "O"="6", "P"="7", "Q"="7", "R"="7", "S"="7", "T"="8", "U"="8", "V"="8", "W"="9", "X"="9", "Y"="9", "Z"="9") # nolint

# create name_digits column
df_customers$name_digits <- sapply(df_customers$last_name, function(x) {
  paste0(sapply(strsplit(toupper(x), ""), function(y) {
    lookup[y]
  }), collapse = "")
})

# create phone_stripped column
df_customers$phone_stripped <- gsub("-", "", df_customers$phone)

# compare name_digits with phone_stripped
df_customers$match <-
  ifelse(df_customers$name_digits == df_customers$phone_stripped, "Match", "No match")
matched_rows <- df_customers[df_customers$match == "Match", ]
matched_rows

# DAY 2: The Contractor
# Create initials column
df_customers <- df_customers %>% mutate(initials = paste0(substr(first_name, 1, 1), substr(last_name, 1, 1)))

df_orders_items <- read.csv("5784/noahs-orders_items.csv")

# SKU for rug cleaner, quick search throught products.csv
sku <- "HOM2761"
# get order id from sku
order_ids <- df_orders_items[df_orders_items$sku == sku, "orderid"]
order_ids

df_orders <- read.csv("5784/noahs-orders.csv")
# get customer id from order id and ordered starts with 2017
customer_ids <- df_orders[df_orders$orderid %in% order_ids & substr(df_orders$ordered, 1, 4) == "2017", "customerid"]
customer_ids

# get customers from df_customers where df_customers$customerid is in customer_ids and df_customers$initials is "JP"
customers_with_initials_JP <- df_customers[df_customers$customerid %in% customer_ids & df_customers$initials == "JP", ]
customers_with_initials_JP

# Day 3: The Neighbor
# Create a vector of Rabbit years
rabbit_years <- c(1927, 1939, 1951, 1963, 1975, 1987, 1999, 2011, 2023)

# Initialize an empty data frame to store the final results
final_range <- data.frame()

# Loop through Rabbit years
for (year_rabbit in rabbit_years) {
  # Define the date range for Cancer
  cancer_start_date <- as.Date(paste(year_rabbit, "-06-21", sep = ""))
  cancer_end_date <- as.Date(paste(year_rabbit, "-07-22", sep = ""))

  # Filter for individuals born between June 21 and July 22 in a specific Rabbit year
  cancer_range <- df_customers[df_customers$birthdate >= cancer_start_date & df_customers$birthdate <= cancer_end_date, ]

  # Filter for individuals born in the specific Rabbit year
  rabbit_range <- df_customers[year(df_customers$birthdate) %% 12 == (year_rabbit %% 12), ]

  # Find the intersection of the two ranges
  intersection_range <- intersect(cancer_range$customerid, rabbit_range$customerid)

  # Filter the original dataframe based on the intersection and append to final_range
  final_range <- rbind(final_range, df_customers[df_customers$customerid %in% intersection_range, ])
}

# View the resulting data frame
print(final_range$birthdate)

# get customerid that is in df_orders
customer_ids_rug_cleaner <- df_orders[df_orders$orderid %in% order_ids, "customerid"]
customer_ids_rug_cleaner

# get customers from df_customers where df_customers$customerid is in customer_ids_rug and df_customers$birthdate is in final_range$birthdate
customers_rc__with_rug_cleaner <- df_customers[df_customers$customerid %in% customer_ids_rug_cleaner & df_customers$birthdate %in% final_range$birthdate, ]
customers_rc__with_rug_cleaner