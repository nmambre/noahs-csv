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

# DAY 3: The Neighbor
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

# DAY 4: The Early Bird
# split df_orders$ordered into df_orders$ordered_date and df_orders$ordered_time
df_orders <- df_orders %>% separate(ordered, c("ordered_date", "ordered_time"), " ")

# split df_orders$shipped into df_orders$shipped_date and df_orders$shipped_time
df_orders <- df_orders %>% separate(shipped, c("shipped_date", "shipped_time"), " ")

# get df_orders$orderid where both ordered_time and shipped_time  before 5 AM
#df_orders_45 <- df_orders[df_orders$ordered_time <= "05:00:00" & df_orders$shipped_time <= "05:00:00", ]
# get df_orders$orderid where both ordered_time and shipped_time  are between 4 AM and 5 AM
df_orders_45 <-
  df_orders[df_orders$ordered_time >= "04:00:00" & df_orders$ordered_time <= "05:00:00" & df_orders$shipped_time >= "04:00:00" & df_orders$shipped_time <= "05:00:00", ]
#df_orders_45

# create df_sku_45 and get all rows where df_orders_items$orderid is in df_orders_45$orderid
#df_sku_45 <- df_orders_items[df_orders_items$orderid %in% df_orders_45$orderid, ]
# create df_sku_45 and get all rows where df_orders_items$orderid is in df_orders_45$orderid and sku starts with "DLI"
# df_sku_45 <- df_orders_items[df_orders_items$orderid %in% df_orders_45$orderid & substr(df_orders_items$sku, 1, 3) == "DLI", ]
# create df_sku_45 and get all rows where df_orders_items$orderid is in df_orders_45$orderid and sku starts with "BKY"
df_sku_45 <- df_orders_items[df_orders_items$orderid %in% df_orders_45$orderid & substr(df_orders_items$sku, 1, 3) == "BKY", ]
df_sku_45

df_products <- read.csv("5784/noahs-products.csv")
# get df_products$description from where df_products$sku is in df_sku_45
df_products_45 <- df_products[df_products$sku %in% df_sku_45$sku, ]
df_products_45

# get customerid from df_orders_45 where df_orders_45$orderid is in df_orders_45_sku$orderid
df_orders_45_customerid <- df_orders_45[df_orders_45$orderid %in% df_sku_45$orderid, ]
df_orders_45_customerid

# get rows of df_customers where df_customers$customerid is in df_orders_45_sku$customerid
df_customers_45 <- df_customers[df_customers$customerid %in% df_orders_45_customerid$customerid, ]
df_customers_45

# Day 5: The Cat Lady
#sku_cat <- "PET0272"

# get order id from sku
#order_ids_cat <- df_orders_items[df_orders_items$sku == sku_cat, "orderid"]
#order_ids_cat

# get all skus from df_products that in the (lowercase) description has the word "cat" and either "adult" or "senior"
#df_products_cat <- df_products[grepl("cat", tolower(df_products$desc)) & (grepl("adult", tolower(df_products$desc)) | grepl("senior", tolower(df_products$desc))), ]
# get all skus from df_products that in the (lowercase) description has the word "cat" "senior"
df_products_cat <- df_products[grepl("cat", tolower(df_products$desc)) &  grepl("senior", tolower(df_products$desc)), ]
#df_products_cat$desc

# get all the order ids from df_orders_items where df_orders_items$sku is in df_products_cat$sku or df_products_cat$sku is sku_cat
order_ids_cat <- df_orders_items[df_orders_items$sku %in% df_products_cat$sku , "orderid"]
#order_ids_cat <- df_orders_items[df_orders_items$sku %in% df_products_cat$sku | df_orders_items$sku == sku_cat, "orderid"]

#order_ids_cat

# get unique customer ids from order id
customer_ids_cat <- df_orders[df_orders$orderid %in% order_ids_cat, ]
#customer_ids_cat

# get a df with the count of orders per customer
customer_ids_cat_count <- customer_ids_cat %>% count(customerid, sort = TRUE)

# get customers from df_customers where df_customers$customerid is in customer_ids_cat and df_customers$citystatezip starts with "Staten Island"
customers_cat <- df_customers[df_customers$customerid %in% customer_ids_cat_count$customerid &   substr(df_customers$citystatezip, 1, 13) == "Staten Island", ]
#customers_cat

# add customer_ids_cat_count to customers_cat
customers_cat <- merge(customers_cat, customer_ids_cat_count, by = "customerid")
#customers_cat

# order customers_cat by n (descending)
customers_cat <- customers_cat[order(customers_cat$n, decreasing = TRUE), ]
# print first row of customers_cat
customers_cat[1, ]
