# Load the necessary packages
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(chron)

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
#order_ids

df_orders <- read.csv("5784/noahs-orders.csv")
# get customer id from order id and ordered starts with 2017
customer_ids <- df_orders[df_orders$orderid %in% order_ids & substr(df_orders$ordered, 1, 4) == "2017", "customerid"]
#customer_ids

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
#print(final_range$birthdate)

# get customerid that is in df_orders
customer_ids_rug_cleaner <- df_orders[df_orders$orderid %in% order_ids, "customerid"]
#customer_ids_rug_cleaner

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

# DAY 6: The Bargain Hunter
# nrow(df_customers)

# # get number of unique names
# length(unique(df_customers$name))

# df_customers <- df_customers[order(df_customers$birthdate), ]

# # list names that are not unique
# df_duplicated_names <- df_customers[duplicated(df_customers$name),]
# df_duplicated_names <- df_duplicated_names[order(df_duplicated_names$name),]


# # Find rows with the same name
# duplicate_names <- df_customers[duplicated(df_customers$name) | duplicated(df_customers$name, fromLast = TRUE), ]
# # Order the resulting data frame by name
# sorted_duplicate_names <- duplicate_names[order(duplicate_names$name), ]

# # Find rows with the same birthday
# duplicate_birthday <- df_customers[duplicated(df_customers$birthday) | duplicated(df_customers$birthday, fromLast = TRUE), ]
# # Order the resulting data frame by birthday
# sorted_duplicate_birthday <- duplicate_birthday[order(duplicate_birthday$birthday), ]


# # get rows that are df_customers$last_name is Wilson
# df_wilsons <- df_customers[df_customers$last_name == "Wilson", ]


# # get rows that are not unique and df_customers$last_name is Wilson
# #df_wilsons <- df_customers[!duplicated(df_customers$name) & df_customers$last_name == "Wilson", ]

# # order df_wilsons by name
# df_wilsons <- df_wilsons[order(df_wilsons$name), ]

# Merge df_orders_items and df_products on the 'sku' column
df_price_compare <- merge(df_orders_items, df_products, by = 'sku', all.x = TRUE)

# Filter rows where unit_price is lower than wholesale_cost
df_sold_at_loss <- df_price_compare[df_price_compare$unit_price < df_price_compare$wholesale_cost, ]
head(df_sold_at_loss)

# Extract unique orderids from df_sold_at_loss
unique_orderids <- unique(df_sold_at_loss$orderid)

# Filter df_orders based on the unique orderids
df_bargain_hunters <- df_orders[df_orders$orderid %in% unique_orderids, ]
head(df_bargain_hunters)


# Group by customerid and count the number of orderid for each customer
customer_order_counts <- df_bargain_hunters %>%
  group_by(customerid) %>%
  summarize(order_count = n()) %>%
  arrange(desc(order_count))

# View the resulting data frame
head(customer_order_counts)

bargain_hunter_customerid <- as.integer(customer_order_counts[1, 1])

df_customers[df_customers$customerid == bargain_hunter_customerid, ]

# DAY 7: The Meet Cute
#get all rows of df_orders where df_orders$customerid is bargain_hunter_customerid
bargain_hunter_orders <- df_orders[df_orders$customerid == bargain_hunter_customerid, ]

# get all rows of df_orders_items where df_orders_items$orderid is in bargain_hunter_orders$orderid
bargain_hunter_order_items <- df_orders_items[df_orders_items$orderid %in% bargain_hunter_orders$orderid, ]

# get all rows of df_products where df_products$sku is in bargain_hunter_order_items$sku
bargain_hunter_products <- df_products[df_products$sku %in% bargain_hunter_order_items$sku, ]

# get all rows of bargain_hunter_products where sku starts with "COL"
bargain_hunter_products_col <- bargain_hunter_products[substr(bargain_hunter_products$sku, 1, 3) == "COL", ]
bargain_hunter_products_col

# get all rows of bargain_hunter_order_items where sku starts with "COL"
bargain_hunter_order_items_col <- bargain_hunter_order_items[substr(bargain_hunter_order_items$sku, 1, 3) == "COL", ]
bargain_hunter_order_items_col

# get al rows of bargain_hunter_orders where orderid is in bargain_hunter_order_items_col$orderid
bargain_hunter_orders_col <- bargain_hunter_orders[bargain_hunter_orders$orderid %in% bargain_hunter_order_items_col$orderid, ]
bargain_hunter_orders_col

# get all rows of df_orders where shipped_date is in bargain_hunter_orders_col$shipped_date
same_shipped <- df_orders[df_orders$shipped_date %in% bargain_hunter_orders_col$shipped_date, ]
head(same_shipped)

same_shipped_order_items <- df_orders_items[df_orders_items$orderid %in% same_shipped$orderid, ]
head(same_shipped_order_items)

same_shipped_order_items_col <- same_shipped_order_items[substr(same_shipped_order_items$sku, 1, 3) == "COL", ]
head(same_shipped_order_items_col)

same_shipped_orders <- same_shipped[same_shipped$orderid %in% same_shipped_order_items_col$orderid, ]
#head(same_shipped_orders$shipped_time)

# # Convert shipped_time to chron format
# same_shipped_orders$shipped_time <- chron(times = same_shipped_orders$shipped_time)


# # Filter rows for the bargain hunter customer
# bargain_hunter_data <- same_shipped_orders %>%
#   filter(customerid == bargain_hunter_customerid)

# # Calculate time differences for each shipped_date
# time_diff_data <- same_shipped_orders %>%
#   group_by(shipped_date) %>%
#   summarize(time_diff = abs(as.numeric(diff(chron(times = c(bargain_hunter_data$shipped_time, shipped_time))))))

# # Merge time_diff_data back into same_shipped_orders
# same_shipped_orders <- merge(same_shipped_orders, time_diff_data, by = 'shipped_date', all.x = TRUE)

# # sort by time_diff
# same_shipped_orders <- same_shipped_orders[order(same_shipped_orders$time_diff), ]

# # View the resulting data frame
# head(same_shipped_orders)

# Order same_shipped_orders by shipped_date and shipped_time
same_shipped_orders <- same_shipped_orders %>%
  arrange(shipped_date, shipped_time)

# Initialize an empty data frame to store the result
result_df <- data.frame()

# Iterate through rows to find rows for the bargain hunter customer
for (i in 1:nrow(same_shipped_orders)) {
  if (same_shipped_orders$customerid[i] == bargain_hunter_customerid) {
    # Get the row before
    row_before <- same_shipped_orders[i - 1, , drop = FALSE]
    
    # Get the current row
    current_row <- same_shipped_orders[i, , drop = FALSE]
    
    # Get the row after
    row_after <- same_shipped_orders[i + 1, , drop = FALSE]
    
    # Combine the rows and bind to the result_df
    combined_rows <- bind_rows(row_before, current_row, row_after)
    result_df <- bind_rows(result_df, combined_rows)
  }
}

# View the resulting data frame
#print(result_df)

ex_customerid <- 5783
df_customers[df_customers$customerid == ex_customerid, ]
