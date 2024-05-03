

# --------------Data Preparation--------------------
## Packages

if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)
pacman::p_load(tidyverse,
               ggplot2,
               rio, #import, export
               here,
               treemap,
               MetBrewer, #color palette
               lubridate, #working with time
               ggrepel,
               listviewer,
               Rtrack,
               d3r
)

## Data
customers <- import(here("Data", "customers.csv"))
orders <- import(here("Data","orders.csv"))
products <- import(here("Data", "products.csv"))
regions <- import(here("Data", "regions.csv"))
coordinate <- import(here("Processed_data", "nation_region_coordinate.csv" ))

## Remove spaces in column name
customers <- customers %>% rename_with(make.names)
orders <- orders %>% rename_with(make.names)
products <- products %>% rename_with(make.names)
regions <- regions %>% rename_with(make.names)

## Convert date variable to corret type
date_column <- c("OrderDate", "DeliveryDate")
orders <- orders %>% mutate(OrderDate = as.Date(OrderDate, format = "%d/%m/%Y"),
                            DeliveryDate = as.Date(DeliveryDate, format = "%d/%m/%Y"))

col_names <- names(regions)
regions <- regions %>% mutate(across(all_of(col_names), as.factor))

col_names <- names(customers)
customers <- customers %>% mutate(across(all_of(col_names), as.factor))

col_names <- names(products)
products <- products %>% mutate(across(all_of(col_names), as.factor))

# merge coordinate info with regions
coordinate2 <- coordinate %>% select (-c("Region"))
regions_coord <- merge(regions, coordinate2, by = "Nation", all.x=TRUE)


## Merge customers and regions:
custom_region <- merge(customers, regions_coord, by = "Territory", all.x = TRUE)

## Merge orders, customers, regions:

order_custom_region <- merge(orders, custom_region, by.x = "CustomerID", by.y = "Account.Code", all.x = TRUE)

## long data format of orders
order_custom_region_long <- order_custom_region %>% 
  #mutate(Products = gsub(", b", "- b")) %>%
  separate_rows(productsIDs, Quantities, ProductPricesInCP, sep = ", ")

##merge orders with product
order_custom_region_long$productsIDs <- as.factor(order_custom_region_long$productsIDs)
order_custom_region_long$Quantities <- as.factor(order_custom_region_long$Quantities)
order_custom_region_long$ProductPricesInCP <- as.numeric(order_custom_region_long$ProductPricesInCP)
order_custom_region_product <- merge(order_custom_region_long, products, by.x = "productsIDs", by.y = "product_code", all.x = TRUE)

# ------------ Best selling products overall------------------------ 
hot_product_all <- order_custom_region_product %>% group_by(productsIDs) %>% 
  summarise(revenue_thousand_CP = round(sum(ProductPricesInCP)/1000, digits =2))  %>% ungroup %>%
  arrange(desc(revenue_thousand_CP)) %>% slice(1:10)
hot_product_all2 <- merge(hot_product_all, products, by.x = "productsIDs", by.y = "product_code", all.x = TRUE)

hot_product_all2 <- hot_product_all2 %>% mutate(product_brand = 
                                                  paste0(Product.Name," (", Brand.Name, ")"),
                                                hierarchy = paste(Type,Subtype,product_brand, 
                                                                  sep="-"))

## Write JSON file
hot_product_all_nest <- hot_product_all2 %>% 
  select(Type, Subtype, product_brand,revenue_thousand_CP) %>%
  d3_nest(value_cols = "revenue_thousand_CP", root = "Overall")
hot_product_all_nest %>% listviewer::jsonedit()
export(hot_product_all_nest, here("Processed_data", "hot_product_all_nest.json"))

# -------------- Best selling products per nation ---------------------
hot_product <- order_custom_region_product %>% group_by(nation_id, Nation, productsIDs ) %>% 
  summarise(revenue_thousand_CP = round(sum(ProductPricesInCP)/1000, digits =2))  %>% ungroup %>%
  arrange(desc(revenue_thousand_CP)) %>% 
  group_by(Nation) %>% slice(1:10)
  
hot_product_nation <- merge(hot_product, products, by.x = "productsIDs", 
                            by.y = "product_code", all.x = TRUE)

## Write JSON file
hot_product_nation_nest <- hot_product_nation %>%
  mutate(product_brand = 
           paste0(Product.Name," (", Brand.Name, ")"),
         hierarchy = paste(Type,Subtype,product_brand, 
                           sep="-")) %>%
  select(Nation, Type, Subtype, product_brand,revenue_thousand_CP) %>%
  d3_nest(value_cols = "revenue_thousand_CP", root = "All Nations")

hot_product_nation_nest %>% listviewer::jsonedit()
export(hot_product_nation_nest, here("Processed_data", "hot_product_nation_nest.json"))
