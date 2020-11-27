# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2020Q3
# Purpose:      Price
# programmer:   Zhe Liu
# Date:         2020-11-24
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Origin Price ----
price.market <- msd.raw %>% 
  filter(quarter %in% c('2020Q3'), 
         sales > 0, units > 0)

price.origin <- price.market %>% 
  group_by(packid, quarter, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by city year ----
price.city <- price.market %>% 
  group_by(packid, year, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_city = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by province quarter ----
price.province <- price.market %>% 
  group_by(packid, quarter, province) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_prov = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by province year ----
price.year <- price.market %>% 
  group_by(packid, year, province) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_year = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by pack quarter ----
price.pack <- price.market %>% 
  group_by(packid, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by pack year ----
price.pack.year <- price.market %>% 
  group_by(packid, year) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack_year = sales / units) %>% 
  select(-sales, -units)


##---- Add new price ----
msd.price <- universe.proj %>% 
  left_join(price.origin, by = c("province", "city", "quarter", "packid")) %>% 
  left_join(price.city, by = c("province", "city", "year", "packid")) %>% 
  left_join(price.province, by = c("province", "quarter", "packid")) %>% 
  left_join(price.year, by = c("province", "year", "packid")) %>% 
  left_join(price.pack, by = c("quarter", "packid")) %>% 
  left_join(price.pack.year, by = c('year', 'packid')) %>% 
  mutate(price = if_else(is.na(price), price_city, price), 
         price = if_else(is.na(price), price_prov, price),
         price = if_else(is.na(price), price_year, price), 
         price = if_else(is.na(price), price_pack, price), 
         price = if_else(is.na(price), price_pack_year, price)) %>% 
  mutate(units = sales / price) %>% 
  filter(price > 0, units > 0) %>% 
  select(year, quarter, province, city, market, atc3, 
         molecule, packid, price, units, sales)

write.xlsx(msd.price, "03_Outputs/05_MSD_CHC_Price.xlsx")
