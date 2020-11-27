# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2020Q3
# Purpose:      Non-sample Cities Projection
# programmer:   Zhe Liu
# Date:         2020-11-24
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
## city tier
city.tier <- read.xlsx("02_Inputs/pchc_city_tier.xlsx") %>% 
  group_by(city) %>% 
  mutate(tier = ifelse(is.na(city_tier), first(na.omit(city_tier)), city_tier)) %>% 
  ungroup() %>% 
  mutate(tier = ifelse(is.na(tier), 5, tier)) %>% 
  distinct(city, tier)

## universe PCHC
universe.city <- pchc.universe %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            pop = first(na.omit(`人口`)),
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup() %>% 
  filter(!is.na(est), !is.na(pop)) %>% 
  left_join(city.tier, by = "city") %>% 
  mutate(tier = ifelse(is.na(tier), 1, tier)) %>% 
  group_by(province, city, tier) %>% 
  summarise(pop = sum(pop, na.rm = TRUE),
            est = sum(est, na.rm = TRUE)) %>% 
  ungroup()

## universe district
proj.market <- msd.imp %>% 
  filter(year == '2020') %>% 
  left_join(city.tier, by = "city") %>% 
  mutate(tier = ifelse(is.na(tier), 1, tier)) %>% 
  group_by(year, quarter, province, city, tier, market, atc3, molecule, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(universe.city, by = c("province", "city", "tier"))


##---- Projection ----
proj.region.list <- vector("list", length = nrow(universe.city))

pb <- txtProgressBar(min = 1, max = nrow(universe.city), initial = 1) 

for (i in 1:nrow(universe.city)) {
  setTxtProgressBar(pb, i)
  
  proj.region.list[[i]] <- universe.city[i, ] %>% 
    left_join(proj.market, by = c("tier")) %>% 
    mutate(est_gap = abs(est.x - est.y)) %>% 
    filter(est_gap <= min(est_gap)) %>% 
    mutate(slope = ifelse(is.infinite(est.x / est.y) | is.na(est.x / est.y) | is.nan(est.x / est.y), 
                          1, 
                          est.x / est.y)) %>% 
    gather(quarter, sales, -setdiff(1:ncol(.), starts_with("20"))) %>% 
    select(quarter, province = province.x, city = city.x, tier, market, atc3, 
           molecule, packid, sales, est.x, est.y, slope)
}

universe.proj <- proj.region.list %>% 
  bind_rows() %>% 
  mutate(slope = ifelse(slope > quantile(slope, 0.9, na.rm = TRUE), 
                        quantile(slope, 0.9, na.rm = TRUE), 
                        slope),
         final_sales = sales * slope,
         year = stri_sub(quarter, 1, 4)) %>% 
  filter(final_sales > 0, !(city %in% unique(sample.proj$city))) %>% 
  mutate(flag = 1) %>% 
  select(year, quarter, province, city, market, atc3, molecule, 
         packid, sales = final_sales, flag) %>% 
  bind_rows(sample.proj)

write.xlsx(universe.proj, "03_Outputs/04_MSD_CHC_Universe_Projection.xlsx")
