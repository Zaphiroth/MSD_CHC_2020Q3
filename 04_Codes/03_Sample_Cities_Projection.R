# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2020Q3
# Purpose:      Sample Cities Projection
# programmer:   Zhe Liu
# Date:         2020-11-24
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
## hospital universe
hospital.universe.raw <- pchc.universe %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            hospital = first(na.omit(`单位名称`)), 
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup()

hospital.universe <- msd.raw %>% 
  distinct(province, city, district, pchc) %>% 
  bind_rows(hospital.universe.raw) %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district)), 
            est = first(na.omit(est))) %>% 
  ungroup()

universe.pchc <- hospital.universe %>% 
  distinct(province, city, district, pchc, est)

## segment
proj.segment <- read.xlsx("02_Inputs/seg_45cities.xlsx") %>% 
  mutate(seg_city = if_else(city == "上海", paste0(city, district), city)) %>% 
  select(seg_city, segment = seg_up)

## sampel PCHC
sample.pchc.list <- unique(msd.imp$pchc)


##---- Projection ----
## quarter sales
msd.quarter <- msd.imp %>% 
  filter(year %in% c("2020")) %>% 
  group_by(year, quarter, province, city, district, pchc, 
           market, atc3, molecule, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

## universe set
universe.set <- msd.quarter %>% 
  distinct(city, market, atc3, molecule, packid) %>% 
  inner_join(universe.pchc, by = "city") %>% 
  mutate(seg_city = if_else(city == "上海", paste0(city, district), city)) %>% 
  inner_join(proj.segment, by = "seg_city") %>% 
  merge(distinct(msd.quarter, year, quarter)) %>% 
  left_join(msd.quarter, by = c("year", "quarter", "province", "city", "district", 
                                "pchc", "market", "atc3", "molecule", "packid")) %>% 
  mutate(sample_label = if_else(city == "上海", 1, 0))

## projection parameter
proj.parm <- data.table(universe.set[universe.set$sample_label == 1, ])[, {
  ux <- mean(est, na.rm = TRUE)
  uy <- mean(sales, na.rm = TRUE)
  slope <- uy / ux
  intercept <- 0
  predict_sales = est * slope
  spearman_cor <- cor(sales, predict_sales, method = "spearman")
  list(slope = slope, intercept = intercept, spearman_cor = spearman_cor)
}, by = list(quarter, segment, market, packid)]

## projection result
sample.proj <- universe.set %>% 
  left_join(proj.parm, by = c("quarter", "segment", "market", "packid")) %>% 
  mutate(predict_sales = est * slope,
         predict_sales = if_else(predict_sales < 0, 0, predict_sales),
         final_sales = if_else(is.na(sales), predict_sales, sales),
         flag = 0) %>% 
  filter(final_sales > 0) %>% 
  group_by(year, quarter, province, city, market, atc3, molecule, packid, flag) %>% 
  summarise(sales = sum(final_sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(sample.proj, "03_Outputs/03_MSD_CHC_Sample_Cities_Projection.xlsx")
