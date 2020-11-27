# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2020Q3
# Purpose:      Readin Raw Data
# programmer:   Zhe Liu
# Date:         2020-11-24
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
## PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20201118.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

## market definition
market.def <- read.xlsx("02_Inputs/默沙东CMAX 项目通知书-2020-0605.xlsx", sheet = "市场定义") %>% 
  distinct(Molecule, Molecule_CN) %>% 
  filter(!is.na(Molecule), !is.na(Molecule_CN)) %>% 
  mutate(Molecule = toupper(Molecule))

## target city
kTargetCity <- c("北京", "上海", "广州", "杭州", "苏州", "南京", "福州", "宁波")


##---- Formatting raw data ----
servier.raw <- read_feather('02_Inputs/data/01_Servier_CHC_Raw.feather')

msd.raw <- servier.raw %>% 
  filter(market == "OAD", 
         molecule %in% market.def$Molecule) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  group_by(year, date, quarter, pchc, market, atc3, molecule, packid) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district)),
            units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(msd.raw, "03_Outputs/01_MSD_CHC_Raw.xlsx")

## QC
prod.desc <- read.csv("02_Inputs/pfc与ims数据对应_20200824.csv") %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0)) %>% 
  select(packid = Pack_Id, Prd_desc = `商品名`)

raw.check <- servier.raw %>% 
  filter(market == "OAD", 
         molecule %in% market.def$Molecule, 
         year %in% c("2020")) %>% 
  left_join(prod.desc, by = 'packid') %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  group_by(city) %>% 
  mutate(pchc_n = length(unique(pchc))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, pchc_n, market, atc3, 
           molecule, Prd_desc) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(raw.check, '03_Outputs/01_MSD_CHC_Raw_Check.xlsx')
