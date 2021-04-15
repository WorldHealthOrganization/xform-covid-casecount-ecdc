suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(httr))
install.packages("ISOweek")
suppressPackageStartupMessages(library(ISOweek))




message("Test message")


dir.create("output", showWarning = FALSE)
dir.create("output/admin0", showWarning = FALSE)

#httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", # JS # 2021-02-19
httr::GET("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv",
  httr::authenticate(":", ":", type = "ntlm"),
  httr::write_disk(tf <- tempfile(fileext = ".csv")))

if(FALSE){
data <- suppressMessages(readr::read_csv(tf, na = "")) %>%
  dplyr::mutate(dateRep = as.Date(dateRep, format = "%d/%m/%Y")) %>%
  dplyr::select(-year_week, -countryterritoryCode,
    -countriesAndTerritories, -popData2019) %>%
  dplyr::rename(date = "dateRep", cases = "cases_weekly",
    deaths = "deaths_weekly", admin0_code = "geoId") %>%
  dplyr::mutate(admin0_code = ifelse(admin0_code == "JPG11668",
    "International Conveyance", admin0_code)) %>%
  dplyr::select(admin0_code, date, cases, deaths) %>%
  dplyr::arrange(admin0_code, date)
}

### Changed ECDC structure and missing ISO2 2020-02-19 START
ISO_2_3 <- data.frame(
ISO3 = c("AFG","ALB","DZA","AND","AGO","AIA","ATG","ARG","ARM","ABW","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BMU","BTN","BOL","BES","BIH","BWA","BRA","VGB","BRN","BGR","BFA","BDI","CPV","KHM","CMR","CAN","CYM","CAF","TCD","CHL","CHN","COL","COM","COG","CRI","CIV","HRV","CUB","CUW","CYP","CZE","COD","DNK","DJI","DMA","DOM","ECU","EGY","SLV","GNQ","ERI","EST","SWZ","ETH","FLK","FRO","FJI","FIN","FRA","PYF","GAB","GEO","DEU","GHA","GIB","GRC","GRL","GRD","GUM","GTM","GGY","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND","IDN","IRN","IRQ","IRL","IMN","ISR","ITA","JAM","JPN","JEY","JOR","KAZ","KEN","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LIE","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MHL","MRT","MUS","MEX","FSM","MDA","MCO","MNG","MNE","MSR","MAR","MOZ","MMR","NAM","NPL","NLD","NCL","NZL","NIC","NER","NGA","MKD","MNP","NOR","OMN","PAK","PSE","PAN","PNG","PRY","PER","PHL","POL","PRT","PRI","QAT","ROU","RUS","RWA","KNA","LCA","VCT","SMR","STP","SAU","SEN","SRB","SYC","SLE","SGP","SXM","SVK","SVN","SLB","SOM","ZAF","KOR","SSD","ESP","LKA","SDN","SUR","SWE","CHE","SYR","TWN","TJK","TZA","THA","GMB","VAT","TLS","TGO","TTO","TUN","TUR","TCA","UGA","UKR","ARE","GBR","USA","URY","VIR","UZB","VUT","VEN","VNM","WLF","ESH","YEM","ZMB","ZWE"),
ISO2 = c("AF","AL","DZ","AD","AO","AI","AG","AR","AM","AW","AU","AT","AZ","BS","BH","BD","BB","BY","BE","BZ","BJ","BM","BT","BO","BQ","BA","BW","BR","VG","BN","BG","BF","BI","CV","KH","CM","CA","KY","CF","TD","CL","CN","CO","KM","CG","CR","CI","HR","CU","CW","CY","CZ","CD","DK","DJ","DM","DO","EC","EG","SV","GQ","ER","EE","SZ","ET","FK","FO","FJ","FI","FR","PF","GA","GE","DE","GH","GI","EL","GL","GD","GU","GT","GG","GN","GW","GY","HT","HN","HU","IS","IN","ID","IR","IQ","IE","IM","IL","IT","JM","JP","JE","JO","KZ","KE","KW","KG","LA","LV","LB","LS","LR","LY","LI","LT","LU","MG","MW","MY","MV","ML","MT","MH","MR","MU","MX","FM","MD","MC","MN","ME","MS","MA","MZ","MM","NA","NP","NL","NC","NZ","NI","NE","NG","MK","MP","NO","OM","PK","PS","PA","PG","PY","PE","PH","PL","PT","PR","QA","RO","RU","RW","KN","LC","VC","SM","ST","SA","SN","RS","SC","SL","SG","SX","SK","SI","SB","SO","ZA","KR","SS","ES","LK","SD","SR","SE","CH","SY","TW","TJ","TZ","TH","GM","VA","TL","TG","TT","TN","TR","TC","UG","UA","AE","UK","US","UY","VI","UZ","VU","VE","VN","WF","EH","YE","ZM","ZW")
)

data <- suppressMessages(readr::read_csv(tf, na = "")) %>% 
left_join(ISO_2_3,by = c("country_code"="ISO3")) %>% 
  dplyr::mutate(date = ISOweek2date(paste0(substring(year_week,1,4),"-W",substring(year_week,6,7),"-1"))) %>%
  dplyr::mutate(date = date + 6) %>%
  dplyr::select(weekly_count,indicator,date,ISO2) %>%
  pivot_wider(names_from=indicator,values_from=weekly_count)  %>%
  dplyr::rename(admin0_code = "ISO2") %>%
  dplyr::mutate(admin0_code = ifelse(admin0_code == "JPG11668",
                                     "International Conveyance", admin0_code)) %>%
   dplyr::arrange(admin0_code, date)


### Changed ECDC structure and missing ISO2 2020-02-19 END
message("Test message1")
message(dim(data)[1])

data$idx <- seq_len(nrow(data))

# needs to be daily - fill in other days with zeros
data <- data %>%
  dplyr::group_by(admin0_code) %>%
  tidyr::complete(
    date = seq.Date(min(date), max(date), by = "day"),
    fill = list(cases = 0, deaths = 0)) %>% 
  ungroup() %>%
  tidyr::fill(admin0_code, idx, .direction = "up")

# average out counts across each week so that they add up weekly
# but we have daily counts so that daily plots look good
avg_counts <- function(x) {
  n <- length(x)
  avg <- sum(x) / n
  fl <- floor(avg)
  n2 <- as.integer(round(n * (avg - fl), 0))
  c(rep(fl, n - n2), rep(fl + 1, n2))
}
data <- data %>%
  group_by(idx) %>%
  mutate(cases = avg_counts(cases), deaths = avg_counts(deaths)) %>%
  ungroup() %>%
  select(-idx)
  
message("Test message2")
message("Most recent date: ", max(data$date))

# fix the codes they use for Greece and UK
data$admin0_code[data$admin0_code == "EL"] <- "GR"
data$admin0_code[data$admin0_code == "UK"] <- "GB"
# International Conveyance should be coded as "ZZ"
data$admin0_code[data$admin0_code == "International Conveyance"] <- "ZZ"
# if any coutry has all cases zero, get rid of it
ccd <- data %>%
  dplyr::group_by(admin0_code) %>%
  dplyr::summarise(n = sum(cases)) %>%
  dplyr::filter(n == 0) %>%
  dplyr::pull(admin0_code)
message("Test message3")
if (length(ccd) > 0)
  data <- dplyr::filter(data, !admin0_code %in% ccd)
message("Test message4")
admin0 <- data %>%
  dplyr::group_by(admin0_code) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(cases = cumsum(cases), deaths = cumsum(deaths)) %>%
  dplyr::filter(date >= min(date[cases > 0])) %>%
  dplyr::arrange(admin0_code, date)
message("Test message5")
# fill in any date that is missing
mdate <- max(admin0$date)
admin0 <- admin0 %>%
  group_by(admin0_code) %>%
  tidyr::complete(date = seq.Date(max(date), mdate, by = "day")) %>%
  tidyr::fill(cases, deaths) %>%
  arrange(admin0_code, date)
message("Test message6")
# admin0 %>%
#   group_by(admin0_code) %>%
#   summarise(m = max(date)) %>%
#   filter(m < mdate)

continents <- admin0 %>%
  dplyr::left_join(geoutils::admin0, by = "admin0_code") %>%
  dplyr::group_by(continent_code, date) %>%
  dplyr::filter(!is.na(continent_code)) %>%
  dplyr::summarise(cases = sum(cases), deaths = sum(deaths))

who_regions <- admin0 %>%
  dplyr::left_join(geoutils::admin0, by = "admin0_code") %>%
  dplyr::group_by(who_region_code, date) %>%
  dplyr::summarise(cases = sum(cases), deaths = sum(deaths)) %>%
  dplyr::filter(!is.na(who_region_code)) %>%
  dplyr::filter(who_region_code != "Conveyance")

global <- admin0 %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases), deaths = sum(deaths))

readr::write_csv(admin0, "output/admin0/all.csv")
readr::write_csv(continents, "output/continents.csv")
readr::write_csv(who_regions, "output/who_regions.csv")
readr::write_csv(global, "output/global.csv")

# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
