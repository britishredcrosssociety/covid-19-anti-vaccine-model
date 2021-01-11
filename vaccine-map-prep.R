# ----------------------- INTRO --------------------------
# This model is based on predictors for anti-vax attitudes identified by Paul
# et al.: https://doi.org/10.1101/2020.10.21.20216218
#
# The predictors are:
#   - flu vaccine uptake
#   - compliance with restrictions
#   - low incomes
#   - female
#   - living with children
#   - refused a recommended vaccine
#
# --------------------------------------------------------

# ---- Load libraries ----
library(tidyverse)
library(sf)
library(httr)
library(readxl)
library(arrow)
library(curl)

# ----- Load lookups & helpers -----
# Help functions
source("helpers/functions.R")

# load VI shapefile
la_shp <-
  read_sf("https://raw.githubusercontent.com/britishredcrosssociety/covid-19-vulnerability/master/output/vulnerability-LA.geojson") %>%
  select(la_code = Code, la_name = Name, geometry)

# Create LA lookup table
la_lookup <-
  la_shp %>%
  as_tibble() %>%
  select(-geometry)

# Load county lookup
lad_county <- read_csv("https://opendata.arcgis.com/datasets/1512e57c5aa34d049569750083765eba_0.csv")

# Popoulation estimates
pop <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/covid-19-vulnerability/master/data/population%20estimates%20msoa11%20lad17%20lad19%20tacticall%20cell.csv") %>%
  distinct(LAD19CD, pop_lad19)

# Geog lookup table
geog_lookup <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/covid-19-vulnerability/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv")

# London LA's
london_las <-
  lad_county %>%
  select(starts_with("LAD"),
    county = CTY19NM
  ) %>%
  filter(str_detect(county, "London")) %>%
  select(-county) %>%
  arrange(LAD19NM)

# Postcode lookup
postcode_lookup <- read_feather("analysis/vaccines/data/lookup.feather")

# ---- Flu vaccine uptake by local authority ----
# - England -
# Source: https://www.gov.uk/government/statistics/seasonal-flu-vaccine-uptake-in-gp-patients-monthly-data-2019-to-2020
# Data pre-cleaned
# load
flu <- read_csv("analysis/vaccines/data/flu-vaccine-uptake.csv")

# Select vars of interest
flu_select <-
  flu %>%
  select(
    la_name = `LA name`,
    vac_rate_over_65 = `% Vaccine Uptake (65 and over)`,
    vac_rate_under_65 = `% Vaccine Uptake (Under 65 (at-risk only))`,
    vac_rate_pregnant = `% Vaccine Uptake (Pregnant)`
  )

# Match to LA codes
flu_matched_la <-
  flu_select %>%
  mutate(la_name = str_to_lower(la_name)) %>%
  inner_join(
    la_lookup %>%
      select(la_name, la_code) %>%
      mutate(la_name = str_to_lower(la_name)),
    by = "la_name"
  )

# Match LA's incorrectly labeled as counties
flu_matched_counties <-
  flu_select %>%
  mutate(
    la_name = if_else(la_name == "Leicestershire and Rutland",
                      "Leicestershire",
                      la_name
    ),
    la_name = str_to_lower(la_name)
  ) %>%
  anti_join(flu_matched_la,
            by = "la_name"
  ) %>%
  inner_join(
    lad_county %>%
      mutate(CTY19NM = str_to_lower(CTY19NM)),
    by = c("la_name" = "CTY19NM")
  ) %>%
  select(
    CTY19NM = la_name,
    la_code = LAD19CD,
    la_name = LAD19NM,
    starts_with("vac_")
  )

# Manually match any LA's that had no match
flu_matched_manually <-
  flu_select %>%
  mutate(la_name = str_to_lower(la_name)) %>%
  anti_join(flu_matched_la,
            by = "la_name"
  ) %>%
  anti_join(flu_matched_counties,
            by = c("la_name" = "CTY19NM")
  ) %>%
  mutate(la_code = case_when(
    la_name == "city and hackney" ~ "E09000001/E09000012",
    la_name == "leicestershire and rutland" ~ "E06000017",
    la_name == "bournemouth, poole and christchurch" ~ "E06000058",
    la_name == "kernow (cornwall and isles of scilly)" ~ "E06000052/E06000053"
  )) %>%
  # separate rows with multiple LA's
  separate_rows(la_code, sep = "/")

# Combine la matches
flu_cleaned <-
  bind_rows(
    flu_matched_la,
    flu_matched_counties %>% select(-CTY19NM),
    flu_matched_manually
  ) %>%
  select(-la_name) %>%
  relocate(la_code)

# Calculate mean flu vaccination uptake rate for a give area
eng_flu_uptake <-
  flu_cleaned %>%
  rowwise() %>%
  mutate(vac_rate_mean = mean(c(vac_rate_over_65, vac_rate_under_65, vac_rate_pregnant)))

# - Wales -
# Data has to be manually scraped from pdf
# source: http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/($All)/E3F7BE45AAB413658025841700552272/$File/Seasonal%20influenza%20in%20Wales%20201819_v1a(final).pdf
wales_flu_uptake <-
  tribble(
    ~la_code, ~la_name, ~vac_rate_over_65, ~vac_rate_under_65,
    "W06000001", "Isle of Anglesey", 72.3, 52.0,
    "W06000002", "Gwynedd", 68.3, 45.4,
    "W06000003", "Conwy", 69.7, 45.2,
    "W06000004", "Denbighshire", 68.6, 68.6,
    "W06000005", "Flintshire", 74.3, 50.5,
    "W06000006", "Wrexham", 72.6, 49.1,
    "W06000008", "Ceredigion", 57.9, 37.8,
    "W06000009", "Pembrokeshire", 64.1, 38.8,
    "W06000010", "Carmarthenshire", 64.9, 37.7,
    "W06000011", "Swansea", 67.2, 43.0,
    "W06000012", "Neath Port Talbot", 67.4, 44.3,
    "W06000013", "Bridgend", 70.2, 41.8,
    "W06000014", "Vale of Glamorgan", 70.9, 41.4,
    "W06000015", "Cardiff", 69.3, 44.8,
    "W06000016", "Rhondda Cynon Taf", 66.8, 39.7,
    "W06000018", "Caerphilly", 68.3, 45.4,
    "W06000019", "Blaenau Gwent", 66.5, 45.1,
    "W06000020", "Torfaen", 71.0, 46.8,
    "W06000021", "Monmouthshire", 74.3, 53.5,
    "W06000022", "Newport", 68.0, 45.9,
    "W06000023", "Powys", 65.5, 43.1,
    "W06000024", "Merthyr Tydfil", 68.1, 41.0
  )

wales_flu_uptake <-
  wales_flu_uptake %>%
  rowwise() %>%
  mutate(vac_rate_mean = mean(c(vac_rate_over_65, vac_rate_under_65)))

# - Scotland -
# Vaccine data is only available in a limited format at the Health Board level
# Disaggregate Health Boards to LA's.
sco_flu_uptake <- read_excel("analysis/vaccines/data/HSCP_Flu_Vacc_Uptake1920_65Over.xlsx",
                             sheet = "Report",
                             skip = 3)

sco_flu_uptake <-
  sco_flu_uptake %>%
  slice(1:31) %>%
  select(la_name = HSCP,
         vac_rate_over_65 = `Percentage Uptake at end of season (Week 15)`)

# Match LA's
# Clackmannanshire and Stirling form one HSCP but need splitting
sco_flu_uptake <-
  sco_flu_uptake %>%
  mutate(la_name = case_when(
    la_name == "Edinburgh" ~ "City of Edinburgh",
    la_name == "Clackmannanshire and Stirling" ~ "Clackmannanshire",
    la_name == "Western Isles" ~ "Na h-Eileanan Siar",
    TRUE ~ la_name
  ))%>%
  add_row(la_name = "Stirling", vac_rate_over_65 = 0.767)

# setequal(
#   sco_flu_uptake %>% pull(la_name),
#   la_lookup %>% filter(str_detect(la_code, "^S")) %>% pull(la_name)
# )
sco_flu_uptake <-
  left_join(sco_flu_uptake, la_lookup, by = "la_name") %>%
  select(la_code, vac_rate_over_65) %>%
  mutate(vac_rate_over_65 = vac_rate_over_65*100)

# - NI -
# Data unavailable at LA level - confirmed by PHA

# Combine nations
flu_uk <-
  bind_rows(
    eng_flu_uptake %>% select(la_code, vac_rate_over_65),
    wales_flu_uptake %>% select(la_code, vac_rate_over_65),
    sco_flu_uptake %>% select(la_code, vac_rate_over_65)
  )

# ---- Female ----
# - England & Wales
# Use ONS 2019 population estimates: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates
GET(
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fmiddlesuperoutputareamidyearpopulationestimates%2fmid2019sape22dt4/sape22dt4mid2019msoasyoaestimatesunformatted.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = "analysis/vaccines/data/population")
unlink(tf)
rm(tf)

pop_fem_eng_wal <- read_excel("analysis/vaccines/data/population/SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx", sheet = "Mid-2019 Females", skip = 4)

pop_fem_eng_wal <-
  pop_fem_eng_wal %>%
  select(
    la_code = `LA Code (2019 boundaries)`,
    count = `All Ages`
  ) %>%
  group_by(la_code) %>%
  summarise(female_count = sum(count))

pop_total_eng_wal <- read_excel("analysis/vaccines/data/population/SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx", sheet = "Mid-2019 Persons", skip = 4)

pop_total_eng_wal <-
  pop_total_eng_wal %>%
  select(
    la_code = `LA Code (2019 boundaries)`,
    count = `All Ages`
  ) %>%
  group_by(la_code) %>%
  summarise(total_count = sum(count))

pop_eng_wal_female <-
  left_join(
    pop_fem_eng_wal,
    pop_total_eng_wal,
    by = "la_code"
  ) %>%
  mutate(perc_female = female_count / total_count) %>%
  select(
    la_code,
    perc_female
  )

# - Scotland -
# Scotland Intermediate Zone (2011) Population Estimates
# source: https://www.opendata.nhs.scot/dataset/population-estimates/resource/93df4c88-f74b-4630-abd8-459a19b12f47
#
pop_sco <- read_csv("https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/93df4c88-f74b-4630-abd8-459a19b12f47/download/iz2011-pop-est_02042020.csv")

pop_sco_msoa <-
  pop_sco %>%
  filter(Year == max(Year)) %>%
  filter(!str_detect(IntZone, "S9")) %>% # removes non msoa aggregates
  select(msoa_code = IntZone, sex = Sex, count = AllAges) %>%
  filter(sex != "Male") %>%
  pivot_wider(names_from = sex, values_from = count)

pop_sco_female <-
  pop_sco_msoa %>%
  left_join(
    geog_lookup,
    by = c("msoa_code" = "MSOA11CD")
  ) %>%
  select(
    la_code = LAD19CD,
    All,
    Female
  ) %>%
  group_by(la_code) %>%
  summarise(
    all = sum(All),
    female = sum(Female)
  ) %>%
  mutate(perc_female = female / all) %>%
  select(la_code, perc_female)

# - Northern Ireland -
# Source: https://www.nisra.gov.uk/publications/2019-mid-year-population-estimates-northern-ireland
GET(
  "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/MYE19_AGE_BANDS.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

ni_pop <- read_excel(tf, sheet = "Flat")

pop_ni_female <-
  ni_pop %>%
  filter(
    year == 2019,
    gender == "Females" | gender == "All persons",
    area_code %in% la_lookup$la_code
  ) %>%
  select(
    la_code = area_code,
    gender,
    age = age_5,
    count = MYE
  ) %>%
  group_by(la_code, gender) %>%
  summarise(total = sum(count)) %>%
  pivot_wider(names_from = gender, values_from = total) %>%
  mutate(perc_female = Females / `All persons`) %>%
  select(-`All persons`, -Females)

pop_uk_female <-
  bind_rows(
    pop_eng_wal_female,
    pop_sco_female,
    pop_ni_female
  )

# ---- Compliance with restrictions ----
# - Google data -
# Attribution required:
# If you publish results based on this data set, please cite as:
#
# Google LLC "Google COVID-19 Community Mobility Reports".
# https://www.google.com/covid19/mobility/ Accessed: <date>.

GET(
  "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = "analysis/vaccines/data/google-mobility")
unlink(tf)
rm(tf)

google <- read_csv("analysis/vaccines/data/google-mobility/2020_GB_Region_Mobility_Report.csv")

google <- google %>%
  select(
    region = sub_region_1,
    date,
    retail_and_recreation_percent_change_from_baseline,
    transit_stations_percent_change_from_baseline,
    residential_percent_change_from_baseline
  ) %>%
  drop_na(region)

# Match "region" to counties
google_cty_match <-
  google %>%
  mutate(region = str_to_lower(region)) %>%
  inner_join(
    lad_county %>% mutate(CTY19NM = str_to_lower(CTY19NM)),
    by = c("region" = "CTY19NM")
  ) %>%
  select(
    la_code = LAD19CD,
    everything(),
    -FID,
    -LAD19NM,
    -CTY19CD
  )

# Match "region" directly to LA's
google_la_match <-
  google %>%
  mutate(region = str_to_lower(region)) %>%
  anti_join(
    google_cty_match,
    by = "region"
  ) %>%
  inner_join(
    la_lookup %>% mutate(la_name = str_to_lower(la_name)),
    by = c("region" = "la_name")
  ) %>%
  relocate(la_code)

# Manually match "region" to LA's
google_manual_match <-
  google %>%
  mutate(region = str_to_lower(region)) %>%
  anti_join(
    google_cty_match,
    by = "region"
  ) %>%
  anti_join(
    google_la_match,
    by = "region"
  ) %>%
  mutate(la_code = case_when(
    region == "angus council" ~ "S12000041",
    region == "argyll and bute council" ~ "S12000035",
    region == "borough of halton" ~ "E06000006",
    region == "bridgend county borough" ~ "W06000013",
    region == "bristol city" ~ "E06000023",
    region == "caerphilly county borough" ~ "W06000018",
    region == "conwy principal area" ~ "W06000003",
    region == "derry and strabane" ~ "N09000005",
    region == "dundee city council" ~ "S12000042",
    region == "east ayrshire council" ~ "S12000008",
    region == "east dunbartonshire council" ~ "S12000045",
    region == "east lothian council" ~ "S12000010",
    region == "east renfrewshire council" ~ "S12000011",
    region == "edinburgh" ~ "S12000036",
    region == "greater london" ~ str_c(london_las$LAD19CD, collapse = "/"),
    region == "herefordshire" ~ "E06000019",
    region == "highland council" ~ "S12000017",
    region == "kingston upon hull" ~ "E06000010",
    region == "merthyr tydfil county borough" ~ "W06000024",
    region == "na h-eileanan an iar" ~ "S12000013",
    region == "neath port talbot principle area" ~ "W06000012",
    region == "north ayrshire council" ~ "S12000021",
    region == "orkney" ~ "S12000023",
    region == "rhondda cynon taff" ~ "W06000016",
    region == "south ayrshire council" ~ "S12000028",
    region == "torfaen principal area" ~ "W06000020",
    region == "west dunbartonshire council" ~ "S12000039",
    region == "wrexham principal area" ~ "W06000006"
  )) %>%
  separate_rows(la_code, sep = "/") %>%
  relocate(la_code)

# Write test to check if all Google regions have been matched
if (google %>%
  mutate(region = str_to_lower(region)) %>%
  anti_join(
    google_cty_match,
    by = "region"
  ) %>%
  anti_join(
    google_la_match,
    by = "region"
  ) %>%
  anti_join(
    google_manual_match,
    by = "region"
  ) %>%
  nrow() != 0) {
  "Locations remain unmatched, you are not a legend."
} else {
  "You are a legend for matching such messy data!"
}

# Join matched google data
google_matched <-
  bind_rows(
    google_cty_match,
    google_la_match,
    google_manual_match
  ) %>%
  select(-region)

# Calculate lockdown adherence score
# - Use only retail and recreation, and transit stations. The other variables
#   likely capture movement in line with Lockdown regulations.
#   The score is the mean score of the percentage changes from baseline. A more
#   positive score = less adherence to rules.
# - Average score across time to create cross-sectional measure
google_score <-
  google_matched %>%
  rowwise() %>%
  mutate(score = mean(c(
    retail_and_recreation_percent_change_from_baseline,
    transit_stations_percent_change_from_baseline
  ), na.rm = TRUE)) %>%
  select(la_code, date, score) %>%
  filter(date >= "2020-10-14") %>%
  group_by(la_code) %>%
  summarise(score = mean(score, na.rm = TRUE))

# Impute missing scores
google_eng_av <-
  google_score %>%
  filter(str_detect(la_code, "^E")) %>%
  pull(score) %>%
  mean()

google_sco_av <-
  google_score %>%
  filter(str_detect(la_code, "^S")) %>%
  drop_na() %>%
  pull(score) %>%
  mean()

# Isles of Scilly = Cornwall score
# Telford and Wrekin and bournemouth, Christchurch and Poole eng averages
google_missing <-
  tibble(
    la_code = c(
      "E06000020", "E06000053", "E06000058", "S12000023"
    ),
    score = c(
      google_eng_av,
      google_score %>% filter(la_code == "E06000052") %>% pull(score),
      google_eng_av,
      google_sco_av
    )
  )

google_score <-
  bind_rows(
    google_score %>% drop_na(),
    google_missing
  ) %>%
  select(la_code, compliance_score = score)

# ---- Low childhood vaccine uptake ----
# Source: https://digital.nhs.uk/data-and-information/publications/statistical/nhs-immunisation-statistics/england---2019-20
# - England -
# First dose of MMR at 24 months
# The raw .csv files do not contain the counts of children aged 2 years.
# The .xlsx containing these counts was manually downloaded and cleaned
childhood <- read_csv("analysis/vaccines/data/childhood-vaccines-nhs.csv")

# Note many of the "LA" codes from the NHS are not actually LA's, but counties.
childhood_las_matched <-
  childhood %>%
  semi_join(
    la_lookup,
    by = "la_code"
  )

childhood_counties_matched <-
  childhood %>%
  anti_join(
    la_lookup,
    by = "la_code"
  ) %>%
  select(CTY19CD = la_code, MMR) %>%
  left_join(
    lad_county %>% select(LAD19CD, CTY19CD),
    by = "CTY19CD"
  ) %>%
  select(la_code = LAD19CD, MMR)

mmr_eng <-
  bind_rows(
    childhood_las_matched,
    childhood_counties_matched
  ) %>%
  select(-la_name,
    mmr_uptake = MMR
  )

# From footnote of Excel data:
#   (3) Leicestershire also contains data for Rutland.
#   (4) Hackney also contains data for City of London.
#   (5) Cornwall also contains data for Isles of Scilly.
mmr_eng_missing <-
  tribble(
    ~la_code, ~mmr_uptake,
    # City of London
    "E09000001", 77.1,
    # Rutland
    "E06000017", 91.2,
    # Isles of Scilly
    "E06000053", 91.6
  )

mmr_eng <-
  bind_rows(
    mmr_eng,
    mmr_eng_missing
  )

# - Scotland -
GET(
  "https://beta.isdscotland.org/media/5713/child_imms_latestrates_quarter_220_la.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

scot_mmr <- read_excel(tf, sheet = "Table 2 - 24m (LA)", skip = 4)

scot_mmr <-
  scot_mmr %>%
  slice(-1:-2) %>%
  slice(-33:-44) %>%
  select(
    la_name = `Local authority1`,
    mmr_uptake = ...6
  )

# Join LA codesg
mmr_scot <-
  scot_mmr %>%
  left_join(la_lookup, by = "la_name") %>%
  mutate(la_code = case_when(
    la_name == "Argyll & Bute" ~ "S12000035",
    la_name == "Dumfries & Galloway" ~ "S12000006",
    la_name == "Edinburgh City" ~ "S12000036",
    la_name == "Na h-Eileanan Siar3" ~ "S12000013",
    la_name == "Orkney Islands3" ~ "S12000023",
    la_name == "Perth & Kinross" ~ "S12000048",
    la_name == "Shetland Islands3" ~ "S12000027",
    TRUE ~ la_code
  )) %>%
  select(la_code, mmr_uptake) %>%
  mutate(mmr_uptake = as.double(mmr_uptake))

# - Wales -
wales_mmr <- download_wales("http://open.statswales.gov.wales/en-gb/dataset/hlth1011")

mmr_wales <-
  wales_mmr %>%
  as_tibble() %>%
  filter(
    Year_ItemName_ENG == "2018-19",
    Measure_ItemName_ENG == "MMR"
  ) %>%
  select(
    la_code = Area_AltCode1,
    mmr_uptake = Data
  ) %>%
  filter(la_code %in% la_lookup$la_code)

# - NI -
# Data unavailable at LA level - confirmed by PHA

mmr_uk <-
  bind_rows(
    mmr_scot,
    mmr_eng,
    mmr_wales
  )

# ---- Low income ----
caci <- read_csv("analysis/vaccines/data/caci-vul-indicators.csv")

low_income <-
  caci %>%
  select(postcode = Postcode, income = Household_income) %>%
  drop_na()

low_income_matched <-
  low_income %>%
  mutate(
    postcode = str_to_upper(postcode),
    postcode = str_remove_all(postcode, " ")
  ) %>%
  left_join(
    postcode_lookup %>% select(postcode, la_code),
    by = "postcode"
  )

income_score <-
  low_income_matched %>%
  add_count(la_code, name = "la_size") %>%
  filter(income == 1 | income == 2) %>%
  add_count(la_code, name = "deprived_count") %>%
  distinct(la_code, la_size, deprived_count) %>%
  mutate(income_score = deprived_count / la_size) %>%
  select(la_code, income_score)

# ---- Living with children ----
dep_child <-
  caci %>%
  select(postcode = Postcode, dep_children = `Young_dependent_children_-_Decile`)

dep_child_matched <-
  dep_child %>%
  mutate(
    postcode = str_to_upper(postcode),
    postcode = str_remove_all(postcode, " ")
  ) %>%
  left_join(
    postcode_lookup %>% select(postcode, la_code),
    by = "postcode"
  )

dep_child_score <-
  dep_child_matched %>%
  add_count(la_code, name = "la_size") %>%
  filter(dep_children == 1 | dep_children == 2) %>%
  add_count(la_code, name = "dep_count") %>%
  distinct(la_code, la_size, dep_count) %>%
  mutate(dep_score = dep_count / la_size) %>%
  select(la_code, dep_score)
