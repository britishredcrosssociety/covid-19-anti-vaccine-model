# ---- Source ----
source("analysis/vaccines/vaccine-map-prep.R")
source("helpers/indicator-functions.r")

# ---- Library ----
library(viridis)

# ---- Load ----
shp_uk <- read_sf("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-LA.geojson")

# ---- Build Map Scores ----
all_indicators <-
  google_score %>%
  left_join(
    dep_child_score,
    by = "la_code"
  ) %>%
  left_join(
    income_score,
    by = "la_code"
  ) %>%
  left_join(
    pop_uk_female,
    by = "la_code"
  ) %>%
  left_join(
    mmr_uk,
    by = "la_code"
  ) %>%
  left_join(
    flu_uk,
    by = "la_code"
  )

# - England, Wales & Scotland -
vaccine_scores_eng_wal_sco <-
  all_indicators %>%
  rename(vac_rate = vac_rate_over_65) %>%
  filter(!str_detect(la_code, "^N")) %>%
  calc_vaccine_scores()

# - NI-
# Missing vaccination indicators flu/mmr
vaccine_scores_ni <-
  all_indicators %>%
  filter(str_detect(la_code, "^N")) %>%
  calc_vaccine_scores_incomplete_indicators()


# ---- Custom map theme ----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "FiraCode-Retina", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      # Add labs elements
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5,
        margin = margin(
          b = 0.2,
          t = 0.2,
          l = 2,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = margin(
          t = 0.2,
          b = 0,
          unit = "cm"
        ),
        color = "#939184"
      ),
      ...
    )
}


# ---- Plot Map ----
# Prep LA shape file
shp_file <-
  shp_uk %>%
  select(la_code = Code, geometry)

# - Prep overlaying CCG's-
# Note: only England uses CCG's.
# See: https://mstrust.org.uk/a-z/nhs-uk for equivalents.

# England: Prep CCG shape file
GET(
  "https://opendata.arcgis.com/datasets/dbfaf69873794690af4acddaf581572f_1.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = "analysis/vaccines/data/ccg")
unlink(tf)
rm(tf)

ccg_shp <- read_sf("analysis/vaccines/data/ccg/Clinical_Commissioning_Groups__April_2020__EN_BUC.shp")

# Wales: Prep HBW shape file
GET(
  "https://opendata.arcgis.com/datasets/87e71b2c79fc4ac894eeb79359cda131_4.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = "analysis/vaccines/data/hbw")
unlink(tf)
rm(tf)

hbw_shp <- read_sf("analysis/vaccines/data/hbw/Local_Health_Boards__December_2016__Boundaries.shp")

# Scotland: Prep HBS shape file
# The shapefile is too large to render
# The Below code pulls the raw data. This was then passed into
# https://mapshaper.org/ and the shape was simplified using the
# visvalingram method.

# GET(
#   "http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_NHS_HealthBoards_2019.zip",
#   write_disk(tf <- tempfile(fileext = ".zip"))
# )
#
# unzip(tf, exdir = "analysis/vaccines/data/hbs")
# unlink(tf)
# rm(tf)
#
hbs_shp <- read_sf("analysis/vaccines/data/hbs-mapshaper/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp")

# - NI -
# Mapshaper routine was followed in the same fasion
# Source: https://www.opendatani.gov.uk/dataset/department-of-health-trust-boundaries/resource/645f8eef-8813-47a9-bb1e-a4932ada721a

hsct_shp <- read_sf("analysis/vaccines/data/hsct/trustboundaries/trustboundaries.shp")

# Prep plot data
plot_data <-
  bind_rows(
    vaccine_scores_eng_wal %>%
      select(la_code, decile = `Vulnerability decile`),
    vaccine_scores_sco_ni %>%
      select(la_code, decile = `Vulnerability decile`)
  )

shp_file %>%
  left_join(
    plot_data,
    by = "la_code"
  ) %>%
  ggplot() +
  geom_sf(
    color = "darkgrey",
    size = 0.15,
    mapping = aes(fill = decile)
  ) +
  geom_sf(
    data = ccg_shp,
    color = "black",
    size = 0.2,
    fill = NA
  ) +
  geom_sf(
    data = hbw_shp,
    color = "black",
    size = 0.2,
    fill = NA
  ) +
  geom_sf(
    data = hbs_shp,
    color = "black",
    size = 0.2,
    fill = NA
  ) +
  geom_sf(
    data = hsct_shp,
    color = "black",
    size = 0.2,
    fill = NA
  ) +
  scale_fill_viridis(
    breaks = seq(1, 10, by = 1),
    labels = seq(1, 10, by = 1),
    na.value = "transparent",
    option = "D",
    name = expression(paste("Most\nUnwilling")),
    alpha = 0.8, # make fill a bit brighter
    begin = 0.1, # this option seems to be new (compared to 2016):
    # with this we can truncate the
    # color scale, so that extreme colors (very dark and very bright) are not
    # used, which makes the map a bit more aesthetic
    end = 0.95,
    discrete = F, # discrete classes, thus guide_legend instead of _colorbar
    direction = -1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T
    )
  ) +
  theme_map() +
  labs(
    title = "Willingness to take vaccines coloured at the Local Authority level (grey boundaries)",
    subtitle = "Clinical Commissioning Groups, Health and Social Care Trusts, and Local Health Boards are overlayed for \nEngland, Northern Ireland, and Wales & Scotland respectively (black boundaries)",
    caption = "10 = most unwilling | 1 = most willing"
  )

# ---- Save spatial data ----
shp_file %>%
  left_join(
    plot_data,
    by = "la_code"
  ) %>%
  st_transform(crs = 27700) %>%
  write_sf("analysis/vaccines/data/vax index.shp")

# ---- Save indicators ----
calc_scores <- bind_rows(
  vaccine_scores_eng_wal,
  vaccine_scores_sco_ni
)

write_csv(calc_scores, "analysis/vaccines/data/vax index scores.csv")

# ---- List worst ten LAs ----
la <- read_csv("https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.csv")

la <- la %>%
  left_join(calc_scores, by = c("LAD19CD" = "la_code"))

la_list <- la %>%
  mutate(Country = str_sub(LAD19CD, 1, 1)) %>%

  group_by(Country) %>%
  filter(`Vulnerability rank` > max(`Vulnerability rank`) - 10) %>%

  arrange(Country, desc(`Vulnerability rank`)) %>%
  select(Country, LAD19NM, `Vulnerability rank`, `Vulnerability decile`)

