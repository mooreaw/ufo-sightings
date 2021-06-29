# libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(lubridate)
library(magrittr)
library(plotly)
library(broom)
library(leaflet)
library(leaflet.extras)
library(tidycensus)
library(sf)

# import data -------------------------------------------------------------

# we'll look at the 'scrubbed' data, no incomplete reports for now
scrubbed <- read_csv("../data/ufo-sightings/scrubbed.csv.zip")

# let's look only at US sightings
us <- scrubbed %>% 
  filter(country == "us") %>%
  mutate(
    date_time    = mdy_hm(datetime),
    year_sighted = year(date_time),
    date_posted  = as.Date(`date posted`, "%m/%d/%Y"),
    duration     = `duration (seconds)`
  )

us <- filter(us, year_sighted < 2014)

# median sightings over time ----------------------------------------------

by_year_boxplot <- us %>% 
  count(year_sighted, state) %>% 
  ungroup() %>% 
  filter(year_sighted > 1990) %>% 
  ggplot(aes(x = factor(year_sighted), y = n)) + 
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(x = "", y = "# of Sightings", title = "Median Number of State Sightings, 1991-2013")

ggplotly(by_year_boxplot)

# shapes people report seeing ---------------------------------------------

shape_lut <- c(
  "cylinder"  = "cylinders, cigars, crescents, cigars, crosses",
  "circle"    = "circles, spheres, ovals, teardrops, eggs, round",
  "light"     = "lights, fireballs, flashes, formations",
  "sphere"    = "circles, spheres, ovals, teardrops, eggs, round",
  "disk"      = "disk",
  "fireball"  = "lights, fireballs, flashes, formations",
  "unknown"   = "unknown, other, changing",
  "oval"      = "circles, spheres, ovals, teardrops, eggs, round",
  "other"     = "unknown, other, changing",
  "rectangle" = "rectangle, hexagon",
  "chevron"   = "chevrons, deltas, triangles, pyramids, diamonds, cones",
  "formation" = "lights, fireballs, flashes, formations",
  "triangle"  = "chevrons, deltas, triangles, pyramids, diamonds, cones",
  "cigar"     = "cylinders, cigars, crescents, cigars, crosses",
  "NA"        = "missing",
  "delta"     = "chevrons, deltas, triangles, pyramids, diamonds, cones",
  "changing"  = "unknown, other, changing",
  "diamond"   = "chevrons, deltas, triangles, pyramids, diamonds, cones",
  "flash"     = "lights, fireballs, flashes, formations",
  "egg"       = "circles, spheres, ovals, teardrops, eggs, round",
  "teardrop"  = "circles, spheres, ovals, teardrops, eggs, round",
  "cone"      = "chevrons, deltas, triangles, pyramids, diamonds, cones",
  "cross"     = "cylinders, cigars, crescents, cigars, crosses",
  "pyramid"   = "chevrons, deltas, triangles, pyramids, diamonds, cones",
  "round"     = "circles, spheres, ovals, teardrops, eggs, round",
  "flare"     = "lights, fireballs, flashes, formations",
  "hexagon"   = "rectangle, hexagon",
  "crescent"  = "cylinders, cigars, crescents, cigars, crosses",
  "changed"   = "unknown, other, changing"
)

us$shape_simp <- shape_lut[us$shape]

shapes_frame <- us %>% 
  mutate(shape_simp = fct_explicit_na(shape_simp, "missing")) %>% 
  count(year_sighted, shape_simp) %>%       # need to fill years without all the
  spread(shape_simp, n, fill = 0) %>%       # levels with 0s to make sure the plot
  gather(shape_simp, n, -year_sighted) %>%  # doesn't get messed up
  group_by(year_sighted) %>%
  mutate(pct  = n / sum(n)) %>%
  ungroup() %>%
  arrange(year_sighted, pct) %>%
  filter(year_sighted > 1949)

shapes_frame$shape_simp <- shapes_frame %$%
  fct_reorder(factor(shape_simp), n, .desc = TRUE)

shapes <- shapes_frame %>% 
  filter(year_sighted != 1995, year_sighted != 1996) %>% 
  ggplot(aes(x = year_sighted, y = pct, fill = shape_simp, group = shape_simp)) +
  geom_area(aes(fill = shape_simp, group = shape_simp), alpha = .8) +
  theme_minimal(base_size = 15) +
  scale_y_continuous(label = percent) +
  theme(
    panel.grid.minor = element_blank(),
    legend.title     = element_blank(),
    legend.position  = "bottom"
  ) +
  labs(
    x = "", 
    y = "", 
    title = "USA: UFO Shapes by Year, 1950 - 2013", 
    caption = "* 1995 & 1996 excluded due to high levels of missing data.") +
  scale_fill_brewer(palette = 16)

shapes

# heatmap by month*year ---------------------------------------------------

# dataset from tidyr
uspop <- filter(population, country == "United States of America")

p2 <- us %>% 
  count(mo = month(date_time), year_sighted) %>% 
  inner_join(uspop, by = c("year_sighted" = "year")) %>% 
  mutate(
    Rate  = n %>% divide_by(population) %>% multiply_by(1000000),
    Month = factor(mo, levels = 1:12, labels = month.abb),
    Year  = factor(year_sighted, labels = c("1995", str_c("'", c(96:99, str_c("0", 0:9), 10:13))))
  ) %>% 
  ggplot(aes(x = Year, y = Month, fill = Rate)) +
  geom_tile() +
  theme_minimal(base_size = 15) +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "", title = "UFO Sightings in the USA per 1,000,000 People")

ggplotly(p2)

# bivariate choropleth ----------------------------------------------------

us$geometry <- 1:nrow(us) %>%
  lapply(function(x) st_point(c(us$longitude[x], us$latitude[x]))) %>% 
  st_sfc(crs = 4269)

us2 <- st_sf(us)

countydat <- 
  get_acs(
    geography = "county",
    variables = c("B17006_001", "B17006_002"),
    output    = "wide",
    geometry  = TRUE
  ) %>%
  mutate(area = st_area(geometry)) %>% 
  st_transform(4269) %>% 
  st_join(us2, join = st_contains)

rates <- countydat %>%
  as_tibble() %>%
  count(NAME) %>% 
  right_join(
    countydat %>% 
      select(NAME, B17006_001E, area, geometry) %>%
      as_tibble() %>%
      distinct(NAME, .keep_all = TRUE)
  ) %>%
  mutate(
    d = B17006_001E / as.numeric(area / 1000000),
    dm = Hmisc::cut2(d, g = 3, levels.mean = TRUE),
    nm = Hmisc::cut2(n, g = 3, levels.mean = TRUE)
  )

levels(rates$dm) <- 1:3
levels(rates$nm) <- 1:3

rates$bin <- str_c(rates$dm, "-", rates$nm) %>% 
  factor(levels = c(
    "3-1", "2-1", "1-1",  # col 1 -->
    "3-2", "2-2", "1-2",  # col 2 -->
    "3-3", "2-3", "1-3"   # col 3 -->
  ))

vals <- c(
  "#8c510a", "#bf812d", "#dfc27d", # col 1 -->
  "#f6e8c3", "#f5f5f5", "#c7eae5", # col 2 -->
  "#80cdc1", "#35978f", "#01665e"  # col 3 -->
)

pal <- colorFactor(palette = rev(vals), rates$bin)

county_poly <- rates %>%
  filter(!str_detect(NAME, "Alaska|Hawaii|Puerto")) %>%
  st_sf() %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84")

county_ctr <- county_poly %>%
  st_cast("POLYGON") %>%
  st_centroid()

bv_choro <- leaflet() %>%
  addPolygons(
    data = county_poly,
    color = "black",
    popup = ~str_glue_data(., "{NAME}; Sightings: {n}, Density: {round(d, 2)}"),
    stroke = TRUE,
    weight = 1,
    smoothFactor = 0.2,
    fillOpacity = 1,
    fillColor = ~ pal(bin),
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
  ) %>%
  addResetMapButton()

legend_p <- rates %>% 
  select(dm, nm, bin) %>% 
  distinct() %>% 
  arrange(bin) %>% 
  ggplot(aes(x = nm, y = dm, fill = bin)) +
  geom_tile() +
  scale_fill_manual(values = rev(vals)) +
  theme_minimal() +
  labs(x = sprintf("Sightings \u2192"), y = sprintf("Density \u2192")) +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    legend.position = "none"
  )

# rates %>% 
#   group_by(bin) %>% 
#   arrange(desc(n), desc(d)) %>% 
#   slice(1) %>%  
#   select(bin, n, d) %>% 
#   arrange(bin)

bv_choro

legend_p

# modeling counts from each state, 2013 -----------------------------------

census <- get_acs(
  geography = "state", 
  endyear = 2013,
  variables = c(
    "B25064_001",  # median gross rent
    "B06011_001",  # median income
    "B01002_001",  # median age
    "B02001_003",  # black alone
    "B02001_002",  # white alone
    "B03002_012",  # hispanic or latino
    "B02001_005",  # asian alone
    "B16010_028",  # some college or associates
    "B16010_041"   # bachelor's or higher
  ),
  summary_var = "B01003_001"      # total people in unit
)

vlut <- c(
  "B25064_001" = "median gross rent",
  "B06011_001" = "median income",
  "B01002_001" = "median age",
  "B02001_003" = "black alone",
  "B02001_002" = "white alone",
  "B03002_012" = "hispanic or latino",
  "B02001_005" = "asian alone",
  "B16010_028" = "some college or associates",
  "B16010_041" = "bachelors or higher"
)

census$variable <- vlut[census$variable]

medians <- census %>%
  filter(str_detect(variable, "median")) %>%
  select(NAME, variable, estimate, total_population = summary_est) %>%
  mutate(variable = str_replace_all(variable, " ", "_")) %>%
  spread(variable, estimate)

props <- census %>%
  mutate(variable = str_replace_all(variable, " ", "_")) %>%
  filter(!variable %in% names(medians)) %>%
  mutate(prop = estimate %>% divide_by(summary_est) %>% multiply_by(100)) %>%
  select(NAME, variable, prop) %>%
  spread(variable, prop)

demog <- medians %>%
  left_join(props, by = "NAME") %>% 
  rename(state = NAME)

binge <- read_csv("../data/ufo-sightings/2015-binge-prev-cdc.csv")
b5tsc <- read_csv("../data/ufo-sightings/rentfrow-state-t-scores-b5.csv")
relig <- read_csv("../data/ufo-sightings/wiki-relig.csv")

relig$region <- str_to_title(relig$region)

binge <- rename(binge, binge_prev = pct)

yr13 <- us %>% 
  filter(year_sighted == 2013) %>% 
  count(state.abb = str_to_upper(state)) %>%
  rename(sightings = n) %>% 
  left_join(tibble(state.abb, state = state.name), "state.abb")

demog <- medians %>%
  rename(state = NAME) %>%
  left_join(props, c("state" = "NAME")) %>%
  left_join(b5tsc, "state") %>% 
  left_join(binge, "state") %>% 
  left_join(relig, c("state" = "region")) %>% 
  left_join(yr13, "state") %>% 
  filter(!is.na(sightings))

sight_desc <- summarise(demog, avg = mean(sightings), sd = sd(sightings))

demog <- demog %>%
  rename(religiosity = pct) %>%
  mutate(
    pop_norm_z = (total_population - mean(total_population)) / sd(total_population),
    rent_norm_z = (median_gross_rent - mean(median_gross_rent)) / sd(median_gross_rent),
    income_norm_z = (median_income - mean(median_income)) / sd(median_income)
  )

levs <- c(
  "pop_norm_z", "income_norm_z", "rent_norm_z", "median_age", "bachelors_or_higher", 
  "black_alone", "white_alone", "hispanic_or_latino", "asian_alone",
  "binge_prev", "religiosity",
  "o", "c", "e", "a", "n"
)

labs <- c(
  "Population (Z-Score)", "Median Income (Z-Score)", "Median Rent (Z-Score)",
  "Median Age", "% Bachelors or higher",
  "% pop. Black-Alone", "% pop. White-Alone", "% pop. Hispanic/Latino", "% pop. Asian",
  "Binge Drinking", "% Religious Important",
  "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism"
)

scatter_dat <- demog %>% 
  select(
    state, 
    sightings,
    contains("_alone"),
    contains("or"), e:o, binge_prev, religiosity, median_age,
    -contains("some_college")
  ) %>% 
  gather(var, val, -state, -sightings) %>% 
  mutate(var = factor(var, levs, labs))

scatter_facet <- scatter_dat %>% 
  ggplot(aes(x = val, y = sightings)) +
  geom_point() +
  facet_wrap(~var, scales = "free") + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "", y = "# Sightings")

scatter_facet

# performs a poisson regression, modeling sightings as a function of 
# state characteristics
# http://stats.stackexchange.com/questions/18480/interpretation-of-log-transformed-predictor
sightings_pois <- glm(
  sightings ~
    pop_norm_z  +
    white_alone +
    black_alone +
    asian_alone +
    hispanic_or_latino +
    rent_norm_z +
    median_age +
    income_norm_z +
    bachelors_or_higher +
    religiosity +
    binge_prev +
    o +
    c +
    e +
    a +
    n
  ,
  data = demog,
  family = poisson
)

roundy <- function(x) round(x, 2)

# http://www.biostat.umn.edu/~dipankar/bmtry711.11/lecture_13.pdf
# http://stats.idre.ucla.edu/stata/output/poisson-regression/
# exponentiating the betas gives you the *incident rate ratio*
# poisson regression coefficients are the differences between
# the log of expected counts, i.e.:
# log(y x+1) - log(y x) or -> log(y x+1 / y x)
#           ^ difference of two logs is equal to their quotient
# IRR: the outcome (y) is multiplied by the beta
sightings_est <- sightings_pois %>%
  tidy() %>%
  bind_cols(confint_tidy(sightings_pois)) %>%
  select(term, estimate, contains("conf")) %>%
  mutate_each(funs(roundy(exp(.))), -term) %>%
  arrange(desc(abs(1 - estimate)))

p_fitted <- augment(sightings_pois, type.predict = "response") %>%
  ggplot(aes(x = .fitted, y = sightings)) +
  geom_abline(slope = 1, intercept = 0, lty = "dashed") +
  geom_point() +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Fitted (Predicted) Value",
    y = "# of Sightings (2013)"
  )

p_fitted

pois_rmse <- yardstick::rmse(p_fitted$data, sightings, .fitted)
pois_mae <- yardstick::mae(p_fitted$data, sightings, .fitted)

tidy(sightings_pois, conf.int = TRUE) %>%
  mutate(
    across(.cols = matches("estimate|conf"), .fns = exp),
    term = factor(term, levs, labs),
    term = fct_reorder(term, 0 - estimate),
    term = fct_rev(term)
  ) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_text(aes(label = ifelse(estimate > 1.1, as.character(round(estimate, 2)), "")), vjust = 1.6) +
  geom_text(aes(label = ifelse(estimate < 0.8, as.character(round(estimate, 2)), "")), vjust = -1.3) +
  coord_flip() +
  theme_minimal(base_size = 15) +
  theme(panel.grid.major.y = element_blank()) +
  labs(x = "", y = "IRR (& 95% CIs)", title = "Estimated IRRs for each predictor")
