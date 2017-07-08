# libraries ----------------------------------------------------------------

library(tidyverse)
library(scales)
library(lubridate)
library(magrittr)
library(plotly)
library(forcats)
library(broom)
library(stringr)
library(glue)
library(leaflet)
library(tidycensus)
library(sf)
library(glue)

# import base data & clean -------------------------------------------------

# we'll look at the 'scrubbed' data, no incomplete reports for now
scrubbed <- read_csv("scrubbed.csv.zip")

# let's look only at US sightings
us <- scrubbed %>%
  filter(country == "us") %>%
  mutate(
    date_time    = mdy_hm(datetime),
    year_sighted = year(date_time),
    date_posted  = as.Date(`date posted`, "%m/%d/%Y"),
    duration     = `duration (seconds)`
  )

us$season <- us %$%
  case_when(
    month(date_time) %in% c(12, 1:2) ~ "winter",
    month(date_time) %in% 3:5        ~ "spring",
    month(date_time) %in% 6:8        ~ "summer",
    TRUE                             ~ "fall"
  ) %>%
  factor(levels = c("fall", "winter", "spring", "summer"))

# 2014 is incomplete
us <- filter(us, year_sighted < 2014)

# median state sightings over time -----------------------------------------

# looking at the most recent 20~ years
by_year_boxplot <- us %>%
  count(year_sighted, state) %>%
  ungroup %>%
  filter(year_sighted > 1990) %>%
  ggplot(aes(x = factor(year_sighted), y = n)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(x = "", y = "# of Sightings", title = "Median Number of State Sightings, 1991-2013<br>(Hover for detail)")

ggplotly(by_year_boxplot)

# shapes that people report seeing -----------------------------------------

# could probably be re-organized as a csv file to import, but here's a lookup table
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

# spread() call with fill = 0 important: it ensures that each year actually gets
# an observation, which would otherwise mess up how the area plot gets rendered
shapes_frame <- us %>%
  mutate(shape_simp = fct_explicit_na(shape_simp, "missing")) %>%
  count(year_sighted, shape_simp) %>%
  spread(shape_simp, n, fill = 0) %>%
  gather(shape_simp, n, -year_sighted) %>%
  mutate(pct  = n / sum(n)) %>%
  arrange(year_sighted, pct) %>%
  filter(year_sighted > 1949)

shapes_frame$shape_simp <- shapes_frame %$%
  fct_reorder(
    factor(shape_simp),
    n,
    .desc = TRUE
  )

shapes <- shapes_frame %>%
  filter(year_sighted != 1995, year_sighted != 1996) %>%
  ggplot(aes(x = year_sighted, y = pct, fill = shape_simp, group = shape_simp)) +
  geom_area(aes(fill = shape_simp, group = shape_simp)) +
  theme_minimal() +
  scale_y_continuous(label = percent) +
  theme(
    panel.grid      = element_blank(),
    legend.title    = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "",
    title = "USA: UFO Shapes by Year, 1950 - 2013",
    caption = "* 1995 & 1996 excluded due to high levels of missing data."
  ) +
  scale_fill_brewer(palette = 16)

shapes

# heatmap by month*year ----------------------------------------------------

# dataset from tidyr
data(population)

uspop <- filter(population, country == "United States of America")

# compute a rate of sightings*month per 1M people
p2 <- us %>%
  count(mo = month(date_time), year_sighted) %>%
  inner_join(uspop, by = c("year_sighted" = "year")) %>%
  mutate(
    Rate = n %>% divide_by(population) %>% multiply_by(1000000),
    Month = factor(mo, levels = 1:12, labels = str_sub(month.name, 1, 3)),
    Year  = factor(year_sighted)
  ) %>%
  ggplot(aes(
    x    = Year,
    y    = Month,
    fill = Rate
  )) +
  geom_tile() +
  theme_minimal() +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "", title = "UFO Sightings in the USA per 1,000,000 People")

ggplotly(p2)

# bivariate choropleth showing density*sightings ---------------------------

# my census API key
source("../census-api-key.R")

# loop over each lat/lon pair in the data and convert it to a st_point() type
# hmm... can't remember why I was forced to set the CRS to this value
us$geometry <- 1:nrow(us) %>%
  lapply(function(x) st_point(c(us$longitude[x], us$latitude[x]))) %>%
  st_sfc(crs = 4269)

us2 <- st_sf(us)

# this is the actual call to the census API
# it downloads tigris files because we've set geometry = TRUE
# coordinate reference system gets updated here again...
countydat <-
  get_acs(
    geography = "county",
    variables = c("B17006_001", "B17006_002"),
    output    = "wide",
    geometry  = TRUE
  ) %>%
  mutate(area = st_area(geometry)) %>%
  st_transform(4269) %>%
  st_join(us2, join = st_contains) # here we match each point into their county

# drop the st class here, then create a count of sightings per county
rates <- countydat %>%
  as.data.frame %>%
  count(`NAME`) %>%
  right_join(
    countydat %>%
      select(NAME, B17006_001E, area, geometry) %>%
      distinct
  ) %>%
  mutate(
    d = B17006_001E / as.numeric(area / 1000000),    # <-- gotta make sure it's km^2
    dm = Hmisc::cut2(d, g = 3, levels.mean = TRUE),  # next we cut up the distribution
    nm = Hmisc::cut2(n, g = 3, levels.mean = TRUE)   # on both vars, will form basis of bivariate comparison
  )

levels(rates$dm) <- 1:3
levels(rates$nm) <- 1:3

rates$bin <- str_c(rates$dm, "-", rates$nm) %>%
factor(levels = c(
"3-1", "2-1", "1-1",  # col 1 -->
"3-2", "2-2", "1-2",  # col 2 -->
"3-3", "2-3", "1-3"   # col 3 -->
))

# from colorbrewer
vals <- c(
"#8c510a", "#bf812d", "#dfc27d", # col 1 -->
"#f6e8c3", "#f5f5f5", "#c7eae5", # col 2 -->
"#80cdc1", "#35978f", "#01665e"  # col 3 -->
)

# set up a palette for leaflet
pal <- colorFactor(palette = rev(vals), rates$bin)

# build the map itself-- another crs transformation (probably because we're dropping the islands)
bv_choro <- rates %>%
  filter(!str_detect(NAME, "Alaska|Hawaii|Puerto")) %>%
  st_sf() %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  leaflet() %>%
  addPolygons(       # uses geometry features to draw counties
    color = "black", # popups will be highlighted and contain data on n of sightings & density
    popup = ~glue_data(., "{NAME}; Sightings: {n}, Density: {round(d, 2)}"),
    stroke = TRUE,
    weight = 1,
    smoothFactor = 0.2,
    fillOpacity = 1,
    fillColor = ~ pal(bin),
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
  )

# no clean way to integrate our special legend into the plot itself
# will build one from scratch using ggplot2
legend_p <- rates %>%
  select(dm, nm, bin) %>%
  distinct %>%
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

bv_choro
legend_p

# -- testing to make sure that the bins are accurately characterized
# rates %>%
#   group_by(bin) %>%
#   arrange(desc(n), desc(d)) %>%
#   slice(1) %>%
#   select(bin, n, d) %>%
#   arrange(bin)

# modeling counts from each state during 2013 ------------------------------

#### census variables first

# no downloading of shapefiles this time
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

# what we'll recode each variable to
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

#### bring it all together

demog <- medians %>%
  left_join(props, by = "NAME") %>%
  rename(state = NAME)

# pull in the other supplemental variables & prepare
binge <- read_csv("2015-binge-prev-cdc.csv")
b5tsc <- read_csv("rentfrow-state-t-scores-b5.csv")
relig <- read_csv("wiki-relig.csv")

relig$region <- str_to_title(relig$region)

binge <- rename(binge, binge_prev = pct)

yr13 <- us %>%
  filter(year_sighted == 2013) %>%
  count(state.abb = str_to_upper(state)) %>%
  rename(sightings = n) %>%
  left_join(data_frame(state.abb, state = state.name), "state.abb")

# modeling frame
demog <- medians %>%
  rename(state = NAME) %>%
  left_join(props, c("state" = "NAME")) %>%
  left_join(b5tsc, "state") %>%
  left_join(binge, "state") %>%
  left_join(relig, c("state" = "region")) %>%
  left_join(yr13, "state") %>%
  filter(!is.na(sightings))

# mean/sd should be close if the distribution is poisson
sight_desc <- summarise(demog, avg = mean(sightings), sd = sd(sightings))

# create some z-scores for easier interpretation
demog <- demog %>%
  rename(religiosity = pct) %>%
  mutate(
    pop_norm_z = (total_population - mean(total_population)) / sd(total_population),
    rent_norm_z = (median_gross_rent - mean(median_gross_rent)) / sd(median_gross_rent),
    income_norm_z = (median_income - mean(median_income)) / sd(median_income)
  )

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
  tidy %>%
  bind_cols(confint_tidy(sightings_pois)) %>%
  select(term, estimate, contains("conf")) %>%
  mutate_each(funs(roundy(exp(.))), -term) %>%
  arrange(desc(abs(1 - estimate)))

#### some scatter plots of each characteristic against state-sightings

# make sure the levels/labels are set up correctly
levs <- c(
  "pop_norm_z", "income_norm_z", "rent_norm_z", "median_age", "bachelors_or_higher",
  "black_alone", "white_alone", "hispanic_or_latino", "asian_alone",
  "binge_prev", "religiosity",
  "o", "c", "e", "a", "n"
)

labs <- c(
  "Population (Z-Score)", "Median Income (Z-Score)", "Median Rent (Z-Score)",
  "Median Age", "% Bachelors or higher",
  "% population Black-Alone", "% population White-Alone", "% population Hispanic/Latino", "% population Asian",
  "Binge Drinking Prevalence", "% Religious Important",
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
  facet_wrap(~var, scales = "free") +  # make sure each plot has independent scales
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +  # appropriate smoother applied
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "", y = "# Sightings")

scatter_facet

#### model fit; frequentist way?

# https://onlinecourses.science.psu.edu/stat501/node/377

# deviance test to check model fit
# https://stats.stackexchange.com/questions/108995/interpreting-residual-and-null-deviance-in-glm-r
with(sightings_pois, 1 - pchisq(null.deviance - deviance, df.null - df.residual))

hist(demog$sightings)

anova(sightings_pois, test = "Chisq")

# model fit & residuals
sightings_fit <- glance(sightings_pois)
plot(sightings_pois)

# may not be normal, but don't have to be to indicate good fit
qqnorm(sightings_pois$resid)
qqline(sightings_pois$resid)

#### interpretation using marginal effects

# compute average marginal effects (AME) with the margins package
# enable us to think about changes in the DV additively
# Leeper's forthcoming paper was really good in explaining interpretation
marginal_fx <- margins::margins(sightings_pois) %>%
  summary %>%
  mutate(
    term = factor(factor, levs, labs),
    term = fct_reorder(term, 100 - abs(AME)),
    term = fct_rev(term)
  ) %>%
  ggplot(aes(x = term, y = AME, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_text(aes(x = term, y = AME, label = ifelse(abs(AME) < 1, "", round(AME, 2))), vjust = 1.3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  labs(
    x = "",
    y = "Change in expected sightings per 1-unit increase",
    title = "Estimated impact of state characteristics on sightings reported",
    caption = ""
  ) +
  coord_flip()

marginal_fx
