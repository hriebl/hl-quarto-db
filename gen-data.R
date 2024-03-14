library(dplyr)
library(kosisDE)
library(lubridate)
library(readr)
library(tidyr)

jahr <- 2022
datum <- paste0(jahr, "1231")
ewo <- read_rds(paste0("../duva/ewo-", datum, ".rds"))
bew <- kosis_trim(kosis_read("data/bew.txt.xz", "data/bew-satz.xml"))

stadtteile <- tribble(
  ~key, ~value,
  99,   "00 - Lübeck",
  1,    "01 - Innenstadt",
  2,    "02 - St. Jürgen",
  3,    "03 - Moisling",
  4,    "04 - Buntekuh",
  5,    "05 - St. Lorenz Süd",
  6,    "06 - St. Lorenz Nord",
  7,    "07 - St. Gertrud",
  8,    "08 - Schlutup",
  9,    "09 - Kücknitz",
  0,    "10 - Travemünde"
)

# Lübeck gesamt
# =============

# Bestand
# -------

gesamt_ewo <- lapply(
  X = 2010:2022,
  FUN = function(jahr) {
    datum <- paste0(jahr, "1231")
    df <- read_rds(paste0("../duva/ewo-", datum, ".rds"))

    df %>%
      filter(WOHNUNGSSTATUS_DER_PERSON_AN_D == "1") %>%
      mutate(alter = interval(ymd(P01), ymd(datum)) / years(1)) %>%
      summarize(
        jahr               = jahr,
        stadtteil          = 99,
        einwohner          = n(),
        durchschnittsalter = mean(alter, na.rm = TRUE),
        kinder             = sum(alter < 18, na.rm = TRUE),
        erwachsene         = sum(alter >= 18 & alter < 65, na.rm = TRUE),
        aeltere            = sum(alter >= 65, na.rm = TRUE),
      )
  }
)

# Bewegungen
# ----------

gesamt_bew <- bew %>%
  mutate(jahr = as.integer(substr(Z01, 1, 4))) %>%
  filter(W01 == "1", jahr >= 2010, jahr <= 2022) %>%
  summarize(
    stadtteil    = 99,
    geburten     = sum(EA == "01") - sum(EA == "15"),
    sterbefaelle = sum(EA == "11") - sum(EA == "05"),
    zuzuege      = sum(EA == "02") - sum(EA == "16"),
    wegzuege     = sum(EA == "12") - sum(EA == "06") +
                   sum(EA == "13") - sum(EA == "07"),
    .by = jahr
  )

gesamt <- left_join(bind_rows(gesamt_ewo), gesamt_bew)

# Pro Stadtteil
# =============

# Bestand
# -------

teile_ewo <- lapply(
  X = 2010:2022,
  FUN = function(jahr) {
    datum <- paste0(jahr, "1231")
    df <- read_rds(paste0("../duva/ewo-", datum, ".rds"))

    df %>%
      filter(WOHNUNGSSTATUS_DER_PERSON_AN_D == "1") %>%
      mutate(
        alter     = interval(ymd(P01), ymd(datum)) / years(1),
        stadtteil = as.integer(STADTTEIL)
      ) %>%
      summarize(
        jahr               = jahr,
        einwohner          = n(),
        durchschnittsalter = mean(alter, na.rm = TRUE),
        kinder             = sum(alter < 18, na.rm = TRUE),
        erwachsene         = sum(alter >= 18 & alter < 65, na.rm = TRUE),
        aeltere            = sum(alter >= 65, na.rm = TRUE),
        .by = stadtteil
      )
  }
)

# Bewegungen
# ----------

# TODO: Wohnungsstatuswechsel (EA == "24") berücksichtigen,
# aber geringe Fallzahl

teile_bew <- bew %>%
  mutate(
    jahr = as.integer(substr(Z01, 1, 4)),
    r021 = as.integer(substr(R02, 1, 1)),
    rqz1 = as.integer(substr(RQZ, 1, 1))
  ) %>%
  filter(
    W01  == "1",
    jahr >= 2010,
    jahr <= 2022
  ) %>%
  summarize(
    geburten     = sum(EA == "01") - sum(EA == "15"),
    sterbefaelle = sum(EA == "11") - sum(EA == "05"),
    zuzuege      = sum(EA == "02") - sum(EA == "16") +
                   sum(EA == "21" & r021 != rqz1 & B03 == "1", na.rm = TRUE),
    wegzuege     = sum(EA == "12") - sum(EA == "06") +
                   sum(EA == "13") - sum(EA == "07") +
                   sum(EA == "21" & r021 != rqz1 & B03 == "2", na.rm = TRUE),
    .by = c(jahr, r021)
  ) %>%
  rename(stadtteil = r021)

teile <- left_join(bind_rows(teile_ewo), teile_bew)

df <- bind_rows(gesamt, teile) %>%
  filter(!is.na(stadtteil)) %>%
  mutate(stadtteil = factor(stadtteil, stadtteile$key, stadtteile$value)) %>%
  arrange(desc(jahr), stadtteil)

write_csv(df, "data/data.csv")

# Alterspyramide
# ==============

alter <- ewo %>%
  filter(WOHNUNGSSTATUS_DER_PERSON_AN_D == "1") %>%
  mutate(
    alter = interval(ymd(P01), ymd(datum)) / years(1),
    alter = if_else(alter >= 100, 100L, as.integer(alter)),
    geschlecht = if_else(P02 == "1", "maenner", "frauen")
  ) %>%
  summarize(n = n(), .by = c(alter, geschlecht)) %>%
  pivot_wider(names_from = geschlecht, values_from = n) %>%
  relocate(alter, maenner, frauen) %>%
  arrange(alter)

write_csv(alter, "data/alter.csv")
