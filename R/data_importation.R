# Importation d'un fichier atp_matches_* du dépôt tennis_atp ----

## Important : le dépôt tennis_atp est supposé être au même niveau de l'aroboresence que le dépôt dashboard_atp_tour

#### Configuration préliminaire ####
path <- "tennis_atp/" # Chemin relatif vers le dépôt tennis_atp depuis le dépôt dashboard_atp_tour
lst_files <- list.files(path = path) # Liste des fichiers présents
lst_atp_matches <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst_files, value = TRUE) # Liste des noms de fichiers de la forme atp_matches_xyzt
library("dplyr")
library("readr")
# Importation et fusion de tous les fichiers ----

## Importation de tous les fichiers sous forme d'une liste de tibbles

library("purrr") # Pour les fonctions map et reduce
lst_tibs <- map(.x = lst_atp_matches,
                .f = function(fichier) read_csv(file = paste0(path, fichier),
                                                show_col_types = FALSE))
names(lst_tibs) <- stringr::str_sub(lst_atp_matches, start = 1L, end = -5L)



#### Correction de la fusion des seed et entry en 2022 ####

lst_tibs[[55]] %>%
  rowwise() %>%
  mutate(winner_entry = case_when(is.na(winner_seed) ~ NA_character_,
                                  !grepl(pattern = "[[:digit:]]+", x = winner_seed) ~ winner_seed,
                                  TRUE ~ winner_entry),
         winner_seed = case_when(is.na(winner_seed) ~ NA_real_,
                                 !grepl(pattern = "[[:digit:]]+", x = winner_seed) ~ NA_real_,
                                 TRUE ~ as.numeric(winner_seed)),
         loser_entry = case_when(is.na(loser_seed) ~ NA_character_,
                                  !grepl(pattern = "[[:digit:]]+", x = loser_seed) ~ loser_seed,
                                  TRUE ~ loser_entry),
         loser_seed = case_when(is.na(loser_seed) ~ NA_real_,
                                 !grepl(pattern = "[[:digit:]]+", x = loser_seed) ~ NA_real_,
                                 TRUE ~ as.numeric(loser_seed))) %>%
  ungroup() %>%
  identity() -> lst_tibs[[55]]


#### Uniformisation des types des colonnes ####

lst_libs <- map(.x = lst_tibs,
                .f = function(tib) mutate(tib, across(.cols = where(~ is.logical(.x)),
                                                      .fns = ~ as.double(.x))))

#### Fusion des fichiers en une seule base ####
atp <- reduce(.x = lst_tibs,
              .f = bind_rows)

# Ajout de l'année ----

library("magrittr")
atp %<>% 
  mutate(year = stringr::str_sub(tourney_id, start=1L, end=4L)) %>%
  # select(year) %>%
  # pull() %>%
  # unique() %>%
  identity()

# Ajout d'un flag pour la catégorie de tournoi
atp %<>%
  mutate(tourney_type = case_when(grepl(pattern = "^Davis", x = tourney_name) ~ "Davis",
                                  grepl(x = tourney_name, pattern = "Olympics") ~ "Olympics",
                                  tourney_name %in% c("Masters Cup", "Tour Finals") ~ "Masters",
                                  tourney_name == "Laver Cup" ~ "Laver",
                                  TRUE ~ "ATP"))
# Transformation de la variable round en facteur ordonnée
atp %<>% mutate(round = factor(round, levels = c("RR", "R128", "R64", "R32", "R16", "QF", "SF", "BR", "F"),
                               ordered = TRUE))

# Extraction des informations des tournois ----

atp %>% 
  mutate(year = stringr::str_sub(tourney_id, start=1L, end=4L)) %>%
  select(year, tourney_id, tourney_name, tourney_level, tourney_date, tourney_type, surface, draw_size) %>%
  distinct() -> atp_tourneys

# Removing all temporary variables ----

rm("lst_libs", "lst_tibs")


# Uniformisation noms de tournois

atp%>%
  mutate(year=stringr::str_sub(tourney_id, start=1L, end=4L))%>%
  mutate(tourney_date = lubridate::ymd(tourney_date)) %>%
  arrange(tourney_date) %>%
  mutate(tourney_name=case_when(tourney_name=="Us Open" ~ "US Open",
                                tourney_name=="Australian Open-2" ~ "Australian Open",
                                tourney_name=="Indian Wells" ~ "Indian Wells Masters",
                                tourney_name=="Monte Carlo" ~ "Monte Carlo Masters",
                                tourney_name =="Los Cabos" ~ "Cabo san Lucas",
                                tourney_name == "Madrid Masters" ~ "Madrid Open",
                                tourney_name == "New York" ~ "New York Open",
                                tourney_name == "NextGen Finals" ~ "Next Generation Finals",
                                tourney_name == "Tour Finals" ~ "ATP Finals",
                                tourney_name %in% c("Miami", "Miami WCT", "Boca Raton", "Key Biscayne", "Boca West") ~ " Miami Masters",
                                tourney_name %in% c("Adelaide", "Adelaide 1", "Adelaide 2") ~ "Adelaide",
                                tourney_name %in% c ("Belgrade", "Belgrade 2") ~ "Serbia Open",
                                tourney_name %in% c ("Marrakech","Casablanca") ~ "Morocco Open",
                                tourney_name %in% c ("Cologne 1", "Cologne 2") ~ "Cologne",
                                tourney_name %in% c("Kuala Lumpur") ~ "Malaysia Open",
                                tourney_name %in% c("Vina del Mar","Santiago") ~ "Chile Open",
                                tourney_name %in% c("St. Petersburg","St Petersburg") ~ "St Petersburg",
                                tourney_name %in% c("Muray River Open","Great Ocean Road Open", "Melbourne") ~ "Melbourne",
                                TRUE ~ tourney_name))->atp
         


# Selectionner uniquement les tournois existants depuis 2011
atp %>%
  filter(year >= 2011) %>%
  group_by(tourney_name) %>%
  dplyr::summarize(unique(tourney_name)) %>%
  select(tourney_name) -> tour_2011

# Retirer coupe Davis, Jeux Olympiques et Laver Cup
atp %>%
  filter(!grepl(pattern = "^Davis", x = tourney_name)) %>%
  filter(tourney_name %in% tour_2011$tourney_name) -> atp

atp %>%
  filter(!grepl(pattern = "Olympics$", x = tourney_name)) %>%
  filter(tourney_name %in% tour_2011$tourney_name) -> atp

atp %>%
  filter(!grepl(pattern = "^Laver", x = tourney_name)) %>%
  filter(tourney_name %in% tour_2011$tourney_name) -> atp


 #Nombre de tournois conservés
#n_distinct(atp$tourney_name)#= 104

#Liste des tournois conservés
atp %>%
  select(tourney_name)%>%
  group_by(tourney_name)%>%
  count()%>%
print(n=105)


# Création de la variable nationality pour loser et winner

## Tableau des country code 
atp%>% 
select(winner_ioc)%>%
  group_by(winner_ioc)%>%
  count(name="nombre") -> winner_countrycode


atp%>% 
  select(loser_ioc)%>%
  group_by(loser_ioc)%>%
  count(name="nombre") -> loser_countrycode

## On extrait les variables winner_ioc et loser_ioc sous forme de vecteur

vec1 <- winner_countrycode$winner_ioc
vec2 <- loser_countrycode$loser_ioc

## Conversion du country code en country name


library("countrycode")#Pour la fonction countrycode

countrycode(vec1, origin = 'iso3c', destination = 'country.name', 
            custom_match= c('ALG'='Algeria', 'BAH'='Bahamas','BAR'='Barbados', 'BUL'='Bulgarian', 'CHI'='Chile', 'CRC'='Costa Rica', 'CRO'='Croatia',
                            'DEN'='Denmark', 'ESA'='El Salvador', 'GER'='Germany','GRE'='Greece', 'INA'='Indonesia',
                            'IRI'='Iran', 'KUW'='Kuwait','LAT'='Latvia','MAS'='Malaysia', 'MON'='Monaco','NED'='Netherlands',
                            'NGR'='Nigeria', 'PAR'='Paraguay','PHI'='Philippines','POR'='Portugal','PUR'='Puerto Rico',
                            'RSA'='South Africa','SLO'='Slovenia','SUI'='Switzerland','TCH'='Czechoslovakia',
                            'TPE'='Taiwan', 'UNK'='UNK', 'URS'='Soviet Union', 'URU'='Uruguay','YUG'='Yugoslavia',
                            'ZIM'='Zimbabwe', 'NA'='NA')) -> vec1bis

countrycode(vec2, origin = 'iso3c', destination = 'country.name', 
            custom_match= c('ALG'='Algeria', 'BAH'='Bahamas','BAR'='Barbados', 'BUL'='Bulgarian', 'CHI'='Chile', 'CRC'='Costa Rica', 'CRO'='Croatia',
                            'DEN'='Denmark', 'ESA'='El Salvador', 'GER'='Germany','GRE'='Greece', 'INA'='Indonesia',
                            'IRI'='Iran', 'KUW'='Kuwait','LAT'='Latvia','MAS'='Malaysia', 'MON'='Monaco','NED'='Netherlands',
                            'NGR'='Nigeria', 'PAR'='Paraguay','PHI'='Philippines','POR'='Portugal','PUR'='Puerto Rico',
                            'RSA'='South Africa','SLO'='Slovenia','SUI'='Switzerland','TCH'='Czechoslovakia',
                            'TPE'='Taiwan', 'UNK'='UNK', 'URS'='Soviet Union', 'URU'='Uruguay','YUG'='Yugoslavia',
                            'ZIM'='Zimbabwe', 'NA'='NA', 'HAI'='Haiti', 'RHO'='Southern Rhodesia', 'UAE'='United Arab Emerirates')) -> vec2bis


## On ajoute la variable nationality au tableau des country_code

winner_countrycode$winner_nationality = vec1bis
loser_countrycode$loser_nationality = vec2bis

## Jointure des bases country_code comportant les nationality des joueurs avec la base de données atp
atp %>% left_join(winner_countrycode,by="winner_ioc") -> atp
atp <- subset( atp, select = -nombre )


atp %>% left_join(loser_countrycode,by="loser_ioc") -> atp
atp <- subset( atp, select = -nombre )


# Removing all temporary variables ----
rm("atp_tourneys","loser_countrycode","winner_countrycode","tour_2011","vec1","vec2","vec2bis","vec1bis","path","lst_files","lst_atp_matches")


