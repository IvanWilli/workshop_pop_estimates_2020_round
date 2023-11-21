library(tidyverse)
library(DDSQLtools)
library(data.table)
DDSQLtools::get_indicators()
DDSQLtools::get_dataprocess()
DDSQLtools::get_datatypes()
DDSQLtools::get_locations()

# all countries to read
LAC_countries <- c(660,28,32,533,44,52,84,68,
                  535,76,92,136,152,170,188,192,
                  531,212,214,218,222,238,254,308,
                  312,320,328,332,340,388,474,484,
                  500,558,591,600,604,630,652,659,
                  662,663,670,534,740,780,796,850,
                  858,862)
LA_CELADE_countries <- c(32,68,76,152,170,188,192,214,
                         218,222,320,332,340,388,484,558,
                         591,600,604,858,862)


# census ------------------------------------------------------------------

# download census data from DemoPortal
options(timeout = 5000)
all_censuses <- map(
     LA_CELADE_countries,
     function(X){
          print(X)
          tryCatch(
               {DT = get_recorddataadditional(
                    dataProcessTypeIds = 2,    ## Census 
                    startYear = 1945,
                    endYear = 2022,
                    indicatorIds = c(58, 60), # abridged or complete
                    locIds = X,            
                    locAreaTypeIds = 2,     ## "Whole area"
                    subGroupIds = 2,        ## "Total or All groups"
                    includeUncertainty = FALSE,
                    collapse_id_name = FALSE)},      
               error = function(e) {})
     }) 
all_censuses_LA <- rbindlist(all_censuses) %>% filter(SexName != "Both sexes")
save(all_censuses_LA, file = "docs/all_censuses_LA.rda")

# filter only raw and first complete
load("docs/all_censuses_LA.rda")
census_LA <- all_censuses_LA %>% 
     filter(LocID %in% LA_CELADE_countries,
            DataTypeName == "Population by age and sex",
            DataSourceShortName %in% c("DYB", "CELADE"),
            AgeLabel != "Total") %>% 
     select(LocID, LocName, 
            DataTypeName, DataSourceShortName,
            IndicatorShortName, StatisticalConceptName,
            TimeStart, StatisticalConceptName,
            TimeLabel,TimeMid,
            SexName,
            AgeStart, AgeSpan, AgeLabel, 
            DataValue)%>% 
     mutate(complete = ifelse(IndicatorShortName == "Population1", 1, 0),
            celade_dyb = ifelse(DataSourceShortName == "CELADE", 1, 0),
            score = complete + celade_dyb) %>% 
     group_by(LocID, LocName, 
              TimeLabel,
              SexName) %>% 
     arrange(-score) %>% 
     filter(score == max(score)) %>% ungroup %>% 
     arrange(LocID, SexName, AgeStart)

# rename for spanish
census_LA <- census_LA %>% 
     select(LocID, 
            Pais = LocName,
            Anio = TimeLabel, 
            Anio_medio = TimeMid,
            Tipo = StatisticalConceptName,
            Fecha = TimeStart,
            Sexo = SexName,
            Edad = AgeStart,
            Edad_interv = AgeSpan,
            Edad_grupo = AgeLabel,
            Pop = DataValue) %>% 
     mutate(Fecha = substr(Fecha, 1, 10),
            Edad = case_when(Edad == -2 ~ NA, T ~ Edad),
            Edad_interv = case_when(Edad_interv == -2 ~ NA, T ~ Edad_interv),
            Sexo = ifelse(Sexo == "Female", "Mujer", "Hombre"))

# add panama, brazil and ecuador
censos_ronda_2020 <- readxl::read_xlsx("docs/censos_ronda_2020.xlsx")
census_LA <- bind_rows(
     census_LA,
     censos_ronda_2020 %>% 
          filter(Pais == "Panama") %>% 
          mutate(LocID = 591, Pais = "Panama", 
                    Anio = 2023, Anio_medio = 2023.5,
                    Tipo = "De-jure", Fecha = "01/02/2023",
                    Edad_interv = 1, Edad_grupo = as.character(Edad)),
     censos_ronda_2020 %>% 
          filter(Pais == "Brazil") %>% 
          mutate(LocID = 76, Pais = "Brazil", 
                 Anio = 2022, Anio_medio = 2022.5,
                 Tipo = "De-jure", Fecha = "01/08/2022",
                 Edad_interv = 1, Edad_grupo = as.character(Edad)),
     censos_ronda_2020 %>% 
          filter(Pais == "Ecuador") %>% 
          mutate(LocID = 76, Pais = "Ecuador", 
                 Anio = 2022, Anio_medio = 2022.5,
                 Tipo = "De-jure", Fecha = "01/08/2022",
                 Edad_interv = 1, Edad_grupo = as.character(Edad))) %>% 
     arrange(Pais, Anio, Sexo, Edad)

# panama 2010 is De-facto
census_LA <- census_LA %>% 
     mutate(Tipo = ifelse(Pais == "Panama" & Anio == 2010, "De-facto", Tipo))

# check panama
census_LA %>% filter(Pais == "Panama", Anio == 2023) %>% as.data.frame()

# write
write.csv(census_LA, "docs/census_LA.csv", row.names = F)

# births by mother age ----------------------------------------------------

options(timeout = 5000)
all_births_age <- map(
     LA_CELADE_countries,
     function(X){
          print(X)
          tryCatch(
               {DT = get_recorddataadditional(
                    dataProcessTypeIds = 9,    ## VR 
                    startYear = 1945,
                    endYear = 2022,
                    indicatorIds = 170,
                    locIds = X,            
                    locAreaTypeIds = 2,     ## "Whole area"
                    subGroupIds = 2,        ## "Total or All groups"
                    includeUncertainty = FALSE,
                    collapse_id_name = FALSE)},      
               error = function(e) {})
     }) 
all_births_age_LA <- rbindlist(all_births_age)
save(all_births_age_LA, file = "docs/all_births_age_LA.rda")

# make spanish
load("docs/all_births_age_LA.rda")
all_births_age_LA <- all_births_age_LA %>% 
     filter(SexID != 3, AgeLabel != "Total") %>% 
     select(LocID, 
            Pais = LocName,
            Anio = TimeLabel, 
            Anio_medio = TimeMid,
            Tipo = StatisticalConceptName,
            Sexo = SexName, 
            Edad = AgeStart,
            Edad_interv = AgeSpan,
            Edad_grupo = AgeLabel,
            Births = DataValue) %>%
     mutate(Edad = case_when(Edad == -2 ~ NA, T ~ Edad),
            Edad_interv = case_when(Edad_interv == -2 ~ NA, T ~ Edad_interv),
            Sexo = ifelse(Sexo == "Female", "Mujer", "Hombre")) %>% 
     arrange(Pais, Anio, Edad)

# add Panama
panama_births <- readxl::read_xlsx("docs/panama_Nacimientos y Defunciones por edad 2022.xlsx", 
                                   sheet = "eventos") %>% 
     filter(Evento == "Nacimientos", Edad > 0) %>% 
     rename(Births = Cantidad) %>% 
     mutate(LocID = 591, Pais = "Panama", Tipo = "Year of occurrence", Anio = 2022, Anio_medio = 2022.5) %>% 
     select(-Evento)
all_births_age_LA <- bind_rows(all_births_age_LA, panama_births)

# write 
write.csv(all_births_age_LA, "docs/births_age_LA.csv", row.names = F)

# defunciones -------------------------------------------------------------

# download census data from DemoPortal
options(timeout = 5000)
all_deaths <- map(
     c(591, 76, 152, 32), # LA_countries, 
     function(X){
          print(X)
          tryCatch(
               {DT = get_recorddataadditional(
                    dataProcessTypeIds = 9,    ## VR 
                    startYear = 2000,
                    endYear = 2022,
                    indicatorIds = c(194, 195),
                    locIds = X,            
                    locAreaTypeIds = 2,     ## "Whole area"
                    subGroupIds = 2,        ## "Total or All groups"
                    includeUncertainty = FALSE,
                    collapse_id_name = FALSE)},      
               error = function(e) {})
     }) 
all_deaths_LA <- rbindlist(all_deaths) %>% filter(SexName != "Both sexes")
save(all_deaths_LA, file = "docs/all_deaths_LA.rda")

# filter only raw and first complete
load("docs/all_deaths_LA.rda")
deaths_LA <- all_deaths_LA %>% 
     filter(DataSourceShortName %in% c("DYB"), AgeLabel != "Total") %>% 
     select(LocID, LocName, 
            DataTypeName, DataSourceShortName,
            IndicatorShortName, StatisticalConceptName,
            TimeStart, StatisticalConceptName,DataSourceYear,
            TimeLabel,TimeMid,
            SexName,
            AgeStart, AgeSpan, AgeLabel, 
            DataValue)%>%
     group_by(LocID, TimeLabel, SexName) %>%
     filter(DataSourceYear == max(DataSourceYear)) %>%
     filter(IndicatorShortName == min(IndicatorShortName)) %>% 
     arrange(LocID, TimeLabel)
deaths_LA %>% 
     filter(LocName == "Panama", SexName == "Female") %>% arrange(TimeLabel, AgeStart)  %>% as.data.frame() %>% View

# rename for spanish
deaths_LA <- deaths_LA %>% 
     select(LocID, 
            Pais = LocName,
            Anio = TimeLabel, 
            Anio_medio = TimeMid,
            Tipo = StatisticalConceptName,
            Edad = AgeStart,
            Sexo = SexName,
            Edad_interv = AgeSpan,
            Edad_grupo = AgeLabel,
            Deaths = DataValue) %>%
     mutate(Edad = case_when(Edad == -2 ~ NA, T ~ Edad),
            Edad_interv = case_when(Edad_interv == -2 ~ NA, T ~ Edad_interv),
            Sexo = ifelse(Sexo == "Female", "Mujer", "Hombre")) %>% 
     arrange(Pais, Anio, Sexo, Edad)

# add panama deaths
panama_deaths <- readxl::read_xlsx("docs/panama_Nacimientos y Defunciones por edad 2022.xlsx", 
                                   sheet = "eventos") %>% 
     filter(Evento == "Defunciones") %>% rename(Deaths = Cantidad) %>% 
     mutate(LocID = 591, Pais = "Panama", Tipo = "Year of occurrence", Anio = "2022", Anio_medio = 2022.5) %>% 
     select(-Evento)
deaths_LA <- bind_rows(deaths_LA, panama_deaths)

# write
write.csv(deaths_LA, "docs/deaths_LA.csv", row.names = F)

# all data together-------------------------------------------------------------------------

census_LA <- read.csv("docs/census_LA.csv") %>% 
     rename(País = Pais, Año = Anio, Año_medio = Anio_medio) %>% 
     distinct()
# births_LA <- "docs/births_LA.csv"
births_LA <- read.csv("docs/births_age_LA.csv") %>% 
     rename(País = Pais, Año = Anio, Año_medio = Anio_medio) %>% 
     arrange(País, Año, Sexo, Edad) %>% 
     distinct()
deaths_LA <- read.csv("docs/deaths_LA.csv") %>% 
     rename(País = Pais, Año = Anio, Año_medio = Anio_medio) %>% 
     arrange(País, Año, Sexo, Edad) %>% 
     distinct()
save(census_LA, births_LA, deaths_LA, file = "docs/data_LA.rda")


# otros no usados ---------------------------------------------------------
# 5y age group wpp population

base_url <- 'https://population.un.org/dataportalapi/api/v1'
target <- paste0(base_url,'/indicators/?format=csv')
codes <- read.csv(target, sep='|', skip=1) 
all_wpp_pop <- map_df(LA_CELADE_countries, 
                      function(my_location){
                           print(my_location)
                           read.csv(paste0(base_url,
                                           '/data/indicators/',46,
                                           '/locations/',my_location,
                                           '/start/',2010,
                                           '/end/',2022,
                                           '/?format=csv'), 
                                    sep='|', skip=1)})
save(all_wpp_pop, file = "all_wpp_pop.rda")

# filter median
load("all_wpp_pop.rda")
all_wpp_pop <- all_wpp_pop %>%
     filter(VariantLabel == "Median", Sex != "Both sexes") %>% 
     select(Pais = Location, Anio = TimeLabel, Edad = AgeStart, Sexo = Sex, Pop = Value) %>% 
     mutate(Sexo = ifelse(Sexo == "Male", "Hombre", "Mujer"))

# write
write.csv(all_wpp_pop, "docs/wpp22_pop5_LA.csv", row.names = F)


# total births

options(timeout = 5000)
all_births <- map(
     LA_countries,
     function(X){
          print(X)
          tryCatch(
               {DT = get_recorddataadditional(
                    dataProcessTypeIds = 9,    ## VR 
                    startYear = 1945,
                    endYear = 2022,
                    indicatorIds = 159,
                    locIds = X,            
                    locAreaTypeIds = 2,     ## "Whole area"
                    subGroupIds = 2,        ## "Total or All groups"
                    includeUncertainty = FALSE,
                    collapse_id_name = FALSE)},      
               error = function(e) {})
     }) 
all_births_LA <- rbindlist(all_births) %>% filter(SexName == "Both sexes")
save(all_births_LA, file = "docs/all_births_LA.rda")

# filter only raw and first complete
load("docs/all_births_LA.rda")
births_LA <- all_births_LA %>% 
     filter(DataSourceShortName %in% c("DYB")) %>% 
     select(LocID, LocName, 
            DataTypeName, DataSourceShortName,
            IndicatorShortName, StatisticalConceptName,
            TimeStart, StatisticalConceptName,DataSourceYear,
            TimeLabel,TimeMid,
            SexName,
            AgeStart, AgeSpan, AgeLabel, 
            DataValue)%>%
     group_by(LocID, TimeLabel) %>% arrange(-DataSourceYear) %>% slice(1) %>% 
     arrange(LocID, TimeLabel)

# rename for spanish
births_LA <- births_LA %>% 
     select(LocID, 
            Pais = LocName,
            Anio = TimeLabel, 
            Anio_medio = TimeMid,
            Tipo = StatisticalConceptName,
            Births = DataValue)

# add panama nacs
panama_births <- readxl::read_xlsx("docs/panama_Nacimientos y Defunciones por edad 2022.xlsx", 
                                   sheet = "eventos") %>% 
     filter(Evento == "Nacimientos") %>% 
     rename(Births = Cantidad) %>%  
     mutate(LocID = 591, Pais = "Panama", Tipo = "Year of occurrence", Anio = 2022, Anio_medio = 2022.5) %>% 
     select(-Evento, -Edad, -Edad_interv, -Edad_grupo)
births_LA <- bind_rows(births_LA, 
                       panama_births %>% slice(1) %>% 
                            mutate(Births = sum(panama_births$Births)) %>% 
                            select(-Sexo)) 

# write
write.csv(births_LA, "docs/births_LA.csv", row.names = F)
