### código para descarga de datos desde DataPortal a través de DDSQLtools - Naciones Unidas

library(tidyverse)
library(DDSQLtools)
library(data.table)
# inspección de 
# DDSQLtools::get_indicators()
# DDSQLtools::get_dataprocess()
# DDSQLtools::get_datatypes()
# DDSQLtools::get_locations()

# all countries to read
LAC_countries <- c(660,28,32,533,44,52,84,68,
                   535,76,92,136,152,170,188,192,
                   531,212,214,218,222,238,254,308,
                   312,320,328,332,340,388,474,484,
                   500,558,591,600,604,630,652,659,
                   662,663,670,534,740,780,796,850,
                   858,862)

# some celade countries only
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

# check
census_LA %>% filter(Pais == "Panamá" & Anio == 2015)

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

# check
all_births_age_LA %>% filter(Pais == "Panama", Anio == 2015) %>% distinct()

# write 
write.csv(all_births_age_LA %>% 
               mutate(Births = trunc(Births)) %>% 
               distinct(), "docs/births_age_LA.csv", row.names = F)

# defunciones -------------------------------------------------------------

# download census data from DemoPortal
options(timeout = 5000)
all_deaths <- map(
     c(591, 76, 152, 32, 218, 188, 170), # some LA_countries, too much data 
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
     group_by(LocID, TimeLabel, SexName, AgeStart) %>%
     filter(DataSourceYear == max(DataSourceYear)) %>%
     filter(IndicatorShortName == min(IndicatorShortName)) %>% 
     arrange(LocID, TimeLabel)

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
