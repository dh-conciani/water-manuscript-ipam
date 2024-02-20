## compute lulcc changes from first to last decade
## dhemerson.costa@ipam.org.br

## avoid sci notations
options(scipen=9e3)

## read libraries
library(reshape2)
library(dplyr)
library(xlsx)

## read table
data <- read.csv('./table/lulcc.csv', sep=';', dec= ',')

## get only for cerrado
data <- subset(data, biome == 'Cerrado')

# replace NA by zero
data[is.na(data)] <- 0

## melt table
data <- melt(data, id.vars=c('feature_id', 'biome', 'municipality', 'state_acronym', 'geocode',
                          'biome..municipality.', 'class_id', 'level_0', 'level_1', 'level_2',
                          'level_3', 'level_4', 'color', 'category'))

## replace X by ''
data$variable <- as.numeric(gsub('X', '', data$variable))

# Create a new column 'period' based on conditions
data <- data %>%
  mutate(period = case_when(
    between(variable, 1985, 1994) ~ '1985-1994',
    between(variable, 2013, 2022) ~ '2013-2022',
    TRUE ~ 'Mid'
  ))

## get native vegetation 
nat <- subset(data, level_0 == 'Natural' 
              & level_2 != 'River, Lake and Ocean' 
              & level_2 != 'Rocky outcrop' 
              & level_2 != 'Magrove' 
              & level_2 != 'Salt flat' 
              & level_2 != 'Magrove'
              & level_2 != 'Beach and Dune'
              & level_2 != 'Flooded Forest')

## aggregate by year
nat <- aggregate(x= list(area= nat$value), 
            by= list(municipality= nat$municipality,
                     year= nat$variable),
            FUN= 'sum')

## create recipe
lulcc <- as.data.frame(NULL)
## compute the change from start to end per municipality
for(i in 1:length(unique(nat$municipality))) {
  print(paste0(i, ' of ', length(unique(nat$municipality))))
  ## get data
  x <- subset(nat, municipality == unique(nat$municipality)[i] & year == 1985 | 
                   municipality == unique(nat$municipality)[i] & year == 2022)
  
  ## compute aboslute difference
  x$absolute_change <-  c(NA, x$area[2] - x$area[1])
  ## compute relative loss
  x$relative_change <- c(NA, round((x$area[2] - x$area[1]) / x$area[1] * 100, digits=2))
  ## isnert label
  x$class <- 'Native Vegetation'
  ## bind
  lulcc <- rbind(lulcc, x)
}; rm(x)

## Do the same with farming
farming <- subset(data, level_1 == '3. Farming')

## aggregate by year
farming <- aggregate(x= list(area= farming$value), 
                 by= list(municipality= farming$municipality,
                          year= farming$variable),
                 FUN= 'sum')

## compute the change from start to end per municipality
for(i in 1:length(unique(farming$municipality))) {
  print(paste0(i, ' of ', length(unique(farming$municipality))))
  ## get data
  x <- subset(farming, municipality == unique(farming$municipality)[i] & year == 1985 | 
                municipality == unique(farming$municipality)[i] & year == 2022)
  
  ## compute aboslute difference
  x$absolute_change <-  c(NA, x$area[2] - x$area[1])
  ## compute relative loss
  x$relative_change <- c(NA, round((x$area[2] - x$area[1]) / x$area[1] * 100, digits=2))
  ## isnert label
  x$class <- 'Farming'
  ## bind
  lulcc <- rbind(lulcc, x)
}; rm(x)

## export as xlsx
write.xlsx(lulcc, file = './output/lulcc.xlsx', sheetName = "LULCC",
           row.names= FALSE, showNA= FALSE)

?write.xlsx
