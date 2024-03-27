## dhemerson.costa@ipam.org.br

## avoid sci notations
options(scipen=9e3)

## read libraries
library(reshape2)
library(dplyr)
library(xlsx)
library(sf)

## read table
data <- read.csv('./table/lcluc_otto_n4.csv', sep=',', dec= '.')

## define classes for antive vegetation
library(dplyr)

data <- data %>%
  mutate(class_str = case_when(
    class_id %in% c('3', '4', '11', '12') ~ 'Native Vegetation',
    class_id %in% c('15', '18', '19', '39', '20', '40', '62', '41', '36', '46', '47', '35', '48', '9', '21') ~ 'Farming',
    TRUE ~ as.character(class_id) # Handles all other cases by keeping the original class_id as class_str
  ))

## rmeove other classes
data <- subset(data, class_str == 'Native Vegetation' | class_str == 'Farming')

## aggregate by year
data <- aggregate(x= list(area= data$area), 
                 by= list(wts_pk= data$territory,
                          year= data$year,
                          class_str= data$class_str),
                 FUN= 'sum')



## translate municipalities
vec <- read_sf('../vec/bacias_otto_n4.shp')
vec$wts_pk <- as.numeric(vec$wts_pk)

## join
data <- left_join(x= data, y= vec, by=c('wts_pk' = 'wts_pk'))
rm(vec)

## select columns
data <- data[,c("wts_pk","year", "area", "class_str")]
names(data)[1] <- 'wts_pk'

## aggregate by mun
data <- aggregate(x= list(area= data$area), 
                  by= list(wts_pk= data$wts_pk,
                           year= data$year,
                           class_str= data$class_str),
                  FUN= 'sum')



## compute changes
## create recipe
waterSurface <- as.data.frame(NULL)
## compute the change from start to end per wts_pk
for(i in 1:length(unique(data$wts_pk))) {
  print(paste0(i, ' of ', length(unique(data$wts_pk))))
  
  ## get data
  x <- subset(data, wts_pk == unique(data$wts_pk)[i] & year == 1985 | 
                    wts_pk == unique(data$wts_pk)[i] & year == 2022)
  
  
  ## for each class
  for(j in 1:length(unique(x$class_str))) {

    ## subset class
    y <- subset(x, class_str == unique(x$class_str)[j])
    
    ## check if start and end exists 
    if (nrow(y) == 2) {
      ## compute aboslute difference
      y$absolute_change <-  c(NA, y$area[2] - y$area[1])
      ## compute relative loss
      y$relative_change <- c(NA, round((y$area[2] - y$area[1]) / y$area[1] * 100, digits=2))
      ## isnert label
      y$class <- unique(y$class_str)
      ## bind
      waterSurface <- rbind(waterSurface, y)
      rm(y)
      next
    }
    
    ## if only start exists, fill end with zero
    if (unique(y$year) == 1985) {
      ## build line 2
      y[2,] <- y; y$year[2] <- 2022; y$area[2] <- 0
      ## compute aboslute difference
      y$absolute_change <-  c(NA, y$area[2] - y$area[1])
      ## compute relative loss
      y$relative_change <- c(NA, round((y$area[2] - y$area[1]) / y$area[1] * 100, digits=2))
      ## isnert label
      y$class <- unique(y$class_str)
      ## bind
      waterSurface <- rbind(waterSurface, y)
      rm(y)
      next
      
    }
    
    if (unique(y$year) == 2022) {
      ## build line 2
      y[2,] <- y; y$year[2] <- 1985; y$area[2] <- 0
      ## compute aboslute difference
      y$absolute_change <-  c(y$area[1] - y$area[2], NA)
      ## compute relative loss
      y$relative_change <- c(round((y$area[1] - y$area[2]) / y$area[2] * 100, digits=2), NA)
      ## isnert label
      y$class <- unique(y$class_str)
      ## bind
      waterSurface <- rbind(waterSurface, y)
      rm(y)
      next
      
    }
  }
}

## export as xlsx
write.xlsx(waterSurface, file = './output/ottoN4_lulcc.xlsx', sheetName = "LULCC",
           row.names= FALSE, showNA= FALSE)

