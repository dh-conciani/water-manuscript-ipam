## dhemerson.costa@ipam.org.br

## avoid sci notations
options(scipen=9e3)

## read libraries
library(reshape2)
library(dplyr)
library(xlsx)
library(sf)

## read table
data <- read.csv('./table/waterSurface_cities.csv', sep=',', dec= '.')


## remove the false-positve class
data <- subset(data, class_id != 5)

## translate classes
data$class_str <- 
  gsub(1, 'Natural',
     gsub(2, 'Reservatório',
          gsub(3, 'Reservatório',
               gsub(4, 'Mineração',
                    data$class_id))))

## aggregate by year
data <- aggregate(x= list(area= data$area), 
                 by= list(municipality= data$territory,
                          class_str= data$class_str,
                          year= data$year),
                 FUN= 'sum')



## translate municipalities
vec <- read_sf('../vec/municipios_cerrado.shp')
vec$CD_MUN <- as.numeric(vec$CD_MUN)

## join
data <- left_join(x= data, y= vec, by=c('municipality' = 'CD_MUN'))
rm(vec)

## select columns
data <- data[,c("NM_MUN","year", "area", "class_str")]
names(data)[1] <- 'municipality'

## aggregate by mun
data <- aggregate(x= list(area= data$area), 
                  by= list(municipality= data$municipality,
                           class_str= data$class_str,
                           year= data$year),
                  FUN= 'sum')



## compute changes
## create recipe
waterSurface <- as.data.frame(NULL)
## compute the change from start to end per municipality
for(i in 1:length(unique(data$municipality))) {
  print(paste0(i, ' of ', length(unique(data$municipality))))
  
  ## get data
  x <- subset(data, municipality == unique(data$municipality)[i] & year == 1985 | 
                    municipality == unique(data$municipality)[i] & year == 2022)
  
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
write.xlsx(waterSurface, file = './output/municipality_waterSurface.xlsx', sheetName = "SURFACE",
           row.names= FALSE, showNA= FALSE)

