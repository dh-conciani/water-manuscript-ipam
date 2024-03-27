## dhemerson.costa@ipam.org.br

## avoid sci notations
options(scipen=9e3)

## read libraries
library(reshape2)
library(dplyr)
library(xlsx)
library(sf)

## read table
data <- read.csv('./table/irrigated_agriculture_otto_n4.csv', sep=',', dec= '.')

## aggregate by year
data <- aggregate(x= list(area= data$area), 
                 by= list(wts_pk= data$territory,
                          year= data$year),
                 FUN= 'sum')



## translate municipalities
vec <- read_sf('../vec/bacias_otto_n4.shp')
vec$wts_pk <- as.numeric(vec$wts_pk)

## join
data <- left_join(x= data, y= vec, by=c('wts_pk' = 'wts_pk'))
rm(vec)

## select columns
data <- data[,c("wts_pk","year", "area")]
names(data)[1] <- 'wts_pk'

## aggregate by mun
data <- aggregate(x= list(area= data$area), 
                  by= list(wts_pk= data$wts_pk,
                           year= data$year),
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
  

    ## subset class
    y <- x
    
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

## export as xlsx
write.xlsx(waterSurface, file = './output/ottoN4_irrigation.xlsx', sheetName = "IRRIGATION",
           row.names= FALSE, showNA= FALSE)

