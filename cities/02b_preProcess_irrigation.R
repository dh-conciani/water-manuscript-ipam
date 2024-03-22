## dhemerson.costa@ipam.org.br

## avoid sci notations
options(scipen=9e3)

## read libraries
library(reshape2)
library(dplyr)
library(xlsx)
library(sf)

## read table
data <- read.csv('./table/irrigation.csv', sep=',', dec= '.')

## aggregate by year
data <- aggregate(x= list(area= data$area), 
                 by= list(municipality= data$territory,
                          year= data$year),
                 FUN= 'sum')

## translate municipalities
vec <- read_sf('./vec/municipios_cerrado.shp')
vec$CD_MUN <- as.numeric(vec$CD_MUN)

## join
data <- left_join(x= data, y= vec, by=c('municipality' = 'CD_MUN'))
rm(vec)

## select columns
data <- data[,c("NM_MUN","year", "area")]
names(data)[1] <- 'municipality'

## compute changes
## create recipe
irrigation <- as.data.frame(NULL)
## compute the change from start to end per municipality
for(i in 1:length(unique(data$municipality))) {
  print(paste0(i, ' of ', length(unique(data$municipality))))
  
  ## get data
  x <- subset(data, municipality == unique(data$municipality)[i] & year == 1985 | 
                    municipality == unique(data$municipality)[i] & year == 2022)
  
  ## check if start and end exists 
  if (nrow(x) == 2) {
    ## compute aboslute difference
    x$absolute_change <-  c(NA, x$area[2] - x$area[1])
    ## compute relative loss
    x$relative_change <- c(NA, round((x$area[2] - x$area[1]) / x$area[1] * 100, digits=2))
    ## isnert label
    x$class <- 'Irrigation'
    ## bind
    irrigation <- rbind(irrigation, x)
    rm(x)
    next
  }
  
  ## if only start exists, fill end with zero
  if (unique(x$year) == 1985) {
    ## build line 2
    x[2,] <- x; x$year[2] <- 2022; x$area[2] <- 0
    ## compute aboslute difference
    x$absolute_change <-  c(NA, x$area[2] - x$area[1])
    ## compute relative loss
    x$relative_change <- c(NA, round((x$area[2] - x$area[1]) / x$area[1] * 100, digits=2))
    ## isnert label
    x$class <- 'Irrigation'
    ## bind
    irrigation <- rbind(irrigation, x)
    rm(x)
    next
    
  }
  
  if (unique(x$year) == 2022) {
    ## build line 2
    x[2,] <- x; x$year[2] <- 1985; x$area[2] <- 0
    ## compute aboslute difference
    x$absolute_change <-  c(x$area[1] - x$area[2], NA)
    ## compute relative loss
    x$relative_change <- c(round((x$area[1] - x$area[2]) / x$area[2] * 100, digits=2), NA)
    ## isnert label
    x$class <- 'Irrigation'
    ## bind
    irrigation <- rbind(irrigation, x)
    rm(x)
    next
    
  }
  
}

## export as xlsx
write.xlsx(irrigation, file = './output/irrigation.xlsx', sheetName = "IRRIGATION",
           row.names= FALSE, showNA= FALSE)

