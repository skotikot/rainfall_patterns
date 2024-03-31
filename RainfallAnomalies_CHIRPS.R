#Download worldclim rainfall data, calculate seasonal mean, standard deviation and anomalies.

#Load required libraries
library(terra)
library(rnaturalearth)
library(sf)
library(geodata)
library(utils)

#Create folder to store data
dir.create(file.path(getwd(), "data"), showWarnings = FALSE)
outDir <- file.path(getwd(), "data")

#download worldclim historical rainfall data
download.file(url="https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m_prec_1990-1999.zip",
              destfile = paste0(outDir, "/", "wc2.1_cruts4.06_2.5m_prec_1990-1999.zip"), quiet = FALSE)

#unzip the data
inF1 <- list.files(outDir, pattern = "*.zip$", full.names = TRUE )
unzip(inF1, exdir = "./data" )

#get admin boundaries of study area
admin <- ne_countries(type = "countries", scale = "medium") %>%
  st_as_sf() %>% 
  dplyr::filter(sovereignt=="Kenya")

inFiles <- list.files(path=outDir, pattern = "*.tif$", full.names = TRUE)

#read in the files
terraFiles <- terra::rast(inFiles)
dim(terraFiles)

#crop to study area boundary
croppedFiles <- crop(terraFiles, admin)
plot(croppedFiles[[1]])

#create dates sequence for subsetting
dates <- seq(as.Date("1990/1/1"), by = "month", length.out = 120)

#define season of interest -March April, May (MAM)
monthsSub <- format.Date(dates, "%m")== "03" | format.Date(dates, "%m")== "04" | format.Date(dates, "%m")== "05"

#subset the for season of interest
MAMdates <- dates[monthsSub]
stackNames <- as.character(dates[monthsSub])

names(croppedFiles) <- as.character(dates)

ind <- match(stackNames, names(croppedFiles))

croppedFiles_seas <- croppedFiles[[ names(croppedFiles)[ind] ]]

#Create list of file names for saving seasonal sums 
yearrs <- as.numeric(format(MAMdates, format='%Y'))

monNames <- as.character(yearrs)

out_names = paste0("wclim_MAM_",monNames)

monNms <- unique(out_names)

#group the data into season and year
k_rasters = as.list(croppedFiles_seas)

k_split = split(k_rasters, ceiling(seq_along(k_rasters)/3))

#define folder to save output
setwd(file.path(getwd(), "outFolder"))

#Create empty stack to save output files
MAMminT <- terra::rast()


#Loop through each year and calculate total seasonal rainfall.
for (y in 1:length(k_split)){
  year1 <- terra::rast(k_split[[y]])
  
  season_total = terra::app(year1, fun=sum)
  
  MAMminT <- c(MAMminT, season_total)
  
  name_out = monNms[y]

  # writeRaster(season_total,
  #             filename = paste0(name_out, "_AnnSum.tif"),
  #             format="GTiff", overwrite=TRUE)
  
  plot(season_total, main = y+1999)
  print(paste0(name_out, " done!!"))
}

#=====================================seasonal mean
#create seasonal mean map
seasonMean <- terra::app(MAMminT, fun=mean)

plot(seasonMean)

#=====================================Standard deviation
#Create seasonal standard deviation
seasonSD <- terra::app(MAMminT, fun=sd)

plot(seasonSD)

#=====================================Anomalies


