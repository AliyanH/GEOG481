# Libraries needed, can download these from the packages (Install) section on the bottom right section of R studio
library(sf)
library(sp)
library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
library(spatialEco)
library(RStoolbox)
library(readr)

# Directory where your shp and data files are stored
wd = 'Neighbourhoods'
wd1 = 'Shp' 

# loading toronto non-spatial data to be merged with shapefile
data = read_delim('toronto_data.csv', delim=",")
head(data) # checking the first 5 rows of the data
names(data) # getting the column names
spec(data) # info about data

# Loading Toronto Neighbourhoods Shapefile
shp = readOGR(dsn = wd, layer = "Neighbourhoods")
plot(shp, col='green', bg='lightblue',lwd=2)


# Merge Data to one spatial file
new = sp::merge(shp, data, by.x = "FIELD_5", by.y = "ID")
plot(new)

# checking how each column is loaded
sapply(data.frame(new), class)


# Creating an empty raster layer object with same extent as toronto shapefile, also choosing resolution, will have 4001 by 4001 pixels
# This raster will be used to create new rasters based on the variables in the shapefile 
r = raster(ncol=4001, nrow=4001)
extent(r) = extent(new)

# ----- Taking variables from toronto shapefile to rasterize them based on the empty raster created ------ #
# Do this with the variables we need to use

# Covid-19 cases ratio
Cases_Percentage = rasterize(new, r, 'Cases_Percentage_mapped') # Cases_Percentage
plot(Cases_Percentage)
title(main = "Cases Percentage")
#writeRaster(Cases_Percentage, "map/Cases_Percentage.tiff")

# population density percentage
pop_density_corrected = rasterize(new, r, 'pop_density_corrected')
plot(pop_density_corrected)
title(main = "Population Density")
#writeRaster(pop_density_corrected, "map/pop_density_corrected.tiff")

# visible minority percentage
ethnicity_visible_minority_percentage = rasterize(new, r, 'ethnicity_visible_minority_percentage_mapped') # ethnicity_visible_minority_percentage
plot(ethnicity_visible_minority_percentage)
title(main = "Visible Minority")
#writeRaster(ethnicity_visible_minority_percentage, "map/ethnicity_visible_minority_percentage.tiff")

# Median household income
Median_Household_income = rasterize(new, r, 'Median_Household_income_corrected_inverted')
plot(Median_Household_income)
title(main = "Household Income")
#writeRaster(Median_Household_income, "map/Median_Household_income.tiff")

# poverty percentage
Poverty_percentage = rasterize(new, r, 'Poverty_percentage_mapped') # Poverty_percentage
plot(Poverty_percentage)
title(main = "Poverty")
#writeRaster(Poverty_percentage, "map/Poverty_percentage.tiff")

# highschool or below percentage
Edu_highschool_or_below_percentage = rasterize(new, r, 'Edu_highschool_or_below_percentage_mapped') # Edu_highschool_or_below_percentage
plot(Edu_highschool_or_below_percentage)
title(main = "Highschool or below")
#writeRaster(Edu_highschool_or_below_percentage, "map/Edu_highschool_or_below_percentage.tiff")

# average household size
average_household_size_corrected = rasterize(new, r, 'average_household_size_corrected')
plot(average_household_size_corrected)
title(main = "Average Household Size")
#writeRaster(average_household_size_corrected, "map/average_household_size_corrected.tiff")

# apartment
apartments_percentage = rasterize(new, r, 'apartments_percentage_mapped') # apartments_percentage
plot(apartments_percentage)
title(main = "Apartment Percentage")
#writeRaster(apartments_percentage, "map/apartments_percentage.tiff")

#weighted = (Cases_Percentage * 0.4) + (pop_density_corrected * 0.1) + (ethnicity_visible_minority_percentage * 0.1) + (Median_Household_income * 0.05) + (Poverty_percentage * 0.05) + (Edu_highschool_or_below_percentage * 0.1) + (average_household_size_corrected * 0.1) + (apartments_percentage * 0.1)
weighted = (Cases_Percentage * 0.4)+(pop_density_corrected*0.1)+(ethnicity_visible_minority_percentage*0.1)+(Median_Household_income*0.1)+(Poverty_percentage*0.1)+(Edu_highschool_or_below_percentage*0.1)+(average_household_size_corrected*0.05)+(apartments_percentage*0.05)
plot(weighted)
title(main = "COVID-19 INEQUALITY")
#writeRaster(weighted, "map/covid19_inequality.tiff")






### ----------------- MAPPING -------------------- ###


library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(RColorBrewer)

# Color Ramp
colr <- colorRampPalette(brewer.pal(11, 'Oranges'))

# Using a outline boundary that I made in ArcGIS
outline = readOGR(dsn = wd1, layer = "Toronto_Outline1")


levelplot(weighted,
          margin=FALSE, # Margin statistics graphics
          xlab = NULL,
          ylab = NULL,
          main = list("COVID-19 Inequality", font=2, cex=1.75),
          colorkey=list(
            space='bottom',                   # plot legend at bottom
            labels=list(at=0:1, font=4, labels=(c("Low", "High"))),      # legend ticks and labels
            axis.line=list(col='black')
          ),
          par.settings=list(
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=colr,                # Colour Ramp
          at=seq(0, 1, len=101)) +
  layer(sp.polygons(outline, lwd=3))
