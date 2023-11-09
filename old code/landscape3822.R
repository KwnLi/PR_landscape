#install.packages("landscapemetrics")
library(landscapemetrics)
library(sf)
library(terra)
library(tidyverse)
library(ggspatial)

#load landscape data
lf = rast("C:/Users/miljanic/Documents/LF2016_Puerto_Rico_Virgin_Islands_200_IA/LF2016_PRVI_200_IA/LF2016_NVC_200_PRVI/Tif/LV16_NVC_200.tif")
lf
plot(lf)
activeCat(lf)
activeCat(lf) = "GROUP"
activeCat(lf)
plot(lf)

#extract attribute table
lf.table = cats(lf)[[1]]

lf2 = catalyze(lf) #create multiple rasters - one for column each attribute table
plot(lf2)

lf.macrogroup = lf2$MACROGRO_1
plot(lf.macrogroup)

macrgrouplvls = levels(as.factor(lf.table$MACROGRO_1))
#now rerun

macrogroup.reclass = data.frame(class = c(1:length(macrgrouplvls)))

#load farm points
farms <- read.csv("https://drive.google.com/uc?export=download&id=1dvNtK4ZrTTOdCIiYh9DzIgczzSSPP8yK")
head(farms)

farms.sf = st_as_sf(farms, 
                    coords = c(4,3), 
                    crs = st_crs(4326)) %>%
  select(-farmName)
ggplot(farms.sf) + annotation_map_tile(zoomin = -1) + geom_sf() + geom_sf_text(aes(label = siteName), size = 3)

#check landscape to see if we can proceed
check_landscape(lf)

#check what the projected system is
crs(lf)


#project farms.sp to match projected system of lf
farms.pr = st_transform(farms.sf, crs = crs(lf))
farms.pr

# make farms from table into spatial data
farms.sp = as_Spatial(farms.pr)
plot(farms.sp)

#create buffers
farms.buff500 = construct_buffer(farms.sp,"circle", size = 500)

#look at buffers
plot(farms.buff500)

plot(farms.sp)
plot(farms.buff500, add = TRUE) #this plots on top of points

#instead of buffers we can sample shapes
farmsample500 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500, what = "lsm_p_area", plot_id = farms.sp$PESTS_sitename)
farmsample500
#okay so now let's get into landscape metrics
list_lsm()

classarea500 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500, what = "lsm_c_area_mn")
classarea500

#lets try it all together
farmsample500 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500, what = "lsm_p_area", plot_id = farms.sp$PESTS_sitename)
farmsample500

# put all metrics in and see if it will run...would run for 3 metrics  
metrics500 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500, what = c("lsm_p_area", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
metrics500

#yes it takes forever but it will run! 

#metrics for five buffers 
## won't let me just add multiple sizes to the function
a = c(500,1000,2000,5000)
metrics_fun = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = a, what = c("lsm_p_area", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
metrics_allbuff = lapply(c(500,1000,2000,5000), metrics_fun)


#okay i'll do it separately for each buffer radii
metrics1000 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 1000, what = c("lsm_p_area", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
metrics1000

metrics2000 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 2000, what = c("lsm_p_area", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
metrics2000

metrics5000 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 5000, what = c("lsm_p_area", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
metrics5000

#re-run with patch area at landscape level rather than patch level
lmetrics500 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500, what = c("lsm_l_area_mn", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
lmetrics500

lmetrics1000 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 1000, what = c("lsm_l_area_mn", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
lmetrics1000

lmetrics2000 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 2000, what = c("lsm_l_area_mn", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
lmetrics2000

lmetrics5000 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 5000, what = c("lsm_l_area_mn", "lsm_c_ca", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_c_clumpy","lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
lmetrics5000


###scratch notes###


#install.packages("landscapetools")
landscapetools::show_landscape(lf) # this didnt work

show_correlation(lmetrics500, method = "pearson")

metrics = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500, what = c("class", "landscape"))
                    
show_correlation(lmetrics500 %>% dplyr::select(-percentage_inside,-plot_id))

lsmmetrics500 = sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500, what = c("lsm_l_area_mn", "lsm_l_ed", "lsm_l_te", "lsm_l_shape_mn", "lsm_l_contig_mn", "lsm_l_core_mn", "lsm_l_mesh", "lsm_l_division", "lsm_l_shdi", "lsm_l_pr", "lsm_l_shei"), plot_id = farms.sp$PESTS_sitename)
lsmmetrics500


rbind(lsm_p_area(x), lsm_c_area_mn(x))

lmetrics = calculate_lsm((sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500)), what = c("lsm_p_area", "lsm_l_ed", "lsm_c_ca"))

calculate_lsm(sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500, ))

datasets= list(iris, mtcars,npk)
rbind()

for(sample_lsm(landscape = lf, y = farms.sp, shape = "circle", size = 500))
  
                         