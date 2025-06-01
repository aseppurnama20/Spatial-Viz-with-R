library(readxl)
library(dplyr)
library(zoo)
library(sp)
library(sf)
library(gstat)
library(lubridate)
library(viridis)
library(scales)
library(ggplot2)

list_files <- list.files("C:/Default Folder/Downloads/Rainfall - Jakarta", pattern = '.xlsx$', 
                         full.names = T, recursive = T) ##list semua files excel yang akan digunakan di directory penyimpanan
list_files <- data.frame(list_files) ## ubah menjadi dataframe, opsional, hanya untuk mempermudah


raw_data= data.frame(stringsAsFactors = FALSE) ## membuat dataframe kosong untuk menyimpan data
for (i in 1:nrow(list_files)) {
  # i = 1
  print(i)
  files <- list_files$list_files[i] ## select 1 file excel untuk dibukan
  file <- readxl::read_excel(files) ## membaca file excel
  colnames(file)[3] <- 'info' ## mengganti nama kolom ketiga
  rainfall_df <- file[9:39, 1:3] ## select jumlah row yang hanya berisi data utama
  colnames(rainfall_df)[1] <- 'Date' ##
  colnames(rainfall_df)[2] <- 'Temperature'
  colnames(rainfall_df)[3] <- 'Rainfall'
  ## mengambil info2 penting yang dibutuhkan
  rainfall_df$Station <- file$info[1]
  rainfall_df$Station <- gsub('^:?\\s*', '', rainfall_df$Station) ## Menghilangkan karakter tertentu
  rainfall_df$Longitude <- file$info[3]
  rainfall_df$Longitude <- as.numeric(gsub('^:?\\s*', '', rainfall_df$Longitude))
  rainfall_df$Latitude <- file$info[2]
  rainfall_df$Latitude <- as.numeric(gsub('^:?\\s*', '', rainfall_df$Latitude))
  rainfall_df$Elevation <- file$info[4]
  rainfall_df$Elevation <- as.numeric(gsub('^:?\\s*', '', rainfall_df$Elevation))
  rainfall_df <- rainfall_df[c('Station', 'Longitude', 'Latitude', 'Elevation', 'Date', 'Rainfall', 'Temperature')] ## menyusun dataframe
  raw_data<- rbind(raw_data, rainfall_df)
}
## memfilter data yang tidak digunakan berdasarkan kolom tertentu
raw_data <- raw_data[nchar(raw_data$Date) == 10,]
# write.csv(raw_data, "C:/Default Folder/Downloads/Rainfall - Jakarta/Analysis/raw_data.csv")

## list bulan
month_list <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

rainfall <- raw_data[-c(7)]
rainfall$Rainfall <- as.numeric(rainfall$Rainfall)
temperature <- raw_data[-c(6)]
temperature$Temperature <- as.numeric(temperature$Temperature)

## Remove Missing Value
rainfall <- rainfall[!is.na(rainfall$Date),]
rainfall <- as.data.frame(rainfall[!duplicated(rainfall), ])

## Filter data dengan kondisi tertentu / tidak diperlukan
# rainfall$Rainfall <- ifelse(rainfall$Rainfall == 8888 | rainfall$Rainfall == 9999, NA, rainfall$Rainfall)
rainfall <- rainfall[(!is.na(rainfall$Rainfall) & rainfall$Rainfall != 8888 & rainfall$Rainfall != 9999),]


## menggunakan library dplyr untuk merubah data format dari 2 kolom
rainfall <- rainfall %>% mutate(
  Date = as.Date(Date, format = "%d-%m-%Y"),
  Rainfall = as.numeric(Rainfall)
)


rainfall$year_month <- format(rainfall$Date, "%Y-%m")
rainfall$year <- format(rainfall$Date, "%Y")
rainfall$month <- as.numeric(format(rainfall$Date, "%m"))
rainfall$month <- month.abb[rainfall$month]

## Aggregate monthly
monthly_data <- rainfall %>% group_by(Station, Longitude, Latitude, year_month, year, month) %>% 
  summarize(monthly_rainfall = mean(Rainfall, na.rm = T),
            elevation = mean(Elevation, na.rm = T))

monthly_viz <- ggplot(monthly_data[monthly_data$Station == 'Stasiun Klimatologi Banten',], 
                      aes(x = year_month, y = monthly_rainfall, color = Station,  group = Station)) +
  geom_line(linewidth = 1) +
  labs(title = "Monthly Rainfall Time Series", x = "Year", y = "Rainfall (mm)") +
  theme_minimal()

monthly_viz

monthly_data$year_month <- as.Date(paste0(monthly_data$year_month, "-01"))

monthly_viz <- ggplot(monthly_data, 
                      aes(x = year_month, y = monthly_rainfall, color = Station,  group = Station)) +
  geom_line(linewidth = 1) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(title = "Monthly Rainfall Time Series", x = "Year", y = "Rainfall (mm)") +
  theme_minimal()

monthly_viz

## Annual Data
annual_data <- monthly_data %>% group_by(Station, Longitude, Latitude, year, elevation) %>%
  summarize(annual_rainfall = sum(monthly_rainfall, na.rm = T))

a <- rainfall %>% group_by(Station, Longitude, Latitude, year) %>% 
  summarize(monthly_rainfall = sum(Rainfall, na.rm = T),
            elevation = mean(Elevation, na.rm = T))


## Get Average rainfall
avg_rainfall <- monthly_data %>% group_by(Station, Longitude, Latitude, month) %>%
  summarize(avg_rr = mean(monthly_rainfall, na.rm = T),
            elevation = mean(elevation, na.rm = T))

###### SPATIAL ANALYSIS ########
aoi <- st_read("D:/SHP/AOI.shp")
aoi_jkt <- aoi[aoi$WADMPR == 'DKI Jakarta',]
aoi_jkt_buffer <- st_buffer(aoi_jkt, 0.5) %>% st_union() %>% st_sf()

# Create grid within the AOI boundary
grid <- st_make_grid(
  aoi_jkt_buffer,           # Base grid on the geometry
  cellsize = 0.01,   # Adjust resolution as needed
  what = "centers"   # Generate point grid
)

# Convert grid to an sf object
grid_sf <- st_as_sf(grid)

# Assign CRS to grid (match CRS of aoi_jkt)
st_crs(grid_sf) <- st_crs(aoi_jkt)
grid_sf <- st_intersection(grid_sf, aoi_jkt_buffer)

# Convert grid to SpatialPoints object for krige()
grid_sp <- as(grid_sf, "Spatial")

# Ensure monthly data has SpatialPoints and matching CRS
coordinates(avg_rainfall) <- ~Longitude+Latitude
proj4string(avg_rainfall) <- proj4string(grid_sp)  # Align CRS with grid

# Monthly Spatial Interpolation
months <- unique(avg_rainfall$month)
interpolated_monthly <- list()

for (m in 1:length(months)) {
  # m = 1
  # Subset data for the current month
  monthly <- subset(avg_rainfall, month == months[m])
  
  # Perform IDW interpolation
  idw_result <- idw(monthly$avg_rr ~ 1, monthly, grid_sp, idp = 2.0)
  
  # Convert result to data frame and add the month# Convert result to data frame and add the month# Convert result to data frame and add the month
  monthly_result <- as.data.frame(idw_result)
  monthly_result$month <- months[m]
  
  # Store the result in the list
  interpolated_monthly[[as.character(months[m])]] <- monthly_result
}

# Combine all daily interpolations into a single data frame
final_monthly_results <- do.call(rbind, interpolated_monthly)
# final_monthly_results <- left_join(final_monthly_results, monthly_data[c('X', 'Station')], by = 'X')
final_monthly_results <- final_monthly_results %>%
  mutate(month = factor(month, levels = month.abb, ordered = TRUE))

library(ggplot2)
c <- st_as_sf(final_monthly_results, coords = c("coords.x1", "coords.x2"), crs = 4326 )
st_zm(aoi_jkt)
aoi_jkt <- st_zm(aoi_jkt, drop = TRUE) 
aoi_jkt <- sf::st_transform(aoi_jkt, st_crs(c))

## Ploting Point

# Extract unique coordinates from monthly data
point <- as.data.frame(monthly_data[c('Station', 'Longitude', 'Latitude')])
point <- point %>% distinct(Station, Longitude, Latitude)
point_sf <- st_as_sf(point, coords = c('Longitude', 'Latitude'), crs = 4326)

stations_sf <- rainfall %>%
  distinct(Station, Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Ensure CRS of 'aoi' is consistent with 'point'
# aoi <- st_transform(aoi, crs = 4326)
st_geometry_type(aoi)
st_zm(aoi)
aoi <- st_zm(aoi, drop = TRUE) 
aoi <- st_transform(aoi, crs = 4326)

# Check and convert 'X' and 'Y' columns in 'point' to numeric
point$Longitude <- as.numeric(point$Longitude)
point$Latitude <- as.numeric(point$Latitude)

# Plot with points overlaid
plot_point <- ggplot() +
  geom_sf(data = aoi, fill = NA, color = "black") +  # Plot the AOI
  geom_point(data = point, aes(x = Longitude, y = Latitude), color = "blue", size = 3) +
  geom_text(data = point, aes(x = Longitude, y = Latitude, label = Station),
            color = "red", hjust = 0.5, vjust = -1 , size = 3.5) +
  coord_sf() +
  theme_bw() +
  labs(
    title = "Station of Observation (BMKG) Locations"
  )

plot_point

month_list <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
month_labels <- setNames(month_list, unique(final_monthly_results$month))

# Plot the results with a specific boundary
ggplot() +
  geom_raster(data = final_monthly_results, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
  geom_sf(data = aoi_jkt, fill = NA, color = "black", size = 0.5) +
  facet_wrap(~ month) +
  scale_fill_viridis_c(name = "Rainfall (mm)", direction = -1) +
  labs(
    title = "Average Monthly Rainfall 2013 - 2023",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



rf_trend <- ggplot(final_monthly_results, aes(x=month, y=var1.pred, fill=month)) +
  geom_boxplot()+
  scale_x_discrete(labels = month_list) +
  scale_fill_manual(values = final_monthly_results$month,labels = month_list) +
  xlab("Month")+
  ylab("Average Rainfall")+
  ggtitle('Monthly Rainfall in 2023') +
  theme_bw()
rf_trend

elevation_analysis <- monthly_data %>% group_by(Station, elevation) %>%
  summarize(mean_rr = mean(monthly_rainfall, na.rm = T))

# Scatterplot: Rainfall vs Elevation
ggplot(elevation_analysis, aes(x = elevation, y = mean_rr)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Rainfall vs Elevation", x = "Elevation (m)", y = "Mean Rainfall (mm)") +
  theme_minimal()

temperature$Date <- as.Date(temperature$Date, format = "%d-%m-%Y")
temperature <- temperature %>% mutate(year = format(Date, "%Y"))

annual_temp <- temperature %>% group_by(Station, year) %>%
  summarize(avg_temp = mean(Temperature, na.rm = T))

annual <- left_join(annual_data, annual_temp, by=c('Station', 'year'))

max_rainfall <- max(annual$annual_rainfall)
max_temperature <- max(annual$avg_temp)

annual <- annual %>% mutate(scaled_temp = avg_temp * (max_rainfall / max_temperature))

# Combine rainfall and temperature (dual-axis plot)
annual_viz <- ggplot(annual, aes(x = year)) +
  geom_bar(aes(y = annual_rainfall), stat = "identity", fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = scaled_temp, group = Station), color = "darkorange", size = 1) +
  geom_point(aes(y = scaled_temp), color = "darkorange", size = 2) +
  scale_y_continuous(
    name = "Rainfall (mm)",
    sec.axis = sec_axis(~ . / (max_rainfall / max_temperature), name = "Temperature (C)")
  ) +
  labs(title = "Annual Rainfall and Temperature by station", x = "Year") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold")
  ) +
facet_wrap(~Station, scales = "free_y")

annual_viz

### OPSIONAL ###
# Use a single station for demonstration
station_ts <- rainfall %>%
  filter(Station == "Stasiun Meteorologi Kemayoran") %>%
  select(Date, Rainfall) %>%
  arrange(Date)

ts_data <- ts(station_ts$Rainfall, frequency = 365, start = c(2013, 1))
decomposed <- stl(ts_data, s.window = "periodic")
plot(decomposed)
