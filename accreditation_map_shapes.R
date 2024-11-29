library(ggmap)
library(readxl)
library(dplyr)
library(ggthemes)
library(sf)
library(colorspace)
library(openxlsx)

scriptlocation <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptlocation)

# bring in data
df <- read_excel("241105 Endoscopy Stocktake Analysis + JAG Census.xlsx", 
                 sheet = "JAG_Plus_Survey_Map_Data")

AdditionalRoomsData <- read_excel("241105 Endoscopy Stocktake Analysis + JAG Census.xlsx", 
                                  sheet = "Endoscopy Estate (Slide5)", range="D2:I27")

AdditionalRooms <- AdditionalRoomsData %>% 
  select(1,6) %>% 
  rename(Location = 1) %>% 
  rename(ExtraRooms = 2) %>% 
  mutate(Location = recode(Location,
                           'BUCKINGHAMSHIRE, OXFORDSHIRE AND BERKSHIRE WEST ICB' = 'NHS Buckinghamshire, Oxfordshire and Berkshire West ICB',
                           'FRIMLEY ICB' = 'NHS Frimley ICB',
                           'HAMPSHIRE AND ISLE OF WIGHT ICB' = 'NHS Hampshire and Isle of Wight ICB',
                           'KENT AND MEDWAY ICB' = 'NHS Kent and Medway ICB',
                           'SURREY HEARTLANDS ICB' = 'NHS Surrey Heartlands ICB',
                           'SUSSEX ICB' = 'NHS Sussex ICB'))


AdditionalRooms$ExtraRooms <- as.numeric(AdditionalRooms$ExtraRooms)


AdditionalRooms$ExtraRooms <- round(AdditionalRooms$ExtraRooms,2)

AdditionalRooms <- AdditionalRooms[grep("ICB$", AdditionalRooms$Location), ]

# bring in postcodes lat lon lookup table
ukpostcodes <- read.csv("ukpostcodes.csv")

question_filter <- c("What is the units postcode",
                     "What is the units JAG accreditation status",
                     "How many endoscopy rooms are in use at this unit? include any that are only used from time to time")

# filter for what I need
JAG_status <- df %>%
  filter(Question %in% question_filter) %>%
  subset(select = c('Unit Name', 'Numerical Answer','Set response answer', 'Free comment'))

# change col names to something less difficult
colnames(JAG_status) <- c("name", "num_endo_rooms", "accreditation_status", "postcode")

JAG_status$accreditation_status <- ifelse(grepl("ccred", JAG_status$accreditation_status) == TRUE, JAG_status$accreditation_status, NA) # getting rid of anything not re accreditation status

# set the data out properly
JAG_status <- JAG_status %>% 
  group_by(name) %>%
  reframe(num_endo_rooms = as.numeric(last(num_endo_rooms)),
            accreditation_status = accreditation_status[!is.na(accreditation_status)], # remove NAs from list to bring into one row per name
            postcode = first(postcode))

# left join data with postcodes to get lat and lon
JAG_status <- left_join(JAG_status, ukpostcodes, by = 'postcode')

JAG_status$endo_room_character <- as.character(JAG_status$num_endo_rooms)


rm(ukpostcodes, df)
# register API key at https://client.stadiamaps.com/dashboard/ and enter here
register_stadiamaps(key = "c5c80bbf-69e1-422a-a060-24bf4322e9e1")

# create map for region (tip: use OpenStreetMap to get co-ordinates for bbox)
south_east_map <- get_stadiamap(
  bbox = c(left = -2, bottom = 50.5, right = 1.6, top = 52.3),
  maptype = "outdoors",
  zoom = 9
)
south_east_map

# get ICB boundaries GeoJSON
# https://geoportal.statistics.gov.uk/datasets/76dad7f9577147b2b636d4f95345d28d_0/explore
# File name = Integrated Care Boards (April 2023) EN BSC
icb_data <- sf::read_sf("Integrated_Care_Boards_April_2023_EN_BSC.geojson")

se_codes <- subset(icb_data, FID %in% c(18, 19, 26, 28, 41, 42))

# rename ICB names for joining, keep South East
icb_boundaries <- se_codes %>% 
  mutate(icb_name = stringr::str_replace(ICB23NM, "Integrated Care Board", "ICB")) %>% 
  select(c(icb_name, LONG, LAT, geometry)) %>% 
  rename(c(lon = LONG, lat = LAT)) %>% 
  left_join(AdditionalRooms, by = c("icb_name" = "Location"))

#Transform coordinate reference system to match the one in Stadia Maps  
icb_boundaries <- st_transform(icb_boundaries, crs = 4326)

# Markers used for JAG status
JAG_shape_ref = data.frame(accreditation_status = c("Accredited","Accredition deferred","Not Accredited. Application Pending","Not accredited"),
                shape_ref = as.factor(c(1,2,3,4)))

JAG_status2 <- JAG_status %>% 
  left_join(JAG_shape_ref, by = c("accreditation_status" = "accreditation_status"))


# create map with markers
map <- ggmap(south_east_map) 
map

# Add ICB Boundaries
map <- map + geom_sf(data = icb_boundaries, alpha = 0.01, show.legend = NA, linewidth = 0.75, colour="#000000") +
  coord_sf(expand = FALSE, datum = sf::st_crs(4326))
map

# Create fill colour for ICB polygons
br <- c(-10, 0, 1.5, 3,10)
icb_boundaries$bins <- cut(icb_boundaries$ExtraRooms,breaks = br)

# Labels for legend
labs <- c(-10, 0, 1.5, 3,10)
labs_plot <- c("<0","0 - 1.5","1.5 - 3","3+")
#pal <- hcl.colors(4, "RdYlGn", rev = TRUE, alpha = 1)
pal2 <- c("#11BB00", "#FFFF00", "#FF9B00", "#F80B0B")
pal3 <- adjustcolor(pal2, alpha.f = 0.15)

# Add overlay
map <- map +
  # Add choropleth overlay
  geom_sf(data = icb_boundaries,
          aes(fill = bins), color = NA) +
  labs(fill = "Extra rooms to meet 3.5 per 100k") +
  scale_fill_manual(values = pal3,
                    drop = FALSE,
                    na.value = "grey80",
                    label = labs_plot
  )
map

# Remove lat&long axis labels
# Add in legends
# Assign JAG Status to the shapes used
map <- map +
  geom_point(data = JAG_status2,
             aes(x = longitude, y = latitude, colour=as.factor(num_endo_rooms), stroke=2, shape=as.factor(shape_ref), size=6), alpha = 0.95) +
  labs(colour = "No. Endoscopy Rooms") +
  #labs(size = "No. Endoscopy Rooms") +
  labs(shape = "JAG Accreditation Status") +
  guides(colour = guide_legend(override.aes = list(size = 5))) + #increases JAG colour blob size in legend
  guides(size = "none") +
  guides(shape = guide_legend(override.aes = list(size = 5))) + #increases no. endo rooms size in legend
  scale_size(range = c(3, 10)) +
  theme(legend.text = element_text(size=12), legend.title = element_text(size=12)) + scale_shape_manual(
  labels = c("Accredited","Accredition deferred","Not Accredited. Application Pending","Not accredited"),
  values = c(16,15,17,18)
) 
map

# Add blob colours and formatting

map <- map + scale_color_manual(values=c("#ff91a7","#9880cf","#2269db","#79943d","#d8813d","#9f1e58","#034a32"))+
  theme(axis.line = element_blank(),                                             # remove lat lon axes and whitespace margin
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('') 
map





