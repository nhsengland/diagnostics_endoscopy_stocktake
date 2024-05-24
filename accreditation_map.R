# remove ggmaps and reinstall to allow for stadia maps setup
#remove.packages("ggmap")
#install.packages("devtools")
#devtools::install_github("stadiamaps/ggmap")
library(ggmap)
library(readxl)
library(dplyr)
library(ggthemes)
library(sf)
library(colorspace)


# bring in data
df <- read_excel("C:/Users/GeorginaCable/NHS/Regional Analytics - South East - Analysis/Diagnostics/Endoscopy Stocktake/Endoscopy Stocktake Database with pivot table.xlsx", 
                 sheet = "Backing Data")

AdditionalRoomsData <- read_excel("C:/Users/GeorginaCable/NHS/Regional Analytics - South East - Analysis/Diagnostics/Endoscopy Stocktake/Endoscopy Stocktake Database with pivot table.xlsx", 
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
ukpostcodes <- read.csv("C:/Users/GeorginaCable/Downloads/ukpostcodes.csv")

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
icb_data <- sf::read_sf("C:/Users/GeorginaCable/Downloads/Integrated_Care_Boards_April_2023_EN_BSC_7929772807133590321.geojson")

se_codes <- subset(icb_data, FID %in% c(18, 19, 26, 28, 41, 42))

# rename ICB names for joining, keep South East
icb_boundaries <- se_codes %>% 
  mutate(icb_name = stringr::str_replace(ICB23NM, "Integrated Care Board", "ICB")) %>% 
  select(c(icb_name, LONG, LAT, geometry)) %>% 
  rename(c(lon = LONG, lat = LAT)) %>% 
  left_join(AdditionalRooms, by = c("icb_name" = "Location"))

#Transform coordinate reference system to match the one in Stadia Maps  
icb_boundaries <- st_transform(icb_boundaries, crs = 4326)

# create map
map <- ggmap(south_east_map)  +
  geom_point(data = JAG_status,
               aes(x = longitude, y = latitude, colour=accreditation_status, size = num_endo_rooms, stroke=2), alpha = 0.95) +
  labs(colour = "JAG Accreditation Status") +
  labs(size = "No. Endoscopy Rooms") +
  guides(colour = guide_legend(override.aes = list(size = 5))) + #increases blob size in legend
  scale_size(range = c(3, 10)) +
  theme(legend.text = element_text(size=12), legend.title = element_text(size=12))
  theme_map()
map

# Add blob colours and formatting
map <- map + scale_color_manual(values=c("#096B49", "#0149AF", "#B50B5B", "#F88705")) + # set colours to something which makes more sense
  theme(axis.line = element_blank(),                                             # remove lat lon axes and whitespace margin
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('') 
map

# Add ICB Boundaries
map <- map + geom_sf(data = icb_boundaries, alpha = 0.01, show.legend = NA, linewidth = 0.75, colour="black") +
  coord_sf(expand = FALSE, datum = sf::st_crs(4326))
map

# Create colour for ICBs
br <- c(-10, 0, 1.5, 3,10)
icb_boundaries$bins <- cut(icb_boundaries$ExtraRooms,breaks = br)

# Labels for legend
labs <- c(-10, 0, 1.5, 3,10)
labs_plot <- c("<0","0 - 1.5","1.5 - 3","3+")
#pal <- hcl.colors(4, "RdYlGn", rev = TRUE, alpha = 1)
pal2 <- c("#11BB00", "#FFFF00", "#FF9B00", "#F80B0B")
pal3 <- adjustcolor(pal2, alpha.f = 0.175)

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

