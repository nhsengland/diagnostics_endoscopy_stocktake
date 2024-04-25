# remove ggmaps and reinstall to allow for stadia maps setup
#remove.packages("ggmap")
#install.packages("devtools")
#devtools::install_github("stadiamaps/ggmap")
library(ggmap)
library(readxl)
library(dplyr)
library(ggthemes)


# bring in data
df <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Restricted Library/SE/Analysis/Diagnostics/Endoscopy Stocktake/Endoscopy Stocktake Database with pivot table.xlsx", 
                                                            sheet = "Backing Data")


# bring in postcodes lat lon lookup table
ukpostcodes <- read.csv("C:/Users/martin.bloyce/OneDrive - NHS England/Reference/ukpostcodes.csv")

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


# register API key at https://client.stadiamaps.com/dashboard/ and enter here
register_stadiamaps(key = "c5c80bbf-69e1-422a-a060-24bf4322e9e1")

# create map for region (tip: use OpenStreetMap to get co-ordinates for bbox)
south_east_map <- get_stadiamap(
  bbox = c(left = -1.8, bottom = 50.5, right = 1.6, top = 51.9),
  maptype = "outdoors",
  zoom = 9
)

# create map

map <- ggmap(south_east_map) +
  geom_point(data = JAG_status,
               aes(x = longitude, y = latitude, colour=accreditation_status, size = num_endo_rooms)) +
  scale_size(range = c(3, 10))
  theme_map()

map <- map + scale_color_manual(values=c("#005EB8", "#41B6E6", "#8A1538", "#FFB81C")) + # set colours to something which makes more sense
  theme(axis.line = element_blank(),                                             # remove lat lon axes and whitespace margin
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('')

rm(list = setdiff(ls(), "map"))