# The following data cleaning was completed before working with the tree swallow data downloaded from: https://figshare.com/articles/dataset/Tree_Swallow_Nest_Box_Productivity_Dataset_from_Long-Point_Ontario_Canada_1977-2014_/14156801/1?file=26736311

# Required packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Import data
banding <- read.csv("TRES/Tres.banding.csv")
egg <- read.csv("TRES/Tres.egg.csv")
nest <- read.csv("TRES/Tres.nest.csv")
nestling <- read.csv("TRES/Tres.nestling.csv")
tresCodes <- read.csv("TRES/TRES codes.csv")



# These are the changes/cleaning that were done to the final data for these tutorials:

# egg

egg2 <- egg %>%
  filter(is.na(egg_weight) == F,
         egg_weight != "NO WEIGHT",
         egg_weight != "NA",
         egg_weight != "-",
         egg_weight != "0",
         egg_weight != "97.21",
         egg_weight != "97.54",
         egg_weight != "42.82",
         !grepl("NA", egg_weight_day),
         is.na(egg_weight_day) == F) %>%
  mutate(egg_weight = gsub("-", "", egg_weight),
         egg_weight_day = gsub("20005-", "2005-", egg_weight_day),
         egg_weight_day = gsub("20006-", "2006-", egg_weight_day),
         egg_weight_day = gsub("20007-", "2007-", egg_weight_day),
         egg_weight_day = gsub("19906-", "1996-", egg_weight_day),
         egg_weight_day = gsub("19907-", "1997-", egg_weight_day),
         egg_weight_day = gsub("19905-", "1995-", egg_weight_day),
         egg_weight_day = gsub("19807-", "1987-", egg_weight_day),
         egg_weight_day = gsub("19078-", "1978-", egg_weight_day),
         egg_weight_day = gsub("19079-", "1979-", egg_weight_day),
         egg_weight_day = gsub("-205", "-25", egg_weight_day),
         egg_weight_day = gsub("-206", "-26", egg_weight_day),
         egg_weight_day = gsub("-207", "-27", egg_weight_day),
         egg_weight_day = gsub("-107", "-17", egg_weight_day),
         egg_weight_day = gsub("-106", "-16", egg_weight_day),
         egg_weight_day = gsub("-105", "-15", egg_weight_day),
         egg_weight_day = as.Date(egg_weight_day, format = "%Y-%m-%d"),
         clutch_size = gsub("-","", clutch_size))

# readr::write_csv(egg2,"egg_final.csv", append = FALSE, col_names = TRUE)


# banding

# fix column class
banding$date <- as.Date(banding$date)
banding$weight <- as.numeric(banding$weight)
banding$wing_chord <- as.numeric(banding$wing_chord)
banding$wing_flat <- as.numeric(banding$wing_flat)
banding$tail <- as.numeric(banding$tail)
banding$band_number <- as.numeric(banding$band_number)
banding$p_9 <- as.numeric(banding$p_9)

# omit weights above 50
banding <- subset(banding, weight < 50)

# omit wing_chord values below 100 to deal with the switched columns
banding <- subset(banding, wing_chord >= 100)

# omit primary wing lengths less than 60 to deal with the switched columns and tail lengths greater than 100
banding <- subset(banding, p_9 >= 60)

banding <- subset(banding, tail <= 100)

# omit age INT (mistake)
banding <- subset(banding, age != "INT")

# omit columns we wont be using:

banding <- subset(banding, select = -c(X, age, how, time_capture, time_release, bander, logger, date_20, weight_after_logger, bp, comments))
banding <- subset(banding, select = -c(site))

banding$weight <- as.character(banding$weight)

#readr::write_csv(banding,"banding_final.csv", append = FALSE, col_names = TRUE)


# nesting 
nestling <- read.csv("TRES/Tres.nestling.csv")

# omit columns we wont be using:

nestling <- subset(nestling, select = -c(day_1_date, age, time_nestling_found, bander))
nestling <- subset(nestling, select = -c(day_12_primary, day_16_primary,marking,
                                         condition, mean_hatch_date))
nestling <- subset(nestling, select = -c(day_15_primary, hours_since_hatch, est_hatch_date, mark, dp))
nestling <- subset(nestling, select = -c(original_egg_number))
nestling <- subset(nestling, select = -c(comments))


# if clutch_number = NA, assign clutch_no
nestling$clutch_number <- ifelse(is.na(nestling$clutch_number), nestling$clutch_no, nestling$clutch_number)

# drop clutch_no
nestling <- subset(nestling, select = -c(clutch_no))


# readr::write_csv(nestling,"nestling_final.csv", append = FALSE, col_names = TRUE)


# banding + nest
# We plan to look at the birds in the nest datasets that we have banding and nest data for (omit any entry that don't have banding data)

nest <- subset(nest, band_number != "NA")
nest$band_number <- as.numeric(nest$band_number)

# merge nest and banding by band_number (all=FALSE because will only merge the ones that have matching columns)
banding_nest <- merge(banding, nest, by=c("band_number", "year"), all = FALSE)

# drop columns that aren;t needed
banding_nest <- subset(banding_nest, select = -c(X.x, band_or_recapture, age.x, how, time_capture, time_release, bander, comments.x, logger, date_20, weight_after_logger, age_code.x, bp, X.1, X.y))
banding_nest <- subset(banding_nest, select = -c(site.x, site.y, site_section, female, male, first_egg_date, X2011.01.01, comments.y, geolocator_band_number, tagged, age.y, sex.y, code, age_code.y))

banding_nest <- subset(banding_nest, select = -c(nest_code))

banding_nest <- subset(banding_nest, select = -c(nest_box.x))

# rename some columns
banding_nest <- banding_nest %>% 
  dplyr::rename(sex = sex.x)

banding_nest <- banding_nest %>% 
  dplyr::rename(nest_box = nest_box.y)

# if number_hatched > clutch_size, omit (an error?? or we don't have final counts of clutch size)
banding_nest <- banding_nest %>% filter(number_hatch <= clutch_size)

# add a location based on nest_box
# first need to separate out the code (PT, MC, SL) from the box number
banding_nest$location <- banding_nest$nest_box
banding_nest$location <- substr(banding_nest$location, start = 1, stop = 2) # these keeps just the first two characters

# remove a few more columns
banding_nest <- subset(banding_nest, select = -c(number_fledged, hatch_date, brood_size, proportion_fledge))
banding_nest$weight <- as.numeric(banding_nest$weight)
banding_nest$tail <- as.numeric(banding_nest$tail)
banding_nest <- subset(banding_nest, weight <= 80)
banding_nest <- subset(banding_nest, tail <= 100)

# readr::write_csv(banding_nest,"banding_nest_final.csv", append = FALSE, col_names = TRUE)


# nest

nest <- read.csv("TRES/Tres.nest.csv")

# remove some columns
nest <- subset(nest, select = -c(X.1, X, site_section, female, male, X2011.01.01, comments, geolocator_band_number, tagged, code))

nest <- subset(nest, select = -c(first_egg_date, band_number))
       
nest <- subset(nest, select = -c(proportion_fledge, hatch_date, brood_size))

# move number_fledge to no_of young fledge
nest$no_of_young_fledged <- ifelse(is.na(nest$no_of_young_fledged), nest$number_fledged, nest$no_of_young_fledged)
nest <- subset(nest, select = -c(number_fledged))

nest <- nest %>% 
  dplyr::rename(location = site)


# fix location (just sepearte the nest box code to get location - first need to seperate out the code (PT, MC, SL) from the box number)
nest$location <- nest$nest_box
nest$location <- substr(nest$location, start = 1, stop = 2) # these keeps just the first two characters
nest <- subset(nest, select = -c(sex))
nest <- subset(nest, select = -c(age))

nest$clutch_number <- as.integer(nest$clutch_number)
nest$clutch_size <- as.integer(nest$clutch_size)

# aggregate nests by year, clutch_number, and nest_box so that there is not multiple measures of the same nest

nest <- aggregate(clutch_size ~ year + nest_box + clutch_number + location + nest_code, nest, max) # by doing max we will get the largest clutch count taken from the same nest

# subset for clutch_numbers less than 3 (only one or two data points ever higher than 2)
nest <- subset(nest, clutch_number == 1 | clutch_number == 2)

# if location is P0 or P1 assign to PT

## rename Treatments, Red= Control, Pink = PDI_25, Orange = PDI_50
nest$location[nest$location == "P0"] <- "PT"
nest$location[nest$location == "P1"] <- "PT"

# readr::write_csv(nest,"nest_final.csv", append = FALSE, col_names = TRUE)