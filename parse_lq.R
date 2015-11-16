library(stringr)
library(magrittr)
library(rvest)


files = dir("data/lq/",pattern = "*.html",full.names = TRUE)

lqHotels = data.frame(matrix(NA, nrow = length(files), ncol = 16 ))
# Find and collect names of each location #
locationLabels = rep(0, length(files))
# Collect address info, street, city, phone and fax for each location #
colnames(lqHotels) = c("Street", "City/Zip", "Phone", "Fax", "Latitude", "Longitude",
                       "Number of Floors", "Number of Rooms", "Number of Suites",
                       "Check-In Time", "Check-Out Time", 
                       "Wifi", "Swimming Pool", "Fitness Center",
                       "Breakfast", "Hair Dryer")

# Counter for number of files in for loop #
n = 1

for(file in files)
{
  html = read_html(file)
  
  lqAddr = html_nodes(html, ".hotelDetailsBasicInfoTitle p") %>% html_text()  
  lqDets = html_nodes(html, ".hotelFeatureList li") %>% html_text()
  lqAmn = html_nodes(html, ".section:nth-child(1) .pptab_contentL li") %>% html_text()
  
  # Find Location Name #
  pos1 = regexpr('details.', file)
  pos2 = regexpr('.address', file)
  locNames = substr(file, pos1, pos2)
  locNames = str_replace(locNames, "\\.$", "")
  locationLabels[n] = gsub('details.', "", locNames)
  
  
  
  # Format Address to remove spaces #
  lqAddr = str_trim(lqAddr)
  lqAddr = str_replace(lqAddr, "Phone:", " ")
  lqAddr = str_replace(lqAddr, "Fax:", " ")
  lqAddr = unique(str_trim(unlist(str_split(lqAddr, "\n"))))
  # Street Address #
  lqHotels[n,1] = lqAddr[lqAddr != ""][1] %>%
    str_replace(., "\\,$", "")
  # City/Zip #
  lqHotels[n,2] = lqAddr[lqAddr != ""][2]
  # Phone Number #
  lqHotels[n,3] = lqAddr[lqAddr != ""][3]
  # Fax Number #
  lqHotels[n,4] = lqAddr[lqAddr != ""][4]
  
  lat_long = html_nodes(html, ".minimap") %>% html_attr("src")
  # Want to only access latitude and longitude #
  pos1 = regexpr('.gif', lat_long)
  pos2 = regexpr('&size', lat_long)
  lqCoord = substr(lat_long, pos1, pos2)
  
  # Remove.gif, replace with whitespace #
  lqCoord = sub('.gif|', " ", lqCoord)
  
   #Remove &, replace with whitespace #
  lqCoord = sub('&', " ", lqCoord)
  lqCoord = str_trim(sub("([.-])|[[:punct:]]", "", lqCoord))
  lqCoord = strsplit(lqCoord, ",")
  
  # seperate into vectors of latitude and longitude #
  lqHotels[n,5] = as.numeric(unlist(lqCoord)[1])
  lqHotels[n,6] = as.numeric(unlist(lqCoord)[2])
  
  
  # Format Hotel Details #
  lqDets = str_trim(lqDets) %>% str_replace(., "Floors:", " ") %>%
    str_replace(., "Rooms:", " ") %>% 
    str_replace(., "Suites:", " ") %>%
    str_replace(., "Check-In Time:", " ") %>%
    str_replace(., "Check-Out Time:", " ")
  
  
  lqDets = unique(str_trim(unlist(str_split(lqDets, "\n"))))
  # Number of Floors #
  lqHotels[n,7] = lqDets[lqDets != ""][1]
  # Number of Rooms #
  lqHotels[n,8] = lqDets[lqDets != ""][2]
  # Number of Suites #
  if(length(lqDets) < 5){
    lqHotels[n,9] = NA
    # Check-in Time #
    lqHotels[n,10] = lqDets[lqDets != ""][3]
    # Check Out Time #
    lqHotels[n,11] = lqDets[lqDets != ""][4]
  }
  
  else{
    lqHotels[n,9] = lqDets[lqDets != ""][3]
    # Check-in Time #
    lqHotels[n,10] = lqDets[lqDets != ""][4]
    # Check Out Time #
    lqHotels[n,11] = lqDets[lqDets != ""][5]
  }
  
  # Format Hotel Amenities #
  lqAmn = str_trim(lqAmn) %>% tolower()
  
  
  # Find Hotel Amenities #
  # Wifi #
  lqHotels[n,12] = any(str_detect(lqAmn, "wireless"))
  
  # Pool #
  lqHotels[n,13] = any(str_detect(lqAmn, "pool"))
  
  # Fitness Center #
  lqHotels[n,14] = any(str_detect(lqAmn, "fitness"))
  
  # Breakfast #
  lqHotels[n,15] = any(str_detect(lqAmn, "breakfast"))
  
  # Hair Dryer #
  lqHotels[n,16] = any(str_detect(lqAmn, "hair"))
  
  
  
  
  n = n+1
  
}

# Convert True/False Values to Yes or NA #
logicalToYes = function(value, colData){
  colData[which(value %in% TRUE)] = "Yes"
  colData[which(!value %in% TRUE)] = NA
  return(colData)
}

# Apply function to relevant columns #
newCols = lapply(12:16, function(x) return(logicalToYes(lqHotels[,x], lqHotels[,x])))
# Paste new columns back into lqHotels data frame #
lqHotels[,12] = newCols[[1]]
lqHotels[,13] = newCols[[2]]
lqHotels[,14] = newCols[[3]]
lqHotels[,15] = newCols[[4]]
lqHotels[,16] = newCols[[5]]

rownames(lqHotels) = locationLabels


# Save lqHotels as lq.Rdata #
save(lqHotels, file = "data/lq.Rdata")
