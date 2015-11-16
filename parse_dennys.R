library(xml2)
library(stringr)
library(magrittr)
library(rvest)


files = dir("data/dennys/", pattern = "*.xml",full.names = TRUE)



# Counter for iteration of loop #
n = 1

# Store info from each .xml file into a nested list #
# Overall list for each quantity of interest #
# Nested list for info from each .xml file #
address = list()
city = list()
state = list()
zip = list()
phone = list()
lat = list()
long = list()
uid = list()

for (file in files) {
  

dennysFile = read_xml(file)

address[[n]] = xml_nodes(dennysFile, "address1") %>%
            html_text()
city[[n]] = xml_nodes(dennysFile, "city") %>%
            html_text()

state[[n]] = xml_nodes(dennysFile, "state") %>%
          html_text()

zip[[n]] = xml_nodes(dennysFile, "postalcode") %>%
          html_text()

phone[[n]] = xml_nodes(dennysFile, "phone") %>%
          html_text()

lat[[n]] = xml_nodes(dennysFile, "latitude") %>%
          html_text() 
          

long[[n]] = xml_nodes(dennysFile, "longitude") %>%
          html_text()

uid[[n]] = xml_nodes(dennysFile, "uid") %>%
        html_text()
n = n+1
}

# Unlist each list of different info #
# Bind lists together into data frame #
address = unlist(address)
city = unlist(city)
state = unlist(state)
zip = unlist(zip)
phone = unlist(phone)
lat = unlist(lat)
long = unlist(long)
uid = unlist(uid)


# Bind columns into data frame #
dennysData = cbind(address, city, state, zip, phone, lat, long)
row.names(dennysData) = uid
colnames(dennysData) = c("Street", "City", "State", "Zip", "Phone", "Latitude", "Longitude")
dennysData = as.data.frame(dennysData)


# Remove duplicate elements, search by uid #
dennysData = dennysData[unique(uid), ]

# Save dennysData as dennys.Rdata #
save(dennysData, file = "data/dennys.Rdata")
