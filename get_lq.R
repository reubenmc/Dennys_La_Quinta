library(rvest)
library(stringr)
library(magrittr)


base_url = "http://www.lq.com"
listing_page = "/en/findandbook/hotel-listings.html"

listings = read_html(paste0(base_url, listing_page))

states1 = read.csv("lq_states.csv", header = FALSE)[1]

# function to scrape La Quita website #
# takes imputs of read_html package #
# csv file of US states #
# url for LQ website #
# and directory to save results #
get_state_hotels = function(html, states, base_url, out_dir = "data/lq/")
{
  for(state in states)
  {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    hotels = html_nodes(html, "#hotelListing a")
    
    # Find the start of a state's hotels by finding its name in the node text #
    # +2 to skip State and Back to Top anchor tags #
    start = 2+which(html_text(hotels) %>% str_trim() %in% paste("Hotels in", state))
    stopifnot(length(start) == 1)
    
    # State label anchor have no href result in NA #
    urls = html_attr(hotels, "href")
    label_index = which(is.na(urls))
    end = label_index[label_index > start] %>% min() - 1
    
    cat("Downloading",state,"...\n")
    for(url in urls[start:end])
    {
      download.file(paste0(base_url,url),
                    destfile = paste0(out_dir,basename(url)),
                    quiet = TRUE)
      Sys.sleep(5)
    }
  }
}

get_state_hotels(listings, states1[,1], base_url)
