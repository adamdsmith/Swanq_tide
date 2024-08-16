get_qclcd <- function(years = 2013:2015, station = "998426-99999", #746925-03741", 
                        url = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa")
{

  ### COULD GET ASOS 5-MIN
  asos = "ftp://ftp.ncdc.noaa.gov/pub/data/asos-fivemin"
    
  out <- lapply(years, function(yr) {
    
    tmp_url <- paste(url, yr, "", sep = "/")
    #tmp_url <- paste(url, paste0("6401-", yr), "", sep = "/")
    filenames <- getURL(tmp_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    filenames <- paste(tmp_url, strsplit(filenames, "\r*\n")[[1]], sep = "")
    filenames <- filenames[grep(station, filenames)]
    
    # Write and read zipped files from temporary files
    #sapply(filenames, function(x) { ### Not yet...
    dat <- readr::read_fwf(filenames, #x, 
                           col_positions = fwf_positions(c(16, 24, 61, 66, 70), 
                                                         c(23, 27, 63, 69, 70),
                                                         col_names = c("date", "time", 
                                                                       "wdir", "wsp", "trash")),
                           col_types = "cciic")
    dat <- dat %>% select(-trash) %>% 
      mutate(dt = lubridate::ymd_hms(paste(date, time, "00")),
             wdir = ifelse(wdir == 999, NA, wdir),
             wsp = ifelse(wdir == 999, NA, round(wsp/10, 1))) %>%
      select(dt, wdir, wsp)
    # dat
    #})
    
  })
  
}
