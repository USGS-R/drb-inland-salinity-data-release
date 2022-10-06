retrieve_network <- function(network_sf_fl) {
  network <- readRDS(network_sf_fl)
  out <- network$edges %>%
    select(-start_pt, -end_pt, -subseg_updown) %>%
    mutate(subseg_length = round(subseg_length, 2)) %>%
    mutate(from_segs = ifelse(from_segs == '', NA, from_segs)) %>%
    rename(subsegid = subseg_id, subsegseg = subseg_seg, subseglen = subseg_length, fromsegs = from_segs, toseg = to_seg, tosubseg = to_subseg, segidnat = seg_id_nat)
  out <- sf::st_transform(out, crs = 4326)
  return(out)
}

retrieve_vertices <- function(network_sf_fl) {
  network <- readRDS(network_sf_fl)
  out <- network$vertices
  out <- sf::st_transform(out, crs = 4326) %>% as_Spatial()
  return(out)
}

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf_out <- dplyr::select(sf_object, seg_id_nat, geometry)
  
  sf::st_write(sf_out, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)

  setwd(dsn)
  # zip::zip works across platforms
  zip::zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}
sf_to_zip2 <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  
  setwd(dsn)
  # zip::zip works across platforms
  zip::zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

reduce_and_zip <- function(zip_filename, in_dat, layer_name) {
  out <- in_dat %>%
    select(-ID, -bird_filtered, -fish_filtered) %>%
    mutate(site_type = ifelse(site_type %in% c('Stream', 'stream'), 'ST', site_type)) %>%
    distinct() %>%
    rename(siteid = site_id, sitetype = site_type, origsource = original_source, 
           subsegid = subseg_id, fishdist = fish_dist_to_outlet_m, 
           birddist = bird_dist_to_subseg_m, segidnat = seg_id_nat) %>%
    mutate(birddist = round(birddist, 2),
           fishdist = round(fishdist, 2)) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)
    
  sf_to_zip2(zip_filename = zip_filename, sf_object = out, layer_name = layer_name)
}