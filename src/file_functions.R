zip_this <- function(out_file, .object){
  if ('data.frame' %in% class(.object)){
    filepath <- basename(out_file) %>% tools::file_path_sans_ext() %>% paste0('.csv') %>% file.path(tempdir(), .)
    write_csv(.object, file = filepath)
    zip_this(out_file = out_file, .object = filepath)
  } else if (class(.object) == 'character' & all(file.exists(.object))){
    # works for single or multiple files
    curdir <- getwd()
    on.exit(setwd(curdir))
    setwd(unique(dirname(.object)))
    zip::zip(file.path(curdir, out_file), files = basename(.object))
  } else {
    stop("don't know how to zip ", .object)
  }
}

zip_obs <- function(out_file, in_file){
  if (grepl('csv', in_file)) {
    zip_this(out_file, .object = readr::read_csv(in_file))
  } else if (grepl('rds', in_file)) {
    zip_this(out_file, .object = readRDS(in_file))
  } else {
    message('There is no reader for this filetype. Please modify function zip_obs.')
  }
}

zip_files <- function(out_file, ...) {
  files <- c(...)
  zip_this(out_file, files)
}

get_distance_matrix <- function(out_file, in_file) {
  distance <- readRDS(in_file)
  from <- rownames(distance$updown)

  out <- as_tibble(distance$updown) %>%
    mutate(from = from) %>%
    select(from, everything())

  readr::write_csv(out, file = out_file)
}

get_sntemp_output <- function(out_file, in_file){
  sntemp <- feather::read_feather(in_file)
  readr::write_csv(sntemp, out_file)
}

extract_reach_attributes <- function(res_file, attr_file, out_file) {
  res <- readRDS(res_file) %>% select(-subseg_seg) %>% rename(reach_class = type_res)
  attr <- readRDS(attr_file)
  attr <- attr$edges %>% st_drop_geometry()

  out <- left_join(res, select(attr, -subseg_updown, -start_pt, -end_pt, -to_subseg)) %>%
    mutate(subseg_length = round(subseg_length, 2))

  readr::write_csv(out, out_file)

}

get_sites <- function(in_dat) {

  sites <- in_dat$edges %>%
    filter(!is.na(seg_id_nat))
  return(unique(sites$seg_id_nat))
}

filter_reservoirs <- function(in_dat, keep, out_file) {
  dat <- readr::read_csv(in_dat) %>%
    filter(reservoir %in% keep)

  readr::write_csv(dat, out_file)
}

filter_to_subset <- function(out_file, in_file, segments) {
  dat <- readr::read_csv(in_file) %>%
    filter(seg_id_nat %in% segments)

  readr::write_csv(dat, out_file)
}

get_grands <- function(in_file) {
  dat <- readr::read_csv(in_file)
  return(unique(dat$GRAND_ID))
}

filter_res_ids <- function(in_file, out_file, res_keep) {
  dat <- readr::read_csv(in_file) %>%
    filter(GRAND_ID %in% res_keep)

  readr::write_csv(dat, out_file)
}


# Write a layer of an sf object as a zipped-up shapefile
sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()

  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites

  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)

  setwd(dsn)
  zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

# bring together various water level sources
combine_level_sources <- function(out_csv, nwis_levels, nyc_levels, hist_levels) {
  nwis <- readr::read_csv(nwis_levels)
  nyc <- readRDS(nyc_levels)
  hist <- readRDS(hist_levels) %>%
    mutate(date = as.Date(date)) %>%
    rename(surface_elevation_m = res_level_m) %>%
    # filtering out monthly interpolated values since we now of NYC-DEP data that covers same period
    filter(!data_type %in% 'monthly')

  obs_dat <- bind_rows(nwis, nyc, hist, .id = 'source') %>%
    group_by(site_id, date) %>%
    # sources were ordered by priority, so can filter on min source
    slice_min(source) %>%
    mutate(data_type = 'daily observed') %>%
    mutate(source = case_when(source %in% 1 ~ 'nwis',
                              source %in% 2 ~ 'nyc',
                              source %in% 3 ~ 'usgs')) %>%
    arrange(date, site_id)

  # linearly interpolate from beginning to end of this record
  # when any day does not have a water level value

  first_res <- data.frame(date = seq(min(obs_dat$date[obs_dat$site_id %in% unique(obs_dat$site_id)[1]]), max(obs_dat$date[obs_dat$site_id %in% unique(obs_dat$site_id)[1]]), by = 1)) %>%
    full_join(obs_dat[obs_dat$site_id %in% unique(obs_dat$site_id)[1], ], by = "date") %>%
    mutate(site_id = unique(obs_dat$site_id)[1])

  second_res <- data.frame(date = seq(min(obs_dat$date[obs_dat$site_id %in% unique(obs_dat$site_id)[2]]),
                                      max(obs_dat$date[obs_dat$site_id %in% unique(obs_dat$site_id)[2]]), by = 1)) %>%
    full_join(obs_dat[obs_dat$site_id %in% unique(obs_dat$site_id)[2],], by = "date") %>%
    mutate(site_id = unique(obs_dat$site_id)[2])

  out_dat <- bind_rows(first_res, second_res) %>%
    arrange(site_id, date) %>%
    group_by(site_id) %>%
    mutate(surface_elevation_m = na.approx(surface_elevation_m)) %>%
    mutate(data_type = ifelse(is.na(data_type), 'daily interpolated', data_type),
           source = ifelse(is.na(source), 'interpolated', source)) %>%
    select(-source_id)

  out_dat$surface_elevation_m <- round(out_dat$surface_elevation_m, 2)
  readr::write_csv(out_dat, out_csv)
}
