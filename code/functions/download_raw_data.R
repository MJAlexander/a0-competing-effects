


## function to download deaths

download_linked_deaths <- function (year, download_dir = "./raw_data") 
{
  base_url <- "https://www.nber.org/lbid"
  file_name <- sprintf("linkco%sus_num.csv", year)
  file_url <- sprintf("%s/%s/%s.zip", base_url, year, file_name)
  dest_file <- sprintf("%s/%s.zip", download_dir, file_name)
  if (download_dir != "./") {
    dir.create(download_dir, showWarnings = FALSE)
  }
  if(file.exists(paste0(download_dir, "/", file_name, ".zip"))){
    cat("File already exits.\n")
  }
  else{
    utils::download.file(file_url, dest_file)
  }
}

## function to download births

download_births <- function (year, download_dir = "./raw_data") 
{
  base_url <- "https://www.nber.org/lbid"
  file_name <- sprintf("linkco%sus_den.csv", year)
  file_url <- sprintf("%s/%s/%s.zip", base_url, year, file_name)
  dest_file <- sprintf("%s/%s.zip", download_dir, file_name)
  if (download_dir != "./") {
    dir.create(download_dir, showWarnings = FALSE)
  }
  if(file.exists(paste0(download_dir, "/", file_name, ".zip"))){
    cat("File already exits.\n")
  }
  else{
    utils::download.file(file_url, dest_file)
  }
}
