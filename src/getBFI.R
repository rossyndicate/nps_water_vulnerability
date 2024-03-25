
getBFI <- function(aoi) {

    if (!dir.exists("templeod_dataall/")) {
      dir.create("templeod_dataall/")
      }

      download.file("https://www.sciencebase.gov/catalog/file/get/631405c5d34e36012efa3192?f=__disk__e5%2F34%2F59%2Fe53459c9f3d112cf7790150c97bd648c808d2d24", 
                    destfile = "templeod_dataall/data.zip")
      
      
      unzip("templeod_dataall/data.zip", exdir = "templeod_dataall")
      
      bfi <- terra::rast("templeod_dataall/bfi48grd/")
      
      # transform crs of aoi (faster than transforming netCDF)
      aoi <- aoi %>%
        sf::st_transform(st_crs(bfi))
      
      # crop gridmet data to park and clean for export
      bfi_crop <- terra::crop(bfi, aoi, mask = TRUE)
      
      unlink("templeod_dataall/", recursive = TRUE)
      
      return(bfi_crop)
      
}
