getBoundingBox<-function(inRst){
  require(raster)
  r<-raster(inRst)
  ext<-extent(r)
  ext<-paste(ext[1], ext[3], ext[2], ext[4])
  res<-paste(res(r)[1], res(r)[2])
  result<-list(ext=ext, res=res)
  return (result)
}

resample2grid<-function(inRst, targetRst, outRst, ram=2000, method="cubic"){
  require(raster)
  
  r<-raster(targetRst)
  target_srs<-projection(r)                                               #Get or set the coordinate reference system (CRS) of a Raster* object.
  param<-getBoundingBox(targetRst)                                        #Get raster bounding box and x,y resolution
  
  command<-'gdalwarp'                                                     #image reprojection and warping utility
  command<-paste(command, "--config GDAL_CACHEMAX", ram)                  #Speed-up with more cache (avice: max 1/3 of your total RAM)
  command<-paste(command, "-t_srs", paste0('"',target_srs, '"'))          #target spatial reference set. The coordinate systems that can be passed are anything supported by the OGRSpatialReference.SetFromUserInput() call, which includes EPSG PCS and GCSes (ie. EPSG:4296), PROJ.4 declarations (as above), or the name of a .prf file containing well known text.
  command<-paste(command, "-tr", param$res)                               #set output file resolution (in target georeferenced units)
  command<-paste(command, "-te", param$ext)                               #set georeferenced extents of output file to be created (in target SRS).
  command<-paste(command, "-r", method)                                   #Resampling method to use. See http://www.gdal.org/gdalwarp.html for available methods are:
  command<-paste(command, "-overwrite -co 'COMPRESS=LZW' -co 'TILED=YES'")
  command<-paste(command, inRst)
  command<-paste(command, outRst)
  system(command)

  return('done')
}

x<-list.files("/export/homes/dajacques/PhD/WorkingData/Spot4_and_Radarsat2_crop/")
inRst<-"/export/homes/dajacques/PhD/WorkingData/Spot4_and_Radarsat2_crop/CROP_Num_NoDataDespeckle_20130718_HV_GEO.tif"
targetRst<-"/export/homes/dajacques/PhD/WorkingData/SPOT4tk5_NDVI/SPOT4_20130318__NDVI_clip"
outRst<-"/export/homes/dajacques/PhD/WorkingData/RADARSAT_resample/20130718.tif"

resample2grid(inRst, targetRst, outRst)
