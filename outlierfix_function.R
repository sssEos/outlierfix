#' Outlierfix
#'
#' This function fix the outliers both in continuous and categorical data.
#' @param data a data table
#' @param type a character string indicating which type of data is,  "continuous","categorical"
#' @param 
#' @keywords
#' @export
#' @examples
grDevices::boxplot.stats()
data.table::as.data.table()
outlierfix <- function(data,col_name,method,fix){
  # Take below types of data into consideration: 
  # 1.numerical:(discrete, continuous)
  # 2.categorical: nominal/ordinal/binary (discrete, no numeric relationship)
  # 3.count
  # 4.time
  # type:c("continuous","categorical")
  # fix:c("remove","nearestmd","meanfill")
  # method:c("zscore","IQR")
  
  #-- test
  data=warpbreaks
  col_name="breaks"
  fix="remove"
  #-- test
  
  dt = as.data.table(data)
  type=class(dt[,get(col_name)])
  
  if(type=="numeric"){
    # ensure there are no missing values in data
    if(sum(is.na(dt[,get(col_name)]))>0){
      print("There are missing values in data.")
    }
    # return the outlier index in datatable
    else{
      if(method=="IQR"){
        if(sum(is.na(boxplot.stats(dt[,get(col_name)])$out))==0){
          print("There are no outliers based on IQR.")
        }else{
          outlierind = which(dt[,get(col_name)]%in%boxplot.stats(dt[,get(col_name)])$out)
          if(fix=="remove"){
            dt[outlierind,(col_name):=NA]
          }
          if(fix=="nearestmd"){
            dt[outlierind,(col_name):=median(dt[!outlierind,get(col_name)])]  
            # exclude the outlier and get the median, replace the outlier with median
          }
          if(fix="meanfill"){
            dt[outlierind,(col_name):=mean(dt[!outlierind,get(col_name)])]  
            # exclude the outlier and get the mean, replace the outlier with mean
          }
        }
      }
      if(method=="zscore"){
        
      }
    }
      
  }
  else if(type=="factor"){
    
  }
  else{
    print('Data type is not accurate. Ensure the type to be numeric or factor.')
  }
  
  return(dt)
}

