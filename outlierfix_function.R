#' Outlierfix
#'
#' This function fix the outliers both in continuous and categorical data.
#' @param x a vector
#' @param type a character string indicating which type of data is,  "continuous","categorical"
#' @param
#' @keywords
#' @export
#' @examples
grDevices::boxplot.stats()
data.table::as.data.table()
univOutl::boxB()
outlierfix <- function(dt,col_name, type, rangeLU=NULL, k, fix.method,weights=NULL, exclude=NA, logt){
  # Take below types of data into consideration:
  # 1.numerical:(continuous)
  # 2.categorical: nominal/ordinal/binary (discrete, no numeric relationship)
  # 3.count
  # 4.time
  # type:c("continuous","categorical")
  # fix:c("remove","nearestmd","meanfill")
  # method:c("IQR")

  #-- test
  dt <- data.table(type = factor(rep(c("A","B"), each = 50)),
                     rating = c(rnorm(200),rnorm(200, mean=.6)))

  col_name="rating"
  exclude = NA
  weights=NULL
  logt = FALSE
  k=1.5
  rangeLU=NULL
  #-- test

  if(type=="numeric"){
    # exclude the data that no-need as NA
    if(is.null(rangeLU)) rangeLU <- range(dt[,get(col_name)],na.rm=TRUE)
    dt <- dt[(get(col_name)<=rangeLU[2])&(get(col_name)>=rangeLU[1]),]

    # display the distribution for deciding which method to identify outliers
    print(ggplot(dt, aes(x =get(col_name))) + geom_histogram(aes(y = ..density..),binwidth = .5,colour = "blue", fill = "white")+geom_density(alpha = .2, fill="#FF6655"))

    # enter the methods to be used
    iden.mnbr <- as.numeric(readline(prompt=cat("Enter number for identified method used on outliers: ","\n","1.resistant: standard boxplot fences","\n","2.asymmetric: modification of standard method to deal with (moderately) skewed data","\n","3.adjbox: adjusted boxplot for skewed distributions")))
    while(is.null(iden.mnbr)|is.numeric(iden.mnbr)){
      iden.mnbr <- as.numeric(readline(prompt=cat("Enter number for identified method used on outliers: ","\n","1.resistant: standard boxplot fences","\n","2.asymmetric: modification of standard method to deal with (moderately) skewed data","\n","3.adjbox: adjusted boxplot for skewed distributions")))
    }
    identify.method <- c("resistant","asymmetric","adjbox")[iden.mnbr]

  # enter the fixed methods
    fix.mnbr <- as.numeric(readline(prompt=cat("Enter number for fixed method used on outliers: ","\n","1.dropBoth: drop the outliers from both sides of the data","\n","2.dropLeft: drop the outliers from left side of the data","\n","3.dropRight: drop the outliers from right side of the data","\n","4.asNABoth: convert outliers from both sides of the data as missing values","\n","5.asNALeft: convert outliers from left side of the data as missing values","\n","6.asNARight: convert outliers from right side of the data as missing values","\n","7.median: replace outliers with median","\n","8.mean: replace outliers with mean")))
    while(is.null(fix.mnbr)|is.numeric(fix.mnbr)){
      fix.mnbr <- as.numeric(readline(prompt=cat("Enter number for fixed method used on outliers: ","\n","1.dropBoth: drop the outliers from both sides of the data","\n","2.dropLeft: drop the outliers from left side of the data","\n","3.dropRight: drop the outliers from right side of the data","\n","4.asNABoth: convert outliers from both sides of the data as missing values","\n","5.asNALeft: convert outliers from left side of the data as missing values","\n","6.asNARight: convert outliers from right side of the data as missing values","\n","7.median: replace outliers with median","\n","8.mean: replace outliers with mean")))
    }
    fix.method <- c("dropBoth","dropLow","dropUp","asNABoth","asNALow","asNAUp","median","mean")[fix.mnbr]

    # get the position of outliers
    outlierind <- univOutl::boxB(dt[,get(col_name)],k=k,method=identify.method,weights=weights,exclude=exclude,logt=logt)
    if(fix.method=="dropBoth") dt <- dt[!outlierind$outliers,get(col_name)]
    if(fix.method=="dropLow") dt <- dt[!outlierind$lowOutl,get(col_name)]
    if(fix.method=="dropUp") dt <- dt[!outlierind$upOutl,get(col_name)]
    if(fix.method=="asNABoth") dt[outlierind$outliers,(col_name):=NA]
    if(fix.method=="asNALow") dt[outlierind$lowOutl,(col_name):=NA]
    if(fix.method=="asNAUp") dt[outlierind$upOutl,(col_name):=NA]
    if(fix.method=="median") dt[outlierind$outliers,(col_name):=median(dt[!outlierind$outliers,get(col_name)])]
    if(fix.method=="mean") dt[outlierind$outliers,(col_name):=mean(dt[!outlierind$outliers,get(col_name)])]
   }
  if(type=="factor"){

  }
  else{
    print('Data type is not accurate. Ensure the type to be numeric or factor.')
  }

  return(dt)
}

