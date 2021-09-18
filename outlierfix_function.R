#' Outlierfix
#'
#' This function fix the outliers both in continuous and categorical data.
#' @param
#' @param type a character string indicating which type of data is,  "continuous","categorical"
#' @param
#' @keywords
#' @export
#' @examples
data.table::as.data.table()
univOutl::boxB()

dt <- data.table(type = factor(rep(c("A","B"), each = 50)),
                 rating = c(rnorm(200),rnorm(200, mean=2)))
# dt[c(3,6,12,17,300,250),"rating"]=300
dt[c(3),"rating"]=300
col_name="rating"
type="continuous"
rangeLU=c(-Inf,Inf)
k=1.5
exclude = NA
logt = FALSE

dt = as.data.table(mtcars)
col_name="wt"
dt[4,]$wt=NA
type="continuous"
rangeLU=c(2.5:3.5)
k=1.5
exclude = NA
logt = FALSE


outlierfix <- function(dt,col_name, interactive = FALSE, type="continuous", rangeLU=NULL, k, exclude=NA, logt){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(data.table, univOutl, base, ggplot, ggpubr)
  # check dataset is datatable
  if(is.element("data.table", class(dt))==FALSE) dt = as.data.table(dt)
  if(is.element(type,c("continuous","categorical"))==FALSE){
    stop('Data type is not accurate. Ensure the type to be continuous or categorical.')
  }
  if(interactive=="TRUE"){
    if(type=="continuous"){
      # exclude the data that no-need as NA
      # show error if not logical
      if(is.null(rangeLU)) rangeLU <- c(-Inf,Inf)
      dt <- dt[(get(col_name)<=rangeLU[2])&(get(col_name)>=rangeLU[1]),]

      # display the distribution for deciding which method to identify outliers
      bp <- ggplot2::ggplot(dt, aes(x =get(col_name))) + geom_histogram(aes(y = ..density..),binwidth = .5,colour = "blue", fill = "white")+geom_density(alpha = .2, fill="#FF6655")
      bp_box <- ggplot2::ggplot(dt, aes(get(col_name))) + geom_boxplot(colour = "blue", fill = "white",outlier.colour = "red", outlier.shape = 1)
      ggpubr::ggarrange(bp,bp_box, labels=c("histogram","boxplot"),ncol=1,nrow=2)

      # enter the methods to be used
      iden.mnbr <- as.numeric(readline(prompt=cat("Enter number for identified method used on outliers: ","\n","1.resistant: standard boxplot fences","\n","2.asymmetric: modification of standard method to deal with (moderately) skewed data","\n","3.adjbox: adjusted boxplot for skewed distributions")))
      while(is.null(iden.mnbr)|!is.numeric(iden.mnbr)){
        iden.mnbr <- as.numeric(readline(prompt=cat("Enter number for identified method used on outliers: ","\n","1.resistant: standard boxplot fences","\n","2.asymmetric: modification of standard method to deal with (moderately) skewed data","\n","3.adjbox: adjusted boxplot for skewed distributions")))
      }
      identify.method <- c("resistant","asymmetric","adjbox")[iden.mnbr]

      # enter the fixed methods
      fix.mnbr <- as.numeric(readline(prompt=cat("Enter number for fixed method used on outliers: ","\n","1.dropBoth: drop the outliers from both sides of the data","\n","2.dropLeft: drop the outliers from left side of the data","\n","3.dropRight: drop the outliers from right side of the data","\n","4.asNABoth: convert outliers from both sides of the data as missing values","\n","5.asNALeft: convert outliers from left side of the data as missing values","\n","6.asNARight: convert outliers from right side of the data as missing values","\n","7.median: replace outliers with median","\n","8.mean: replace outliers with mean")))
      while(is.null(fix.mnbr)|!is.numeric(fix.mnbr)){
        fix.mnbr <- as.numeric(readline(prompt=cat("Enter number for fixed method used on outliers: ","\n","1.dropBoth: drop the outliers from both sides of the data","\n","2.dropLeft: drop the outliers from left side of the data","\n","3.dropRight: drop the outliers from right side of the data","\n","4.asNABoth: convert outliers from both sides of the data as missing values","\n","5.asNALeft: convert outliers from left side of the data as missing values","\n","6.asNARight: convert outliers from right side of the data as missing values","\n","7.median: replace outliers with median","\n","8.mean: replace outliers with mean")))
      }
      fix.method <- c("dropBoth","dropLeft","dropRight","asNABoth","asNALeft","asNARight","median","mean")[fix.mnbr]

      # get the position of outliers
      outlierind <- univOutl::boxB(dt[,get(col_name)],k=k,method=identify.method,exclude=exclude,logt=logt)
      fix.name <- paste0(col_name,".","fixed")
      dt[,fix.name] <- dt[,get(col_name)]
      if(fix.method=="dprowBoth") dt <- dt[!outlierind$outliers,]
      if(fix.method=="dprowLeft") dt <- dt[!outlierind$lowOutl,]
      if(fix.method=="dprowRight") dt <- dt[!outlierind$upOutl,]
      if(fix.method=="asNABoth") dt[outlierind$outliers,(fix.name):=NA]
      if(fix.method=="asNALeft") dt[outlierind$lowOutl,(fix.name):=NA]
      if(fix.method=="asNARight") dt[outlierind$upOutl,(fix.name):=NA]
      if(fix.method=="median") dt[outlierind$outliers,(fix.name):=median(dt[!outlierind$outliers,get(fix.name)])]
      if(fix.method=="mean") dt[outlierind$outliers,(fix.name):=mean(dt[!outlierind$outliers,get(fix.name)])]

      # show plots before and after fixing outliers
      ap <- ggplot2::ggplot(dt, aes(x =get(fix.name))) + geom_histogram(aes(y = ..density..),binwidth = .5,colour = "blue", fill = "white")+geom_density(alpha = .2, fill="#FF6655")
      ap_box <- ggplot2::ggplot(dt, aes(get(fix.name))) + geom_boxplot(colour = "blue", fill = "white",outlier.colour = "red", outlier.shape = 1)
      ggpubr::ggarrange(bp,ap, bp_box,ap_box, labels=c("before","after","before","after"),ncol=2,nrow=2)


    }
    if(type=="categorical"){

  }




  }
  if(interactive==FALSE){

  }

  return(dt)
}

#-- test1
dt <- data.table(type = factor(rep(c("A","B"), each = 50)),
                 rating = c(rnorm(200),rnorm(200, mean=.6)))

outlierfix(dt,col_name="rating",type="continuous",rangeLU=NULL,k=1.5,exclude = NA,logt = FALSE)


#-- test2
dt = as.data.table(mtcars)
outlierfix(dt,col_name="wt",type="continuous",rangeLU=c(2.5,3.5),k=1.5,exclude = NA,logt = FALSE)


# Take below types of data into consideration:
# 1.numerical:(continuous)
# 2.categorical: nominal/ordinal/binary (discrete, no numeric relationship)
# 3.count
# 4.time
# type:c("continuous","categorical")
# fix:c("remove","nearestmd","meanfill")
# method:c("IQR")
#Outlier cleaning function ----
