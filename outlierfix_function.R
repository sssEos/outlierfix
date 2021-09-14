outlierfix <- function(dt,col_name, type, rangeLU=NULL, k, exclude=NA, logt){
  if(is.element("data.table", class(dt))==FALSE) stop("Data class should be data.table")
  if(is.element(type,c("continuous","categorical"))==FALSE){
    stop('Data type is not accurate. Ensure the type to be continuous or categorical.')
  }

  if(type=="continuous"){
    # exclude the data that no-need as NA
    if(is.null(rangeLU)) rangeLU <- range(dt[,get(col_name)],na.rm=TRUE)
    dt <- dt[(get(col_name)<=rangeLU[2])&(get(col_name)>=rangeLU[1]),]

    # display the distribution for deciding which method to identify outliers
    print(ggplot(dt, aes(x =get(col_name))) + geom_histogram(aes(y = ..density..),binwidth = .5,colour = "blue", fill = "white")+geom_density(alpha = .2, fill="#FF6655"))

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
    outlierind <- univOutl::boxB(dt[,get(col_name)],k=k,method=identify.method,weights=weights,exclude=exclude,logt=logt)
    if(fix.method=="dropBoth") dt <- dt[!outlierind$outliers,]
    if(fix.method=="dropLeft") dt <- dt[!outlierind$lowOutl,]
    if(fix.method=="dropRight") dt <- dt[!outlierind$upOutl,]
    if(fix.method=="asNABoth") dt[outlierind$outliers,(col_name):=NA]
    if(fix.method=="asNALeft") dt[outlierind$lowOutl,(col_name):=NA]
    if(fix.method=="asNARight") dt[outlierind$upOutl,(col_name):=NA]
    if(fix.method=="median") dt[outlierind$outliers,(col_name):=median(dt[!outlierind$outliers,get(col_name)])]
    if(fix.method=="mean") dt[outlierind$outliers,(col_name):=mean(dt[!outlierind$outliers,get(col_name)])]
   }
  if(type=="categorical"){



  }

  return(dt)
}

#-- test1
dt <- data.table(type = factor(rep(c("A","B"), each = 50)),
                 rating = c(rnorm(200),rnorm(200, mean=.6)))

outlierfix(dt,col_name="rating",type="continuous",rangeLU=NULL,k=1.5,exclude = NA,logt = FALSE)


#-- test2
dt = as.data.table(mtcars)
outlierfix(dt,col_name="wt",type="continuous",rangeLU=NULL,k=1.5,exclude = NA,logt = FALSE)
