#' @title fixout_conti
#' @description This function fix outliers in continuous data. It assumes that the columns containing continuous numbers in data table and fix the column without changing original data set with chosen methods accordingly.
#'
#' @param dt A variable shows the name of data table.
#' @param col_name A string indicates column name with data need to be fixed.
#' @param interactive TRUE/FALSE, whether to fix the outliers in an interactive way.
#' @param iden.m A string shows the chosen method for identifying outliers."resistant": standard boxplot; "asymmetric": modification of standard method to deal with (moderately) skewed data;"adjbox": adjusted boxplot for skewed distributions;"winsorize": the smallest and/or the largest values are replaced by less extreme values.
#' @param fix.m A string shows the chosen method for fixing outliers. "dropBoth": drop the whole row where outliers are from both sides of the data; "dropLeft": drop the whole row where outliers are from left side of the data; "dropRight": drop the whole row where outliers are from right side of the data; "asNABoth": convert outliers from both sides of the data as missing values; "asNALeft": convert outliers from left side of the data as missing values,"asNARight": convert outliers from right side of the data as missing values; "median": replace outliers with median; "mean": replace outliers with mean.
#' @param rangLU The lower and upper limit of range for the data.
#' @param k A constant to determine the lines outside the upper and lower quartiles.
#' @param exclude Values that will be excluded in the numbers to be processed. Missing values are removed by default when detecting outliers.
#' @param logt TRUE/FALSE, whether the numbers are transformed with an lognormal distribution.
#' @return A data table contains the fixed column.
#' @examples
#' data <- as.data.table(mtcars)
#' output_table <- fixout_conti(data,col_name="wt",rangeLU=c(2.5,3.5))
#' @export
fixout_conti <-
  function(dt,
           col_name,
           interactive = FALSE,
           iden.m = "resistant",
           fix.m = "asNABoth",
           rangeLU = NULL,
           k = 1.5,
           exclude = NA,
           logt = FALSE) {
    if (!require("pacman"))
      install.packages("pacman")
    pacman::p_load(data.table, univOutl, base, ggplot2, ggpubr, DescTools)
    # check dataset is datatable
    # if (is.element("data.table", class(dt)) == FALSE)
    dt = as.data.table(dt)
    # overwirte ----detect type
    if (length(unique(dt[, ..col_name])) < 10) {
      warning("The data are highly likely not continuous.")
    }
    if (interactive == TRUE) {
      # exclude the data that no-need as NA
      # show error if not logical
      if (is.null(rangeLU))
        rangeLU <- c(-Inf, Inf)
      fix.name <- paste0(col_name, ".", "fixed")
      dt[, fix.name] <- dt[, ..col_name]
      # assign exclude as NA
      dt[get(fix.name) %in% exclude, (fix.name) := NA]

      dt[(get(col_name) >= rangeLU[2]) |
           (get(col_name) <= rangeLU[1]), (fix.name) := NA]

      # display the distribution for deciding which method to identify outliers
      bp <-
        ggplot2::ggplot(dt, aes(x = get(col_name))) + geom_histogram(
          aes(y = ..density..),
          binwidth = .5,
          colour = "blue",
          fill = "white"
        ) + geom_density(alpha = .2, fill = "#FF6655") + labs(x = "histogram")
      bp_box <-
        ggplot2::ggplot(dt, aes(get(col_name))) + geom_boxplot(
          colour = "blue",
          fill = "white",
          outlier.colour = "red",
          outlier.shape = 1
        ) + labs(x = "boxplot")
      print(ggpubr::ggarrange(
        bp,
        bp_box,
        labels = c("histogram", "boxplot"),
        ncol = 1,
        nrow = 2
      ))

      # enter the methods to be used
      iden.mnbr <-
        as.numeric(readline(
          prompt = cat(
            "Enter number for identified method used on outliers: ",
            "\n",
            "1.resistant: standard boxplot fences",
            "\n",
            "2.asymmetric: modification of standard method to deal with (moderately) skewed data",
            "\n",
            "3.adjbox: adjusted boxplot for skewed distributions",
            "\n",
            "4.winsorize: the smallest and/or the largest values are replaced by less extreme values"
          )
        ))
      while (is.null(iden.mnbr) | !is.numeric(iden.mnbr)) {
        iden.mnbr <-
          as.numeric(readline(
            prompt = cat(
              "Enter number for identified method used on outliers: ",
              "\n",
              "1.resistant: standard boxplot fences",
              "\n",
              "2.asymmetric: modification of standard method to deal with (moderately) skewed data",
              "\n",
              "3.adjbox: adjusted boxplot for skewed distributions",
              "\n",
              "4.winsorize: the smallest and/or the largest values are replaced by less extreme values"
            )
          ))
      }

      if (iden.mnbr %in% c(1:3)) {
        # enter the fixed methods
        fix.mnbr <-
          as.numeric(readline(
            prompt = cat(
              "Enter number for fixed method used on outliers: ",
              "\n",
              "1.dropBoth: drop the whole row where outliers are from both sides of the data",
              "\n",
              "2.dropLeft: drop the whole row where outliers are from left side of the data",
              "\n",
              "3.dropRight: drop the whole row where outliers are from right side of the data",
              "\n",
              "4.asNABoth: convert outliers from both sides of the data as missing values",
              "\n",
              "5.asNALeft: convert outliers from left side of the data as missing values",
              "\n",
              "6.asNARight: convert outliers from right side of the data as missing values",
              "\n",
              "7.median: replace outliers with median",
              "\n",
              "8.mean: replace outliers with mean"
            )
          ))
        while (is.null(fix.mnbr) | !is.numeric(fix.mnbr)) {
          fix.mnbr <-
            as.numeric(readline(
              prompt = cat(
                "Enter number for fixed method used on outliers: ",
                "\n",
                "1.dropBoth: drop the whole row where outliers are from both sides of the data",
                "\n",
                "2.dropLeft: drop the whole row where outliers are from left side of the data",
                "\n",
                "3.dropRight: drop the whole row where outliers are from right side of the data",
                "\n",
                "4.asNABoth: convert outliers from both sides of the data as missing values",
                "\n",
                "5.asNALeft: convert outliers from left side of the data as missing values",
                "\n",
                "6.asNARight: convert outliers from right side of the data as missing values",
                "\n",
                "7.median: replace outliers with median",
                "\n",
                "8.mean: replace outliers with mean"
              )
            ))
        }
        fix.method <-
          c(
            "dropBoth",
            "dropLeft",
            "dropRight",
            "asNABoth",
            "asNALeft",
            "asNARight",
            "median",
            "mean"
          )[fix.mnbr]

        # get the position of outliers and fix
        identify.method <-
          c("resistant", "asymmetric", "adjbox")[iden.mnbr]
        outlierind <-
          univOutl::boxB(
            dt[, get(col_name)],
            k = k,
            method = identify.method,
            exclude = exclude,
            logt = logt
          )

        if (fix.method == "dprowBoth")
          dt <- dt[!outlierind$outliers,]
        if (fix.method == "dprowLeft")
          dt <- dt[!outlierind$lowOutl,]
        if (fix.method == "dprowRight")
          dt <- dt[!outlierind$upOutl,]
        if (fix.method == "asNABoth")
          dt[outlierind$outliers, (fix.name) := NA]
        if (fix.method == "asNALeft")
          dt[outlierind$lowOutl, (fix.name) := NA]
        if (fix.method == "asNARight")
          dt[outlierind$upOutl, (fix.name) := NA]
        if (fix.method == "median")
          dt[outlierind$outliers, (fix.name) := median(dt[!outlierind$outliers, get(fix.name)])]
        if (fix.method == "mean")
          dt[outlierind$outliers, (fix.name) := mean(dt[!outlierind$outliers, get(fix.name)])]


      }
      if (iden.mnbr == 4) {
        # # convert values in exclude vector as NA
        # dt[get(fix.name)%in%exclude,(fix.name):=NA]
        dt[, (fix.name) := DescTools::Winsorize(dt[, get(fix.name)], na.rm = T)]
      }

      # show plots before and after fixing outliers
      ap <-
        ggplot2::ggplot(dt, aes(x = get(fix.name))) + geom_histogram(
          aes(y = ..density..),
          binwidth = .5,
          colour = "blue",
          fill = "white"
        ) + geom_density(alpha = .2, fill = "#FF6655") + labs(x = "fix.histogram")
      ap_box <-
        ggplot2::ggplot(dt, aes(get(fix.name))) + geom_boxplot(
          colour = "blue",
          fill = "white",
          outlier.colour = "red",
          outlier.shape = 1
        ) + labs(x = "fix.boxplot")
      print(ggpubr::ggarrange(
        bp,
        ap,
        bp_box,
        ap_box,
        labels = c("before", "after", "before", "after"),
        ncol = 2,
        nrow = 2
      ))
    }
    if (interactive == FALSE) {
      if (is.null(rangeLU))
        rangeLU <- c(-Inf, Inf)
      fix.name <- paste0(col_name, ".", "fixed")
      dt[, fix.name] <- dt[, ..col_name]
      dt[get(fix.name) %in% exclude, (fix.name) := NA]
      dt[(get(col_name) >= rangeLU[2]) |
           (get(col_name) <= rangeLU[1]), (fix.name) := NA]

      bp <-
        ggplot2::ggplot(dt, aes(x = get(col_name))) + geom_histogram(
          aes(y = ..density..),
          binwidth = .5,
          colour = "blue",
          fill = "white"
        ) + geom_density(alpha = .2, fill = "#FF6655") + labs(x = "histogram")
      bp_box <-
        ggplot2::ggplot(dt, aes(get(col_name))) + geom_boxplot(
          colour = "blue",
          fill = "white",
          outlier.colour = "red",
          outlier.shape = 1
        ) + labs(x = "boxplot")

      if (iden.m %in% c("resistant", "asymmetric", "adjbox")) {
        outlierind <-
          univOutl::boxB(
            dt[, get(col_name)],
            k = k,
            method = iden.m,
            exclude = exclude,
            logt = logt
          )

        if (fix.m == "dprowBoth")
          dt <- dt[!outlierind$outliers,]
        if (fix.m == "dprowLeft")
          dt <- dt[!outlierind$lowOutl,]
        if (fix.m == "dprowRight")
          dt <- dt[!outlierind$upOutl,]
        if (fix.m == "asNABoth")
          dt[outlierind$outliers, (fix.name) := NA]
        if (fix.m == "asNALeft")
          dt[outlierind$lowOutl, (fix.name) := NA]
        if (fix.m == "asNARight")
          dt[outlierind$upOutl, (fix.name) := NA]
        if (fix.m == "median")
          dt[outlierind$outliers, (fix.name) := median(dt[!outlierind$outliers, get(fix.name)])]
        if (fix.m == "mean")
          dt[outlierind$outliers, (fix.name) := mean(dt[!outlierind$outliers, get(fix.name)])]
      }
      if (iden.m == "winsorize") {
        # dt[get(fix.name)%in%exclude,(fix.name):=NA]
        dt[, (fix.name) := DescTools::Winsorize(dt[, get(fix.name)], na.rm = T)]
      }

      ap <-
        ggplot2::ggplot(dt, aes(x = get(fix.name))) + geom_histogram(
          aes(y = ..density..),
          binwidth = .5,
          colour = "blue",
          fill = "white"
        ) + geom_density(alpha = .2, fill = "#FF6655") + labs(x = "fix.histogram")
      ap_box <-
        ggplot2::ggplot(dt, aes(get(fix.name))) + geom_boxplot(
          colour = "blue",
          fill = "white",
          outlier.colour = "red",
          outlier.shape = 1
        ) + labs(x = "fix.boxplot")
      print(ggpubr::ggarrange(
        bp,
        ap,
        bp_box,
        ap_box,
        labels = c("before", "after", "before", "after"),
        ncol = 2,
        nrow = 2
      ))


    }

    dt
  }


