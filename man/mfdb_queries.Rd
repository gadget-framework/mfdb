\name{mfdb_queries}
\alias{mfdb_area_size}
\alias{mfdb_temperature}
\alias{mfdb_stock_count}
\alias{mfdb_meanlength_stddev}
\alias{mfdb_meanlength}
\alias{mfdb_meanweight}
\alias{mfdb_meanweight_stddev}
\alias{mfdb_agelength}

\title{MareFrame DB queries}

\description{
    Aggregate data from the database in a variety of ways
}

\usage{
mfdb_temperature(mdb, params)
mfdb_stock_count(mdb, params)
mfdb_meanlength(mdb, params)
mfdb_meanlength_stddev(mdb, params)
mfdb_meanweight(mdb, params)
mfdb_meanweight_stddev(mdb, params)
mfdb_agelength(mdb, params)
}

\arguments{
    \item{mdb}{An object created by \code{mfdb}()}
    \item{params}{A list of parameters, see details.}
}

\details{
The options in \code{params} can both restrict data that is returned, and group
this data. To save specifying the same items repeatedly, you can use list
concatenation to keep some defaults, for example:

\preformatted{
defaults <- list(year = 1998:2000)
mfdb_meanlength(mdb, c(list(), defaults))
}

The following options will group the data:

\describe{
    \item{year}{
        (required) A vector of years, e.g. \code{c(2000,2001)}
    }
    \item{timestep}{
        (required) An mfdb_group representing which months you want to
        include in the data, and how to group them. For example,
        \code{mfdb_group_numbered("ts", 1:6, 7:12)}
        to group years by 2 timesteps, for each half of the year.
    }
    \item{area}{
        (required) An mfdb_group representing the area groupings you
        want to include and aggregate by. For example,
        \code{mfdb_group("101" = c(1011, 1012), "102" = c(1021, 1022, 1023))}
    }
    \item{age}{
        (required) An mfdb_group representing which years you want to
        include in the data, and how to group them. For example,
        \code{mfdb_group('young' = 1:3, 'old' = 4:6)}
        to group ages into "young" (up to 3), or "old" (4--6 years)
    }
    \item{length}{
        (required) An mfdb_interval representing which lengths to
        include in the data, and how to group them. For example,
        \code{mfdb_interval("len", seq(0, 500, by = 10))}
        to include lengths from 0--500, and group them into 0, 10, 20,...
        Note that in the case of \code{mfdb_meanlength}, there is no grouping
        by length (doesn't make sense), but this is still used to provide a
        minimum and maximum length.
    }
    \item{maturity_stage}{
        An mfdb_group of maturity stages, see mfdb::maturity_stage for possible values.
    }
}

In addition, you can use the following arguments to restrict data
returned as part of the query.

\describe{
    \item{institute}{A vector of institute names, see mfdb::institute for possible values}
    \item{gear}{A vector of gear names, see mfdb::gear for possible values}
    \item{vessel}{A vector of vessel names, see mfdb::vessel for possible values}
    \item{sampling_type}{A vector of sampling_type names, see mfdb::sampling_type for possible values}
    \item{species}{A vector of species names, see mfdb::species for possible values}
    \item{sex}{A vector of sex names, see mfdb::sex for possible values}
    \item{maturity_stage}{A vector of maturity stages, see mfdb::maturity_stage for possible values}
}
}

\value{
    All will return a list of data.frame objects. Precisely what columns
    these will contain depends on the function called.
    \describe{
        \item{mfdb_temperature}{
            Returns year, step, area, (mean) temperature
        }
        \item{mfdb_stock_count}{
            Returns year,step,area,stock,age,length,number (of samples)
        }
        \item{mfdb_meanlength}{
            Returns year,step,area,age,number (of samples),mean (length in group)
        }
        \item{mfdb_meanlength_stddev}{
            As mfdb_meanlength, but also returns std. deviation.
        }
        \item{mfdb_meanweight}{
            Returns year,step,area,age,number (of samples),mean (weight of group)
        }
        \item{mfdb_meanweight_stddev}{
            As mfdb_meanweight, but also returns std. deviation.
        }
        \item{mfdb_agelength}{
            Returns year,step,area,age,length,number (of samples)
        }
    }
}

\examples{\dontrun{
agg_data <- mfdb_agelength(mdb,
        params = c(list(
            year = c(1990, 1991),
            length = mfdb_interval("len", seq(0, 500, by = 30)),
            age = mfdb_group('young' = c(1,2,3)),
            null = NULL), opt_catch))
gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
        name = "cdist",
        weight = 0.9,
        data = agg_data[[1]],
        area = attr(agg_data[[1]], "area"),
        age = attr(agg_data[[1]], "age")))
}}
