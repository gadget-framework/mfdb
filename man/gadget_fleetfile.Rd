\name{gadget_fleetfile}
\alias{gadget_fleet_component}

\title{Gadget fleet files}
\description{
    Structures representing fleet file components
}

\usage{
gadget_fleet_component(type,
        name = type,
        livesonareas = unique(data$area),
        multiplicative = 1,
        suitability = NULL,
        fleetfile = 'fleet',
        data = stop("data not provided"),
        ...)
}

\arguments{
    \item{type}{
        Required. Type of fleet component to create, e.g. 'totalfleet'
    }
    \item{name}{
        Optional. A descriptive name for the fleet component, defaults to the type.
    }
    \item{livesonareas}{
        Optional. Vector of area names, defaults to all unique areas in data.
    }
    \item{multiplicative}{
        Optional. Defaults to 1
    }
    \item{suitability}{
        Optional. Defaults to empty string
    }
    \item{fleetfile}{
        Optional. The fleet file to put the component in. Defaults to 'fleet'.
    }
    \item{data}{
        Required. The data.frame to use for 'amountfile'. Areas are translated
        into integers before adding to amountfile.
    }
    \item{...}{
        Extra parameters for the component, see details
    }
}

\details{
\code{effortfleet} requires the following extra parameters:
\describe{
    \item{catchability}{A list of stock names to catchability constants}
}

\code{quotafleet} requires the following extra parameters:
\describe{
    \item{quotafunction}{Function name, e.g. 'simple'}
    \item{biomasslevel}{Vector of biomass levels}
    \item{quotalevel}{Vector of fishing levels}
}

}

\examples{\dontrun{
    # Write a 'totalfleet' component
    gadget_dir_write(gd, gadget_fleet_component(
        'totalfleet',
        name = 'igfs',
        data = mfdb_sample_count(mdb, c(), list(
            year = 1998,
            step = mfdb_timestep_yearly))[[1]]))

    gadget_dir_write(gd, gadget_fleet_component(
        'effortfleet',
        name = 'barry',
        suitability = "function constant 4;",
        catchability = list(stockA=4, stockB=5),
        quotafunction = 'simple',
        biomasslevel = c(1000, 2000),
        quotalevel = c(0.1, 0.4, 0.9),
        data = mfdb_sample_count(mdb, c(), list(
            year = 1998,
            step = mfdb_timestep_yearly))[[1]]))

    gadget_dir_write(gd, gadget_fleet_component(
        'quotafleet',
        name = 'barry',
        suitability = "function constant 4;",
        catchability = list(stockA=4, stockB=5),
        quotafunction = 'simple',
        biomasslevel = c(1000, 2000),
        quotalevel = c(0.1, 0.4, 0.9),
        data = mfdb_sample_count(mdb, c(), list(
            year = 1998,
            step = mfdb_timestep_yearly))[[1]]))
}}
