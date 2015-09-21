\name{mfdb_helpers}
\alias{mfdb_find_species}

\title{MareFrame tools & helpers}
\description{
    Misc. functions to aid working with an MFDB database.
}

\usage{
# Find species from abbreviated names
mfdb_find_species(partial_name)
}
\arguments{
    \item{partial_name}{
         Vector of partial species names, e.g. "Gad Mor", "gad. Mor.",
         "Gadus Mor", will all match "Cod (Gadus Morhua)".
    }
}

\value{
A matrix of all potential id, name & descriptions for each item in
\code{partial_name}.
}

\examples{
mfdb_find_species(c("gad mor", "tube worms"))
#             gad mor              tube worms              
# id          8791030402           1e+10                   
# name        "COD"                "TBX"                   
# description "Cod (Gadus Morhua)" "Tube Worms (Tubeworms)"

# Can also generate a map to help insert a data.frame of foreign data
\dontrun{
# Work out a map from all Prey_Species_Name values to MFDB species codes
species_map <- mfdb_find_species(levels(stomachs$Prey_Species_Name))['name',]
# Only include data where there is one possible solution
species_map <- vapply(species_map, function (names) if(length(names) == 1) names else as.character(NA), "")

# Can then remap the species column with:
plyr::revalue(stomachs$Prey_Species_Name, species_map)
}}
