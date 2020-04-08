\name{mfdb_helpers}
\alias{mfdb_find_species}

\title{MareFrame tools & helpers}
\description{
    Misc. functions to aid working with an MFDB database.
}

\usage{
# Find species from abbreviated names
mfdb_find_species(partial_name, single_matches_only = FALSE)
}
\arguments{
    \item{partial_name}{
         Vector of partial species names, e.g. "Gad Mor", "gad. Mor.",
         "Gadus Mor", will all match "Cod (Gadus Morhua)".
    }
    \item{single_matches_only}{
        Logical, default FALSE. If true, return NA for
        partial_names with multiple or zero matches.
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
stomachs <- read.csv(text = '
stomach_name,species,digestion_stage,length,weight,count
A,Palaemon Elegans,1,1,10,5
A,Palaemon Elegans,1,4,40,1
B,Palaemon Elegans,1,1,10,5
B,Palaemon Elegans,4,1,10,5
B,Palaemon Elegans,5,1,10,NA
B,Palaemon Elegans,5,1,10,NA
C,Crangon Crangon,2,3.5,9.5,3
D,Palaemon Elegans,1,1.4,10,1
D,Crangon Crangon,5,4,40,1
E,Worms,1,1.4,10,1
', stringsAsFactors = TRUE)

# Work out a map from all Prey_Species_Name values to MFDB species codes
species_map <- mfdb_find_species(levels(stomachs$species), single_matches_only = TRUE)['name',]

# Put the new levels back onto the species column
levels(stomachs$species) <- unlist(species_map)

stomachs
}
