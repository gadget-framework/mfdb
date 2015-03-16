\name{gadget_directory}
\alias{gadget_directory}
\alias{gadget_dir_write}
\alias{gadget_dir_read}

\title{Gadget directory objects}
\description{
    Structures representing a directory of data files
}

\usage{
gadget_directory(dir, mainfile = "main")
gadget_dir_write(gd, obj)
gadget_dir_read(gd, file_name, missing_okay = TRUE, file_type = c())
}

\arguments{
    \item{dir}{
        Name of directory, will be created if it doesn't exist.
    }
    \item{mainfile}{
        Name of the GADGET mainfile to use.
    }
    \item{gd}{
        A \code{gadget_directory} object.
    }
    \item{obj}{
        The \code{gadget_file}, or \code{gadget_likelihood_component} to write.
    }
    \item{file_name}{
        File to read out of the directory and turn into a \code{gadget_file}.
    }
    \item{missing_okay}{
        If true, return an empty file instead of complaining that the given
        file does not exist.
    }
    \item{file_type}{
        A character vector that alters how the file is parsed. Currently either
        NULL or "bare_component", which implies we write "something" instead of
        "[something]".
    }
}

\details{
This handles 
}

\examples{\dontrun{
# Create a gadget directory
gd <- gadget_directory('./out')

# Read in the likelihood file
likelihood <- gadget_dir_read(gd, 'likelihood')

# Write out an area file to "./out/areas", replacing any existing file
gadget_dir_write(gd, gadget_file("areas", components = list(list(north = 1:3, south = 4:7))))

# Replace a likelihood component if one already exists with
# the same name/type or append it to the bottom
gadget_dir_write(gd, gadget_likelihood_component("understocking", name = "frank"))
}}
