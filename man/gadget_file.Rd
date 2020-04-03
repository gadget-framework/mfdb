\name{gadget_file}
\alias{gadget_file}
\alias{print.gadget_file}
\alias{as.character.gadget_file}
\alias{read.gadget_file}

\title{Gadget file objects}
\description{
    Structures representing an individual GADGET data file.
}

\usage{
gadget_file(file_name, components = list(), data = NULL, file_type = c())
\method{print}{gadget_file}(x, ...)
\method{as.character}{gadget_file}(x, ...)
read.gadget_file(file_name, file_type = c(), fileEncoding = "UTF-8")
}

\arguments{
    \item{file_name}{
        Filename the output should be written to / read from
    }
    \item{components}{
        A list of lists, representing each component. See details.
    }
    \item{data}{
        A \code{data.frame} representing the tabular data at the end of a file.
    }
    \item{file_type}{
        A character vector that alters how the file is parsed. Currently either
        NULL or "bare_component", which implies we write "something" instead of
        "[something]".
    }
    \item{x}{gadget_file object}
    \item{fileEncoding}{File's characterset. Defaults to UTF-8}
    \item{...}{Unused}
}

\details{
For our purposes, a gadget file is broken down into components, where the first
component is any key/value data at the top of the file. Each section separated
by "[something]" is considered a new component. Each component is a list of key
/values, where values can be vectors of multiple values. Also components can
have comments prepended by adding a "preamble" attribute.

In slight deviation to GADGET spec, we insist that tabular data begins with
"; -- data --", to avoid any ambiguity on when it starts.
}

\value{
\code{gadget_file} Returns a gadget_file object, a list of components.

\code{print.gadget_file} Prints the gadget file as it would be written to the
filesystem.

\code{as.character.gadget_file} Returns a character string of the gadget file
as it would be written to the filesystem.

\code{read.gadget_file} Returns a gadget_file object, a list of components.
}

\examples{
# Simple key/values
gadget_file("age", components = list(
    list(length = 5, age = 1:5)))

# Multiple components
gadget_file("likelihood", components = list(
    list(),
    component = structure(list(type = "penalty"), preamble = list("comment")),
    component = structure(list(type = "penalty"), preamble = list("", "another comment"))))

# Data
gadget_file("agelen", components = list(
    list(stocknames = "cod")), data = data.frame(
    area = c(102, 103),
    number = c(2345, 5023)))
}
