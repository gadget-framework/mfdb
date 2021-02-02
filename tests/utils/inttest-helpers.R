db_params <- list()

# NB: Sys.timezone() will produce spurious warnings on first call depending on environment:
# > Sys.timezone()
# Failed to create bus connection: No such file or directory
# [1] "Europe/London"
# Warning message:
# In system("timedatectl", intern = TRUE) :
#   running command 'timedatectl' had status 1
# https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17421
suppressWarnings(Sys.timezone())

# logging::setLevel("FINEST")
