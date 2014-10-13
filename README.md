MareFrame DB Access library
===========================

Prerequisites
-------------

You need to have PostgreSQL installed and running on your computer. To do this,
either install from http://postgresql.org or use yum/apt-get to install
postgresql-server under Linux.

If you don't want to use a system-wide database, then look into the
mfdb-workspace repository, which will keep all required R dependencies and the
PostgreSQL database in the local directory.

Installing
----------

You can use devtools to install this directly:

    # install.packages("devtools")
    devtools::install_github("mareframe/mfdb")

You will also need to create a database ideally called ``mf``. For information
on how to do this, see: http://www.postgresql.org/docs/9.3/static/tutorial-createdb.html

Using
-----

Before doing anything, you (or your R script) will need to connect to the
database:

    > mdb <- mfdb()

See the help entry for the ``mfdb`` function for more information.

There are also a selection of example scripts to read through in the ``demo/``
folder.

Acknowledgements
----------------

This project has received funding from the European Union’s Seventh Framework
Programme for research, technological development and demonstration under grant
agreement no.613571.
