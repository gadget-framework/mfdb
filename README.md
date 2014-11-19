MareFrame DB Access package
===========================

This package enables automated processing of fisheries data into suitable forms
for running ecosystem models against it, e.g. `GADGET <http://www.hafro.is/gadget/>`__.

This package contains several distinct sets of functions:

* Create a PostgreSQL database schema (``mfdb`` function)
* Import data into a PostgreSQL database (``mfdb_import_*`` functions)
* Sample / group data from database (other ``mfdb_*`` functions)
* Manage GADGET model directories and export data into them (``gadget_*`` functions)

Using this, you can install PostgreSQL locally and have a script automating the
process of:

1. Importing data from your sources
2. Uploading into your local MareFrame database
3. Sampling / grouping this data
4. Producing set of GADGET model files ready to be run by GADGET

Also, this libary can be used to connect to a remote database and generate
model files from that data.

This work is based on it's predecessor, `DST^2 <http://www.hafro.is/dst2/>`__.

Prerequisites
-------------

Besides R, you also need to have PostgreSQL installed and running on your computer.

Linux (Debian / Ubuntu)
^^^^^^^^^^^^^^^^^^^^^^^

Install the ``postgresql`` package using:

    apt-get install postgresql

Some additional instructions are available here: https://wiki.debian.org/PostgreSql

If you don't want to use a system-wide database, then investigate https://github.com/mareframe/mfdb-workspace
which keeps all the required R dependencies and PostgreSQL database in the local directory.

Otherwise create a database called ``mf`` as per the distribution instructions.

Linux (Redhat / Fedora)
^^^^^^^^^^^^^^^^^^^^^^^

Install the ``postgresql-server`` package using:

    yum install postgresql-server

Some additional instructions are available here: https://fedoraproject.org/wiki/PostgreSQL

If you don't want to use a system-wide database, then investigate https://github.com/mareframe/mfdb-workspace
which keeps all the required R dependencies and PostgreSQL database in the local directory.

Otherwise create a database called ``mf`` as per the distribution instructions.

Microsoft Windows
^^^^^^^^^^^^^^^^^

Download the latest database installer from here:

http://www.enterprisedb.com/products-services-training/pgdownload#windows

Create a database called ``mf``. http://www.postgresql.org/docs/9.3/static/tutorial-createdb.html

Apple OS X
^^^^^^^^^^

Install using http://postgresapp.com/

Create a database called ``mf``.

Installing
----------

You can use devtools to install this directly:

    # install.packages("devtools")
    devtools::install_github("mareframe/mfdb")

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
