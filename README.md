MareFrame DB Access package
===========================

[![Build Status](https://travis-ci.org/mareframe/mfdb.svg?branch=6.x)](https://travis-ci.org/mareframe/mfdb)

This package enables automated processing of fisheries data into suitable forms
for running ecosystem models against it, e.g. [GADGET](http://www.hafro.is/gadget/).

This package contains several distinct sets of functions:

* Create and connect to a PostgreSQL database schema (``mfdb`` function)
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

This work is based on it's predecessor, [DST^2](http://www.hafro.is/dst2/).

Prerequisites
-------------

Besides R, you will also need RPostgreSQL set up to access the database, and
PostgreSQL installed if you want to run the database locally too.

### Linux (Debian / Ubuntu)

Installation of RPostgreSQL will happen automatically, however you need some
PostgreSQL libraries before it will work:

    apt-get install libpq-dev

Also, you can install the ``postgresql`` package using:

    apt-get install postgresql

Some additional instructions are available here: https://wiki.debian.org/PostgreSql

Now, create an ``mf`` database that the user on your computer owns:

    $ echo $USER  # Check to see who you are, I'm lentinj
    lentinj
    $ su
    # su - postgres
    $ psql
    postgres=# CREATE USER lentinj
    postgres=# CREATE DATABASE mf OWNER lentinj;

If you don't want to use a system-wide database, then investigate https://github.com/mareframe/mfdb-workspace
which keeps all the required R dependencies and PostgreSQL database in the local directory.

### Linux (Redhat / Fedora)

Installation of RPostgreSQL will happen automatically, however you need some
PostgreSQL libraries before it will work:

    yum install postgresql-devel

Also, you can install the ``postgresql`` package using:

    yum install postgresql-server

Some additional instructions are available here: https://fedoraproject.org/wiki/PostgreSQL and some tips when troubleshooting CentOS installations can be found at: http://stackoverflow.com/questions/17633422/psql-fatal-database-user-does-not-exist

Now, create an ``mf`` database that the user on your computer owns:

    $ echo $USER  # Check to see who you are, I'm lentinj
    lentinj
    $ su
    # su - postgres
    $ psql
    postgres=# CREATE USER lentinj
    postgres=# CREATE DATABASE mf OWNER lentinj;

If you don't want to use a system-wide database, then investigate https://github.com/mareframe/mfdb-workspace
which keeps all the required R dependencies and PostgreSQL database in the local directory.

### Microsoft Windows

If you can use the binary RPostgreSQL packages, then you do not need to install
anything else. See https://code.google.com/p/rpostgresql/w/list for more information.

To install PostgreSQL, download the latest database installer from here:

http://www.enterprisedb.com/products-services-training/pgdownload#windows

Find the psql shell in the start menu, and create both an ``mf`` database and user:

    postgres=# CREATE USER mf PASSWORD 'mf';
    postgres=# CREATE DATABASE mf OWNER mf;

### Apple OS X

Install using http://postgresapp.com/

Create a database called ``mf``.

Installing
----------

You can use devtools to install this directly:

    # install.packages("devtools")
    devtools::install_github("mareframe/mfdb", ref = "5.x")

Or without:-

    # install.packages("downloader")
    pkg_file <- tempfile()
    downloader::download(url =
        'https://github.com/mareframe/mfdb/archive/5.x.tar.gz',
        mode = 'wb', destfile = pkg_file)
    install.packages(pkg_file, repos = NULL, type = 'source')

This should install and/or update dependencies, such as DBI and RPostgreSQL.

Note that you may see the following error:

    Error in get(Info[i, 1], envir = env) : 
      lazy-load database '.../Rpackages/mfdb/R/mfdb.rdb' is corrupt

This is R failing to re-load the package in an existing R session, however the
package is installed. Restart your R session and everything should be fine.

Using
-----

For an introduction to the package, read the ``package?mfdb`` help file in R.
[This is also available online](http://mareframe.github.io/packages/mfdb/html/mfdb-package.html).

There are a selection of example scripts in the ``demo/`` folder. The
``example-*`` scripts show the full process of importing data from specified
sources into the database, then querying this to aggregate into various gadget
model files. The ``inttest-*`` scripts demonstrate other aspects, and also
function as tests to ensure that the library works correctly (thus the ``ok()``
function calls).

Models using MFDB
-----------------

* https://github.com/fishvice/gadget-models

Acknowledgements
----------------

This project has received funding from the European Union’s Seventh Framework
Programme for research, technological development and demonstration under grant
agreement no.613571.
