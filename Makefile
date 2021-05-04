PACKAGE=$(shell awk '/^Package: / { print $$2 }' DESCRIPTION)
VERSION=$(shell awk '/^Version: / { print $$2 }' DESCRIPTION)
TARBALL=$(PACKAGE)_$(VERSION).tar.gz

all: test inttest check-as-cran

install:
	R CMD INSTALL --install-tests --html --example .

# Some things aren't installed by "make install", vignettes for example.
# This is slower, but more accurate.
full-install: build
	R CMD INSTALL --install-tests --html --example "$(TARBALL)"

build:
	R CMD build .

check: build
	LANGUAGE="en" MFDB_FORCE_AVAILABLE="y" R --vanilla --slave CMD check "$(TARBALL)"

check-as-cran: build
	MFDB_FORCE_AVAILABLE="y" R --vanilla --slave CMD check --as-cran "$(TARBALL)"

wincheck: build
	# See https://win-builder.r-project.org/ for more information
	curl --no-epsv -# -T "$(TARBALL)" ftp://win-builder.r-project.org/R-devel/

test: install
	for f in tests/test-*.R; do echo "=== $$f ============="; Rscript $$f || exit 1; done

examples: install
	# Destroy schemas first to have clear environment
	Rscript -e 'library(mfdb) ; tryCatch(mfdb("examples", destroy_schema = TRUE), error = function (x) x)'
	Rscript -e 'library(mfdb) ; tryCatch(mfdb("examples-copy", destroy_schema = TRUE), error = function (x) x)'
	Rscript -e 'library(mfdb) ; tryCatch(mfdb("examples-import-data", destroy_schema = TRUE), error = function (x) x)'
	Rscript -e 'devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE, document = FALSE)'

inttest-sqlite: install 
	for f in */inttest-*.R; do echo "=== $$f (sqlite) ============="; INTTEST_SCHEMA="/tmp/mf-inttest.sqlite" Rscript $$f || exit 1; done

inttest-postgres: install
	for f in */inttest-*.R; do echo "=== $$f (pg) ============="; INTTEST_SCHEMA="inttest" Rscript $$f || exit 1; done

inttest: test examples build-docs inttest-sqlite inttest-postgres

build-docs:
	[ -d docs ] && rm -r docs || true
	echo 'pkgdown::build_site()' | R --vanilla

.PHONY: all install build check check-as-cran wincheck examples inttest-sqlite inttest-postgres inttest build-docs
