PACKAGE=$(shell awk '/^Package: / { print $$2 }' DESCRIPTION)
VERSION=$(shell awk '/^Version: / { print $$2 }' DESCRIPTION)
TARBALL=$(PACKAGE)_$(VERSION).tar.gz

all:
	make test
	make inttest INTTEST_SCHEMA="/tmp/mf-inttest.sqlite"
	make inttest INTTEST_SCHEMA="/tmp/mf-inttest.duckdb"
	make inttest INTTEST_SCHEMA="inttest"
	make check-as-cran

install:
	R CMD INSTALL --install-tests --html --example .

# Some things aren't installed by "make install", vignettes for example.
# This is slower, but more accurate.
full-install: build
	R CMD INSTALL --install-tests --html --example "$(TARBALL)"

build:
	R CMD build .

check: build
	R CMD check "$(TARBALL)"

check-as-cran: build
	R CMD check --as-cran "$(TARBALL)"

wincheck: build
	# See https://win-builder.r-project.org/ for more information
	curl --no-epsv -# -T "$(TARBALL)" ftp://win-builder.r-project.org/R-devel/

examples: install
	# Destroy schemas first to have clear environment
	Rscript -e 'library(mfdb) ; tryCatch(mfdb("examples", destroy_schema = TRUE), error = function (x) x)'
	Rscript -e 'library(mfdb) ; tryCatch(mfdb("examples-copy", destroy_schema = TRUE), error = function (x) x)'
	Rscript -e 'library(mfdb) ; tryCatch(mfdb("examples-import-data", destroy_schema = TRUE), error = function (x) x)'
	Rscript -e 'devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE, document = FALSE)'

vignettes: install
	Rscript -e 'tools::buildVignettes(dir=".")'

serve-docs:
	[ -d docs ] && rm -r docs || true
	Rscript --vanilla -e "pkgdown::build_site() ; servr::httd(dir='docs', host='0.0.0.0', port='8000')"

test: install
	for f in tests/test-*.R; do echo "=== $$f ============="; Rscript $$f || exit 1; done

inttest: install
	for f in */inttest-*.R; do echo "=== $$f ============="; Rscript $$f || exit 1; done

coverage:
	R --vanilla -e 'covr::package_coverage(type = "all", line_exclusions = list())'

.PHONY: all install full-install build check check-as-cran wincheck examples vignettes serve-docs test inttest coverage
