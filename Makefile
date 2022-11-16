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

release: release-description release-news
	git commit -m "Release version $(NEW_VERSION)" DESCRIPTION NEWS.md
	git tag -am "Release version $(NEW_VERSION)" v$(NEW_VERSION)
	#
	R CMD build .
	#
	sed -i 's/^Version: .*/Version: '"$(NEW_VERSION)-999"'/' DESCRIPTION
	git commit -m "Development version $(NEW_VERSION)-999" DESCRIPTION

release-description:
	[ -n "$(NEW_VERSION)" ]  # NEW_VERSION variable should be set
	sed -i 's/^Version: .*/Version: $(NEW_VERSION)/' DESCRIPTION
	sed -i "s/^Date: .*/Date: $$(date +%Y-%m-%d)/" DESCRIPTION
	sed -i 's/^Depends: R .*/Depends: R (>= $(shell curl -s https://api.r-hub.io/rversions/r-oldrel/3 | grep -oiE '"version":"[0-9.]+"' | grep -oE '[0-9]+\.[0-9]+\.')0)/' DESCRIPTION

release-news:
	[ -n "$(NEW_VERSION)" ]  # NEW_VERSION variable should be set
	mv NEWS.md NEWS.md.o
	head -1 NEWS.md.o | grep -E '^\# $(PACKAGE) ' || /bin/echo -e "# $(PACKAGE) $(NEW_VERSION):\n" > NEWS.md
	cat NEWS.md.o >> NEWS.md
	rm NEWS.md.o

.PHONY: all install full-install build check check-as-cran wincheck examples vignettes serve-docs test inttest coverage release release-description release-news
