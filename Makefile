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
	for f in tests/test-*.R; do echo "=== $$f ============="; Rscript $$f || break; done

inttest: install test
	Rscript -e 'devtools::run_examples(test = TRUE, run = TRUE, document = FALSE)'
	for f in demo/inttest-*.R; do echo "=== $$f ============="; Rscript $$f || break; done

gh-pages:
        # To start:
        # sh /usr/share/doc/git/contrib/workdir/git-new-workdir . docs master
        # git -C docs checkout --orphan gh-pages
        # git -C docs rm -rf .
        # (build & commit)
	[ -d docs ] && rm -r docs || true
	sh /usr/share/doc/git/contrib/workdir/git-new-workdir . docs gh-pages
	echo 'pkgdown::build_site()' | R --vanilla
	cd docs/ && git diff
	[ -n "$(GH_COMMIT)" ] && ( cd docs/ && git add -A . && git commit -m "Docs for $(shell git rev-parse --short HEAD)" ) || true
	[ -d docs ] && rm -r docs

.PHONY: all install build check check-as-cran wincheck inttest
