#!/bin/sh
# Script to release package to CRAN.
# Run ./do-release.sh (new-version), and upload the produced tarball to:
#     https://cran.r-project.org/submit.html
exec make release NEW_VERSION="${1-}"
