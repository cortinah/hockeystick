## Test environments:
* local Windows install, R 4.2.2
* GitHub Actions: ubuntu-latest (oldrel-1, release, and devel), macOS-latest (release), windows-latest (release) 

## R CMD check results
There were no ERRORs,  WARNINGs, or notes.

## Persistent datasets
By default NO files are written to the local filesystem.
If user chooses to cache data locally with (write_cache=TRUE option) then location based on the rappdirs package is used, following the ROpenSci Persistent config and data for R packages recommendations.

## Internet connectivity
Since version 0.6.3 tests for connectivity before initiating transfers to conform with CRAN rules.
