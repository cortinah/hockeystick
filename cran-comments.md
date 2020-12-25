## Test environments:
* local Windows install, R 4.0.3
* GitHub Actions: Ubuntu 20.04 (R release and devel), macOS-latest (release), windows-latest (release) 

## R CMD check results
There were no ERRORs,  WARNINGs, or notes.

## Persistent datasets
By default NO files are written to the local filesystem.
If user chooses to cache data localy with (write_cache=TRUE option) then location based on the rappdirs package is used, following the ROpenSci Persistent config and data for R packages recommendations.
