## Resubmission
This is a resubmission. In this version I have:

* fixed bug where STRING fields with escaped characters would get incorrectly written to a fwf file.

## Test environments
* local Ubuntu docker container, R-devel
* Remote Windows server 2022, R-devel (devtools check_win_devel)
* remote mac-os-latest (release) (R-lib actions)
* remote windows-latest (release) (R-lib actions)
* remote ubuntu-latest (release) (R-lib actions)
* remote ubuntu-latest (devel) (R-lib actions)
* remote ubuntu-latest (oldrel-1) (R-lib actions)

## R CMD check results
There were no ERRORs, WARNINGs or NOTES.
