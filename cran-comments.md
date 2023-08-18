## Resubmission
This is a resubmission. In this version I have:

* fixed bug where the word 'fields' anywhere in a variable would prevent a correct parse of the datamodel

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
