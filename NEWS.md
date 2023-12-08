# blaise 1.3.11
fixed bug where STRING fields with escaped characters would get incorrectly written to a fwf file.

# blaise 1.3.10
fixed bug where the word 'fields' anywhere in a variable would prevent a correct parse of the datamodel

# blaise 1.3.9
fixed warnings and errors caused by new Date formatting style in base R

# blaise 1.3.8
Fixed a big with readr that would cause blaise to break on files with empty rows.

# blaise 1.3.7
Fixed several bugs, most notably around significant digits getting set to 0.
Added better support for datamodels using notes and several types of formatting. Datamodels representing surveys
should be useable now.

# blaise 1.3.3
First CRAN version
