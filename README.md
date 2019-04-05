Blaise R package
===============

This package provides function for reading and writing blaise fixed width files
with a datamodel (usually with extension .bla) symmetrically. 
Symmetry in this case means that reading a dataset in R and then immediately
writing it out, should result in the exact same dataset and datamodel.

All types except arrays are currently supported. Datamodels have only been tested 
as separate files, so datamodels within larger manipula or maniplus scripts for 
instance are not guaranteed to work.

In addition, an R dataframe can be forced to conform to a known blaise datamodel.
For this to work variable names in the dataframe need to match the datamodel. 
a simple name matching scheme based on minimizing the Levenshtein distance is supplied.

Since blaise and R datatypes don't exactly overlap, some are automatically converted:
* R Logical type is always converted to an INTEGER with FALSE:0 and TRUE:1 when writing.
* numbered blaise enums are converted to factors with the numbers as labels. The original labels are therefore lost. 
(possibly will be implemented as new R vector in the future). This can be suppressed,
but the default is to convert due to the symmetric design principle.

For reading fwf files an option is available to output LaF objects. In this way the 
package is used as an alternative datamodel parser for the LaF package.
