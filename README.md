RBlaise
===============

package om fwf bestanden met een bijbehorend datamodel te lezen of te schrijven 
in R.

Alle types worden momenteel ondersteund behalve arrays.
Datamodellen met code buiten DATAMODEL blok niet getest, werkt misschien niet.
Moet nog een parser gemaakt worden die enkel het datamodel er uit peutert.

Als je al een datamodel hebt kan deze gebruikt worden om automatisch je dataframe 
te ordenen en types te converteren.

Logical type wordt standaard geconverteerd naar een indicator met als type INTEGER bij het wegschrijven.

Kan ook gebruikt worden als alternatieve parser voor het LaF package, zodat datamodellen die voor dat
package niet worden geaccepteerd alsnog gebruikt kunnen worden om een laf object te krijgen.

Bouw met devtools in Rstudio door de .Rproj file te openen en 'build source package'
uit te voeren.
