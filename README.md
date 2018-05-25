RBlaise
===============

package om fwf bestanden met een bijbehorend datamodel te lezen of te schrijven 
in R.

Alle types worden ondersteund behalve arrays en custom types.
Datamodellen met text buiten DATAMODEL blok niet getest, werkt waarschijnlijk niet.
Moet nog een parser gemaakt worden die enkel de datamodel er uit peutert.

Als je al een datamodel hebt kan deze gebruikt worden om automatisch je dataframe 
te ordenen en types te converteren.

Logical type wordt standaard geconverteerd naar een indicator met als type INTEGER.

Bouw met devtools in Rstudio door de .Rproj file te openen en 'build source package'
uit te voeren.
