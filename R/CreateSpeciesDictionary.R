
#' Get taxon list from NPSpecies database
#'
#' \code{getNPSpeciesTaxa} pulls the latest taxonomy from the NPSpecies/Taxonomy application
#'
#' This function is used to pull the most-recent list of taxonomy from theNPSpecies/Taxonomy
#' application.
#'
#' Note that this is a maintenance function and should only be run while working on the
#' pdfSpeciesScraper project to perform updates to the NPSpeciesJustSpecies.rda or dicSpecies.rda
#' files included with the distribution.
#'
#' @examples
#' getNPSpeciesTaxa()
getNPSpeciesTaxa<-function(){

  ch<-odbcConnect("Taxonomy", uid = "Report_Data_Reader", pwd = "ReportDataUser")
  NPSpeciesTaxa<- sqlFetch(ch,"TaxonGridFields", as.is=T)
  NPSpeciesJustSpecies<-NPSpeciesTaxa[NPSpeciesTaxa[, "Rank"] == "Species",]
  save(NPSpeciesJustSpecies,file="data/NPSpeciesJustSpecies.rda")

}

#' Get species for a category of taxa in NPSpecies
#'
#' \code{taxaCategories} returns all taxa within a taxonomic category in NPSpecies
#'
#' This function is used to extract latin species names from the NPSpecies master taxonomy table for
#' any given taxonomic group. Used by the \code{dicSpecies} function when building the dictionary
#' for text comparison.
#'
#' @param category is one of fifteen taxonomic categories within the NPSpecies/Taxonomy application:
#'
#' \describe{
#'   \item{"Bird"}{Birds}
#'   \item{"Fish"}{Fishes}
#'   \item{"Insect"}{Insects}
#'   \item{"Other Non-vertebrates"}{Other non-vertebrate species}
#'   \item{"Non-vascular Plant"}{Non-vascular plants}
#'   \item{"Fungi"}{Fungi}
#'   \item{"Protozoa"}{Protozoa}
#'   \item{"Vascular Plant"}{Vascular plants}
#'   \item{"Slug/Snail"}{Slugs and snails}
#'   \item{"Amphibian"}{Amphibians}
#'   \item{"Reptile"}{Reptiles}
#'   \item{"Mammal"}{Mammals}
#'   \item{"Crab/Lobster/Shrimp"}{Crabs, lobsters, and shrimp}
#'   \item{"Spider/Scorpion"}{Spiders and scorpions}
#'   \item{"Archaea"}{Prokaryotic microorganisms}
#' }
#'
#' @examples
#' taxaCategories("Bird")
taxaCategories<-function(category) {
  CategoryTaxa <- NPSpeciesJustSpecies[NPSpeciesJustSpecies$CategoryName==category,]
  CategoryTaxa <- as.character(CategoryTaxa$SciName)
  return(CategoryTaxa)
}


#' Create species dictionary file
#'
#' \code{dicSpecies} creates a dictionary file of species to be used for text analysis.
#'
#' This function is used to create a LIWC-compliant dictionary file of latin binomial species
#' names based on taxa currently included in the NPSpecies master taxonomy table.
#' The dictionary file includes the two-word species names as "words" and the NPSpeciestaxonomic
#' group as the "sentiment" column in the LWIC file.
#'
#' Note that this is a maintenance function and should only be run while working on the
#' pdfSpeciesScraper project to perform updates to the dicSpecies.rda file included with the
#' distribution.
#'
#' @examples
#' createSpeciesDictionary()
createSpeciesDictionary<-function(){

  load("../NPSpecies/data/NPSpeciesJustSpecies.rda")

  dicSpecies<-dictionary(list(birds=c(taxaCategories("Bird")),
                   fish=c(taxaCategories("Fish")),
                   insects=c(taxaCategories("Insect")),
                   chromista=c(taxaCategories("Chromista")),
                   bacteria=c(taxaCategories("Bacteria")),
                   othernonverts=c(taxaCategories("Other Non-vertebrates")),
                   NonvascularPlants=c(taxaCategories("Non-vascular Plant")),
                   fungi=c(taxaCategories("Fungi")),
                   protozoans=c(taxaCategories("Protozoa")),
                   VascularPlantslis=c(taxaCategories("Vascular Plant")),
                   slugsandsnails=c(taxaCategories("Slug/Snail")),
                   amphibians=c(taxaCategories("Amphibian")),
                   reptiles=c(taxaCategories("Reptile")),
                   mammals=c(taxaCategories("Mammal")),
                   CrabsLobsersShrimp=c(taxaCategories("Crab/Lobster/Shrimp")),
                   SpiderScorpion=c(taxaCategories("Spider/Scorpion")),
                   Archaea=c(taxaCategories("Archaea"))
  ), tolower=FALSE)

  save(dicSpecies,file="data/dicSpecies.rda")
}

#' Update species dictionary file
#'
#' \code{dicSpecies} updates a dictionary file of species to be used for text analysis.
#'
#' This function is used to update the LIWC-compliant dictionary file of latin binomial species
#' names based on taxa currently included in the NPSpecies master taxonomy table.
#' The dictionary file includes the two-word species names as "words" and the NPSpeciestaxonomic
#' group as the "sentiment" column in the LWIC file.
#'
#' Note that this is a maintenance function and should only be run while working on the
#' pdfSpeciesScraper project to perform updates to the dicSpecies.rda file included with the
#' distribution.
#'
#' @examples
#' updateSpeciesDictionary()
updateSpeciesDictionary<-function(){

  getNPSpeciesTaxa()

  dicSpecies<-dictionary(list(birds=c(taxaCategories("Bird")),
                              fish=c(taxaCategories("Fish")),
                              insects=c(taxaCategories("Insect")),
                              chromista=c(taxaCategories("Chromista")),
                              bacteria=c(taxaCategories("Bacteria")),
                              othernonverts=c(taxaCategories("Other Non-vertebrates")),
                              NonvascularPlants=c(taxaCategories("Non-vascular Plant")),
                              fungi=c(taxaCategories("Fungi")),
                              protozoans=c(taxaCategories("Protozoa")),
                              VascularPlantslis=c(taxaCategories("Vascular Plant")),
                              slugsandsnails=c(taxaCategories("Slug/Snail")),
                              amphibians=c(taxaCategories("Amphibian")),
                              reptiles=c(taxaCategories("Reptile")),
                              mammals=c(taxaCategories("Mammal")),
                              CrabsLobsersShrimp=c(taxaCategories("Crab/Lobster/Shrimp")),
                              SpiderScorpion=c(taxaCategories("Spider/Scorpion")),
                              Archaea=c(taxaCategories("Archaea"))
  ), tolower=FALSE)

  save(dicSpecies,file="data/dicSpecies.rda")
}
