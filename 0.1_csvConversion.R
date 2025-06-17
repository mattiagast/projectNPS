library(xml2)
library(XML)

# Directory dei file XML
input_dir <- "C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/DatasetXML"

# Directory dove salvare i file CSV
output_dir <- "C:/Users/Mattia/Desktop/NONPARAMETRIC STATISTICS/projectNPS/DatasetCSV"

# Lista completa di file XML con percorso
listxml <- dir(path = input_dir, pattern = "\\.xml$", full.names = TRUE)
# listxml <- listxml[seq(671, 731)]

for (file_path in listxml) {
  df <- tryCatch({
    xmlToDataFrame(file_path)[-1, -1]  # adattalo se serve
  }, error = function(e) {
    message(paste("Errore con file:", file_path))
    return(NULL)
  })
  
  if (!is.null(df)) {
    file_base <- tools::file_path_sans_ext(basename(file_path))
    output_file <- file.path(output_dir, paste0(file_base, ".csv"))
    
    write.csv(df, file = output_file, row.names = FALSE)
    message(paste("Salvato CSV:", output_file))
  }
}
