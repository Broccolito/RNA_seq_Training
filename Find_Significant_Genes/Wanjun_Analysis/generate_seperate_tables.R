rm(list = ls())

library(dplyr)
library(knitr)

get_directory = function(){
  args <- commandArgs(trailingOnly = FALSE)
  file <- "--file="
  rstudio <- "RStudio"
  match <- grep(rstudio, args)
  if(length(match) > 0){
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  }else{
    match <- grep(file, args)
    if (length(match) > 0) {
      return(dirname(normalizePath(sub(file, "", args[match]))))
    }else{
      return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
    }
  }
}

setwd(get_directory())

d = read.csv(file = "comparisons_master_table.csv")
d = select(d, ENSEMBL:gene_type,pi_C1_C5_C7,pi_C10_C18_C19,pi_C9_C12_C15,
           chr:end_pos,logFC_C1:sig_pi_C19)


fasting_rest = arrange(d, desc(pi_C1_C5_C7))[1:300,]
exercise_second_order = arrange(d, desc(pi_C10_C18_C19))[1:300,]
exercise_paired = arrange(d, desc(pi_C9_C12_C15))[1:300,]

write.csv(fasting_rest, file = "fasting_rest.csv", 
          row.names = FALSE, quote = FALSE)
write.csv(exercise_second_order, file = "exercise_second_order.csv", 
          row.names = FALSE, quote = FALSE)
write.csv(exercise_paired, file = "exercise_paired.csv", 
          row.names = FALSE, quote = FALSE)