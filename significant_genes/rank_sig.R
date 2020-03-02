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

wd = get_directory()
setwd(wd)

dff = vector()
fl = list.files(pattern = "csv")
for(f in fl){
  dff = rbind.data.frame(dff, read.csv(f))
}

dff$ENSEMBL = as.character(dff$ENSEMBL)
unigene = unique(dff$ENSEMBL)


# for(g in unigene){
#   expdiff = subset(dff, ENSEMBL == g)
#   # print(sum(abs(expdiff$logFC)))
#   data.frame(gene_code = g, expdiff = expdiff)
# }