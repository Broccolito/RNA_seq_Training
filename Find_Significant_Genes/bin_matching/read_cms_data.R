library(dplyr)
library(readxl)

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

threshold = 6

cms = readLines("CDP_CompMultSign_list.txt")
cms = sapply(cms, function(x){
  s = gsub(" ","", unlist(strsplit(x, "  ")))
  s = s[nchar(s)>0]
  s = unlist(s)
  names(s) = NULL
  # s = ifelse(length(s) == 4, c(s, "NOMATCH"), s)
  return(s)
})
names(cms) = NULL

for(i in 1:length(cms)){
  if(length(cms[[i]]) == 4){
    cms[[i]] = c(cms[[i]], "NONAME")
  }
}

cms = as.data.frame(matrix(unlist(cms), ncol = 5, byrow = TRUE))
colnames(cms) = c("chr", "pos", "score", "cms_score", "genes")
cms = select(cms, chr:pos, cms_score:genes) %>%
  mutate(pos = as.numeric(as.character(pos))) %>%
  mutate(cms_score = as.numeric(as.character(cms_score))) %>%
  mutate(genes = as.character(genes))
cms = subset(cms, cms_score >= threshold)
head(cms, 50)

exer = read_excel("exercise_paired_annotate_chrominfo.xlsx")
exer = select(exer, ENSEMBL, SYMBOL, pi_value_C1:pi_value_C1_C5_C7, chromosome_name:end_position) %>%
  mutate(chromosome_name = paste0("chr", chromosome_name))

dist_mat = vector()
for(i in 1:dim(cms)[1]){
  for(j in 1:dim(exer)[1]){
    
  }
}