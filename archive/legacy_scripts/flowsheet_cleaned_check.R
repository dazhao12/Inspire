################################################################################
#### This scrip checkes data elements from flowsheet tables
#### By: Jiangqiong Li, 04/22/2026
################################################################################
require(pacman)
p_load(dplyr, tidyr, stringr, rlang, openxlsx, purrr, arrow, readr)
path = "/N/project/analgesia_perioperation/data/MOVER/raw/flowsheets_cleaned/"
file = "flowsheet_part2.csv"
flowsheet_part2 = read.csv(paste0(path, file))
################################################################################
### produce frequency table of categorical columns
id = function(idcol, df){## for unique count of ids
  out = NULL
  for (i in idcol) {
    vec = df[[i]]
    n = unique(vec) %>% length()# 
    out = c(out, n)
  }
  return(out)
}
cat = function(catcol, df){## for unique count of categorical variables
  out = NULL
  for (i in catcol) {
    vec = df[[i]]
    vec1 = if(length(unique(vec)) <= 5 & any(unique(vec) %in% c(0:9))) vec else 
      vec[!grepl("^[0-9+-.]", vec)]
    n = unique(vec1) %>% length()# 
    out = c(out, n)
  }
  return(out)
}
freq = function(catcol, df){## for frequency categorical variables
  out = list()
  for (i in catcol) {
    vec = df[[i]]
    nm = names(df)[i]
    vec1 = if(length(unique(vec)) <= 5 & any(unique(vec) %in% c(0:9))) vec else 
      vec[!grepl("^[0-9+-.]", vec)]
    if(length(vec1[!is.na(vec1)]) > 0) {
      fr = table(vec1) %>% data.frame() %>% arrange(desc(Freq)) 
      names(fr) = c(nm, paste0(nm, '_freq'))
    } else {
      fr = data.frame(setNames(list('NA','NA'), c(nm, paste0(nm, '_freq'))))
    }
    out[[i]] = fr
  }
  return(out)
}
date = function(datecol, df){
  out = NULL
  for (i in datecol) {
    vec = df[[i]]
    r = if (length(na.omit(vec)) == 0) NA else{
      mn = min(vec, na.rm = T)
      mx = max(vec, na.rm = T)
      r = paste(mn, mx, sep = '--')
    }
    out = c(out, r)
  }
  return(out)
}
ftb = function(dflist, maxr) {## bind final table
  padded_list = lapply(dflist, function(df) {
    n_add = maxr - nrow(df)
    if (n_add > 0) {
      emptyrow = as.data.frame(matrix('', nrow = n_add, ncol = ncol(df)))
      colnames(emptyrow) = colnames(df)
      df = rbind(df, emptyrow)
    } else {
      df
    }
  })
  bind_cols(padded_list)
}
maxr = 1000
################################################################################
### based on file part2
names(flowsheet_part2)
df = flowsheet_part2
head(df)
vars = data.frame(Variable = names(df), uniqueN='', CatLevel='', NumStat='')
vars[nrow(vars)+1, 1:2] = c('total_row', nrow(df))

vars[c(2,3),2] = id(c(2,3), df)
vars[7,4] = date(7, df)
vars[c(4:6,9),3] = cat(c(4:6,9),df)

tb = freq(c(4:6,9), df)#
t4 = tb[[4]]
t5 = tb[[5]]
t6 = tb[[6]]
t9 = tb[[9]]

dflist = list(vars,t4, t5, t6, t9)
dftb = ftb(dflist, maxr)

path = "/N/project/analgesia_perioperation/documents/"
list.files(path)
wb = createWorkbook()
tbnm = paste0(path, "/flowsheet_cleanedp2_check.xlsx")

addWorksheet(wb, "flowsheet_cleaned_part2")
writeData(wb, "flowsheet_cleaned_part2", dftb)
saveWorkbook(wb, tbnm, overwrite = T)



