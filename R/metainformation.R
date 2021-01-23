# Title     : Analyse the metainformation of the TRY database in order to be used for
# remote sensing analysis
# Created by: Konsti
# Created on: 22.01.2021

packages = c("ggplot2", "plotly", "gridExtra", "grid", "data.table", "dplyr", "readr")
lapply(packages, require, character.only = TRUE)

# LOAD DATASETS
p.traits = "./data/traits.txt"

(traits = read_tsv(p.traits, skip = 2) %>% as.data.table())

# save as excel
write_excel_csv2(traits, file = "./data/traits.xls")

traits %>% arrange(desc(TraitID))

stichwort = "leaf area"


query = c(3117,
47,
14,
15,
56,

3395,
3396,
3397,
3398,
3399,
3107,
42,
21,
3106,
3120,
46,
38,
197,
13,
48,
3122,
2,
413,
3441,
410,
3446,
3445,
773) %>% sort()

library(stringr)
str_flatten(query, collapse = ", ")