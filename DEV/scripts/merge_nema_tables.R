# Script to merge bouwplan table with crops.obic and nema.obic
library(data.table)
library(OBIC)

# Load tables
crops.obic <- OBIC::crops.obic
nema.obic <- OBIC::nema.obic
bp <- OBIC::nema.crop.rot.obic

## Merge bp and nema.obic
bp <- bp[,name_scientific := gsub('_', ' ', name_scientific)]
# check which species occur aaltjesschema and not in nema.obic
bpnames <- unique(bp$name_scientific)
miss.bpnames <- bpnames[!bpnames %in% nema.obic$species]
miss.bpnames <- sort(miss.bpnames)

# check which species occur in nema.obic but not in aaltjesschema
nonames <- unique(nema.obic$species)
miss.nonames <- nonames[!nonames %in% bp$name_scientific]
miss.nonames <- sort(miss.nonames)
miss.nonames
