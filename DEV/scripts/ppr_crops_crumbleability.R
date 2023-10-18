# ppr_eval_crumbleability

# read in csv file
eval_crumbleability <- fread('dev/data/crumbleability.csv',sep=';')

# save file
usethis::use_data(eval_crumbleability, version = 3, overwrite = TRUE, compress = 'xz')
