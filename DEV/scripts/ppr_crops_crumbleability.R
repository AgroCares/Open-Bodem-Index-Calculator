# ppr_eval_crumbleability

# read in csv file
eval.crumbleability <- fread('dev/data/crumbleability.csv',sep=';')

# save file
save(eval.crumbleability, file = 'data/eval_crumbleability.RData')
