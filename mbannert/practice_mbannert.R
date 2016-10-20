objs <- mget(ls("package:base",all=T),inherits = T)
funs <- Filter(is.function,objs)

# scan's the winner... 
max_arg_fun <- which.max(lapply(funs,
                                function(x) length(formals(x))))

count_arg_funs <- sapply(funs,function(x) length(formals(x)))
count_arg_funs[which.max(count_arg_funs)]

# how many w/o args
table(count_arg_funs == 0)


