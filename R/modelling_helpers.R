make_term_sets <- function(possible_terms,interactions=T){
	as.vector(unique(unlist(sapply(seq(1,length(possible_terms)),function(x){
		combn(possible_terms,x,function(y){
			plusses = paste0(y,collapse = " + ")
			q <- gregexpr("\\+", plusses)[[1]]
			if(q[1]!=-1&interactions){
				r <-  strsplit(unlist(unique(combinat::permn(c(rep("*",length(q)),rep("+",length(q))),function(r){paste0(r[1:length(q)],collapse = " ")})))," ")

				sapply(seq(1,length(r)),function(t){
					d <- r[t][[1]]
					w <- plusses
					for(u in seq(1,length(q))){
						w <- paste0(substr(w,1,q[u]-1),d[u],substr(w,q[u]+1,nchar(w)))
					}
					w
				})
			} else {
				plusses
			}
		})
	}))))
}

get_best_model <- function(models){
	models_anova = do.call(anova,models)
	models_anova$model = rownames(models_anova)
	index = as.numeric(gsub("[^0-9]","",arrange(models_anova,-AIC)[nrow(models_anova),"model"]))
	return <- models[[index]]
}

report_model <- function(y){
	print(attr(y,"call"))
	cat("\n")
	cat("Estimates:\n")
	print(fixef(y))
	cat("\n")
	print(car::Anova(y))
}
