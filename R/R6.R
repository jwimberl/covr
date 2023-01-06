replacements_R6 <- function(env) {
  unlist(recursive = FALSE, eapply(env, all.names = TRUE,
    function(obj) {
      if (inherits(obj, "R6ClassGenerator")) {
        unlist(recursive = FALSE, eapply(obj,
          function(o) {
            if (inherits(o, "list")) {
              lapply(names(o),
                  function(f_name) {
                  f <- get(f_name, o)
                  if (inherits(f, "function")) {
                    replacement(f_name, env = env, target_value = f)
                  }
})
            }
    }))
}
    }))
}

replacements_R6_static <- function(env) {
  unlist(recursive = FALSE, eapply(env, all.names = TRUE,
    function(obj) {
      if (inherits(obj, "R6ClassGenerator")) {
        static_methods <- unlist(recursive = FALSE, names(Filter(isTRUE, eapply(obj, is.function))))
        static_methods <- setdiff(static_methods, c("new","set","get_inherit","is_locked","lock","unlock","debug","undebug","has_private","clone_method"))
        olist <- eapply(obj,function(o) o)
        mret <- lapply(static_methods,
          function(smethod) {
            iret <- replacement(smethod, env = env, target_value = olist[[smethod]])
            return(iret)
          }
        )
        if (!is.null(mret) && length(mret) > 0) {
          names(mret) <- static_methods
        }
        return(mret)
      }
    }))
}

