# R set implementation using lists with uniqueness enforced.
# R has no native set type, so we use lists and check equality
# via identical().

# --- Helper ---

morloc_set_contains <- function(s, x) {
    for (e in s) {
        if (identical(e, x)) return(TRUE)
    }
    return(FALSE)
}

# --- Pack/Unpack ---

morloc_packSet <- function(xs) {
    result <- list()
    for (x in xs) {
        if (!morloc_set_contains(result, x)) {
            result <- c(result, list(x))
        }
    }
    result
}

morloc_unpackSet <- function(s) {
    s
}

# --- Construction ---

morloc_singleton <- function(x) {
    list(x)
}

morloc_fromList <- function(xs) {
    morloc_packSet(xs)
}

morloc_toList <- function(s) {
    s
}

# --- Query ---

morloc_member <- function(x, s) {
    morloc_set_contains(s, x)
}

morloc_size <- function(s) {
    length(s)
}

# --- Modify ---

morloc_insertSet <- function(x, s) {
    if (morloc_set_contains(s, x)) {
        return(s)
    }
    c(s, list(x))
}

morloc_deleteSet <- function(x, s) {
    result <- list()
    for (e in s) {
        if (!identical(e, x)) {
            result <- c(result, list(e))
        }
    }
    result
}

# --- Set Operations ---

morloc_union <- function(a, b) {
    result <- a
    for (x in b) {
        if (!morloc_set_contains(result, x)) {
            result <- c(result, list(x))
        }
    }
    result
}

morloc_intersection <- function(a, b) {
    result <- list()
    for (x in a) {
        if (morloc_set_contains(b, x)) {
            result <- c(result, list(x))
        }
    }
    result
}

morloc_difference <- function(a, b) {
    result <- list()
    for (x in a) {
        if (!morloc_set_contains(b, x)) {
            result <- c(result, list(x))
        }
    }
    result
}

morloc_symmetricDifference <- function(a, b) {
    morloc_union(morloc_difference(a, b), morloc_difference(b, a))
}

# --- Predicates ---

morloc_isSubset <- function(a, b) {
    for (x in a) {
        if (!morloc_set_contains(b, x)) {
            return(FALSE)
        }
    }
    return(TRUE)
}

morloc_isDisjoint <- function(a, b) {
    for (x in a) {
        if (morloc_set_contains(b, x)) {
            return(FALSE)
        }
    }
    return(TRUE)
}

# --- Transform ---

morloc_filterSet <- function(f, s) {
    result <- list()
    for (x in s) {
        if (f(x)) {
            result <- c(result, list(x))
        }
    }
    result
}

morloc_mapSet <- function(f, s) {
    result <- list()
    for (x in s) {
        fx <- f(x)
        if (!morloc_set_contains(result, fx)) {
            result <- c(result, list(fx))
        }
    }
    result
}

# --- Foldable ---

morloc_fold_set <- function(f, init, s) {
    acc <- init
    for (x in s) {
        acc <- f(acc, x)
    }
    acc
}

morloc_fold1_set <- function(f, s) {
    if (length(s) == 0) stop("fold1 on empty set")
    acc <- s[[1]]
    if (length(s) > 1) {
        for (i in 2:length(s)) {
            acc <- f(acc, s[[i]])
        }
    }
    acc
}

morloc_safeFold1_set <- function(f, s) {
    if (length(s) == 0) return(NULL)
    acc <- s[[1]]
    if (length(s) > 1) {
        for (i in 2:length(s)) {
            acc <- f(acc, s[[i]])
        }
    }
    acc
}

# --- Monoid ---

morloc_emptySet <- function() {
    list()
}
