#' Extract repo database
#'
#' Extract a database with the R packages hosted by an user on GH
#'
#' @param user GitHub User
#'
#' @param n_repos Number of repos to fetch.
#'
#' @export
extract_repo_database <- function(user, n_repos = 100) {
  my_repos <- gh::gh(
    "/users/{username}/repos",
    username = user, per_page = n_repos
  )

  # Create database of repos
  names <- vapply(my_repos, "[[", "", "name")


  db <- lapply(my_repos, function(x) {
    x[c("name", "html_url", "default_branch", "language")]
  })

  # Clean language R
  lang_r <- unlist(lapply(db, function(x) {
    "R" %in% x$language
  }))

  db <- db[lang_r]

  commits <- lapply(db, get_latest_commit, owner = user)

  # Check against current
  if (file.exists("db/db_current.csv")) {
    current <- read.csv("db/db_current.csv")
    current$already <- TRUE

    # Check if name is already there

    df <- data.frame(
      name = unlist(lapply(commits, function(x) x$name)),
      commit = unlist(lapply(commits, function(x) x$commit))
    )


    dfjoin <- dplyr::full_join(df, current)

    dfjoin[is.na(dfjoin$already), "already"] <- FALSE

    # Extract packages that needs to be assessed

    update <- commits[(dfjoin$already == FALSE)]
    if (length(update) == 0) {
      message("Database up-to-date")
      return(invisible())
    }
  } else {
    update <- commits
  }


  # Check if it is a R package
  check_r_package <- lapply(update, r_package_check)

  flat <- dplyr::bind_rows(check_r_package)
  flat <- flat[flat$r_package == TRUE, ]

  if (!dir.exists("db")) dir.create("db")
  if (!file.exists("db/db_current.csv")) {
    write.csv(flat,
      "db/db_current.csv",
      row.names = FALSE
    )
  }
  if (file.exists("db/db_new.csv")) file.remove("db/db_new.csv")

  write.csv(flat, "db/db_new.csv", row.names = FALSE)


  message("Added packages: ", paste0(flat$name, collapse = ", "))

  return(invisible(flat))
}


get_latest_commit <- function(db, owner) {
  user <- owner
  repo <- db$name
  default_branch <- db$default_branch
  branches <- gh::gh(
    "/repos/{owner}/{repo}/branches",
    owner = user,
    repo = repo
  )
  db$commit <- branches[vapply(branches, "[[", "", "name") == default_branch][[1]]$commit$sha

  return(db)
}

#' @importFrom utils write.csv
NULL

r_package_check <- function(db) {
  # Build DESCRIPTION path
  desc_path <- file.path(db$html_url, "blob", db$default_branch, "DESCRIPTION")
  status <- httr::http_status(httr::GET(desc_path))$reason


  if (identical(status, "OK")) {
    db$r_package <- TRUE
    return(db)
  }

  db$r_package <- FALSE
  return(db)
}
