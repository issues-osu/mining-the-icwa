library(httr2)
library(purrr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(forcats)
library(readr)
library(digest)

SEARCH_BASE <- "https://www.courtlistener.com/api/rest/v4/search/"
UA <- "R/httr2 (barboza-salerno.1@osu.edu) – ICWA metadata collection"

query_params <- list(
  q = '"indian child welfare act"',
  type = "o",
  order_by = "score desc",
  stat_Published = "on",
  court = "ohio ohioctapp ohioctcl"
)

pull_chr <- function(x, ...) {
  v <- purrr::pluck(x, ..., .default = NA)
  if (is.null(v) || length(v) == 0) return(NA_character_)
  as.character(v)
}
pull_num <- function(x, ...) {
  v <- purrr::pluck(x, ..., .default = NA_real_)
  suppressWarnings(as.numeric(v))
}
get_json <- function(url) {
  resp <- request(url) |> req_user_agent(UA) |> req_url_query() |> req_perform()
  resp_check_status(resp)
  resp_body_json(resp, simplifyVector = FALSE)
}

fetch_search_pages <- function() {
  req <- request(SEARCH_BASE) |> req_user_agent(UA) |> req_url_query(!!!query_params)
  dat <- req |> req_perform() |> (\(r){resp_check_status(r); resp_body_json(r, simplifyVector=FALSE)})()
  pages <- list(dat)
  while (!is.null(dat$`next`)) {
    dat <- request(dat$`next`) |> req_user_agent(UA) |> req_perform() |>
      (\(r){resp_check_status(r); resp_body_json(r, simplifyVector=FALSE)})()
    pages <- append(pages, list(dat))
  }
  pages
}

pages   <- fetch_search_pages()
results <- pages |> map("results") |> flatten()

meta <- map_dfr(results, function(r) {
  collapse_chr <- function(xs) {
    xs <- xs[!is.na(xs) & nzchar(xs)]
    if (length(xs) == 0) NA_character_ else paste(xs, collapse = "; ")
  }
  
  cites_vec <- purrr::pluck(r, "citation", .default = list())
  cites_chr <- if (length(cites_vec)) paste(unlist(cites_vec), collapse = "; ") else NA_character_
  
  op_list <- purrr::pluck(r, "opinions", .default = list())
  op_ids  <- if (length(op_list)) map_chr(op_list, ~ pull_chr(.x, "id")) else character(0)
  op_urls <- if (length(op_list)) map_chr(op_list, ~ pull_chr(.x, "download_url")) else character(0)
  
  tibble(
    case_name        = pull_chr(r, "caseName"),
    date_filed       = as.Date(pull_chr(r, "dateFiled")),
    status           = pull_chr(r, "status"),
    court_name       = pull_chr(r, "court"),
    court_id         = pull_chr(r, "court_id"),
    court_cite_str   = pull_chr(r, "court_citation_string"),
    cluster_id       = pull_chr(r, "cluster_id"),
    absolute_url     = paste0("https://www.courtlistener.com", pull_chr(r, "absolute_url")),
    judge            = pull_chr(r, "judge"),
    docket_id        = pull_chr(r, "docket_id"),
    docket_number    = pull_chr(r, "docketNumber"),
    neutral_cite     = pull_chr(r, "neutralCite"),
    citations        = cites_chr,
    cite_count       = pull_num(r, "citeCount"),
    # opinions info
    opinion_ids      = collapse_chr(op_ids),
    pdf_downloads    = collapse_chr(op_urls),
    # scoring/meta
    bm25_score       = pull_num(r, "meta", "score", "bm25"),
    indexed_at       = pull_chr(r, "meta", "timestamp"),
    created_at       = pull_chr(r, "meta", "date_created"),
    # extras (often blank but included for completeness)
    attorney         = pull_chr(r, "attorney"),
    panel_names      = collapse_chr(purrr::flatten_chr(purrr::pluck(r, "panel_names", .default = list()))),
    posture          = pull_chr(r, "posture"),
    syllabus         = pull_chr(r, "syllabus")
  )
}) |>
  arrange(desc(bm25_score), desc(date_filed))

print(meta, n = 5)
# write.csv(meta, "courtlistener_icwa_ohio_metadata.csv", row.names = FALSE)



save_opinion_text_by_court <- function(
    meta,
    results,
    court_name,
    out_dir = "courtlistener_text_by_court",
    user_agent = "R/httr2 (barboza-salerno.1@osu.edu) – ICWA text export",
    polite_sleep = 0.6    # default pause between requests
) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  
  safe_slug <- function(x, max_chars = 60) {
    x <- ifelse(is.na(x) | x == "", "unknown", x)
    x <- str_to_lower(x) |> str_replace_all("[^a-z0-9]+", "-") |>
      str_replace_all("-+", "-") |> str_replace_all("(^-|-$)", "")
    if (nchar(x) > max_chars) substr(x, 1, max_chars) else x
  }
  
  make_fname <- function(dir, case_name, date_filed, docket_num, op_index, op_id = NULL) {
    case_part   <- safe_slug(case_name, 60)
    date_part   <- if (!is.na(date_filed) && nzchar(as.character(date_filed))) as.character(date_filed) else "undated"
    docket_part <- if (!is.na(docket_num) && nzchar(docket_num)) safe_slug(docket_num, 40) else "nodocket"
    id_part     <- if (!is.null(op_id)) paste0("_id", op_id) else ""
    fname <- paste0(case_part, "_", date_part, "_", docket_part, "_op", op_index, id_part, ".txt")
    if (nchar(fname) > 180) fname <- paste0(substr(fname, 1, 160), "_", substr(digest(fname), 1, 12), ".txt")
    file.path(dir, fname)
  }
  
fetch_opinion_text <- function(op_id, max_tries = 6) {
    url <- sprintf("https://www.courtlistener.com/api/rest/v4/opinions/%s/", op_id)
    attempt <- 1
    repeat {
      if (polite_sleep > 0) Sys.sleep(polite_sleep)
      resp <- tryCatch(
        {
          request(url) |> req_user_agent(user_agent) |> req_perform()
        },
        error = function(e) e
      )
      # network error -> backoff
      if (inherits(resp, "error")) {
        if (attempt >= max_tries) stop(resp)
        Sys.sleep(min(60, 2 ^ attempt)); attempt <- attempt + 1; next
      }
      status <- resp_status(resp)
      if (status == 200) {
        j <- resp_body_json(resp, simplifyVector = FALSE)
        txt <- j$html_with_citations %||% j$html %||% j$plain_text %||% ""
        if (!is.null(j$html_with_citations) || !is.null(j$html)) {
          txt <- txt |>
            str_replace_all("<br\\s*/?>", "\n") |>
            str_replace_all("<p\\s*/?>", "\n") |>
            str_replace_all("<[^>]+>", "") |>
            str_squish()
        }
        return(trimws(txt))
      }
       if (status %in% c(429, 500, 502, 503, 504) && attempt < max_tries) {
        ra <- resp_header(resp, "retry-after")
        wait <- if (!is.na(ra)) suppressWarnings(as.numeric(ra)) else min(60, 2 ^ attempt)
        Sys.sleep(ifelse(is.na(wait), min(60, 2 ^ attempt), wait))
        attempt <- attempt + 1
        next
      }
     
      resp_check_status(resp)
    }
  }
  
  full_url <- function(x) ifelse(startsWith(x, "http"), x, paste0("https://www.courtlistener.com", x))
  
  need_cols <- c("court_name","absolute_url","case_name","date_filed")
  if (!all(need_cols %in% names(meta))) stop("`meta` must include: ", paste(need_cols, collapse = ", "))
  
  court_name <- trimws(court_name)
  meta <- meta |>
    mutate(
      court_name   = trimws(court_name),
      absolute_url = trimws(as.character(absolute_url))
    )
  
  sub <- meta |> filter(.data$court_name == !!court_name)
  if (nrow(sub) == 0) {
    message("No rows matched court_name = '", court_name, "'. Available:\n",
            paste(sort(unique(meta$court_name)), collapse = "\n"))
    return(tibble())
  }
  
  allowed_urls <- unique(full_url(sub$absolute_url))
  
  docket_col <- if ("docket_number" %in% names(sub)) "docket_number" else if ("docketNumber" %in% names(sub)) "docketNumber" else NA_character_
  sub_info <- sub |>
    mutate(
      absolute_url = full_url(absolute_url),
      docket_num   = if (!is.na(docket_col)) .data[[docket_col]] else NA_character_
    ) |>
    select(absolute_url, case_name, date_filed, docket_num)
  info_map <- split(sub_info, sub_info$absolute_url)
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  court_dir <- file.path(out_dir, safe_slug(court_name, 80))
  dir.create(court_dir, recursive = TRUE, showWarnings = FALSE)

  results_by_abs <- set_names(
    results,
    vapply(results, function(r) full_url(trimws(as.character(r$absolute_url %||% ""))), character(1))
  )
  
  written <- list()
  
  for (u in allowed_urls) {
    r <- results_by_abs[[u]]
    if (is.null(r)) { message("Skip (no matching results entry): ", u); next }
    
    opinions <- r$opinions %||% list()
    if (length(opinions) == 0) { message("Skip (no opinions): ", u); next }
    
    row  <- info_map[[u]][1, , drop = FALSE]
    case <- row$case_name
    date <- row$date_filed
    dnum <- row$docket_num
    
    for (k in seq_along(opinions)) {
      op_id <- opinions[[k]]$id
      if (is.null(op_id)) next
      txt <- fetch_opinion_text(op_id)
      if (!nzchar(txt)) next
      target <- make_fname(court_dir, case, date, dnum, k, op_id)
      ok <- TRUE
      tryCatch({ write_file(txt, target) }, error = function(e) ok <<- FALSE)
      if (!ok) {
        # path too long -> ultra-short fallback
        target <- file.path(
          court_dir,
          paste0(substr(safe_slug(case, 30), 1, 30), "_", substr(digest(paste(case, date, dnum, op_id)), 1, 12), ".txt")
        )
        write_file(txt, target)
      }
      written[[length(written) + 1]] <- target
    }
  }
  
  tibble(court_name = court_name, file = unlist(written))
}

# change this to get other courts
save_opinion_text_by_court(meta, results, "Ohio Court of Appeals", polite_sleep = 0.8)

by_year <- meta %>%
  filter(!is.na(date_filed)) %>%
  mutate(year = year(date_filed)) %>%
  count(year, name = "cases")

p_year <- ggplot(by_year, aes(x = year, y = cases)) +
  geom_col() +
  labs(title = "Cases Filed by Year",
       x = "Year", y = "Number of Cases") +
  theme_minimal()

print(p_year)
ggsave("cases_by_year.png", p_year, width = 8, height = 4, dpi = 300)

by_court <- meta %>%
  count(court_name, name = "cases") %>%
  slice_max(cases, n = 25)

p_court <- ggplot(by_court, aes(x = fct_reorder(court_name, cases), y = cases)) +
  geom_col() +
  coord_flip() +
  labs(title = "Cases per Court (Top 25)",
       x = "Court", y = "Number of Cases") +
  theme_minimal()

print(p_court)
ggsave("cases_per_court_top25.png", p_court, width = 8, height = 8, dpi = 300)
