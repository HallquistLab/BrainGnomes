# Internal, context-preserving guided diagnosis. The public entry point remains
# diagnose_project(); these helpers deliberately operate on its structured
# bg_project_diagnosis result instead of maintaining a second status model.

diagnosis_problem_statuses <- c("FAILED", "BLOCKED", "CANCELLED")

diagnosis_job_id <- function(job_id) {
  if (is.null(job_id)) return(NULL)
  if (length(job_id) != 1L || is.na(job_id)) {
    stop("job_id must be one non-missing scheduler job identifier.", call. = FALSE)
  }
  job_id <- as.character(job_id)
  if (!nzchar(job_id)) {
    stop("job_id must be a non-empty scheduler job identifier.", call. = FALSE)
  }
  job_id
}

diagnosis_current_jobs <- function(diagnosis) {
  jobs <- as.data.frame(diagnosis$jobs)
  if (nrow(jobs) == 0L) return(jobs)
  if (!is.null(diagnosis$focus) && !is.null(diagnosis$focus$job_id)) {
    return(jobs)
  }
  if (identical(diagnosis$scope, "project") &&
      "is_current_attempt" %in% names(jobs)) {
    jobs <- jobs[!is.na(jobs$is_current_attempt) & jobs$is_current_attempt, , drop = FALSE]
  }
  jobs
}

diagnosis_problem_jobs <- function(jobs) {
  jobs[
    !is.na(jobs$lifecycle_status) &
      jobs$lifecycle_status %in% diagnosis_problem_statuses,
    , drop = FALSE
  ]
}

diagnosis_stage_label <- function(stage, stream = NA_character_) {
  label <- switch(as.character(stage),
    flywheel_sync = "Flywheel sync",
    fsaverage_setup = "fsaverage setup",
    prefetch_templates = "TemplateFlow prefetch",
    subject_submission = "Subject submission",
    bids_validation = "BIDS validation",
    bids_conversion = "BIDS conversion",
    mriqc = "MRIQC",
    fmriprep = "fMRIPrep",
    aroma = "ICA-AROMA",
    postprocess = "Postprocessing",
    extract_rois = "ROI extraction",
    other = "Other",
    as.character(stage)
  )
  if (!is.na(stream) && nzchar(stream)) paste0(label, ": ", stream) else label
}

diagnosis_subject_label <- function(sub_id, ses_id = NA_character_) {
  if (is.na(sub_id) || !nzchar(sub_id)) return("project-level")
  label <- paste0("sub-", sub_id)
  if (!is.na(ses_id) && nzchar(ses_id)) {
    label <- paste0(label, "/ses-", ses_id)
  }
  label
}

diagnosis_job_label <- function(job) {
  paste0(
    diagnosis_stage_label(job$stage[[1L]], job$stream[[1L]]),
    " - ",
    diagnosis_subject_label(job$sub_id[[1L]], job$ses_id[[1L]]),
    " - ", job$lifecycle_status[[1L]],
    " (job ", job$job_id[[1L]], ")"
  )
}

diagnosis_status_summary <- function(status) {
  counts <- table(factor(status, levels = diagnosis_problem_statuses))
  present <- counts[counts > 0L]
  paste(paste0(as.integer(present), " ", names(present)), collapse = ", ")
}

diagnosis_problem_groups <- function(jobs) {
  jobs <- diagnosis_problem_jobs(jobs)
  if (nrow(jobs) == 0L) {
    return(data.frame(
      key = character(), stage = character(), stream = character(),
      n_jobs = integer(), n_subjects = integer(), label = character(),
      stringsAsFactors = FALSE
    ))
  }
  streams <- ifelse(is.na(jobs$stream), "", jobs$stream)
  keys <- paste(jobs$stage, streams, sep = "\r")
  key_levels <- unique(keys)
  groups <- lapply(key_levels, function(key) {
    group <- jobs[keys == key, , drop = FALSE]
    stage <- group$stage[[1L]]
    stream <- group$stream[[1L]]
    subjects <- unique(group$sub_id[!is.na(group$sub_id) & nzchar(group$sub_id)])
    label <- if (nrow(group) == 1L) {
      diagnosis_job_label(group)
    } else {
      subject_text <- if (length(subjects) == 0L) {
        "project-level"
      } else if (length(subjects) == 1L) {
        paste0("sub-", subjects)
      } else {
        paste0(length(subjects), " subjects")
      }
      relation <- if (length(subjects) == 1L) " jobs for " else " jobs across "
      paste0(
        diagnosis_stage_label(stage, stream), " - ", nrow(group),
        relation, subject_text, " - ",
        diagnosis_status_summary(group$lifecycle_status)
      )
    }
    data.frame(
      key = key, stage = stage,
      stream = if (is.na(stream)) NA_character_ else stream,
      n_jobs = nrow(group), n_subjects = length(subjects), label = label,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, groups)
}

diagnosis_prompt_menu <- function(labels, prompt) {
  cli::cli_ol(labels)
  prompt_input(
    prompt = prompt,
    type = "integer",
    lower = 1L,
    upper = length(labels),
    required = TRUE,
    len = 1L
  )
}

diagnosis_order_jobs <- function(jobs) {
  if (nrow(jobs) < 2L) return(jobs)
  status_order <- match(
    jobs$lifecycle_status,
    c("FAILED", "CANCELLED", "BLOCKED", "RUNNING", "QUEUED", "UNKNOWN", "COMPLETED")
  )
  jobs[order(
    status_order, jobs$sub_id, jobs$ses_id, jobs$stage, jobs$stream,
    jobs$job_id, na.last = TRUE
  ), , drop = FALSE]
}

diagnosis_choose_job <- function(jobs, heading, allow_back = TRUE) {
  jobs <- diagnosis_order_jobs(jobs)
  if (nrow(jobs) == 0L) {
    cli::cli_alert_info("No tracked jobs match this selection.")
    return(NULL)
  }
  if (nrow(jobs) == 1L) {
    cli::cli_alert_info("Opening the only matching job: {.val {jobs$job_id[[1L]]}}")
    return(jobs[1L, , drop = FALSE])
  }

  cli::cli_h2(heading)
  labels <- vapply(seq_len(nrow(jobs)), function(i) {
    diagnosis_job_label(jobs[i, , drop = FALSE])
  }, character(1))
  if (allow_back) labels <- c(labels, "Back")
  choice <- diagnosis_prompt_menu(labels, "Choose a job")
  if (allow_back && choice == length(labels)) return(NULL)
  jobs[choice, , drop = FALSE]
}

diagnosis_choose_subject <- function(jobs) {
  problems <- diagnosis_problem_jobs(jobs)
  problems <- problems[!is.na(problems$sub_id), , drop = FALSE]
  if (nrow(problems) == 0L) {
    cli::cli_alert_info("No subject-level unresolved problems were found.")
    return(NULL)
  }
  subject_ids <- sort(unique(problems$sub_id))
  counts <- vapply(subject_ids, function(id) {
    sum(problems$sub_id == id, na.rm = TRUE)
  }, integer(1))
  labels <- paste0(
    "sub-", subject_ids, " - ", counts, " problem",
    ifelse(counts == 1L, "", "s")
  )
  labels <- c(labels, "Back")
  cli::cli_h2("Subjects with unresolved problems")
  choice <- diagnosis_prompt_menu(labels, "Choose a subject")
  if (choice == length(labels)) return(NULL)
  diagnosis_choose_subject_job(jobs, subject_ids[[choice]])
}

diagnosis_choose_subject_job <- function(jobs, subject_id) {
  subject_jobs <- jobs[!is.na(jobs$sub_id) & jobs$sub_id == subject_id, , drop = FALSE]
  if (nrow(subject_jobs) == 0L) {
    cli::cli_alert_info("No tracked jobs were found for sub-{subject_id}.")
    return(NULL)
  }
  problems <- diagnosis_problem_jobs(subject_jobs)
  cli::cli_h2("Problems for sub-{subject_id}")
  if (nrow(problems) == 0L) {
    cli::cli_alert_success("This subject has no unresolved problems in the selected scope.")
    return(diagnosis_choose_job(
      subject_jobs, paste0("Tracked jobs for sub-", subject_id)
    ))
  }

  labels <- vapply(seq_len(nrow(problems)), function(i) {
    diagnosis_job_label(problems[i, , drop = FALSE])
  }, character(1))
  labels <- c(
    labels,
    paste0("Show all ", nrow(subject_jobs), " tracked jobs for this subject"),
    "Back"
  )
  choice <- diagnosis_prompt_menu(labels, "Choose a problem to investigate")
  if (choice <= nrow(problems)) return(problems[choice, , drop = FALSE])
  if (choice == nrow(problems) + 1L) {
    return(diagnosis_choose_job(
      subject_jobs, paste0("Tracked jobs for sub-", subject_id)
    ))
  }
  NULL
}

diagnosis_run_label <- function(run) {
  submitted <- if ("submitted" %in% names(run) &&
                   !is.na(run$submitted[[1L]]) && nzchar(run$submitted[[1L]])) {
    paste0(" - submitted ", run$submitted[[1L]])
  } else {
    ""
  }
  paste0(
    run$run_id[[1L]], " - ", run$status[[1L]],
    " - ", run$n_jobs[[1L]], " jobs", submitted
  )
}

diagnosis_choose_run_job <- function(scfg, diagnosis) {
  runs <- diagnosis$inspection$runs
  if (nrow(runs) == 0L) {
    cli::cli_alert_info("No tracked runs were found.")
    return(NULL)
  }
  labels <- vapply(seq_len(nrow(runs)), function(i) {
    diagnosis_run_label(runs[i, , drop = FALSE])
  }, character(1))
  labels <- c(labels, "Back")
  cli::cli_h2("Tracked runs")
  choice <- diagnosis_prompt_menu(labels, "Choose a run")
  if (choice == length(labels)) return(NULL)
  run_id <- runs$run_id[[choice]]
  run_diagnosis <- diagnose_project(
    scfg, run_id = run_id, interactive = FALSE
  )
  diagnosis_choose_scope_job(
    diagnosis_current_jobs(run_diagnosis),
    paste0("Unresolved problems in run ", run_id)
  )
}

diagnosis_choose_scope_job <- function(jobs, heading) {
  problems <- diagnosis_problem_jobs(jobs)
  if (nrow(problems) > 0L) {
    groups <- diagnosis_problem_groups(problems)
    labels <- c(
      groups$label,
      paste0("Show all ", nrow(jobs), " jobs in this scope"),
      "Back"
    )
    cli::cli_h2(heading)
    choice <- diagnosis_prompt_menu(labels, "Choose a problem or broaden the view")
    if (choice <= nrow(groups)) {
      streams <- ifelse(is.na(problems$stream), "", problems$stream)
      keys <- paste(problems$stage, streams, sep = "\r")
      selected <- problems[keys == groups$key[[choice]], , drop = FALSE]
      return(diagnosis_choose_job(
        selected,
        paste0("Problems in ", diagnosis_stage_label(
          groups$stage[[choice]], groups$stream[[choice]]
        )),
        allow_back = nrow(selected) > 1L
      ))
    }
    if (choice == nrow(groups) + 1L) {
      return(diagnosis_choose_job(jobs, "All jobs in the selected scope"))
    }
    return(NULL)
  }
  cli::cli_alert_success("No unresolved problems were found in this scope.")
  diagnosis_choose_job(jobs, "All jobs in the selected scope")
}

diagnosis_top_job <- function(scfg, diagnosis) {
  jobs <- diagnosis_current_jobs(diagnosis)
  problems <- diagnosis_problem_jobs(jobs)
  groups <- diagnosis_problem_groups(problems)
  repeat {
    heading <- if (identical(diagnosis$scope, "project")) {
      "Current unresolved problems"
    } else {
      paste0("Unresolved problems in run ", diagnosis$run_id)
    }
    cli::cli_h2(heading)
    if (nrow(groups) == 0L) {
      cli::cli_alert_success("No unresolved failed, blocked, or cancelled jobs were found.")
    }
    labels <- groups$label
    actions <- character()
    if (any(!is.na(problems$sub_id))) {
      actions <- c(actions, "subjects")
      labels <- c(labels, "Browse subjects with problems")
    }
    actions <- c(actions, "all_jobs")
    labels <- c(labels, "Browse all current jobs")
    if (identical(diagnosis$scope, "project")) {
      actions <- c(actions, "runs")
      labels <- c(labels, "Browse a specific run")
    }
    actions <- c(actions, "exit")
    labels <- c(labels, "Exit")
    choice <- diagnosis_prompt_menu(labels, "Choose a problem or another view")

    if (choice <= nrow(groups)) {
      streams <- ifelse(is.na(problems$stream), "", problems$stream)
      keys <- paste(problems$stage, streams, sep = "\r")
      selected <- problems[keys == groups$key[[choice]], , drop = FALSE]
      job <- diagnosis_choose_job(
        selected,
        paste0("Problems in ", diagnosis_stage_label(
          groups$stage[[choice]], groups$stream[[choice]]
        )),
        allow_back = nrow(selected) > 1L
      )
      if (!is.null(job)) return(job)
      next
    }

    action <- actions[[choice - nrow(groups)]]
    if (action == "exit") return(NULL)
    job <- switch(action,
      subjects = diagnosis_choose_subject(jobs),
      all_jobs = diagnosis_choose_job(jobs, "All current tracked jobs"),
      runs = diagnosis_choose_run_job(scfg, diagnosis)
    )
    if (!is.null(job)) return(job)
  }
}

diagnosis_job_dependencies <- function(scfg, job) {
  run_jobs <- as.data.frame(inspect_project(
    scfg, run_id = job$sequence_id[[1L]]
  )$jobs)
  parent_id <- as.character(job$parent_id[[1L]])
  id <- as.character(job$id[[1L]])
  upstream <- if (!is.na(parent_id) && nzchar(parent_id)) {
    run_jobs[as.character(run_jobs$id) == parent_id, , drop = FALSE]
  } else {
    run_jobs[FALSE, , drop = FALSE]
  }
  downstream <- run_jobs[
    !is.na(run_jobs$parent_id) & as.character(run_jobs$parent_id) == id,
    , drop = FALSE
  ]
  list(upstream = as.data.frame(upstream), downstream = as.data.frame(downstream))
}

diagnosis_print_dependencies <- function(dependencies) {
  cli::cli_h3("Dependency context")
  if (nrow(dependencies$upstream) == 0L) {
    cli::cli_text("Upstream: none")
  } else {
    cli::cli_text("Upstream:")
    for (i in seq_len(nrow(dependencies$upstream))) {
      cli::cli_li(diagnosis_job_label(dependencies$upstream[i, , drop = FALSE]))
    }
  }
  if (nrow(dependencies$downstream) == 0L) {
    cli::cli_text("Direct dependents: none")
  } else {
    cli::cli_text("Direct dependents:")
    for (i in seq_len(nrow(dependencies$downstream))) {
      cli::cli_li(diagnosis_job_label(dependencies$downstream[i, , drop = FALSE]))
    }
  }
}

diagnosis_latest_log <- function(logs, type) {
  paths <- logs$path[logs$type == type]
  if (length(paths) == 0L) return(NULL)
  info <- file.info(paths)
  paths[[which.max(info$mtime)]]
}

diagnosis_show_log <- function(path) {
  n_lines <- prompt_input(
    prompt = "How many lines from the bottom of the log should be shown?",
    type = "integer", default = 20L, lower = 1L
  )
  cli::cli_h3("Log: {.path {path}}")
  lines <- readLines(path, warn = FALSE)
  writeLines(cli::ansi_strip(utils::tail(lines, n_lines)))
}

diagnosis_job_screen <- function(scfg, job, allow_back = TRUE) {
  logs <- find_logs_for_jobs(scfg, job)
  dependencies <- diagnosis_job_dependencies(scfg, job)
  stderr <- diagnosis_latest_log(logs, "stderr")
  stdout <- diagnosis_latest_log(logs, "stdout")

  cli::cli_h2(diagnosis_job_label(job))
  cli::cli_text("Run: {.val {job$sequence_id[[1L]]}}")
  if (!is.na(job$time_submitted[[1L]])) {
    cli::cli_text("Submitted: {job$time_submitted[[1L]]}")
  }
  if (!is.na(job$time_started[[1L]])) {
    cli::cli_text("Started: {job$time_started[[1L]]}")
  }
  if (!is.na(job$time_ended[[1L]])) {
    cli::cli_text("Ended: {job$time_ended[[1L]]}")
  }
  if (nrow(logs) == 0L) {
    cli::cli_alert_warning("No stdout or stderr logs were found for this job.")
  } else {
    cli::cli_alert_info("Found {nrow(logs)} matching log file{?s}.")
  }

  repeat {
    actions <- character()
    labels <- character()
    if (!is.null(stderr)) {
      actions <- c(actions, "view_stderr", "return_stderr")
      labels <- c(labels, "View stderr", "Return stderr as text and exit")
    }
    if (!is.null(stdout)) {
      actions <- c(actions, "view_stdout", "return_stdout")
      labels <- c(labels, "View stdout", "Return stdout as text and exit")
    }
    actions <- c(actions, "dependencies")
    labels <- c(labels, "Show dependency context")
    if (allow_back) {
      actions <- c(actions, "back")
      labels <- c(labels, "Choose another problem")
    }
    actions <- c(actions, "exit")
    labels <- c(labels, "Exit")

    cli::cli_h3("Further diagnosis")
    choice <- diagnosis_prompt_menu(labels, "Choose an action")
    action <- actions[[choice]]
    if (action == "view_stderr") diagnosis_show_log(stderr)
    else if (action == "view_stdout") diagnosis_show_log(stdout)
    else if (action == "return_stderr") return(list(
      action = "return", value = readLines(stderr, warn = FALSE),
      job = job, logs = logs, dependencies = dependencies
    ))
    else if (action == "return_stdout") return(list(
      action = "return", value = readLines(stdout, warn = FALSE),
      job = job, logs = logs, dependencies = dependencies
    ))
    else if (action == "dependencies") diagnosis_print_dependencies(dependencies)
    else return(structure(list(
      action = action, job = job, logs = logs, dependencies = dependencies
    ), class = "bg_interactive_diagnosis"))
  }
}

#' Guided diagnosis backed by the structured project inspection
#'
#' @param input Project configuration, YAML file, or project directory.
#' @param run_id Optional run scope.
#' @param subject_id Optional subject focus.
#' @param job_id Optional exact scheduler job focus.
#' @return A selected interactive diagnosis, returned invisibly, or requested
#'   log text.
#' @noRd
run_interactive_diagnosis <- function(input, run_id = NULL,
                                      subject_id = NULL, job_id = NULL) {
  scfg <- project_config_from_input(input)
  diagnosis <- diagnose_project(
    scfg, run_id = run_id, subject_id = subject_id, job_id = job_id,
    interactive = FALSE
  )
  cli::cli_inform("Running guided project diagnosis...")

  direct_job <- !is.null(job_id)
  repeat {
    jobs <- diagnosis_current_jobs(diagnosis)
    selected <- if (direct_job) {
      jobs[1L, , drop = FALSE]
    } else if (!is.null(subject_id)) {
      diagnosis_choose_subject_job(
        jobs, normalize_inspection_subject_id(subject_id)
      )
    } else if (!is.null(run_id)) {
      heading <- paste0("Unresolved problems in run ", diagnosis$run_id)
      diagnosis_choose_scope_job(jobs, heading)
    } else {
      diagnosis_top_job(scfg, diagnosis)
    }
    if (is.null(selected)) return(invisible(NULL))

    result <- diagnosis_job_screen(scfg, selected, allow_back = !direct_job)
    if (identical(result$action, "return")) return(result$value)
    if (!identical(result$action, "back")) return(invisible(result))
  }
}
