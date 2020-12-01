avpublish_workflow_data <- function(
    .data, fastq_column1 = "fastq1_src", fastq_column2 = "fastq2_src", 
    entity = names(.data)[[1]], namespace = avworkspace_namespace(), 
    name = avworkspace_name()) {

    ## avworkspace("bioconductor-rpci-anvil/Bioconductor-Workflow-DESeq2")

    stopifnot(AnVIL:::.is_scalar_character(fastq_column1),
        AnVIL:::.is_scalar_character(fastq_column2),
        AnVIL:::.is_scalar_character(namespace), 
        AnVIL:::.is_scalar_character(name),
        !all(c("fastq1", "fastq2") %in% names(.data)),
        any(c(entity, paste0(entity,"_id")) %in% names(.data))) 

    ## download files to workspace bucket
    urls1 <- .data %>% pull(fastq_column1)
    urls2 <- .data %>% pull(fastq_column2)
    urls <- c(urls1, urls2)

    ## could be files already exist in gs
    ## could copy from one bucket to another
    dir <- tempfile()
    dir.create(dir)
    fastqs <- file.path(dir, basename(urls))
    idx <- grepl("^(https?|ftp)://", urls)
    results <- Map(download.file, urls[idx], fastqs[idx], "curl") ## should method be set to "curl"?
    fastqs[!idx] <- urls[!idx]

    AnVIL::gsutil_cp(fastqs, file.path(AnVIL::avbucket(), "data"))

    ## push 'tbl' to DATA TABLE
    .data <- .data %>% mutate(
        fastq1 = file.path(AnVIL::avbucket(), "data", basename(urls1)), 
        fastq2 = file.path(AnVIL::avbucket(), "data", basename(urls2))
        )
    .data %>% avtable_import(entity = entity, namespace = namespace, name = name)

    ## push 'tbl_set' to DATA TABLE
    avtable(entity) %>% 
        mutate(set = paste0(entity, "_set")) %>%
        avtable_import_set(entity, "set", "name")
}
