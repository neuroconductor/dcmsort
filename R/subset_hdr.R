#' Subset the Header files and Reshape
#'
#' @param all_hdr An output from \code{\link{read_all_hdr}}
#' @param keep_tags tags to keep in the data
#' @param unique_fields The fields that should uniquely identify a DICOM
#' @param force Should the function warn (as opposed to fail)
#' if \code{unique_fields} has
#' duplicates?
#'
#' @return A wide \code{data.frame} of the information needed for filtering
#' @export
#'
#' @importFrom tidyr spread
#' @importFrom dplyr mutate select
subset_hdr = function(
  all_hdr,
  keep_tags = relevant_tags(),
  unique_fields = relevant_fields(),
  force = TRUE
) {

  ConvolutionKernel = ImageType = StudyDescription = NULL
  ind = name = tag = value = NULL
  rm(list = c("ConvolutionKernel", "ImageType", "StudyDescription",
              "ind", "name", "tag", "value"))

  # (0040,a040)
  sub_hdr = all_hdr %>%
    filter(tag %in% keep_tags) %>%
    select(file, tag, name, value)
  # saveRDS(sub_hdr, file = file.path(
  #   dir, "relevant_tags.rds"))


  # this should remove completely identical tags
  sub_hdr = sub_hdr %>%
    distinct()


  keep_files = sub_hdr %>%
    group_by(file, tag) %>%
    filter(!is.na(value)) %>%
    mutate(ind = seq(n())) %>%
    ungroup()

  multi = keep_files %>%
    filter(ind > 1)
  u_name = unique(multi$name)

  bad = u_name %in% unique_fields
  if (any(bad)) {
    message("Records have multiple fields doubled!")
    print(u_name[bad])
    msg = "Double records in no double data!"
    if (force) {
    warning(msg)
    } else {
      stop(msg)
    }
  }

  # keep
  keep_files = keep_files %>%
    filter(ind == 1)


  wide = keep_files %>%
    select(-tag) %>%
    spread(key = name, value = value)
  cn = colnames(wide)
  for (icn in c("ConvolutionKernel", "StudyDescription",
                "ImageType")) {
    if (!icn %in% cn) {
      warning(paste0("No ", icn))
      wide[, icn] = ""
    }
  }

  wide = wide %>%
    mutate(ImageType = toupper(ImageType),
           ConvolutionKernel = toupper(ConvolutionKernel),
           StudyDescription = toupper(StudyDescription))

  ct_files = sub_hdr %>%
    filter(tag %in% "(0008,0060)") %>%
    distinct()

  ct_files = ct_files %>%
    filter(grepl("CT", value))
  ct_files = ct_files$file

  wide = wide %>%
    mutate(ct = file %in% ct_files)


  return(wide)
}
