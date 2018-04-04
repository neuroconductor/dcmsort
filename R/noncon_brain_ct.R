#' Subset Non-contrast Brain CT DICOM data
#'
#' @param sub_hdr A wide data.frame of header information, from
#' \code{\link{subset_hdr}}
#' @param delete_localizers Should the localizers be deleted (on disk).
#' They are removed from data regardless.
#' @param min_files Minimum number of files in a series to include as a scan.
#'
#' @return A list of the subset CT data and the non-CT data
#' @export
#'
#' @importFrom dplyr group_by ungroup mutate n
noncon_brain_ct = function(
  sub_hdr,
  delete_localizers = FALSE,
  min_files = 6
  ) {

  ct = directory = NULL
  rm(list = c("ct", "directory"))

  wide = sub_hdr
  orig_wide = wide

  # keep only ct
  wide = wide %>%
    filter(ct)

  removed = orig_wide %>%
    filter(!(file %in% wide$file))

  if (!"ImageType" %in% colnames(wide)) {
    warning("ImageType not in the header of the data!")
    wide$ImageType = ""
  }
  wide$ImageType = gsub("\\\\", ",", wide$ImageType)
  ######################################
  # Remove localizers/Dose Reports/
  # Circle of WILLIS (TERARECON)
  ######################################
  bad_files = wide$file[
    grepl(paste(
      "(SCREEN SAVE)", "LOCALIZER", "TERARECON",
      "(CT_SOM5 PROT)", "(DOSE REPORT)",
      "(SECONDARY,OTHER)", "DOSE_INFO", sep = "|"),
      wide$ImageType)]

  # Actually delete these
  fe = file.exists(bad_files)
  if (any(fe) && delete_localizers) {
    file.remove(bad_files[fe])
  }

  wide = wide %>%
    filter(!(file %in% bad_files))


  ######################################
  # Remove Bone scans
  ######################################
  bad_files = wide$file[
    grepl("BONE", wide$ConvolutionKernel)]


  wide = wide %>%
    filter(!(file %in% bad_files))

  wide = wide %>%
    mutate(directory = dirname(file)) %>%
    group_by(directory) %>%
    mutate(n = n()) %>%
    ungroup()

  ######################################
  # Remove Bone scans
  ######################################
  bad_files = wide$file[
    grepl("BONE", toupper(wide$SeriesDescription))]

  wide = wide %>%
    filter(!(file %in% bad_files))

  wide = wide %>%
    mutate(directory = dirname(file)) %>%
    group_by(directory) %>%
    mutate(n = n()) %>%
    ungroup()


  ######################################
  # Remove ANGIO
  ######################################
  # bad_files = wide$file[
  # grepl("ANGIO", wide$StudyDescription)]
  angio = grepl("ANGIO|COW|WILLIS", toupper(wide$SeriesDescription))
  angio = angio | (
    grepl("CTA", wide$SeriesDescription) &
      grepl("ANGIO", wide$StudyDescription))
  bad_files = wide$file[angio]

  wide = wide %>%
    filter(!(file %in% bad_files))

  wide = wide %>%
    mutate(directory = dirname(file)) %>%
    group_by(directory) %>%
    mutate(n = n()) %>%
    ungroup()


  ######################################
  # Remove Spine/CHEST
  ######################################
  angio = grepl("SPINE", toupper(wide$SeriesDescription))
  angio = angio | grepl("CHEST", toupper(wide$SeriesDescription))
  angio = angio | (
    !grepl("BRAIN|BLOOD|HEAD", toupper(wide$SeriesDescription)) &
      grepl("SPINE", toupper(wide$StudyDescription)))
  bad_files = wide$file[angio]

  wide = wide %>%
    filter(!(file %in% bad_files))

  wide = wide %>%
    mutate(directory = dirname(file)) %>%
    group_by(directory) %>%
    mutate(n = n()) %>%
    ungroup()


  # fewer than 10 scans
  wide = wide[ wide$n >= min_files,]
  removed = orig_wide %>%
    filter(!(file %in% wide$file))

  L = list(ct_data = wide,
           non_ct_data = removed)
  return(L)
}
