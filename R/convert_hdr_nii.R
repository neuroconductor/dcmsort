
#' Convert the data from a Header \code{data.frame} into NIfTI files
#'
#' @param sub_hdr Output from \code{\link{subset_hdr}} or the
#' data relevant from \code{\link{noncon_brain_ct}}
#' @param merge_files Should files be merged, passed do \code{dcm2nii} options
#' @param overwrite Should files be overwritten if they exist
#' @param ignore_derived Should derived images be ignored,
#' passed do \code{dcm2nii} options
#'
#' @return A list of the output filenames and the result for each directory
#' \code{dcm2nii} call
#' @export
#'
#' @importFrom dcm2niir dcm2nii check_dcm2nii
#' @importFrom neurobase readnii window_img writenii
#' @importFrom oro.nifti cal_img scl_slope scl_inter aux_file descrip dim_
#' @importFrom oro.nifti scl_slope<- scl_inter<- aux_file<- descrip<-
#'
convert_hdr_nii = function(
  sub_hdr,
  merge_files = TRUE,
  ignore_derived = TRUE,
  overwrite = FALSE) {

  directory = NULL
  rm(list = "directory")

  wide = sub_hdr
  ### still a bone window
  run_dirs = unique(wide$directory)


  all_output = ""
  df = data.frame(
    directory =  run_dirs,
    result = NA, stringsAsFactors = FALSE)
  for (i in seq_along(run_dirs)) {
    print(i)
    basedir = run_dirs[i]
    bn = basename(basedir)
    opts = paste0(
      "-9 ",
      ifelse(ignore_derived, "-i y ", ""),
      ifelse(merge_files, " -m y ", ""),
      " -f %t ")
    opts = gsub("\\s+", " ", opts)
    opts = trimws(opts)
    res = dcm2nii(
      basedir = basedir,
      opts = opts )
    # result
    # if (res$result <= 1 ) {
    #   if (res$result != 0) {
    #     warning(paste0("directory is ", basedir,
    #                 "has non-zero result"))
    #   }
    df$result[i] = res$result
    if (res$result == 0) {
      outfile = check_dcm2nii(res)
      new_file = file.path(
        directory,
        paste0(bn, "_", basename(outfile)))
      all_output = c(all_output, outfile)
      if (length(outfile) > 0 & !all(file.exists(new_file))|| overwrite) {
        for (ifile in seq_along(outfile)) {
          ofile = outfile[ifile]
          if (dim_(ofile)[4] > 1) {
            window = c(-1024, 3071)
            img = readnii(ofile)
            img = window_img(img, window = window)
            img = cal_img(img)
            scl_slope(img) = 1
            scl_inter(img) = 0
            aux_file(img) = ""
            descrip(img) = ""
            ####################################
            # write out the image
            ####################################
            writenii(img, filename = new_file[ifile])
          } else {
            message(
              paste0(
                "Image ", ofile,
                "did not have > 2 dimensions - not converted"))
          }
        }

        # file.copy(outfile, new_file,
        #   overwrite = TRUE)
      } else {
        if (length(outfile) == 0) {
          message(paste0(
            "basedir: ", basedir,
            " had no output!"))
        }
      }
    } else {
      warning(paste0("directory is ", basedir,
                     " and dcm2nii failed"))
    }
  }

  all_output = unique(all_output)
  all_output = all_output[file.exists(all_output)]
  L = list(dcm2nii_results = df,
           output_files = all_output)
  return(L)
}