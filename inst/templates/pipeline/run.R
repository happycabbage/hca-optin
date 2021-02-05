


# Source init  ------------------------------------------------------------


source("init.R")



# Source each src file individually ---------------------------------------


source("./src/s1_archive.R")
source("./src/s2_extract.R")
source("./src/s3_consolidate.R")
source("./src/s3_integrate.R")

# execute process step by step --------------------------------------------



dirPath <- get_log_pages()
logTable <- extract_log_pages(path_logs = dirPath)

usersDT <- build_user_table(logTable$user_guid)



# add data to user table --------------------------------------------------


usersDT[, is_hca_account := stringr::str_detect(email, "happycabbage")]

active_users <- usersDT[locked != TRUE & confirmed == TRUE, .SD, .SDcols = !patterns("locked|confirmed")]


usersList <- setNames(split(active_users, active_users$is_hca_account), c("external", "internal"))

clients <- usersList$external


setkeyv(users, "username")


parse_config_data <- function(){
  usr_conf_dir <- "/home/cabbage/config/client/user"
  org_conf_dir <- "/home/cabbage/config/client/org"

  usr_ymls <- list.files(usr_conf_dir)
  org_ymls <- list.files(org_conf_dir)

  f_rm <- function(x) stringr::str_remove(x, "\\.ya?ml$")


  orgDT <- data.table::rbindlist(
    lapply(org_ymls, function(i) {

      conf <- config::get(
        file = stringr::str_glue("{org_conf_dir}/{i}"),
        config = "default"
      )
      data.table::as.data.table(
        c(list(org = f_rm(i)), conf, list(CONF_PATH = attr(conf, "file")))
      )
    }), fill = TRUE
  )

  usrDT <- data.table::rbindlist(
    lapply(usr_ymls, function(i) {

      conf <- config::get(
        file = stringr::str_glue("{usr_conf_dir}/{i}"),
        config = "default"
      )
      data.table::as.data.table(
        c(list(usr = f_rm(i)), conf, list(CONF_PATH = attr(conf, "file")))
      )
    }), fill = TRUE
  )

  setnames(usrDT, "ORG", "orgname")
  setnames(usrDT, "CONF_PATH", "path_conf")
  setnames(usrDT, "usr", "username")

  setnames(orgDT, "ORG", "access_org")
  setnames(orgDT, "CONF_PATH", "path_conf")

  setnames(orgDT, "org", "orgname")
  setnames(orgDT, "ORG_NAME", "orgname_full")
  setnames(orgDT, "POS_SYSTEM", "pos_sysname")
  setnames(orgDT, "HAS_PRODUCT_INFO", "has_product_info")
  setnames(orgDT, "ORG_TIMEZONE", "account_tz")


  tmp_org <- orgDT[, .SD, .SDcols = c("orgname", "orgname_full",
                                      "pos_sysname", "has_product_info",
                                      "account_tz")]

  tmp_usr <- usrDT[, .SD, .SDcols = c("username", "orgname")]

  setkeyv(tmp_org, "orgname")
  setkeyv(tmp_usr, "orgname")

  tmp_usr[tmp_org][order(orgname, username)][orgname != "demo"]


}
