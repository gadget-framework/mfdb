library(ncdf4)
library(XML)

# Read required details in from bgm file
atlantis_read_areas <- function (adir, bgm_file = Sys.glob(file.path(adir, "*.bgm"))) {
    if (length(bgm_file) != 1) stop("One bgm file required, not ", length(bgm_file))

    get_box_attribute <- function (bgm_lines, field_name, new_name = field_name) {
        # Extract lines we are interested and break up into key/val
        rv <- grep(paste0("^[A-Za-z0-9]+\\.", field_name), bgm_lines, value = TRUE)
        rv <- strsplit(rv, paste0(".", field_name, "\\s+"))

        # Convert to data.frame & transpose
        rv <- t(as.data.frame(rv))
        rownames(rv) <- NULL
        colnames(rv) <- c("id", new_name)
        return(rv)
    }

    # Extract parts of file we are interested in
    bgm_lines <- readLines(bgm_file)
    area_data <- merge(
        get_box_attribute(bgm_lines, "label", "name"),
        get_box_attribute(bgm_lines, "area", "size"),
        sort = FALSE)
    area_data$id <- seq_len(nrow(area_data))
    return(area_data)
}

xmlGetAttributes <- function (xml_doc, group_name, group_attributes) {
    xmlAllAttrs <- Vectorize(XML::xmlAttrs)
    attr_xpath <- paste0(
        "./Attribute[contains('|",
        paste(group_attributes, collapse = "|"),
        "|', concat('|', @AttributeName, '|'))]")
    
    group_nodes <- XML::getNodeSet(xml_doc, paste0("//*[@AttributeGroupName='", group_name, "']"))
    as.data.frame(t(vapply(group_nodes, function (n) {
        # Pull out all attributes we are interested in from the group
        rv <- xmlAllAttrs(XML::getNodeSet(n, attr_xpath))
        structure(rv["AttributeValue",], names = rv["AttributeName",])
    }, rep("", length(group_attributes)))))
}

atlantis_functional_groups <- function (adir, fg_file, bio_file) {
    fg_doc <- XML::xmlParse(file.path(adir, fg_file))
    fg_data <- xmlGetAttributes(fg_doc, 'FunctionalGroup', c('GroupCode', 'Name', 'LongName', 'IsPredator', 'IsTurnedOn', 'NumCohorts', 'NumStages', 'NumAgeClassSize'))

    # Pull out useful flags from biology file and combine
    xmlAllAttrs <- Vectorize(XML::xmlAttrs)
    bio_doc <- XML::xmlParse(file.path(adir, bio_file))
    for (flag in c('FLAG_AGE_MAT', 'FLAG_LI_A', 'FLAG_LI_B')) {
        bio_flags <- xmlAllAttrs(XML::getNodeSet(bio_doc, paste0("//Attribute[@AttributeName='", flag, "']/GroupValue")))
        flag_table <- data.frame(
            GroupCode = bio_flags["GroupName",],
            Value = as.numeric(bio_flags["AttributeValue",]),
            stringsAsFactors = TRUE)
        names(flag_table)[[2]] <- flag
        fg_data <- merge(fg_data, flag_table, all.x = TRUE, sort = FALSE)
    }

    return(fg_data)
}

atlantis_run_options <- function (adir, opt_file) {
    opt_doc <- XML::xmlParse(file.path(adir, opt_file))
    opt_data <- xmlGetAttributes(opt_doc, "ScenarioOptions", c("dt"))

    return(opt_data)
}

atlantis_fg_count <- function (adir,
        nc_file = Sys.glob(file.path(adir, "output*.nc")),
        area_data,
        fg_group,
        start_year = 1948) {
    nc_out <- ncdf4::nc_open(file.path(adir, nc_file))

    # Fetch 1_Nums..x_Nums for functional group, put in a 4 dimensional array,
    nc_variables <- paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts)), '_Nums')
    rv <- array(
        Vectorize(ncdf4::ncvar_get, vectorize.args = 'varid')(nc_out, nc_variables),
        dim = c(
            length(nc_out$dim$z$vals),
            length(nc_out$dim$b$vals),
            length(nc_out$dim$t$vals),
            as.character(fg_group$NumCohorts)),
        dimnames = list(
            depth = nc_out$dim$z$vals,
            area = area_data$name,
            time = nc_out$dim$t$vals,
            ageClass = seq_len(as.character(fg_group$NumCohorts))))

    # Flatten into data frame, convert columns back to integer
    rv <- as.data.frame.table(rv, responseName = 'count')
    age_class_size <- as.numeric(as.character(fg_group$NumAgeClassSize))
    year_secs <- 60 * 60 * 24 * 365  # NB: Atlantis treats years as 365 days, no execeptions
    month_secs <- year_secs / 12 # TODO: If month == 30 days is used, this will slip
    data.frame(
        depth = as.numeric(levels(rv$depth))[rv$depth],
        area = rv$area,
        time = rv$time,
        # Add start_year to years
        year = (as.numeric(levels(rv$time)) / year_secs + start_year)[rv$time],
        # Months are remainder from year_secs divided by month_secs
        month = (((as.numeric(levels(rv$time)) %% year_secs) %/% month_secs) + 1)[rv$time],
        # Age is mid-point of sequence of age_class_size values
        age = seq(age_class_size / 2, to = age_class_size * 10, by = age_class_size)[rv$ageClass],
        # Maturity stage is mature iff ageClass greater than FLAG_AGE_MAT
        maturity_stage = ifelse(as.numeric(levels(rv$ageClass)) > fg_group$FLAG_AGE_MAT, 5, 1)[rv$ageClass],
        count = rv$count,
        stringsAsFactors = TRUE)
}

lv_dir <- 'atlantis-L_Vic-OutputFolderTest2/'
lv_area_data <- atlantis_read_areas(lv_dir)
lv_functional_groups <- atlantis_functional_groups(lv_dir, 'LVGroups.xml', 'LV_biol.xml')
lv_run_options <- atlantis_run_options(lv_dir, 'LV_run.xml')
lv_fg_count <- atlantis_fg_count(lv_dir, 'outputLV.nc', lv_area_data,
    lv_functional_groups[c(lv_functional_groups$Name == 'Birds'),])

ice_dir <- 'atlantis-Iceland-NoFishing20150909-1'
ice_options <- atlantis_run_options(ice_dir, 'RunNoFish.xml')
ice_area_data <- atlantis_read_areas(ice_dir)
ice_functional_groups <- atlantis_functional_groups(ice_dir, 'GroupsIceland.xml', 'BiologyNoFish.xml')
ice_run_options <- atlantis_run_options(ice_dir, 'RunNoFish.xml')
ice_fg_count <- atlantis_fg_count(ice_dir, 'OutputNoFish.nc', ice_area_data,
    ice_functional_groups[c(ice_functional_groups$Name == 'Cod'),])
