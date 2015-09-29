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
        get_box_attribute(bgm_lines, "area", "size"))
    area_data$id <- seq_len(nrow(area_data))
    return(area_data)
}

xmlGetAttributes <- function (xml_doc, group_name, attributes) {
    xmlAllAttrs <- Vectorize(XML::xmlAttrs)
    attr_xpath <- paste0(
        "./Attribute[contains('|",
        paste(attributes, collapse = "|"),
        "|', concat('|', @AttributeName, '|'))]")
    
    group_nodes <- XML::getNodeSet(xml_doc, paste0("//*[@AttributeGroupName='", group_name, "']"))
    as.data.frame(t(vapply(group_nodes, function (n) {
        # Pull out all attributes we are interested in from the group
        rv <- xmlAllAttrs(XML::getNodeSet(n, attr_xpath))
        structure(rv[2,], names = rv[1,])
    }, rep("", length(attributes)))))
}

atlantis_functional_groups <- function (adir, fg_file, bio_file) {
    xmlAllAttrs <- Vectorize(XML::xmlAttrs)
    fg_keys <- c('GroupCode', 'Name', 'LongName', 'IsPredator', 'IsTurnedOn', 'NumCohorts', 'NumStages', 'NumAgeClassSize')

    fg_doc <- XML::xmlParse(file.path(adir, fg_file))
    fg_data <- xmlGetAttributes(fg_doc, 'FunctionalGroup', fg_keys)

    # Pull out useful flags from biology file and combine
    bio_doc <- XML::xmlParse(file.path(adir, bio_file))
    bio_nodes <- XML::getNodeSet(bio_doc, "//Attribute[@AttributeName='FLAG_AGE_MAT']/GroupValue")
    flag_age_mat <- as.data.frame(t(xmlAllAttrs(bio_nodes)))
    names(flag_age_mat) <- c('GroupCode', 'FLAG_AGE_MAT')
    fg_data <- merge(fg_data, flag_age_mat, all.x = TRUE)

    bio_nodes <- XML::getNodeSet(bio_doc, "//Attribute[@AttributeName='SpawningProportion']/GroupValue")
    spawning_proportion <- as.data.frame(t(xmlAllAttrs(bio_nodes)))
    spawning_proportion <- data.frame(
        GroupCode = spawning_proportion$GroupName,
        SpawningProportion = I(lapply(strsplit(as.character(spawning_proportion$AttributeValue), " "), as.numeric)),
        stringsAsFactors = FALSE)
    fg_data <- merge(fg_data, spawning_proportion, all.x = TRUE)

    return(fg_data)
}

atlantis_run_options <- function (adir, opt_file) {
    xmlAllAttrs <- Vectorize(XML::xmlAttrs)
    opt_keys <- c('dt')

    opt_doc <- XML::xmlParse(file.path(adir, opt_file))
    opt_nodes <- XML::getNodeSet(fg_doc, "//*[@AttributeGroupName='ScenarioOptions']")
    
}

atlantis_fg_count <- function (adir, nc_file = Sys.glob(file.path(adir, "output*.nc")), area_data, fg_group) {
    nc_out <- ncdf4::nc_open(file.path(adir, nc_file))

    # Fetch 1_Nums..x_Nums for functional group, put in a 4 dimensional array
    nc_variables <- paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts)), '_Nums')
    count <- array(
        Vectorize(ncdf4::ncvar_get, vectorize.args = 'varid')(nc_out, nc_variables),
        dim = c(4,17,54,10),
        dimnames = list(
            paste0("layer", 1:4), # Depth layers
            area_data$name, # Area boxes
            1:54,  # Times
            seq_len(as.character(fg_group$NumCohorts)))) # Age Groups
   
    # 3-dimensional of bird (age-group 5) count, same as vars$Birds5_Nums / disagg$Birds5_Nums
    # count[input$layer,area?,input$time]
}

lv_dir <- 'atlantis-L_Vic-OutputFolderTest2/'
lv_area_data <- atlantis_read_areas(lv_dir)
lv_functional_groups <- atlantis_functional_groups(lv_dir, 'LVGroups.xml', 'LV_biol.xml')
lv_fg_count <- atlantis_fg_count(lv_dir, 'outputLV.nc', area_data,
    lv_functional_groups[c(lv_functional_groups$Name == 'Birds'),])

ice_dir <- 'atlantis-Iceland-NoFishing20150909-1'
ice_area_data <- atlantis_read_areas(ice_dir)
ice_functional_groups <- atlantis_functional_groups(ice_dir, 'GroupsIceland.xml', 'BiologyNoFish.xml')
ice_fg_count <- atlantis_fg_count(ice_dir, 'OutputNoFish.nc', area_data,
    ice_functional_groups[c(ice_functional_groups$Name == 'Cod'),])
