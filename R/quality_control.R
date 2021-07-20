
############ s QUALITY CONTROL ON 2021 DAT #########################

qu_cont <- function(df){
    # set condition to make quality control #######
    sum(is.na(df$value))
    con_index1 <- df$value == 9999
    sum(con_index1, na.rm= T)
    replacements9999 <- df[con_index1,]

    con_index2 <- df$value >= 100 & df$value != 9999
    sum(con_index2, na.rm= T)
    replacements100 <- df[con_index2,]

    con_index3 <- df$value == -9999
    sum(con_index3, na.rm= T)
    replacements9999m <- df[con_index3,]

    con_index4 <- df$value < 0 & df$value != -9999
    sum(con_index4, na.rm= T)
    replacementsminus <- df[con_index4,]

    # setting values to Na if conditions are true
    df_clean <- df

    df_clean$value[con_index1] <- NA
    df_clean$value[con_index2] <- NA
    df_clean$value[con_index3] <- NA
    df_clean$value[con_index4] <- NA

    return(df_clean)
}
