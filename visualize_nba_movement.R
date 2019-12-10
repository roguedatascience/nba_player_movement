library(tidyverse)
source('functions.R')
library(archive) #https://github.com/jimhester/archive

file_path <-
    'https://github.com/sealneaward/nba-movement-data/raw/master/data/01.18.2016.GSW.at.CLE.7z'

### Get zipped gamefile
download.file(file_path, 'temp.7z', mode = 'wb')

### Extract JSON file from zip
archive_extract('temp.7z')

### Read JSON file - function in functions.R file
df <-
    sportvu_convert_json('0021500622.json')

df <-
    df %>%
    arrange(event.id, player_id)

players <-
    df %>%
    select(player_id, firstname, lastname) %>%
    distinct()

temp_df <-
    df %>%
    #filter(event.id <= 50) %>%
    mutate(grp = trunc(0:(n() - 1) / 5)) %>%
    group_by(event.id, player_id, firstname, lastname, grp) %>%
    summarise(x_loc = mean(x_loc),
              y_loc = mean(y_loc)) %>%
    mutate(x_loc_end = lag(x_loc, n = 1),
           y_loc_end = lag(y_loc, n = 1)) %>%
    ungroup() %>%
    filter(!is.na(x_loc_end) & !is.na(y_loc_end)) %>%
    arrange(grp, player_id)

top_15_df <-
    temp_df %>%
    filter(player_id != '-1') %>%
    group_by(player_id, firstname, lastname) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    arrange(desc(cnt)) %>%
    slice(1:15)

top_15 <-
    top_15_df %>%
    .$player_id %>%
    unique()

colors_df <-
    top_15_df %>%
    mutate(color = ggplotColours(n = 15))

temp_df <-
    temp_df %>%
    left_join(colors_df, by = 'player_id') %>%
    mutate(player_id = factor(player_id, levels = top_15_df$player_id))

temp_df_ball <-
    df %>%
    #filter(event.id <= 50) %>%
    mutate(grp = trunc(0:(n() - 1) / 30)) %>%
    group_by(event.id, player_id, grp) %>%
    summarise(x_loc = mean(x_loc),
              y_loc = mean(y_loc)) %>%
    mutate(x_loc_end = lag(x_loc, n = 1),
           y_loc_end = lag(y_loc, n = 1)) %>%
    ungroup() %>%
    filter(!is.na(x_loc_end) & !is.na(y_loc_end)) %>%
    arrange(grp, player_id) %>%
    mutate(player_id = as.factor(player_id))


pdf('all_players.pdf', width = 10, height = 5)

ggplot() +
    # geom_point(
    #     data = 
    #         temp_df_ball %>%
    #         filter(player_id == '-1'),
    #     aes(x = x_loc, y = y_loc,
    #         #xend = x_loc_end, yend = y_loc_end,
    #         group = player_id, col = player_id),
    #     alpha = .3, size = 1,
    #     col = '#ffffff'
    # ) +
    geom_segment(
        data =
            temp_df %>%
            #filter(player_id %in% c('201939', '2544')),
            filter(player_id %in% top_15),
        aes(x = x_loc, y = y_loc,
            xend = x_loc_end, yend = y_loc_end,
            group = player_id,
            col = color),
        alpha = .05
    ) +
    scale_color_identity() +
    scale_x_continuous(
        limits = c(-8, 100),
        expand = expand_scale(mult = .01)
    ) +
    scale_y_continuous(
        limits = c(-4, 54),
        expand = expand_scale(mult = .03)
    ) +
    theme_void() +
    theme(legend.position = 'none') +
    theme(plot.background = element_rect(fill = '#333333'))


dev.off()



pdf('lebron_v_steph.pdf', width = 10, height = 5)

ggplot() +
    geom_segment(
        data =
            temp_df %>%
            filter(player_id %in% c('201939', '2544')),
            #filter(player_id %in% top_15),
        aes(x = x_loc, y = y_loc,
            xend = x_loc_end, yend = y_loc_end,
            group = player_id,
            col = color),
        alpha = .05
    ) +
    scale_color_identity() +
    scale_x_continuous(
        limits = c(-8, 100),
        expand = expand_scale(mult = .01)
    ) +
    scale_y_continuous(
        limits = c(-4, 54),
        expand = expand_scale(mult = .03)
    ) +
    theme_void() +
    theme(legend.position = 'none') +
    theme(plot.background = element_rect(fill = '#333333'))


dev.off()


pdf('kyrie_v_klay.pdf', width = 10, height = 5)

ggplot() +
    geom_segment(
        data =
            temp_df %>%
            filter(player_id %in% c('202681', '202691')),
        #filter(player_id %in% top_15),
        aes(x = x_loc, y = y_loc,
            xend = x_loc_end, yend = y_loc_end,
            group = player_id,
            col = color),
        alpha = .05
    ) +
    scale_color_identity() +
    scale_x_continuous(
        limits = c(-8, 100),
        expand = expand_scale(mult = .01)
    ) +
    scale_y_continuous(
        limits = c(-4, 54),
        expand = expand_scale(mult = .03)
    ) +
    theme_void() +
    theme(legend.position = 'none') +
    theme(plot.background = element_rect(fill = '#333333'))


dev.off()



pdf('tristan_v_draymon.pdf', width = 10, height = 5)

ggplot() +
    geom_segment(
        data =
            temp_df %>%
            filter(player_id %in% c('202684', '203110')),
        #filter(player_id %in% top_15),
        aes(x = x_loc, y = y_loc,
            xend = x_loc_end, yend = y_loc_end,
            group = player_id,
            col = color),
        alpha = .05
    ) +
    scale_color_identity() +
    scale_x_continuous(
        limits = c(-8, 100),
        expand = expand_scale(mult = .01)
    ) +
    scale_y_continuous(
        limits = c(-4, 54),
        expand = expand_scale(mult = .03)
    ) +
    theme_void() +
    theme(legend.position = 'none') +
    theme(plot.background = element_rect(fill = '#333333'))


dev.off()


ggplotColours <- function(n = 6, h = c(0, 360) + 15){
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

ggplotColours(n = 14)


#A58AFF
# 201939 E38900