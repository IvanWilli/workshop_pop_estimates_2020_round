### funciones auxiliares para workshop
### "Análisis demográfico en R: técnicas básicas y DemoTools"
### "Estimaciones de población y ronda censal 2020: Desafíos y lecciones aprendidas en tiempos de pandemia"
### Algunas funciones fueron creadas especialmente para el workshop, y no testeadas en múltiples contextos.
### Iván Williams. Cualquier duda escribir a ivan.williams@un.org

# CS splines --------------------------------------------------------------
# función creada con base en los scripts de Carl Schmertmann, y adaptada por CELADE
# Detalles en el sitio: http://calibrated-spline.schmert.net/REBEP/
graduate_cs <- 
     function( Age,
               Value ){
          
          age <- min( Age ) : ( max( Age ) + 4 )
          asfr_x5 <- Value
          
          K7 <- 
               as.matrix( 
                    read.csv( "http://calibrated-spline.schmert.net/REBEP/K7-10000.csv", 
                              header = T, 
                              row.names = 1 )
               )
          
          a = as.numeric( dimnames(K7)[[1]] )
          S = outer( age, a, function(aa,xx) { 1 *((aa <= xx)&(xx < aa+1))})
          S = sweep(S, 1, apply(S,1,sum), "/")
          dimnames(S) = list(age, a)
          
          
          tmp <- c( S %*% K7 %*% asfr_x5 )
          names(tmp) <- age
          return( tmp )
     }

# Evolución poblacional por cohorte ---------------------------------------
# tanto pop, births, deaths and nmigr deben tener las variables yb (year of birth), yd (year of death), age, pop, B (births), D (deaths), según corresponda.
# la variable t0 debe ser mayor o igual al primer año de datos en births and deaths 
# como outputs principales esta la población por edad y cohorte para cada 1 de enero en cada año, y un gráfico con un diagrama de Lexis

lexis_pop <- function(pop = NULL, births, deaths, nmigr = NULL, 
                      sex = "b", ax = NULL, NewOAG = 100, 
                      t0 = NULL, plot_lexis = TRUE){
     
     births_age_mother <- births
     births <- births %>% group_by(yb) %>% summarise(B = sum(B))
     delta <- unique(diff(births$yb))
     
     # split death triangles: return yb, yd, age, D    
     deaths <- split_triangles_D(deaths, births, ax = ax)
     
     # split smigr triangles: return yb, yd, age, M    
     if(is.null(nmigr)){
          nmigr <- deaths %>% rename(ym=yd, NM=D) %>% mutate(NM=0)
     }else{
          nmigr <- split_triangles_NM(nmigr)  
     }
     
     # rename pop and filter posterior events
     if(is.null(t0)) t0 <- unique(pop$t)
     deaths <- deaths %>% filter(yd>=t0)
     nmigr <- nmigr %>% filter(ym>=t0)
     
     # new cohorts 
     if(!is.null(births)){
          births <- births %>% filter(yb>=t0)
          pop_new <- births %>% 
               left_join(deaths, by="yb") %>%
               left_join(nmigr %>% rename(yd=ym), by=c("yb","yd","age")) %>% 
               mutate(t = yd + delta) %>% 
               group_by(yb, B, t) %>% 
               summarise(D = sum(D), NM = sum(NM)) %>%
               group_by(yb, B) %>% 
               mutate(Dcum = cumsum(D), NMcum = cumsum(NM)) %>% ungroup() %>% 
               mutate(age = t-yb-delta, pop = B-Dcum+NMcum)
     }else{
          pop_new <- pop <- pop_new %>% slice(0)
     }
     
     # actual cohorts
     if(!is.null(pop)){
          pop <- pop  %>% mutate(yb = t - age - delta)
          pop_act <- pop  %>% 
               rename(t0 = t, age0 = age, pop0 = pop) %>% 
               left_join(deaths %>% 
                              left_join(nmigr %>% rename(yd = ym), by = c("yd", "age", "yb")),
                         by = "yb") %>% 
               group_by(age0, pop0, t0, yb, yd) %>% 
               summarise(D = sum(D), NM=sum(NM)) %>%
               group_by(age0,pop0,t0,yb) %>% 
               mutate(Dcum = cumsum(D), NMcum = cumsum(NM)) %>% 
               ungroup() %>% 
               mutate(age = age0+yd-t0+delta, pop = pop0-Dcum+NMcum, t=t0+age-age0)
     }else{
          pop_act <- pop <- pop_new %>% slice(0)
     }
     
     # join
     pop_out <- rbind(
          pop_new %>% select(t, yb, age, pop),
          pop_act %>% select(t, yb, age, pop),
          pop) %>%
          mutate(age = ifelse(age>NewOAG,NewOAG,age)) %>%
          group_by(t, yb, age) %>% 
          summarise(pop = sum(pop, na.rm=T)) %>% ungroup() %>% 
          mutate(pop = pmax(pop, .5)) %>% # take care of this in oldest
          arrange(t, age)
     
     # lexis plot
     if(plot_lexis){
          # sum deaths and nmigr
          deaths_nmigr <- deaths %>% 
               left_join(nmigr %>% rename(yd = ym), by = c("yd", "age", "yb")) %>% 
               mutate(D = NM - D)
          plot_lexis <- lexis_plot(pop_start = pop, 
                                   deaths = deaths_nmigr, 
                                   births = births, 
                                   pop_end = pop_out, 
                                   ages = sort(unique(pop_out$age)), 
                                   years = sort(unique(pop_out$yd)),
                                   decimals = 0)  
     }else{
          plot_lexis <- NA
     }
     
     # out
     return(list(pop_out = pop_out, 
                 births = births, deaths = deaths, nmigr = nmigr,
                 plot_lexis = plot_lexis))
}

# create lexis plot 
# adaptado de https://stackoverflow.com/questions/68774622/combining-multiple-confusion-matrices-tile-plot-with-rectangles-instead-of-til)
lexis_plot <- function(pop_start, deaths, births, pop_end, ages = NULL, years = NULL, decimals = 0){
     
     if(is.null(ages)) ages <- sort(unique(pop_out$age))
     df <- deaths %>% 
          rename(t = yd) %>% 
          filter(age %in% ages) %>%
          mutate(Type = ifelse((t-age) == yb, "min", "max"),
                 D = round(D, decimals),
                 pos_neg = ifelse(D>0, "red", "#0b6623")) %>% 
          select(t, age, Type, D, pos_neg) %>% 
          arrange(t, age)
     
     # Now get the centre co-ordinates of each tile by converting factors to numbers, 
     # and add a column that records the original row number:
     
     df$x <- as.numeric(factor(df$t))
     df$y <- as.numeric(factor(df$age))
     df$observation <- seq(nrow(df))
     
     # Now make three copies of each row:
     
     df <- df[rep(seq(nrow(df)), each = 3),]
     
     # And we can calculate the co-ordinates of the triangles' vertices:
     
     polyxmin <- df$x + c(-0.5, 0.5,  0.5)
     polyxmax <- df$x + c(-0.5, -0.5, 0.5)
     polyymin <- df$y + c(-0.5, -0.5, 0.5)
     polyymax <- df$y + c(-0.5, 0.5,  0.5)
     
     df$polyx <- numeric(nrow(df))
     df$polyx[df$Type == "min"] <- polyxmin[df$Type == "min"]
     df$polyx[df$Type == "max"] <- polyxmax[df$Type == "max"]
     
     df$polyy <- numeric(nrow(df))
     df$polyy[df$Type == "min"] <- polyymin[df$Type == "min"]
     df$polyy[df$Type == "max"] <- polyymax[df$Type == "max"]
     
     # And add locations for the text:
     
     df$x[df$Type == "min"] <- df$x[df$Type == "min"] + 0.25
     df$x[df$Type == "max"] <- df$x[df$Type == "max"] - 0.25
     df$y[df$Type == "min"] <- df$y[df$Type == "min"] - 0.25
     df$y[df$Type == "max"] <- df$y[df$Type == "max"] + 0.25
     
     # Then we can plot with geom_polygon:
     
     births$x <- as.numeric(factor(births$yb))
     births$y <- .25
     births$B <- round(births$B, decimals)
     
     pop_start <- pop_start %>% filter(age %in% ages)
     pop_start$x <- .25
     pop_start$y <- as.numeric(factor(pop_start$age))
     pop_start$pop <- round(pop_start$pop, decimals)
     
     if(is.null(years)) years <- unique(pop_end$t)
     pop_end <- pop_end %>% filter(t %in% max(years), age %in% ages)
     pop_end$x <- length(years) + .75
     pop_end$y <- as.numeric(factor(pop_end$age))
     pop_end$pop <- round(pop_end$pop, 0)
     
     # plot
     ggplot(df) +
          geom_polygon(aes(x = polyx, y = polyy, group = observation),
                       color = "gray30", fill = "white") +
          geom_text(aes(x = x, y = y, label = D, col = pos_neg), check_overlap = TRUE, size = 2.5) +
          geom_text(data = births, aes(x, y, label = B), col=4 , size = 3)+
          geom_text(data = pop_start, aes(x, y, label = pop), col=1 , size = 3, angle = 90)+
          geom_text(data = pop_end, aes(x, y, label = pop), col=1 , size = 3, angle = 90)+
          scale_y_continuous(labels = ages , breaks = ages + .5) +
          scale_x_continuous(labels = births$yb, breaks = births$x) +
          coord_equal() +
          theme_minimal() +
          theme(legend.position = "none") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          labs(x = "Year", y = "Age")
     
}

# dividir por cohorte las defunciones de período
split_triangles_D <-function(deaths, births = NULL, ax = NULL, ax_default = .5){
     
     # interval
     delta <- unique(diff(unique(deaths$yd)))
     alpha_default <- 1 - ax_default
     
     # split triangles
     if(is.null(ax)){
          alpha_rest <- deaths %>% filter(age!=0) %>% mutate(alpha=alpha_default)
          if(min(deaths$age) == 0 & !is.null(births)){
               IMR_obs <- deaths %>% 
                    filter(age == 0) %>% 
                    inner_join(births, by = c("yd"="yb")) %>% 
                    mutate(IMR = D/B)
               alpha_0 <- IMR_obs %>% 
                    bind_cols(alpha = sapply(IMR_obs$IMR,function(x) DemoTools::lt_rule_1a0_cd(IMR=x,Sex = "f",region = "w"))) %>% 
                    select(-B,-IMR) 
               alpha <- bind_rows(alpha_0, alpha_rest) %>% arrange(age, yd)
          }else{
               message("look not including some age")
               alpha <- alpha_rest
          }
     }else{
          alpha <- ax %>% mutate(alpha = 1 - ax)
     }
     
     # join deaths
     deaths_splitted <- bind_rows(
          deaths %>% left_join(alpha, by = "age") %>% 
               mutate(yb = yd-age-delta, D = D*(1-alpha)),
          deaths %>% left_join(alpha, by = "age") %>% 
               mutate(yb = yd-age, D = D*alpha)) %>% 
          arrange(yd, age) %>% 
          select(yd, age, D, yb) %>% 
          ungroup()
     
     return(deaths_splitted)
}

# split triangles netmigr
split_triangles_NM <-function(nmigr, alpha_input = .5, delta = 1){
     nmigr_splitted <- bind_rows(
          nmigr %>% mutate(alpha = alpha_input, yb = ym-age-delta, NM = NM*alpha),
          nmigr %>% mutate(alpha = alpha_input, yb = ym-age  , NM = NM*(1-alpha))) %>% 
          arrange(ym, age) %>% 
          select(-alpha) %>% 
          ungroup()
     return(nmigr_splitted)
}


# interpolate tidy data -------------------------------------------------------------------------
# función para interpolar por fila, adaptando `interp` from DemoTools
interp_tidy <- function(data, x, y, z, new_y, method = "exponential"){
     x <- enquo(x);y <- enquo(y);z <- enquo(z)
     data_matrix <- data %>% ungroup %>% 
          group_by(!!x, !!y) %>% summarise(value = sum(!!z, na.rm=T)) %>% ungroup() %>% 
          pivot_wider(names_from=!!y, values_from=value) %>% as.matrix()
     data_interp <- tibble(x = data_matrix[,1],
                           DemoTools::interp(popmat = data_matrix[,-1] %>% apply(2, as.numeric),
                                             datesIn = as.numeric(colnames(data_matrix)[-1]),
                                             datesOut = new_y,
                                             method = method, 
                                             extrap = T,
                                             negatives = T) %>% as.data.frame()) %>%
          setNames(c("x",as.character(new_y))) %>% 
          pivot_longer(!x, names_to = "y", values_to = "value") %>%
          mutate(y = as.numeric(y)) %>% 
          setNames(c(as_label(x), as_label(y), as_label(z)))
     return(data_interp)
}


# mlt functions -----------------------------------------------------------
# select level with best fit based on one value, depending the UN or CD family ("UN_Latin_American", "UN_Far_Eastern", "UN_Chilean", "UN_South_Asian", "UN_General") 
# function originally from MortPack, adapted by Sara Hertog (UN-UNDESA)
# mode documentation in https://github.com/sarahertog/MortpakAbacusLegacyMLT/blob/master/R/lt_model_cdun_match_single.R
lt_model_cdun_match_single <- function(type,
                                       indicator, 
                                       value, 
                                       radix = 1e+05, 
                                       a0rule = "cd", 
                                       Sex = "m", 
                                       IMR = NA, 
                                       mod = TRUE, 
                                       SRB = 1.05, 
                                       OAnew = 130)   {
     
     # parse MLT lookup table from MortCast according to type and sex
     sexcode <- ifelse(Sex == "m", 1, ifelse(Sex == "f", 2, NA))
     MLTlookup <- MortCast::MLT1Ylookup
     MLTlookup <- as.data.frame(MLTlookup[MLTlookup$type == type & MLTlookup$sex == sexcode,])
     MLTlookup$index <- MLTlookup$e0
     
     region = "w"
     if (tolower(substr(type,1,2)) == tolower("CD")) {
          region <- tolower(substr(type,4,4))
     }
     
     if (indicator == "e0") {
          mlts       <- MLTlookup[, c("type","sex","index","age","mx")]
          # here we recompute e0 associated with model life tables to ensure that DemoTools functions return the input e0
          mlts$level <- NA
          for (level0 in unique(mlts$index)) {
               mlts$level[mlts$index == level0] <- DemoTools::lt_single_mx(nMx = mlts$mx[mlts$index == level0], 
                                                                           Age = mlts$age[mlts$index == level0], 
                                                                           Sex = Sex, a0rule = a0rule, region = region)$ex[1]
          }
     }
     
     if (indicator == "1q0") {
          # compute q1 levels for model life tables
          mlts       <- MLTlookup[MLTlookup$age == 1, c("type","sex","index","lx")]
          mlts$level <- 1-(mlts$lx / 100000)
          mlts       <- merge(MLTlookup, mlts[,c("type","sex","index","level")], by = c("type","sex","index"))
          
     }
     
     if (indicator == "5q0") {
          # compute q5 levels for model life tables
          mlts       <- MLTlookup[MLTlookup$age == 5, c("type","sex","index","lx")]
          mlts$level <- 1-(mlts$lx / 100000)
          mlts       <- merge(MLTlookup, mlts[,c("type","sex","index","level")], by = c("type","sex","index"))
          
     }
     
     if (indicator == "35q15") {
          # compute 35q15 levels for model life tables
          mlts       <- MLTlookup[MLTlookup$age %in% c(15,50), c("type","sex","index","age","lx")]
          mlts       <- reshape(mlts, direction = "wide", timevar = "age", 
                                idvar = c("type","sex","index"), sep = "_")
          mlts$level <- 1 - (mlts$lx_50 / mlts$lx_15)
          mlts       <- merge(MLTlookup, mlts[,c("type","sex","index","level")], by = c("type","sex","index"))
     }
     
     if (indicator == "45q15") {
          # compute 45q15 levels for model life tables
          mlts       <- MLTlookup[MLTlookup$age %in% c(15,60), c("type","sex","index","age","lx")]
          mlts       <- reshape(mlts, direction = "wide", timevar = "age", 
                                idvar = c("type","sex","index"), sep = "_")
          mlts$level <- 1 - (mlts$lx_60 / mlts$lx_15)
          mlts       <- merge(MLTlookup, mlts[,c("type","sex","index","level")], by = c("type","sex","index"))
     }
     
     # sort by level and age
     mlts         <- mlts[order(mlts$level, mlts$age),]
     
     # identify the model life tables with levels just below and above the value to match
     lvls   <- unique(mlts$level)
     iord   <- which.min(abs(value - lvls)) # identify closest level to value (could be higher or lower)
     lower  <- ifelse(lvls[iord] <= value, iord, iord-1)
     higher <- lower + 1
     ## PG: deal with out of bound levels
     if (lower==length(lvls)) {
          higher <- lower
          lower  <- lower-1
     }
     
     # parse the two matched mlts
     mlt_match <- mlts[mlts$level %in% c(lvls[c(lower,higher)]), c("level","age","mx")]
     mlt_match <- reshape(mlt_match, direction = "wide", timevar = "level", 
                          idvar = "age", sep = "_")
     
     # interpolate log(mx) between the two matched mlts according to the position of the input value
     # relative to the two matched levels (replicates Abacus approach)
     pct_val <- (value - lvls[lower]) / (lvls[higher]-lvls[lower])
     mx_hat  <- exp((1.0-pct_val)*log(mlt_match[,2])+pct_val*log(mlt_match[,3]))  # m(x,n)
     
     # compute the life table
     lt_out <- DemoTools::lt_single_mx(nMx = mx_hat, 
                                       Sex = Sex, 
                                       a0rule = a0rule,
                                       OAG = TRUE,
                                       OAnew = OAnew,
                                       radix = radix, 
                                       region = region, 
                                       IMR = IMR, 
                                       mod = mod, 
                                       SRB = SRB)
     
     
     return(lt_out)
     
}



# select level with best fit based on two values, depending the UN or CD family ("UN_Latin_American", "UN_Far_Eastern", "UN_Chilean", "UN_South_Asian", "UN_General") 
# function originally from MortPack, adapted by Sara Hertog (UN-UNDESA)
# more documentation on https://github.com/sarahertog/MortpakAbacusLegacyMLT/blob/master/R/lt_model_cdun_combin_single.R
lt_model_cdun_combin_single <- function(type, 
                                        Sex, 
                                        q1 = NA, 
                                        q5 = NA, 
                                        indicator_adult, 
                                        value_adult,
                                        OAnew = 110,
                                        radix = 1e+05, 
                                        axmethod = "un", 
                                        a0rule = "cd",
                                        IMR = NA, 
                                        mod = TRUE, 
                                        SRB = 1.05, 
                                        extrapLaw = "kannisto", 
                                        extrapFrom = OAnew-5, 
                                        extrapFit = seq(OAnew-30, OAnew-5, 5))   {
     
     #############################
     ##############################
     ###############################
     # New syntax for Abacus combin
     
     # indicator_adult can be "35q15", "45q15"
     
     # first match on adult mortality indicator via lt_model_cdun_match function;
     lt_temp_adult_single <- lt_model_cdun_match_single(type = type,
                                                        Sex  = Sex,
                                                        indicator = indicator_adult,
                                                        value = value_adult,
                                                        OAnew = 130)
     
     # compute abridged lt that corresponds to single
     lt_temp_adult_abr <- DemoTools::lt_single2abridged(lx  = lt_temp_adult_single$lx,
                                                        nLx = lt_temp_adult_single$nLx,
                                                        ex  = lt_temp_adult_single$ex)
     
     
     # extract model prob of dying bw ages 20 and 25
     q5_20 <- lt_temp_adult_abr$nqx[lt_temp_adult_abr$Age==20] 
     
     # if type is CD family, then use above output 5qx pattern as the user-defined model pattern
     if (tolower(type) %in% tolower(c("CD_West","CD_East","CD_North","CD_South"))) {
          users_model_pattern <- lt_temp_adult_abr$nqx[1:18]
          type_combin <- "user_defined"
     } else {
          users_model_pattern <- NA
          type_combin <- type
     }
     
     # If q5 is not available, use BESTFT with 1q0 and 5q20 to get 4q1, 5q5 and 5q10. 
     if (!is.na(q1) & is.na(q5)) {
          # bestft on q1 and 5q20 to get 4q1, 5q5, and 5q10
          lt_temp_child <- lt_model_un_bestft(type = type_combin,
                                              Sex  = Sex,
                                              age_start_abridged = c(0,20),
                                              qx_abridged = c(q1,q5_20),
                                              user_pattern = users_model_pattern,
                                              lt_compute = FALSE)
          lt_temp_child <- lt_temp_child[lt_temp_child$bestft_components == 2,]
          
          # splice input q1, best_ft q for ages 1-4, 5-9, 10-14, and adult match for older ages
          qx_out <- c(q1,lt_temp_child$nqx[2:4],lt_temp_adult_abr$nqx[5:nrow(lt_temp_adult_abr)])
          
     }
     
     # If q1 is not available, use MLT and match on q5 to get q1.
     if (is.na(q1) & !is.na(q5)) {
          # match on q5 to get q1
          lt_temp_child <- lt_model_cdun_match_single(type = type,
                                                      Sex  = Sex,
                                                      indicator = "5q0",
                                                      value = q5)
          q1 <- lt_temp_child$nqx[1]
     }
     
     # When both q1 and q5 are available, use BESTFT first with 1q0 and 5q20 then with 4q1 and 5q20. Average
     # both results to get 5q5 and 5q10.  Note that 5q20 above is from MLT.
     if (!is.na(q1) & !is.na(q5)) {
          l1 <- 100000*(1-q1)
          l5 <- 100000*(1-q5)
          q1_4 <- 1-(l5/l1)
          # bestft first with 1q0 and 5q20
          lt_out1 <- lt_model_un_bestft(type = type_combin,
                                        Sex = Sex,
                                        age_start_abridged = c(0,20),
                                        qx_abridged = c(q1,q5_20),
                                        user_pattern = users_model_pattern,
                                        lt_compute = FALSE)
          qx_out1 <- lt_out1[lt_out1$bestft_components == 2, "nqx"]
          # bestft second with 4q1 and 5q20
          lt_out2 <- lt_model_un_bestft(type = type_combin,
                                        Sex = Sex,
                                        age_start_abridged = c(1,20),
                                        qx_abridged = c(q1_4,q5_20),
                                        user_pattern = users_model_pattern,
                                        lt_compute = FALSE)
          qx_out2 <- lt_out2[lt_out2$bestft_components == 2, "nqx"]
          
          qx_out <- c(q1, q1_4, c((qx_out1+qx_out2)/2)[3:4], lt_temp_adult_abr$nqx[5:nrow(lt_temp_adult_abr)])
     }
     
     region = "w"
     if (tolower(substr(type,1,2)) == tolower("CD")) {
          region <- tolower(substr(type,4,4))
     }
     
     lt_out_abr <- DemoTools::lt_abridged(nqx = qx_out,
                                          Age = lt_temp_adult_abr$Age, 
                                          Sex = Sex, 
                                          axmethod = axmethod, 
                                          a0rule = a0rule, 
                                          region = region,
                                          OAnew = 130,
                                          mod = mod,
                                          extrapFrom = max(lt_temp_adult_abr$Age) - 5)
     
     lt_out_sng <- DemoTools::lt_abridged2single(Age = lt_out_abr$Age,
                                                 nMx = lt_out_abr$nMx,
                                                 radix = radix,
                                                 a0rule = a0rule, 
                                                 Sex = Sex,
                                                 region = region,
                                                 IMR = q1, 
                                                 mod = mod, 
                                                 SRB = SRB, 
                                                 OAG = TRUE,
                                                 OAnew = OAnew,
                                                 extrapLaw = extrapLaw,
                                                 extrapFit = extrapFit,
                                                 extrapFrom = extrapFrom)
     
     
     return(lt_out_sng)
     
}

# auxiliary function for lt_model_cdun_combin_single 
# function originally from MortPack, adapted by Sara Hertog (UN-UNDESA)
# more documentation in https://github.com/sarahertog/MortpakAbacusLegacyMLT/blob/master/R/lt_model_un_bestft.R
lt_model_un_bestft<-function(type, 
                             Sex, 
                             age_start_abridged, 
                             qx_abridged, 
                             user_pattern = NA,
                             lt_compute = TRUE,
                             OAnew = 110,
                             radix = 1e+05, 
                             axmethod = "un", 
                             a0rule = "ak", 
                             region = "w", 
                             IMR = NA, 
                             mod = TRUE, 
                             SRB = 1.05, 
                             extrapLaw = "kannisto", 
                             extrapFrom = OAnew-5, 
                             extrapFit = (OAnew-30):(OAnew-5)) {
     
     ## Do some checking/validating of input arguments
     
     Sex <- tolower(substr(Sex,1,1))
     sex_check <- Sex %in% c("m","f")
     
     if (!sex_check) 
          stop("The Sex argument must be either 'm' or 'f'.")
     
     type_check <- tolower(type) %in% tolower(c("user_defined", "UN_Latin_American", "UN_Far_Eastern",
                                                "UN_Chilean", "UN_South_Asian", "UN_General"))
     
     if (!type_check) 
          stop("The type argument must be one of 'user_defined', 'UN_Latin_American', 'UN_Far_Eastern',
                            'UN_Chilean', 'UN_South_Asian', 'UN_General'.")
     
     # abridged ages
     age <- c(0,1,seq(5,80,5))
     age_check <- all(age_start_abridged %in% age)
     
     if (!age_check) 
          stop("The age argument must be starting ages for abridged age groups 0,1,5,10,15....")
     
     qx_check <- all(!is.na(qx_abridged) & qx_abridged != 0 )
     
     if (!qx_check) 
          stop("The qx_abridged argument must be all non_zero values.")
     
     if (tolower(type) == tolower("user_defined")) {
          user_check <- length(user_pattern) == 18 # must be length 18 for this implementation
     }
     
     
     
     #  Empirical patterns for five UN life table families
     #  males at places 1:18; females at places 19:36
     
     if (tolower(type) == tolower("UN_Latin_American")) {
          # UN-Latin American
          EMP  <- c(-1.12977,-1.49127,-2.13005,-2.40748,-2.21892,-2.01157, 
                    -1.93591,-1.86961,-1.76133,-1.64220,-1.49651,-1.34160,-1.15720, 
                    -.96945,-.74708,-.52259,-.29449,-.04031,-1.22452,-1.45667, 
                    -2.13881,-2.46676,-2.31810,-2.14505,-2.03883,-1.93924,-1.83147, 
                    -1.74288,-1.62385,-1.47924,-1.28721,-1.07443,-.83152,-.59239, 
                    -.35970,-.08623)
     }
     if (tolower(type) == tolower("UN_Chilean")) {
          # UN-Chilean
          EMP  <- c(-1.04722,-1.81992,-2.42430,-2.52487,-2.24491,-2.02821, 
                    -1.90923,-1.78646,-1.66679,-1.52497,-1.37807,-1.21929,-1.03819, 
                    -.84156,-.63201,-.42070,-.21110,+.01163,-1.12557,-1.82378, 
                    -2.52319,-2.63933,-2.38847,-2.20417,-2.09701,-1.99128,-1.87930, 
                    -1.75744,-1.61558,-1.45886,-1.26115,-1.05224,-.80346,-.58202, 
                    -.35093,-.10587)
     }
     if (tolower(type) == tolower("UN_South_Asian")) {
          # UN-South Asian
          EMP  <- c(-.97864,-1.24228,-2.01695,-2.44280,-2.35424,-2.27012, 
                    -2.16833,-2.05942,-1.90053,-1.71213,-1.51120,-1.28493,-1.08192, 
                    -.84671,-.62964,-.40229,-.19622,-.00129,-0.97055,-1.15424, 
                    -1.93962,-2.36857,-2.19082,-2.09358,-2.04788,-1.95922,-1.87311, 
                    -1.76095,-1.61425,-1.39012,-1.15515,-0.90816,-.68011,-.43231, 
                    -.17489,0.05948)
     }
     if (tolower(type) == tolower("UN_Far_Eastern")) {
          # UN-Far Eastern
          EMP  <- c(-1.53473,-2.15035,-2.61442,-2.66392,-2.42326,-2.23095, 
                    -2.15279,-2.05765,-1.89129,-1.68244,-1.47626,-1.23020,-1.02801, 
                    -.77148,-.54696,-.32996,-.11911,0.10572,-1.42596,-1.95200, 
                    -2.55653,-2.68018,-2.33095,-2.15952,-2.03377,-1.94554,-1.82299, 
                    -1.69084,-1.52189,-1.33505,-1.13791,-0.93765,-.72718,-.50916, 
                    -.28389,-.01285)
     }
     if (tolower(type) == tolower("UN_General")) {
          # UN-General
          EMP  <- c(-1.27638,-1.78957,-2.35607,-2.55527,-2.34263,-2.16193, 
                    -2.09109,-2.00215,-1.86781,-1.70806,-1.52834,-1.33100,-1.12934, 
                    -.91064,-.68454,-.45685,-.23002,0.00844,-1.35963,-1.77385, 
                    -2.39574,-2.64549,-2.44766,-2.28991,-2.18850,-2.08535,-1.97231, 
                    -1.84731,-1.69291,-1.50842,-1.30344,-1.08323,-.84402,-.59485, 
                    -.34158,-.06493)
     }
     
     if (tolower(type) == tolower("user_defined")) {
          # bring in user defined pattern
          EMP <- 0.50*log(user_pattern/(1.0-user_pattern))
          EMP <- rep(EMP,2)
     }
     
     if (Sex == "m") {
          
          EMP <- EMP[1:18]
          
          VEC <- cbind(c(.23686,.36077,.33445,.30540,.28931,.28678,.27950,.28023,.26073,
                         .23626,.20794,.17804,.15136,.13217,.12243,.11457,.10445,.08878),
                       c(-.46007,-.68813,.06414,.12479,.24384,.10713,.06507,.03339,.02833,
                         .06473,.08705,.10620,.11305,.09467,.10809,.14738,.21037,.30918),
                       c(.09331,-.29269,-.47139,-.17403,.10715,.28842,.33620,.33692,.21354,
                         .15269,.06569,.00045,-.03731,-.10636,-.11214,-.22258,-.19631,-.38123))
     }
     if (Sex == "f") {
          
          EMP <- EMP[19:36]
          
          VEC <- cbind(c(.18289,.31406,.31716,.30941,.32317,.32626,.30801,.29047,.25933,
                         .22187,.19241,.17244,.15729,.14282,.12711,.11815,.11591,.09772),
                       c(-.51009,-.52241,.08947,.03525,.03132,.07843,.06762,.00482,-.01409,
                         -.02178,.01870,.04427,.08201,.08061,.15756,.24236,.30138,.50530),
                       c(.23944,-.11117,.07566,.06268,-.26708,-.39053,-.28237,-.14277,-.05923,
                         .18909,.24773,.33679,.34121,.38290,.26731,.14442,.09697,-.13377))
     }
     
     
     
     # some parameters
     GAL1 <- VEC[,1]*VEC[,1]
     GAL2 <- VEC[,2]*VEC[,2]
     GAL3 <- VEC[,3]*VEC[,3]
     BAL1 <- VEC[,1]*VEC[,2]
     BAL2 <- VEC[,1]*VEC[,3]
     BAL3 <- VEC[,2]*VEC[,3]
     
     # initialize coefficients    
     GAMMA1 <- 0.0
     GAMMA2 <- 0.0
     GAMMA3 <- 0.0
     ALPHA1 <- 0.0
     ALPHA2 <- 0.0
     ALPHA3 <- 0.0
     BETA1 <- 0.0
     BETA2 <- 0.0
     BETA3 <- 0.0
     
     NCTR <- 0 # counter
     for (i in 1:length(age_start_abridged)){
          
          NCTR <- NCTR+1 # counter
          R      <- 0.5*log(qx_abridged[i]/(1.0-qx_abridged[i]))
          S      <- R-EMP[age == age_start_abridged[i]]
          SVEC1  <- S*VEC[age == age_start_abridged[i],1]
          SVEC2  <- S*VEC[age == age_start_abridged[i],2]
          SVEC3  <- S*VEC[age == age_start_abridged[i],3]
          GAMMA1 <- GAMMA1+GAL1[age == age_start_abridged[i]]
          GAMMA2 <- GAMMA2+GAL2[age == age_start_abridged[i]]
          GAMMA3 <- GAMMA3+GAL3[age == age_start_abridged[i]]
          ALPHA1 <- ALPHA1+SVEC1
          ALPHA2 <- ALPHA2+SVEC2
          ALPHA3 <- ALPHA3+SVEC3
          BETA1  <- BETA1+BAL1[age == age_start_abridged[i]]
          BETA2  <- BETA2+BAL2[age == age_start_abridged[i]]
          BETA3  <- BETA3+BAL3[age == age_start_abridged[i]]
          
     }
     
     lt_out <- list() # initialize component fits
     
     if(NCTR >= 3) NCTR <- 3
     NTEMP <- 4-NCTR
     for (i in NTEMP:3) {
          IC <- 4-i
          if(IC == 1 | IC == 2) GAMMA3 <- 1.0
          if(IC == 1 | IC == 2) BETA2 <- 0.0
          if(IC == 1 | IC == 2) BETA3 <- 0.0
          if(IC == 1) BETA1 <- 0.0
          if(IC == 1) GAMMA2 <- 1.0
          D  <- GAMMA1*GAMMA2*GAMMA3-GAMMA3*BETA1*BETA1-GAMMA2*BETA2*BETA2-GAMMA1*BETA3*BETA3+2.0*BETA1*BETA2*BETA3
          A1 <- ALPHA1*(GAMMA2*GAMMA3-BETA3*BETA3)+ALPHA2*(BETA2*BETA3-BETA1*GAMMA3)+ALPHA3*(BETA1*BETA3-BETA2*GAMMA2)
          A1 <- A1/D
          A2 <- ALPHA1*(BETA2*BETA3-BETA1*GAMMA3)+ALPHA2*(GAMMA1*GAMMA3-BETA2*BETA2)+ALPHA3*(BETA1*BETA2-BETA3*GAMMA1)
          A2 <- A2/D
          A3 <- ALPHA1*(BETA1*BETA3-BETA2*GAMMA2)+ALPHA2*(BETA1*BETA2-BETA3*GAMMA1)+ALPHA3*(GAMMA1*GAMMA2-BETA1*BETA1)
          A3 <- A3/D
          if (IC == 1) {
               A2 <- 0.0
               A3 <- 0.0
          }
          if (IC == 2) {
               A3 <- 0.0
          }
          CF <- EMP + A1*VEC[,1]+A2*VEC[,2]+A3*VEC[,3]
          CF <- exp(2.0*CF)/(1.0+exp(2.0*CF)) # nqx
          if (lt_compute == TRUE) {
               lt <- DemoTools::lt_abridged(Age = age, 
                                            nqx = CF, 
                                            Sex = Sex, 
                                            OAnew = OAnew,
                                            radix = radix, 
                                            axmethod = axmethod, 
                                            a0rule = a0rule, 
                                            region = region, 
                                            IMR = IMR, 
                                            mod = mod, 
                                            SRB = SRB, 
                                            extrapLaw = extrapLaw, 
                                            extrapFrom = extrapFrom, 
                                            extrapFit = extrapFit)
          } else {
               lt <- data.frame(Age = age, nqx = CF)
          }
          lt$bestft_components <- IC
          lt_out[[i]] <- lt
     }
     
     out.data <- do.call(rbind,lt_out)
     
     return(out.data) 
     
}  

# based on HFD protocol

# cumsum(panama_asfr5$asfr)
# 
# data_asfr <- bind_rows(data.frame(age = 10, asfr = 0),
#                        panama_asfr5 %>% select(age = Edad, asfr))
# 
# data_asfr_cum <- data_asfr %>% 
#      mutate(cum_asfr = cumsum(asfr),
#             age_cum = ifelse(age < max(age), lead(age), 55))  
# data_asfr_cum_interp <- splinefun(data_asfr_cum$age_cum, 
#                                     data_asfr_cum$cum_asfr,
#                                     method = "monoH.FC")(min(data_asfr_cum$age):50)
# 
# 
# 
# 
# db_country_year_complete <- data.frame(age = (min(data_asfr_cum_interp$age_cum_d):(max_age_year-1)), 
#                                        DataValue = diff(db_country_year_interp))
