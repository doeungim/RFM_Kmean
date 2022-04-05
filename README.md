# RFM_Kmean
RFM_Kmean 고객데이터 분석 





    library(tidyverse)
    library(lubridate)
    pacman::p_load(cluster, factoextra, magrittr, pacman, rio, 
                   tidyverse)



    read.table(file ="data/CDNOW_master/CDNOW_master.txt",
               header= TRUE,
               sep = "") -> df



    df %>% 
      set_names(c("ID","date","qty","Amount")) %>% 
      mutate(date = as.character(date)) %>% 
      mutate(date = parse_date(date, format = "%Y%m%d")) -> retention_df


    retention_df %>% 
      sample_frac(0.3) %>% 

      ## 01로 시작하는 p_month 생성 
      mutate(p_month = ymd(paste0(year(date), "-", month(date), "-", 01))) %>% 
      filter(date <= ymd("1997-12-31")) %>% 
      group_by(ID) %>% 

      ## 최초 시작하는 날짜의 최소값 도출 
      mutate(cohort_month = min(p_month)) %>%

      ## index 생성 
      mutate(cohort_index = interval(cohort_month, date) %/% months(1) + 1) -> retention_cleaned_df

      retention_cleaned_df %>% 
        ungroup() %>% 
           group_by(cohort_month, cohort_index) %>% 
      summarise(user_number = n()) %>% 
      spread(cohort_index, user_number) -> retention_tbl


  
        retention_tbl[,1] %>% 
          bind_cols(round(retention_tbl[,-1]/retention_tbl$`1` * 100,1)) 
  

        library(plotly)
        library(extrafont)
        library(tidyquant)

        
        retention_tbl %>% 
          gather(cohort_index, customers,-cohort_month) %>% 
          mutate(cohort_index = as.integer(cohort_index)) %>%  
          
          ggplot(aes(cohort_index,
                     reorder(cohort_month, desc(cohort_month))))+
          
          geom_tile(aes(fill= customers), color = "white")+
            
          geom_text(aes(label = customers), size = 2.5)+
          scale_fill_gradient(low = "white", 
                              high = palette_light()[1])+
          theme_tq()
        
        
        
        
        
        ### RFP 표 
        df %>% 
          set_names(c("id","date","qty","amount")) %>% 
          mutate(date = as.character(date)) %>% 
          mutate(date = parse_date(date, format = "%Y%m%d")) -> cdnow_df

        
        
        
        cdnow_df %>% 
          mutate(recency_days = difftime(ymd("1998-07-01"), date) %>% as.numeric()) %>% 
          group_by(id) %>% 
          summarise(recency = min(recency_days),
                    freqency = n(),
                    monetary = sum(amount)) %>% 
          
          ungroup() %>% 
          mutate(R = ntile(max(recency) - recency, 3),
                 F = ntile(freqency, 3),
                 M = ntile(monetary, 3),
                 RFM_segement = str_c(R,F,M),
                 RFM_score = R + F + M ) -> cdnow_rfm_df 

        cdnow_rfm_df
        
        ## recency, freqency, monetary summarise
        cdnow_rfm_df %>% 
          summarise(num_customer = n(),
                    R_mean = mean(recency),
                    F_mean = mean(freqency),
                    M_mean = mean(monetary))

                
        

        
        
        cdnow_rfm_df %>% 
          mutate(customer_grade = case_when(
            
            RFM_score >= 9 ~ "VVIP",
            RFM_score < 9 & RFM_score >= 6 ~ "VIP",
            TRUE ~ "Loyal"
          )) %>% 
          group_by(customer_grade) %>% 
          summarise(num_customer = n(),
                    R_mean = mean(recency),
                    F_mean = mean(freqency),
                    M_mean = mean(monetary))
        
        
        
        # RFM 뱀 그래프(RFM Snake Plot)을 활용
        
        cdnow_rfm_df %>% 
          mutate(customer_grade = case_when(
            
            RFM_score >= 9 ~ "VVIP",
            RFM_score < 9 & RFM_score >= 6 ~ "VIP",
            TRUE ~ "Loyal"
          )) %>% 
          mutate(customer_grade = factor(customer_grade, levels = c("VVIP","VIP","Loyal"))) %>% 
          mutate( recency = scale(recency),
                 freqency = scale(freqency),
                 monetary = scale(monetary)) %>% 
          
          group_by(customer_grade) %>% 
          summarise(num_customer = n(),
                    R_mean = mean(recency),
                    F_mean = mean(freqency),
                    M_mean = mean(monetary)) %>% 
          gather(rfm_metrics, value, -customer_grade) %>% 
          filter(!row_number() %in% c(1,2,3)) %>% 
          
          ggplot(aes(x = rfm_metrics,
                     y = value,
                     group = customer_grade,
                     color = customer_grade)) + 
          ggrepel::geom_label_repel(aes(label = str_glue("{rfm_metrics} :{value}")))+
          
          geom_point()+
          geom_line()+
          theme_tq()
        
        
        
        ## 
        
        cdnow_rfm_df %>%  
          select(recency, freqency, monetary) %>% 
          mutate_if(is.integer, as.numeric) %>% 
          scale(center = TRUE,scale = TRUE) -> scaled_cdnow_df 

        
                  
        scaled_cdnow_df
        
        
        
        ## optimal_k 
        

        scaled_cdnow_df %>% 
          fviz_nbclust(kmeans,
                       method = "silhouette")
        
        
        ### WSS 
        wss <- 0 
        for (i in 1:15) {
          
          out <- kmeans(scaled_cdnow_df, 
                        i, 
                        nstart = 20, 
                        iter.max = 50)
          
          wss[i] <- out$tot.withinss
          
        }

        
        tibble(cluster = 1:15, 
               wss = wss) %>% 
          
          ggplot(aes(x = cluster,
                     y = wss))+
          geom_point()+
          geom_line()+
          theme_tq()
          
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
                
        
        
        
          
  
  
    
