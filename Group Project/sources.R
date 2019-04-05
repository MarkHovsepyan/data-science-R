library(rvest)

## scrapping function
source_catcher <- function(sources,index,url,text_element,Year){
  txt <- read_html(url) %>%
    html_nodes(text_element) %>%
    html_text() %>%
    paste(collapse = " ")
  eval.parent(substitute(sources[index,1]<-txt))
  eval.parent(substitute(sources[index,2]<-Year)) 
  
}


sources <- data.frame(Article = as.character(),
                      Year = as.numeric(),
                      stringsAsFactors = F)


source_catcher(sources, 1,"http://www.shaulainternational.com/industrial-policy/","#content p",2010)
source_catcher(sources, 2,"https://armenianweekly.com/2010/01/30/listening-to-the-wind-of-change-renewable-energy-in-armenia/",".alignright+ p , p+ p , .alignleft+ p",2010)
source_catcher(sources, 3,"http://www.globalmetals.am/en/news/3023.html","#article_holder",2010)
source_catcher(sources, 4,"https://news.am/eng/news/31849.html", "p", 2010)
source_catcher(sources, 5,"https://news.am/eng/news/57228.html","p",2011)
source_catcher(sources, 6,"https://mediamax.am/en/news/yerevan/354/","h2 , p",2011)
source_catcher(sources, 7,"http://www.gov.am/en/news/item/5539/", "#content div p", 2011)
source_catcher(sources, 8, "https://ayfwest.org/news/can-armenias-economy-thrive-on-services/", ".entry-content p", 2011)
source_catcher(sources, 9, "http://hetq.am/eng/news/2927/diaspora-investors---easy-prey-for-armenias-authorities.html/", ".opennewstext", 2011 )
source_catcher(sources, 10, "http://hetq.am/eng/news/19886/armenian-investors-letter-to-us-ambassador-to-armenia.html", "p", 2012 )
source_catcher(sources, 11, "http://www.gov.am/en/news/item/6364/", "#content p",  2012 )
source_catcher(sources, 12, "http://www.gov.am/en/news/item/6082/", "p", 2012 )
source_catcher(sources, 13, "https://news.am/eng/news/102117.html", "p", 2012)
source_catcher(sources, 16, "http://www.armenia.com.au/news/Armenia-News/English/7434/Businessman-blames-fraud-on-Armenian-President---s-brother", "p", 2012)
source_catcher(sources, 17, "http://www.gov.am/en/news/item/6642/", "p", 2013)
source_catcher(sources, 18, "https://www.osce.org/yerevan/108150", "p", 2013)
source_catcher(sources, 19, "https://armenpress.am/eng/news/719161/john-heffern-considers-coca-cola-activity-in-armenia-to-be-best-example-of-american-investment.html", "#opennewscontainer p", 2013)
source_catcher(sources, 20, "https://itel.am/en/news/interviews/5489", ".text p , strong , em", 2013)
source_catcher(sources, 21, "http://hetq.am/eng/news/22694/armenias-struggling-vintners-government-promotes-cognac-people-prefer-vodka.html", ".opennewstext p", 2013)
source_catcher(sources, 22, "http://www.gov.am/en/news/item/7115/", "#content div p", 2014)
source_catcher(sources, 23, "https://armenpress.am/eng/news/766444/uwc-dilijan---huge-investment-for-future-and-youth.html", "p span", 2014)
source_catcher(sources, 24, "http://www.fao.org/armenia/news/detail-events/en/c/253917/", "p", 2014)
source_catcher(sources, 25, "https://armenianweekly.com/2014/03/21/armenia-us-relations-unfulfilled-promise/", "#main-content .clearfix p", 2014)
source_catcher(sources, 26, "https://www.synopsys.com/company/newsroom/armenia-news-releases/synopsys-celebrates-10th-anniversary-in-armenia-during-synopsys-week.html", ".vert-pad-bottom-xs p", 2014)
source_catcher(sources, 27, "https://news.am/eng/news/303018.html", "p", 2015)
source_catcher(sources, 28, "https://massispost.com/2015/07/ceo-of-franck-muller-to-invest-150000-in-armenia-projects/", "p", 2015)
source_catcher(sources, 29, "http://www.tert.am/en/news/2015/03/26/liana/1628440", "#news-content-container , p:nth-child(3) , #news-content-container p:nth-child(7) , #news-content-container p:nth-child(10) , #news-content-container p:nth-child(2) , #news-content-container strong", 2015)
source_catcher(sources, 30, "https://hetq.am/eng/news/62887/who-owns-the-internet-in-armenia.html", "p , b", 2015)
source_catcher(sources, 31, "https://itel.am/en/news/interviews/7370", ".text p , br+ strong , em", 2015)
source_catcher(sources, 32, "https://news.am/eng/news/311057.html", "p", 2016)
source_catcher(sources, 33, "https://armenpress.am/eng/news/863573/armenia%E2%80%99s-pharmaceutical-sector-recorded-an-annual-growth-of-20-over-last-5-years.html", ".opennewstext p", 2016)
source_catcher(sources, 34, "https://www.armenianow.com/en/economy/2016/05/19/armenia-us-relations-economy-business-minister-minasyan-ambassador/3469/", "p", 2016)
source_catcher(sources, 35, "https://news.am/eng/news/303821.html", "p", 2016)
source_catcher(sources, 36, "https://am.am/eng/news/574/new-foreign-investment-in-armenia-forged-by-partnership-of-am-law-firm-with-fxtm.html", "p", 2016)
source_catcher(sources, 37, "https://armenpress.am/eng/news/904305/2017-large-foreign-direct-investment-flows-to-armenia-recorded-from-germany-uk.html", "#opennewscontainer p", 2017)
source_catcher(sources, 38, "https://armenpress.am/eng/news/879393/armenia%E2%80%99s-investment-attractiveness-to-be-presented-to-20-25-french-companies-business-forum-kicks.html", "p:nth-child(1)", 2017)
source_catcher(sources, 39, "https://jam-news.net/?p=38573", "p", 2017)
source_catcher(sources, 40, "https://news.am/eng/news/385870.html", "p", 2017)
source_catcher(sources, 41, "https://www.azatutyun.am/a/28393572.html", "#article-content p", 2017)
source_catcher(sources, 42, "https://news.am/eng/news/458534.html", "p", 2018)
source_catcher(sources, 43, "https://armenpress.am/eng/news/928860.html", "#opennewscontainer p", 2018)
source_catcher(sources, 44, "http://www.shaulainternational.com/2018/03/27/dfa-will-function-from-now-on-under-the-name-of-business-armenia/", "#content .clearfix p", 2018)
source_catcher(sources, 45, "https://www.panorama.am/en/news/2018/04/12/Government-investment-project/1932644", "p , .fs26", 2018)

sources[, 1] <- str_remove_all(sources[, 1], pattern = '[^a-zA-Z,^\\s]')
sources[, 1] <- str_remove_all(sources[, 1], pattern = '[:punct:]')


write.csv(sources, file = 'investmentSources.csv')