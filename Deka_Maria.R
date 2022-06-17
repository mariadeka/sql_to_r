
install.packages(c("sqldf", "dplyr", "data.table",
                   "compare"))

library("sqldf")
library("dplyr")
library("compare")
library("data.table")
library("stringi")
library("microbenchmark")
options(stringsAsFactors=FALSE)

#ZADANIE 1

Tags <- read.csv("~/Desktop/studia/2.semestr/PDU/projekt 2/Tags.csv")

# ROZWIĄZANIE 1 - sqldf::sqldf()

df_sql_1 <- function(Tags){
  sqldf('SELECT Count, TagName
        FROM Tags
        WHERE Count > 1000
        ORDER BY Count DESC') -> df1
  return(df1)
}

#ROZWIĄZANIE 2 - tylko funkcje bazowe

df_base_1 <- function(Tags){
  wybrane_wiersze <- data.frame(Tags$Count, Tags$TagName)[which(Tags$Count > 1000), ]
  colnames(wybrane_wiersze) <- c("Count", "TagName")
  wybrane_wiersze[order(wybrane_wiersze$Count), ]
  rownames(wybrane_wiersze) <- NULL
  return(wybrane_wiersze)
}

#ROZWIĄZANIE 3 - funkcje z pakietu dplyr

df_dplyr_1 <- function(Tags){
  Tags%>%select(Count, TagName)%>%filter(Count > 1000)%>%arrange(-Count)
}

#ROZWIĄZANIE 4 -  za pomocą funkcji z pakietu data.table

df_table_1 <- function(Tags){
 wynik <- setorder(as.data.table(Tags)[,. (Count, TagName)][Count > 1000], -Count)
 wynik
}

#ZADANIE 2
Users <- read.csv("~/Desktop/studia/2.semestr/PDU/projekt 2/Users.csv")
Posts <- read.csv("~/Desktop/studia/2.semestr/PDU/projekt 2/Posts.csv")

#ROZWIĄZANIE 1 - SQL
df_sql_2 <- function(Users, Posts){
  sqldf("SELECT Location, COUNT(*) AS Count
        FROM (
        SELECT Posts.OwnerUserId, Users.Id, Users.Location
        FROM Users
        JOIN Posts ON Users.Id = Posts.OwnerUserId
        )
        WHERE Location NOT IN ('')
        GROUP BY Location
        ORDER BY Count DESC
        LIMIT 10") -> df2
  return(df2)
}

#ROZWIĄZANIE 2 - base

df_base_2 <- function(Users, Posts){
  zmergowany <- merge(Users, Posts, all.x = FALSE, all.y = FALSE,  by.x = "Id", by.y = "OwnerUserId")
  zrodlo <- data.frame(zmergowany$Id, zmergowany$Location)
  colnames(zrodlo) <- c("Id", "Location")
  lokalizacja <- as.data.frame(zrodlo$Location)
  colnames(lokalizacja) <- c("Location")
  zliczona_lokalizacja <- aggregate(lokalizacja$Location, by = lokalizacja["Location"], FUN = length)
  colnames(zliczona_lokalizacja)[2] <- "Count"
  zliczona_lokalizacja <- zliczona_lokalizacja[zliczona_lokalizacja$Location != '', c("Location", "Count")] 
  lokalizacja_w_kolejnosci <- as.data.frame(zliczona_lokalizacja[order(zliczona_lokalizacja$Count, decreasing =  TRUE), ])
  rownames(lokalizacja_w_kolejnosci) <- NULL
  return(lokalizacja_w_kolejnosci[1:10, ])
}

#ROZWIĄZANIE 3 - dplyr

df_dplyr_2 <- function(Users, Posts){
  x <- select(Users, c("Id", "Location"))
  y <- select(Posts, "OwnerUserId")
  lokalizacja_w_kolejnosci <- inner_join(x, y, by = c("Id" = "OwnerUserId")) %>%
    select(Location) %>% filter(Location != '') %>%
    count(Location, name = "Count") %>%arrange(-Count)
  return(top_n(lokalizacja_w_kolejnosci, 10, Count))
}

#ROZWIĄZANIE 4 - data.table

df_datatable_2 <- function(Users, Posts){
  lokalizacja <- merge.data.table(
    as.data.table(Users), as.data.table(Posts), by.x = "Id", by.y = "OwnerUserId")[,Location]
  posortowana_lokalizacja <- setorder(
    setnames(as.data.table(lokalizacja[
      c(lokalizacja != '')])[,.N, by = V1], c("V1", "N"), c("Location", "Count")),
    cols = -"Count")
  return(posortowana_lokalizacja[1:10])
}


#ZADANIE 3
Badges <- read.csv("~/Desktop/studia/2.semestr/PDU/projekt 2/Badges.csv")

#ROZWIĄZANIE 1 - SQL

df_sql_3 <- function(Badges){
  sqldf("SELECT Year, SUM(Number) AS TotalNumber
        FROM (
        SELECT
        Name, 
        COUNT(*) AS Number,
        STRFTIME('%Y', Badges.Date) AS Year
        FROM Badges
        WHERE Class = 1
        GROUP BY Name, Year
        )
        GROUP BY Year
        ORDER BY TotalNumber") -> df3
  return(df3)
}

#ROZWIĄZANIE 2 - base

df_base_3 <- function(Badges){
  klasa_1 <- Badges[which(Badges$Class == 1), ]
  klasa_1$Date <- format(as.Date(klasa_1$Date), format = "%Y")
  colnames(klasa_1)[colnames(klasa_1) == "Date"] <- "Year"
  df <- as.data.frame(table(klasa_1[,c("Name", "Year")]))
  colnames(df)[3] <- "Number" 
  df <- aggregate(df["Number"], df["Year"], FUN = sum)
  df$Year <- as.character(df$Year)
  colnames(df)[colnames(df) == "Number"] <- "TotalNumber"
  df <- df[order(df$TotalNumber), ]
  rownames(df) <- NULL
  return(df)
}
df_base_3(Badges)

#ROZWIĄZANIE 3 - dplyr

df_dplyr_3 <- function(Badges){
  klasa_1 <- filter(Badges, Class == 1)
  klasa_1$Date <- as.character(year(strptime(klasa_1$Date, "%Y")))
  klasa_1 <- klasa_1%>%rename("Year" = "Date")%>%
    count(Name, Year, name = "Number")%>%
    group_by(Year)%>%
    summarise(TotalNumber = sum(Number))
  return(arrange(klasa_1))
}

#ROZWIĄZANIE 4 - data.table

df_table_3 <- function(Badges){
  klasa_1 <- data.table(Badges)[Class==1][
    , Date := as.character(year(as.POSIXct(Date, format = "%Y")))]
  setnames(klasa_1, "Date", "Year")
  klasa_1 <- klasa_1[, c("Name", "Year")][, .N, by = .(Name, Year)]
  setnames(klasa_1, "N", "Number")
  klasa_1 <- klasa_1[, sum(Number), by = "Year"]
  setnames(klasa_1, "V1", "TotalNumber")
  klasa_1 <- setorder(klasa_1, "TotalNumber")
  return(klasa_1)
}

#ZADANIE 4

Users <- read.csv("~/Desktop/studia/2.semestr/PDU/projekt 2/Users.csv")
Posts <- read.csv("~/Desktop/studia/2.semestr/PDU/projekt 2/Posts.csv")

#ROZWIĄZANIE 1 - SQL

df_sql_4 <- function(Users, Posts){
  sqldf("SELECT
        Users.AccountId,
        Users.DisplayName,
        Users.Location,
        AVG(PostAuth.AnswersCount) as AverageAnswerCount
        FROM
        (
        SELECT
        AnsCount.AnswersCount,
        Posts.Id,
        Posts.OwnerUserId
        FROM (
        SELECT Posts.ParentId, COUNT(*) AS AnswersCount
        FROM Posts
        WHERE Posts.PostTypeId = 2
        GROUP BY Posts.ParentId
        ) AnsCount
        JOIN Posts ON Posts.Id = AnsCount.ParentId
        ) AS PostAuth
        JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
        GROUP BY OwnerUserId
        ORDER BY AverageAnswerCount DESC, AccountId ASC
        LIMIT 10") -> df1
  return(df1)
}

#ROZWIĄZANIE 2 - base

df_base_4 <- function(Users, Posts){
  AnsCount <- as.data.frame(table(Posts[Posts$PostTypeId == 2, "ParentId"]), stringAsFactors = FALSE)
  colnames(AnsCount) <- c("ParentId", "AnswersCount")
  po_join <- merge(x = AnsCount, y = Posts, by.x = "ParentId", by.y = "Id")
  PostAuth <- data.frame(po_join$AnswersCount, po_join$ParentId, po_join$OwnerUserId)
  colnames(PostAuth) <- c("AnswersCount", "ParentId", "OwnerUserId")
  po_join_2 <- merge(x = PostAuth, y = Users, by.x = "OwnerUserId", by.y = "AccountId")
  pogrupowane <- aggregate(po_join_2$AnswersCount, po_join_2["OwnerUserId"], mean, na.rm = TRUE)
  colnames(pogrupowane)[2] <- "AverageAnswersCount"
  z <- merge(x = po_join_2, y = pogrupowane, by = "OwnerUserId", all.x = TRUE)
  z <- z[order(z$AverageAnswersCount, z$OwnerUserId, decreasing=c(TRUE, FALSE)), ]
  z <- data.frame(z$OwnerUserId, z$DisplayName, z$Location, z$AverageAnswersCount)
  colnames(z) <- c("AccountId", "DisplayName", "Location", "AverageAnswerCount")
  return(z[1:10, ])
}

#ROZWIĄZANIE 3 - dplyr

df_dplyr_4 <- function(Users, Posts){
  AnsCount <- filter(Posts, PostTypeId == 2)%>%
    count(ParentId)%>%
    rename(AnswersCount = n )
  PostAuth <- select(inner_join(AnsCount, Posts, by = c("ParentId" = "Id")),
                     AnswersCount, ParentId, OwnerUserId)
  po_join_2 <- inner_join(PostAuth, Users, by = c("OwnerUserId" = "AccountId"))
  pogrupowane <- po_join_2 %>% group_by(OwnerUserId) %>%
    summarize(AverageAnswerCount = mean(AnswersCount))
  z <- left_join(po_join_2, pogrupowane, by = "OwnerUserId")%>%
    arrange(OwnerUserId)%>%
    arrange(desc(AverageAnswerCount))%>%
    select(OwnerUserId, DisplayName, Location, AverageAnswerCount)%>%
    rename("AccountId" = "OwnerUserId")
  return(top_n(z, 10))
}

#ROZWIĄZANIE 4 - data.table

df_table_4 <-function(Users, Posts){
  AnsCount <- setnames(as.data.table(Posts)[PostTypeId=="2"][,.N,by=ParentId], "N", "AnswersCount")
  po_join <- merge.data.table(AnsCount, Posts, by.x = "ParentId", by.y = "Id")
  PostAuth <- po_join[,.(AnswersCount, ParentId, OwnerUserId)]
  po_join_2 <- merge.data.table(PostAuth, Users, by.x = "OwnerUserId", by.y = "AccountId")
  pogrupowane <- po_join_2[,.(AverageAnswerCount=mean(AnswersCount)), by=OwnerUserId]
  z <- setnames(setorder(setorder(
    merge.data.table(
      po_join_2, pogrupowane, by = "OwnerUserId", all.x = TRUE),OwnerUserId), 
    -AverageAnswerCount)[,.(OwnerUserId, DisplayName, Location, AverageAnswerCount)], "OwnerUserId", "AccountId")
  return(z[1:10])
}

#ZADANIE 5

Votes <- read.csv("~/Desktop/studia/2.semestr/PDU/projekt 2/Votes.csv")
Posts <- read.csv("~/Desktop/studia/2.semestr/PDU/projekt 2/Posts.csv")

#ROZWIĄZANIE 1 - SQL

df_sql_5 <- function(Posts, Votes){
  sqldf("SELECT Posts.Title, Posts.Id,
        STRFTIME('%Y-%m-%d', Posts.CreationDate) AS Date,
        VotesByAge.Votes
        FROM Posts
        JOIN (
        SELECT
        PostId,
        MAX(CASE WHEN VoteDate = 'new' THEN Total ELSE 0 END) NewVotes,
        MAX(CASE WHEN VoteDate = 'old' THEN Total ELSE 0 END) OldVotes,
        SUM(Total) AS Votes
        FROM (
        SELECT
        PostId,
        CASE STRFTIME('%Y', CreationDate)
        WHEN '2021' THEN 'new'
        WHEN '2020' THEN 'new'
        ELSE 'old'
        END VoteDate,
        COUNT(*) AS Total
        FROM Votes
        WHERE VoteTypeId IN (1, 2, 5)
        GROUP BY PostId, VoteDate
        ) AS VotesDates
        GROUP BY VotesDates.PostId
        HAVING NewVotes > OldVotes
        ) AS VotesByAge ON Posts.Id = VotesByAge.PostId
        WHERE Title NOT IN ('')
        ORDER BY Votes DESC
        LIMIT 10") -> df5
  return(df5)
}
df_sql_5(Posts, Votes)

#ROZWIĄZANIE 2 - base

df_base_5 <- function(Posts, Votes){
  Votes$CreationDate <- format(as.Date(Votes$CreationDate), format = '%Y')
  Votes$CreationDate<- ifelse(Votes$CreationDate == 2021, 'new', ifelse(Votes$CreationDate == 2020, 'new', 'old'))
  colnames(Votes)[colnames(Votes) == "CreationDate"] <- "VoteDate"
  VotesDates <- as.data.frame(table(Votes[Votes$VoteTypeId == 1 |Votes$VoteTypeId == 2 |Votes$VoteTypeId == 5, c("PostId", "VoteDate")]), stringAsFactors = FALSE)
  colnames(VotesDates)[3] <- "Total"
  VotesByAge <- cbind(VotesDates, allnewvotes = ifelse(VotesDates$VoteDate == 'new', VotesDates$Total, 0))
  VotesByAge <- cbind(VotesByAge, alloldvotes = ifelse(VotesByAge$VoteDate == 'old', VotesByAge$Total, 0))
  newvotes <- aggregate(VotesByAge$allnewvotes, VotesByAge["PostId"], max, na.rm = TRUE)
  colnames(newvotes)[2] <- "NewVotes"
  oldvotes <- aggregate(VotesByAge$alloldvotes, VotesByAge["PostId"], max, na.rm = TRUE)
  colnames(oldvotes)[2] <- "OldVotes"
  VotesByAge <- merge(VotesByAge, newvotes, by = "PostId")
  VotesByAge <- merge(VotesByAge, oldvotes, by = 'PostId')
  votes <- aggregate(VotesByAge$Total, VotesByAge["PostId"], FUN = sum)
  colnames(votes)[2] <- "Votes"
  VotesByAge <- merge(VotesByAge, votes, by = "PostId")
  VotesByAge <- VotesByAge[VotesByAge$NewVotes > VotesByAge$OldVotes, ]
  VotesByAge <- data.frame(VotesByAge$PostId, VotesByAge$NewVotes, VotesByAge$OldVotes, VotesByAge$Votes)
  colnames(VotesByAge) <- c("PostId", "NewVotes", "OldVotes", "Votes")
  zmergowany <- merge(Posts, VotesByAge, by.x = "Id", by.y = "PostId")
  zmergowany <- zmergowany[which(zmergowany$Title != ('')), ]
  zmergowany <- zmergowany[order(zmergowany$Votes, decreasing = TRUE), ]
  zmergowany$CreationDate <- format(as.Date(zmergowany$CreationDate), format = '%Y-%m-%d')
  colnames(zmergowany)[colnames(zmergowany) == "CreationDate"] <- "Date"
  wynik <- data.frame(zmergowany$Title, zmergowany$Id, zmergowany$Date, zmergowany$Votes)
  colnames(wynik) <- c("Title", "Id", "Date", "Votes")
  wynik <- unique(wynik)
  rownames(wynik) <- NULL
  return(head(wynik, 10))
}

#ROZWIĄZANIE 3 - dplyr

df_dplyr_5 <- function(Posts, Votes){
  df <- filter(Votes, (VoteTypeId == 1| VoteTypeId == 2|VoteTypeId == 5))
  df$CreationDate <- year(strptime(df$CreationDate, "%Y"))
  VotesDates <- df%>%select(PostId, CreationDate)%>%mutate(
    VoteDate = 
      case_when(
        CreationDate == "2021" ~ "new",
        CreationDate == "2020" ~ "new",
        TRUE ~ "old"
      )
  )%>% select("PostId", "VoteDate")%>%
    count(PostId, VoteDate, name = "Total")%>%
    mutate( NewVotes =
            case_when(
                      VoteDate == "new" ~ Total,
                      TRUE ~ Total - Total
                     )
    )%>%mutate(OldVotes =
                 case_when(
                   VoteDate == "old" ~ Total,
                   TRUE ~ Total - Total
                 )
    )
  VotesByAge <- VotesDates%>%select("PostId", "NewVotes", "OldVotes", "Total")%>%
    group_by(PostId)%>%summarise(NewVotes = max(NewVotes), OldVotes = max(OldVotes), 
                                                          Votes = sum(Total))%>%
    filter(NewVotes > OldVotes)
  df2 <- select(Posts, c("Title", "Id", "CreationDate"))
  df2$CreationDate <- as.character(strptime(df2$CreationDate, "%Y-%m-%d"))
  df2 <- rename(df2, Date = "CreationDate")
  VotesByAge <- select(VotesByAge, c("Votes", "PostId"))
  VotesByAge <- inner_join(df2, VotesByAge, by = c("Id" = "PostId"))%>%
    filter(Title != "")%>%
   arrange(desc(Votes))
  return(VotesByAge[1:10, ])
  
}

#ROZWIĄZANIE 4 - data.table

df_table_5 <- function(Posts, Votes){
  VotesDates <- as.data.table(Votes)
  VotesDates <- VotesDates[VoteTypeId %in% c("1", "2", "5")
   , .(PostId, CreationDate)]
  VotesDates$CreationDate <- year(strptime(VotesDates$CreationDate, format = "%Y"))
  VotesByAge <- as.data.table(VotesDates[, VoteDate := fifelse(
    CreationDate==2020|CreationDate==2021, "new", "old")])[
      , .N, by = c("PostId", "VoteDate")]
  setnames(VotesByAge, "N", "Total")
  VotesByAge <- as.data.table(VotesByAge[, NewVotes := fifelse(VoteDate == "new", Total, 0)])
  VotesByAge <- as.data.table(VotesByAge[, OldVotes := fifelse(VoteDate == "old", Total, 0)])[
    , .(PostId, Total, NewVotes, OldVotes)][, max(NewVotes), .(PostId, OldVotes, Total)]
  setnames(VotesByAge, "V1", "NewVotes")
  VotesByAge <- VotesByAge[, max(OldVotes), .(PostId, NewVotes, Total)]
  setnames(VotesByAge, "V1", "OldVotes")
  VotesByAge <- VotesByAge[, sum(Total), .(NewVotes, OldVotes, PostId)]
  setnames(VotesByAge, "V1", "Votes")
  VotesByAge <- VotesByAge[which(NewVotes > OldVotes), .(Votes, PostId)]
  Posts <- as.data.table(Posts)[, .(Title, Id, CreationDate)]
  Posts$CreationDate <- as.character(strptime(Posts$CreationDate, format = "%Y-%m-%d"))
  setnames(Posts, "CreationDate", "Date")
  wynik <- (merge.data.table(Posts, VotesByAge, by.x = "Id",by.y = "PostId")[which(Title != ''), ])
  return(setorder(wynik, -Votes)[1:10, ])
}


