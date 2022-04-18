# Tytuł: praca domowa numer 2
# Autor: Jan Skwarek
# Numer indeksu: 305734
# Data: 14.05.2021, 20:00

# Wczytujemy potrzebne biblioteki.
library(sqldf)
library(dplyr)
library(data.table)
Tags <- read.csv("/home/jan/Downloads/Tags.csv")
Posts <- read.csv("/home/jan/Downloads/Posts.csv")
Users <- read.csv("/home/jan/Downloads/Users.csv")
Votes <- read.csv("/home/jan/Downloads/Votes.csv")
Badges <- read.csv("/home/jan/Downloads/Badges.csv")
PostLinks <- read.csv("/home/jan/Downloads/PostLinks.csv")
Comments <- read.csv("/home/jan/Downloads/Comments.csv")

# -----------------------------------------------------------------------------
# ZADANIE 1
# -----------------------------------------------------------------------------

# Pierwszy sposób. Rozwiązanie referencyjne. Wystarczy przekopiować kod w SQL.
df_sql_1 <- function(Tags){
  sqldf::sqldf("SELECT TagName, Count FROM Tags ORDER BY Count DESC LIMIT 10")
}

# Drugi sposób. Użyjemy w nim tylko i wyłącznie funkcji bazowych. Najpierw
# tworzymy data frame (df1) z kolumnami, które nas tylko interesują (czwarta
# i pierwsza w nieprzypadkowej kolejności), potem za pomocą funkcji order
# sortujemy data frame'a po kolumnie Count w kolejności od największej do 
# najmniejszej. Następnie zwracamy 10 wyników z góry.
df_base_1 <- function(Tags){
  df1 <- Tags[c(4, 1)]
  df2 <- df1[order(df1$Count, decreasing = TRUE),]
  row.names(df2) <- NULL
  head(df2, 10)
}

# Trzeci sposób. Używamy bibliotek dplyr. Najpierw wybieramy interesujące nas
# kolumny, a następnie używamy slice_max aby wypisać n rzędów z największą
# wartością Count.
df_dplyr_1 <- function(Tags){
  Tags %>%
    select(TagName, Count) %>%
    # arrange(desc(Count)) %>%
    slice_max(Count, n = 10)
}

# Czwarty sposób. Używamy pakietu data.table.
df_table_1 <- function(Tags){
  df3 <- Tags[c(4, 1)]
  df4 <- df3[order(df3$Count, decreasing = TRUE),]
  row.names(df4) <- NULL
  head(df4, 10)
}

# -----------------------------------------------------------------------------
# ZADANIE 2
# -----------------------------------------------------------------------------

# Pierwszy sposób. Rozwiązanie referencyjne. Wystarczy przekopiować kod w SQL.
df_sql_2 <- function(Posts, Users){
  sqldf::sqldf("SELECT Users.DisplayName, Users.Age, Users.Location,
                       AVG(Posts.Score) as PostsMeanScore,
                       MAX(Posts.CreationDate) AS LastPostCreationDate
                FROM Posts
                JOIN Users ON Users.AccountId=Posts.OwnerUserId
                WHERE OwnerUserId != -1
                GROUP BY OwnerUserId
                ORDER BY PostsMeanScore DESC
                LIMIT 10")
}

# -----------------------------------------------------------------------------
# ZADANIE 3
# -----------------------------------------------------------------------------

# Pierwszy sposób. Rozwiązanie referencyjne. Wystarczy przekopiować kod w SQL.
df_sql_3 <- function(Users, Posts){
  sqldf::sqldf("SELECT DisplayName, QuestionsNumber, AnswersNumber
                FROM
                    (
                        SELECT COUNT(*) as AnswersNumber, Users.DisplayName, Users.Id
                        FROM Users JOIN Posts ON Users.Id = Posts.OwnerUserId
                        WHERE Posts.PostTypeId = 1
                        GROUP BY Users.Id
                    ) AS Tab1
                JOIN
                    (   SELECT COUNT(*) as QuestionsNumber, Users.Id
                        FROM Users JOIN Posts ON Users.Id = Posts.OwnerUserId
                        WHERE Posts.PostTypeId = 2
                        GROUP BY Users.Id
                    ) AS Tab2
                    ON Tab1.Id = Tab2.Id
                WHERE QuestionsNumber < AnswersNumber
                ORDER BY AnswersNumber DESC")
}

# -----------------------------------------------------------------------------
# ZADANIE 4
# -----------------------------------------------------------------------------

# Pierwszy sposób. Rozwiązanie referencyjne. Wystarczy przekopiować kod w SQL.
df_sql_4 <- function(Posts, Comments){
  sqldf::sqldf("SELECT
                    Posts.Title, Posts.CommentCount,
                    CmtTotScr.CommentsTotalScore,
                    Posts.ViewCount
                FROM (
                        SELECT
                            PostID,
                            UserID,
                            SUM(Score) AS CommentsTotalScore
                        FROM Comments
                        GROUP BY PostID, UserID
                ) AS CmtTotScr
                JOIN Posts ON Posts.ID=CmtTotScr.PostID
                WHERE Posts.PostTypeId=1
                ORDER BY CmtTotScr.CommentsTotalScore DESC
                LIMIT 10")
}

# -----------------------------------------------------------------------------
# ZADANIE 5
# -----------------------------------------------------------------------------

# Pierwszy sposób. Rozwiązanie referencyjne. Wystarczy przekopiować kod w SQL.
df_sql_5 <- function(Posts){
  sqldf::sqldf("SELECT
                    Questions.Id,
                    Questions.Title,
                    BestAnswers.MaxScore,
                    Posts.Score AS AcceptedScore,
                    BestAnswers.MaxScore-Posts.Score AS Difference
                FROM (
                        SELECT Id, ParentId, MAX(Score) AS MaxScore
                        FROM Posts
                        WHERE PostTypeId==2
                        GROUP BY ParentId
                     ) AS BestAnswers
                JOIN (
                        SELECT * FROM Posts
                        WHERE PostTypeId==1
                    ) AS Questions
                    ON Questions.Id=BestAnswers.ParentId
                JOIN Posts ON Questions.AcceptedAnswerId=Posts.Id
                ORDER BY Difference DESC
                LIMIT 10")
}

