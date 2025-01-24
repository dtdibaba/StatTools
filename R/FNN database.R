library(odbc)
library(DBI)
library(dplyr)
library(ini)
library(fs)
library("RPostgres")
library(ssh)


# Step 1: Establish SSH Tunnel
home_path <- path.expand(path_home())
database_information <- read.ini(paste0(home_path, "/db.ini"), encoding = getOption("encoding"))
database <- paste0(database_information$dataset$database)
host_db <- paste0(database_information$dataset$host)
db_port <- paste0(database_information$dataset$port)
db_user <- paste0(database_information$dataset$username)
ssh_host <- paste0(database_information$dataset$ssh_host)
db_password <- database_information$dataset$password
ssh_password <- paste0(database_information$dataset$ssh_password)

# https://superuser.com/questions/1643961/how-can-i-connect-to-postgresql-database-on-a-server-through-an-ssh-tunnel-using

# ssh -i "~/.ssh/id_rsa" -L 5555:127.0.0.1:5432 fnn@104.236.11.136
# ssh_agent_add(paste0(path.expand(path_home()), "/.ssh/id_rsa"))
# session <- ssh_connect('fnn@104.236.11.136:22', keyfile = '~/.ssh/id_rsa', passwd =ssh_password)
# tunnel <- ssh_tunnel(session, port=5555, target=paste0(database_information$dataset$host,':', database_information$dataset$port))
# ssh_disconnect(session=session)
con <- dbConnect(Postgres(),
    dbname = database,
    host = host_db,
    port = db_port,
    user = db_user,
    password = db_password
)

post <- dbGetQuery(con, "SELECT title FROM blog_post")
