# The following SSH did not work
session <- ssh_connect(
    paste0(pw$dataset$ssh_user, "@", pw$dataset$ssh_host),
    passwd = "Qabso20Media"
)

cmd <- "ssh::ssh_tunnel(ssh::ssh_connect(host = '104.236.11.136:22'), passwd = 'Qabso20Media'), target ='private-fnn-do-user-8475238-0.b.db.ondigitalocean.com:25060' )"
pid <- sys::r_background(
    std_out = FALSE,
    std_err = FALSE,
    args = c("-e", cmd)
)
pid
cmd <- 'session <- ssh::ssh_connect(paste0(pw$dataset$ssh_user, "@", pw$dataset$ssh_host, ":22");ssh::ssh_tunnel(session, port = 5555, target = "127.0.0.1:5432")'
pid <- sys::r_background(args = c("-e", cmd))
