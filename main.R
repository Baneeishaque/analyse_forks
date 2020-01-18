library(gh)
library(plyr)

# MY_GITHUB_PAC is a personal access token you generate on GitHub
# https://github.com/settings/tokens Supplying this parameter just means
# your daily rate limit will grow from 60 to 5000 You can also drop that
# argument altogether
MY_GITHUB_PAC <- "16392d5bf9af9c7401d8033291b873eae1e85f0c"

forks_new_commits <- function(owner, repo) {
    
    # Get all forks of the repo
    all_forks <- gh("GET /repos/:owner/:repo/forks", owner = owner, repo = repo, 
        .limit = Inf, .token = MY_GITHUB_PAC)
    
    cat("Looks like ", owner, "/", repo, " has ", length(all_forks), " forks\n")
    
    forks_info <- plyr::ldply(all_forks, function(fork) {
        fork_owner <- fork$owner$login
        fork_repo <- fork$name
        fork_full_name <- paste0(fork_owner, "/", fork_repo)
        
        tryCatch({
            
            # Get the last 50 commits on the fork
            fork_repo <- gh("GET /repos/:owner/:repo/commits", owner = fork_owner, 
                repo = fork_repo, .limit = 50, .token = MY_GITHUB_PAC)
            
            # Get the number of commits not by the original owner
            num_new_commits <- sum(unlist(lapply(fork_repo, function(x) {
                x$author$login
            })) != owner)
            
            if (num_new_commits > 0) {
                cat(fork_full_name, " ", num_new_commits, " commits\n", 
                  sep = "")
                data.frame(fork = fork_full_name, num_new_commits = num_new_commits, 
                  url = fork$html_url)
            }
            
        }, error = function(e) {
            paste0("Error : ", e)
        })
    })
    
    dplyr::arrange(forks_info, desc(num_new_commits))
}

# forks_new_commits("mayswind", "AriaNg")
forks_new_commits("Baneeishaque","azad-gold-and-diamonds-website")
