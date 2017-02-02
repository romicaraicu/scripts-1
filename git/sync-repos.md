# Synchronizing Git repositories

```{.bash}
cat repos.txt | scripts/git/sync-repos.sh git@github.com:kuznero
```

Where `repos.txt` contains list of repositories to synchronize, i.e. to clone if
it has not been cloned yet or pull recent changes - otherwise. Here is how
`repos.txt` might look like:

```{.plain}
dotfiles
scripts
```

> `sync-repos.sh` script will also try to `ssh-add` your current key if it will
> detect that it is not yet active.
