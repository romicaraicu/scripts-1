# Scripts

Various scripts to make my life easier :)

## Media file importer

`mimport.sh` script is helping to import media files into a folder
structure with following pattern: `$TARGET\(videos|photos)\YEAR\MONTH\DAY`.

Supported formats:

* Images: `jpeg`, `jpg`
* Videos: `mp4`, `mpg`, `wmv`, `avi`, `lrv`, `mov`

> I was always after keeping my media organized in a simple way like that, but
> could not find simple tool that would work all the time in different
> situations. This script is as simple as it just might get.

## Synchronizing Git repositories

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
