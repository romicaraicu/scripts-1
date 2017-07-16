# Media file importer

`mimport.sh` script is helping to import media files into a folder
structure with following pattern: `$TARGET\(videos|photos)\YEAR\MONTH\DAY`.

Supported formats:

* Images: `jpeg`, `jpg`
* Videos: `mp4`, `mpg`, `wmv`, `avi`, `lrv`, `mov`

> I was always after keeping my media organized in a simple way like that, but
> could not find simple tool that would work all the time in different
> situations. This script is as simple as it just might get.

Example of synchronizing files as well as backing it up onto mirror disc:

* Source of files - `~/Photoes`
* Destination (disc #1) - `/run/media/kuznero/archive-1`
* Mirror (disc #2) - `/run/media/kuznero/archive-2`

```{.bash}
mimport.sh -k ~/Photoes/ /run/media/kuznero/archive-1/ photos videos
mimport.sh -d ~/Photoes/ /run/media/kuznero/archive-1/ photos videos
rsync -avh /run/media/kuznero/archive-1/ /run/media/kuznero/archive-2/
```
