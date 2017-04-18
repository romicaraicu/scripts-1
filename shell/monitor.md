# File monitor

> `inotifywait` is not used any longer. The reason to switch it out was that
> when piping events to `while read` loop commands stopped producing output in
> color. This made it a bit uninteresting. Thus, `monitor.sh` was re-implemented
> using standard tools like `find`, `stat`, `sort`, `head`, etc. that are
> available in most of distributions. So, this time around there is absolutely
> no need to install anything extra to make this file monitoring solution to
> work.

The idea behind this file monitor solution is to invoke arbitrary command when
it detected that certain files changed. Here is how you might want to execute
it:

```{.bash}
$ monitor.sh fs,cs 3 ./build.sh
```

> Notice that extensions should be passed to the script as comma-separated line,
> e.g. `fs,cs,sh`.

This will fetch most recent file changed time out of all C# and F# files found
in the current folder, and if something changed it will execute supplied command
-- `./build.sh` in this case. In between of probing for last changed timestamps
it will wait for `3` seconds (this is what the second argument is for).

Core implementation is plain simple:

```{.bash .numberLines}
last_changed="find ./ -type f \( "
for ext in "${exts[@]}"; do
  last_changed="$last_changed -iname \"*.$ext\" -o"
done
last_changed=${last_changed::-2}
last_changed="$last_changed \) -exec stat -c \"%Y\" {} ';' | sort -r | head -n 1"

t=0
while true; do
  x=$(eval $last_changed)
  if [ -z "$x" ]; then x=0; fi
  if [[ $x > $t ]]; then
    t=$x
    run
  fi
  sleep $delay
done
```

The first thing that happens here is the construction of the command that will
fetch last changed timestamp for all the supplied file extensions. It is using
standard `find` util. Then goes the loop that probes if last changed timestamp
had changed since last time it ran and if it did, then executes `run` function
that wraps the call to `./build.sh` essentially.

Hope this will be helpful for those using Vim/Emacs for hacking around!

Happy hacking!
