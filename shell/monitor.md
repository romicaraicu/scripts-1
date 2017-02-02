# File monitor

`inotifywait` is a very nice tool that emits stream of events that happen to
files in your project. That can help you setup a simple monitoring script that
will trigger the build every time your change certain files. In my case it is F#
files I am after:

```{.bash .numberLines}
#!/usr/bin/env bash
while true; do
  inotifywait -q -r -e moved_to,moved_from . | while read path events file; do
    if [[ $file =~ .fs$ ]]; then
      echo "$file has changed in $path, triggering build ..."
      sleep 5
      ./build.sh;
    fi
  done
done
```

> Here at line `#4` you can see my filter to only test for `F#` files. You can
> generalize it a bit to accept a parameter.

One observation that you might have is the type of events that `inotifywait` is
asked to emit: `moved_to` and `moved_from`. Most of the modern text editors do
not change files, but instead create a new temporary files and move it over the
existing file. This might explain the problem that you do not see `close_write`
events for the files you are interested in, but rather for some cryptic number
based file names.

Let's now try to generalize it a bit. **First**, it will need to accept number
of extension names that should trigger build. **Second**, it will need to accept
arbitrary command to evaluate as a build step. And **last**, it will need to
accept the delay value (a number in seconds) to indicate how long should it wait
before triggering supplied command. Here is how it might look now:

```{.bash}
#!/usr/bin/env bash

if [[ "$#" -ne 3 ]]; then
  echo "Expected arguments are missing: EXTENSIONS DELAY COMMAND"
  exit 1
fi

IFS=',' read -r -a exts <<< "$1"
delay=$2
cmd=$3

while true; do
  inotifywait -q -r -e moved_to,moved_from ./ | while read path events file; do

    for ext in "${exts[@]}"; do
      if [[ $file =~ .$ext$ ]]; then
        echo "$file has changed in $path, triggering build ..."
        sleep $delay
        eval $cmd
      fi
    done

  done
done
```

> Notice that extensions should be passed to the script as comma-separated line,
> e.g. `fs,cs,sh`.

And now here is how you can call it for your project:

```{.bash}
$ monitor.sh fs,cs 3 ./build.sh
```

Happy hacking!
