#!/usr/bin/env bash

command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v exif >/dev/null 2>&1 || { echo >&2 "[exif] is required, but not installed.  Aborting."; exit 1; }
command -v ffprobe >/dev/null 2>&1 || { echo >&2 "[ffprobe] is required, but not installed.  Aborting."; exit 1; }
command -v rsync >/dev/null 2>&1 || { echo >&2 "[rsync] is required, but not installed.  Aborting."; exit 1; }
command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }

if [[ "$#" -ne 4 ]]; then
  echo "Expected arguments are missing: SRC_PATH DEST_PATH IMAGE_PREFIX VIDEO_PREFIX"
  exit 1
fi

src=$1
dst=$2
image_prefix=$3
video_prefix=$4

# ignore case for regex expressions
shopt -s nocasematch

images="jpeg|jpg"
videos="mp4|mpg|wmv|avi"

RED='\033[1;31m'
GREEN='\033[1;32m'
BLUE='\033[1;34m'
NC='\033[0m'

function count_files() {
  total_files=0
  while read p; do
    filename=$(basename "$p")
    extension="${filename##*.}"
    is_media_file="false"
    if [[ "$extension" =~ $images ]]; then
      total_files=$((total_files+1))
    elif [[ "$extension" =~ $videos ]]; then
      total_files=$((total_files+1))
    fi
  done
  echo $total_files
}

total_files=0

function process_files() {
  pct=0
  files=0
  while read p; do
    filename=$(basename "$p")
    extension="${filename##*.}"
    is_media_file="false"
    if [[ "$extension" =~ $images ]]; then
      dt=$(exif --tag=DateTimeDigitized --no-fixup "$p" | grep -i value | sed 's/[^0-9]//g')
      prefix=$image_prefix
      color=$GREEN
      is_media_file="true"
    elif [[ "$extension" =~ $videos ]]; then
      dt=$(ffprobe -v quiet -print_format flat -show_format "$p" | grep creation_time | cut -d= -f2- | sed 's/[^0-9]//g')
      prefix=$video_prefix
      color=$BLUE
      is_media_file="true"
    fi
    if [ "$is_media_file" == "true" ]; then
      if [ "${#dt}" -ne "14" ]; then
        dtpath="UNKNOWN"
      else
        dtpath="${dt:0:4}/${dt:4:2}/${dt:6:2}"
      fi
      if [[ -z "${prefix// }" ]]; then
        target="${dst%/}/$dtpath/"
      else
        target="${dst%/}/${prefix// }/$dtpath/"
      fi
      mkdir -p "$target" > /dev/null
      rsync --size-only "$p" "$target"
      exit_code=$?
      if [ "$exit_code" -ne 0 ]; then
        echo -e "${RED}ERROR: cannot rsync \"$p\" \"$target\"${NC}"
        exit $exit_code
      fi
      files=$((files+1))
      pct=$(printf "% 4.0f" $((files * 100 / total_files)))
      echo -e "[$pct % ]  ${color}$filename${NC} >> ${color}$target${NC} "
    fi
  done
}

total_files=$(find $src -type f -print | count_files)
find $src -type f -print | process_files
