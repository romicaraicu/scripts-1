#!/usr/bin/env bash

command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v exif >/dev/null 2>&1 || { echo >&2 "[exif] is required, but not installed.  Aborting."; exit 1; }
command -v ffprobe >/dev/null 2>&1 || { echo >&2 "[ffprobe] is required, but not installed.  Aborting."; exit 1; }
command -v rsync >/dev/null 2>&1 || { echo >&2 "[rsync] is required, but not installed.  Aborting."; exit 1; }
command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }
command -v date >/dev/null 2>&1 || { echo >&2 "[date] is required, but not installed.  Aborting."; exit 1; }

RED='\033[1;31m'
GREEN='\033[1;32m'
BLUE='\033[1;34m'
NC='\033[0m'
CLREOL=$'\x1B[K'

function usage() {
  echo "Expected arguments are missing: -d|-k SRC_PATH DEST_PATH IMAGE_PREFIX VIDEO_PREFIX"
  echo "    -k    - keeps source files after copying it to destination"
  echo "    -d    - deletes source files after copying it to destination"
}

if [[ "$#" -ne 5 ]]; then
  usage
  exit 1
fi

flag=$1
src=$2
dst=$3
image_prefix=$4
video_prefix=$5

if [ "$flag" != "-d" ]; then
  if [ "$flag" != "-k" ]; then
    echo -e "${RED}ERROR: First argument must be either -k or -d${NC}"
    usage
    exit 1
  fi
fi

if [ ! -d "$src" ]; then
  echo -e "${RED}ERROR: source folder does not exist${NC}"
  exit 1
fi

# ignore case for regex expressions
shopt -s nocasematch

images="jpeg|jpg"
videos="mp4|mpg|wmv|avi|lrv|mov"

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
      dt=$(exif --tag=DateTimeDigitized --no-fixup "$p" 2>> mimport-errors.log | grep -i value | sed 's/[^0-9]//g')
      prefix=$image_prefix
      color=$GREEN
      is_media_file="true"
    elif [[ "$extension" =~ $videos ]]; then
      dt=$(ffprobe -v quiet -print_format flat -show_format "$p" 2>> mimport-errors.log | grep creation_time | cut -d= -f2- | sed 's/[^0-9]//g')
      prefix=$video_prefix
      color=$BLUE
      is_media_file="true"
    fi
    if [ "$is_media_file" == "true" ]; then
      if [ "${#dt}" -ne "14" ]; then
        dtpath="_NOTDATED_"
      else
        dtpath="${dt:0:4}/${dt:4:2}/${dt:6:2}"
      fi
      if [[ -z "${prefix// }" ]]; then
        target="${dst%/}/$dtpath/"
      else
        target="${dst%/}/${prefix// }/$dtpath/"
      fi
      files=$((files+1))
      pct=$(printf "% 4.0f" $((files * 100 / total_files)))
      echo -ne "\r[$pct % ]  ${color}$target${NC} << ${color}${filename:0:20}${NC} ${CLREOL}"
      echo -e "[$pct % ]  ${color}$filename${NC} >> ${color}$target${NC} " >> mimport.log
      mkdir -p "$target" > /dev/null
      rsync --size-only "$p" "$target"
      exit_code=$?
      if [ "$exit_code" -ne 0 ]; then
        echo -e "${RED}ERROR: cannot rsync \"$p\" \"$target\"${NC}"
        echo -e "${RED}ERROR: cannot rsync \"$p\" \"$target\"${NC}" >> mimport-errors.log
        # exit $exit_code
      else
        if [ "$flag" == "-d" ]; then
          rm "$p" > /dev/null
          exit_code=$?
          if [ "$exit_code" -ne 0 ]; then
            echo -e "${RED}ERROR: cannot rm \"$p\"${NC}"
            echo -e "${RED}ERROR: cannot rm \"$p\"${NC}" >> mimport-errors.log
            # exit $exit_code
          fi
        fi
      fi
    fi
  done
  echo
}

echo "Started on $(date)" > mimport.log
echo "Started on $(date)" > mimport-errors.log
total_files=$(find $src -type f -print | count_files)
find $src -type f -print | process_files
echo "Finished on $(date)" >> mimport.log
echo "Started on $(date)" >> mimport-errors.log
