#!/bin/sh
#
# Wrapper to create movies from images
# Adapted from the one I found in $MESASDK_ROOT

set -o noglob

in_files=$1
echo 'Taking' $in_files
out_file=$2
echo 'Making' $out_file

$MESASDK_ROOT/bin/ffmpeg \
    -loglevel warning \
    -pattern_type glob -i $in_files \
    -pix_fmt yuv420p \
    -vf 'crop=trunc(iw/2)*2:trunc(ih/2)*2:0:0' \
    -y \
    $out_file

echo 'Done.'
