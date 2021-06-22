#!/bin/bash

did=1002
err=

random_url() {
    len=${#collection_ids[@]}
    ind=$((RANDOM % $len))
    id=${collection_ids[$ind]}
    echo $collection_url | sed "s/XXX/$id/g"
}

derr() {
    a=`dunstify -p -r $did "X  Wallpaper" "$1"`
    err=1
}
dmsg() {
    a=`dunstify -p -r $did "Wallpaper" "$1"`
    err=
}

collection_ids=(852718 1052683 1457745 1386982 1772170 349213 365720 488182 211 935527 1089430)

wallpaper_dir=/home/bibek/Pictures/wallpapers/unsplash/
collection_url=https://source.unsplash.com/collection/XXX/1920x1080
rand_url=https://source.unsplash.com/random/1920x1080 # not used now

mkdir -p $wallpaper_dir

dmsg "Downloading ..."
location=$((curl -i  $(random_url) || derr "Download failed") | grep Location | sed -e 's/Location: //g')
# location contains trailing \r(don't know why), removing it below
location=${location%$'\r'}  # don't know how this works as well
if [[ -z $err ]]; then
    a=b # do nothing
else
    exit 1;
fi

image_name=`echo $location | cut -d'/' -f 4 | cut -d'?' -f 1`
image_path=$wallpaper_dir$image_name.jpg

if [ ! -f $image_path ]; then
    # get the image
    (curl "$location" --output $image_path) || (derr "Download failed")
    if [[ -z $err ]]; then
        a=b # do nothing
    else
        exit 1
    fi
    dmsg "Downloaded $image_path"
fi
