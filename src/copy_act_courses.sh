#/bin/sh

if [ -z $1 ]; then
	udisksctl mount -b /dev/sda
else
	udisksctl mount -b /dev/$1
fi

printf "\nCopying ACTIVITY files...\n"
SRC_PTH="/run/Parts/Exch/Sync/swap/R/Garmin/data/ACTIVITY"
GAR_PTH="/run/media/ar/GARMIN/GARMIN/ACTIVITY"

for i in $(ls $GAR_PTH)
do
	if [[ ! -e $SRC_PTH/$i ]]; then
		printf '   Fetching %s...\n' "$i"
		cp $GAR_PTH/$i $SRC_PTH/../f
	fi
done

printf "\nCopying COURSES files...\n\n"
SRC_PTH_C="/run/Parts/Exch/Sync/swap/R/Garmin/data/COURSES"
GAR_PTH_C="/run/media/ar/GARMIN/GARMIN/COURSES"

for i in $(ls $GAR_PTH_C)
do
	if [[ ! -e $SRC_PTH_C/$i ]]; then
		printf '   Fetching %s...\n' "$i"
		cp $GAR_PTH_C/$i $SRC_PTH_C
	fi
done
echo

if [ -z $1 ]; then
	udisksctl unmount -b /dev/sda
else
	udisksctl unmount -b /dev/$1
fi
