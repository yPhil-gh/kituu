image=( image*.png )
MAX=${#image[*]}
for i in ${image[*]}
do
   num=${i:5:3} # grab the digits
   compliment=$(printf '%03d' $(echo $MAX-$num | bc))
   cp $i copy_of_image$compliment.png
done
