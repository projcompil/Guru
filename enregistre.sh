n="$3"

for (( c=1; c<=$[n]; c++ )) 
do
	./main.native $1 $2 >> results.txt
done
