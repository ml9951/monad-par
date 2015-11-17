


for bench in mandel nbody
do
    if [ $bench == "mandel" ]; then
	args="1024 1024 256"
    elif [ $bench == "nbody" ]; then
	args="13000" 
    else
	echo "benchmark not recognized"
	exit 1
    fi

    for stm in fine tl2 orig
    do
	make $bench STM=$stm
    done
    
    for i in {1..5}
    do
	./bench.sh $bench/times/times-$i.txt $bench "$args"
    done
    
done




