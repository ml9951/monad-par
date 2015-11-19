
source environment

for bench in mandel nbody queens
do
    if [ $bench == "mandel" ]; then
	args="1024 1024 256"
    elif [ $bench == "nbody" ]; then
	args="13000"
    elif [ $bench == "queens" ]; then
	args="monad 14"
    elif [ $bench == "sumeuler" ]; then
	args="38 8000 100"
    else
	echo "benchmark not recognized"
	exit 1
    fi

    for stm in fine tl2 orig chase-lev
    do
	make $bench STM=$stm GHC=$GHC
    done

    if [ ! -d "$/bench/times" ]; then
	mkdir "$bench/times"
    fi
    
    for i in {1..5}
    do
	./bench.sh $bench/times/times-$i.txt $bench "$args"
    done
    
done




