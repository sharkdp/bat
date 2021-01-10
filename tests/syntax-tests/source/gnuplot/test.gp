set terminal pngcairo enhanced
set output "/tmp/polynomial.png"

set grid

set xrange [-5:5]
set yrange [-5:10]

set samples 10000

set key bottom right

f(x) = 1.0 / 14.0 * ((x+4) * (x+1) * (x-1) * (x-3)) + 0.5

plot \
    f(x) title "polynomial of degree 4" \
    with lines \
    linewidth 2 \
    linetype rgb '#0077ff'
