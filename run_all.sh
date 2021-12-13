# build from clean
make all

# run default case
./particle nx=100 ny=100 problem=single

# generate plots
python3 plot_particle.py
