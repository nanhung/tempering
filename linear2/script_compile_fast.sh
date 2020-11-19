#sudo apt install build-essential gdb cmake pkg-config
#sudo apt install libgsl-dev
#sudo apt install libatlas-base-dev
#wget https://computing.llnl.gov/projects/sundials/download/sundials-3.1.2.tar.gz
#mkdir sundials
#mv sundials-3.1.2.tar.gz sundials
#cd sundials 
#tar -xzvf sundials-3.1.2.tar.gz
#cmake sundials-3.1.2
#make
#sudo make install

gcc -Ofast -I../sim -o mcsim_lin2 ../sim/*.c model.c -lm -lgsl -lgslcblas -lsundials_cvodes -lsundials_nvecserial -llapack
