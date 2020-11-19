gcc -Ofast -I../sim -o mcsim_lin2 ../sim/*.c model.c -lm -lgsl -lgslcblas -lsundials_cvodes -lsundials_nvecserial -llapack
