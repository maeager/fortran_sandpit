CC=nvcc
CFLAGS=-Xcompiler -fopenmp
LIBS=-lgomp
all: add mul

add: add.o
	$(CC) -o $@ $< $(LIBS)

add.o: add.cu
	$(CC) -g -c $< -o $@ $(CFLAGS)

mul: mul.o
	$(CC) -o $@ $< $(LIBS)

mul.o: mul.cu
	$(CC) -g -c $< -o $@ $(CFLAGS)


clean:
	rm -rf add add.o mul mul.o

run:
	tau_exec -T serial,cupti,openmp -cupti ./add
