#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

FILE *archivo;

int main(int argc, char **argv)
{
    int i, j, k;
    int tam_vector = atoi(argv[1]);
    long long int tam_matriz = tam_vector * tam_vector;
    float *a = (float *) malloc (tam_vector * sizeof(float));
    float *b = (float *) malloc (tam_vector * sizeof(float));
    float *c = (float *) malloc (tam_vector * sizeof(float));

    /* Asignamos el numero de hilos deseados */
    omp_set_num_threads(atoi(argv[2]));
    //printf("Hilos: %d\nTamano: %d\n", atoi(argv[2]), tam_vector);
    /* Se llenan los vectores con valores iniciales aleatorios */
    #pragma omp parallel 
    {
        #pragma omp for
        for (i = 0; i < tam_vector; ++i)
        {
            a[i] = rand() / (float) RAND_MAX;
            b[i] = rand() / (float) RAND_MAX;
            c[i] = a[i] + b[i];
        }    
    }


    // Guarda el vector suma en un archivo de salida
    
    // for (i = 0; i < tam_vector; ++i)
    // {
    //     fprintf(archivo, "%2.4f \n", c[i]);
    // }
    // fclose(archivo);
    return 0;
}
